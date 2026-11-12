#' Get audit data from REDCap
#'
#' Reads patient data from REDCap using the `ikdds` package, then selects
#' and transforms the relevant fields for the dashboard.
#'
#' @param config A `dashboard_config` object with REDCap credentials.
#'
#' @return A tibble in the same format as [generate_synthetic_data()].
#'
#' @keywords internal
get_redcap_audit_data <- function(config) {
  if (!requireNamespace("ikdds", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg ikdds} package is required for REDCap data access.",
      "i" = "Install with: {.code devtools::install('../ikdds_r/ikdds')}"
    ))
  }

  # --- TEMPORARY CACHE — remove before production ---
  cache_path <- "C:/Maya/raw_data.rds"
  if (file.exists(cache_path)) {
    cli::cli_inform("Loading cached REDCap data from disk...")
    raw_data <- readRDS(cache_path)
  } else {
    ikdds_config <- list(
      redcap_uri   = config$redcap_uri,
      redcap_token = config$redcap_token,
      batch_size   = as.integer(config$batch_size %||% 25L)
    )
    class(ikdds_config) <- c("ikdds_config", "list")
    raw_data <- ikdds::ikdds_redcap_read(
      ikdds_config,
      forms = c("demographics", "diagnoses", "hd_prescription",
                "hd_sessions", "lab_results", "observations")
    )
    saveRDS(raw_data, cache_path)
  }
  # --- END TEMPORARY CACHE ---

  # --- Aggregate all repeating instruments -----------------------------------
  session_summary <- aggregate_sessions(raw_data, freq_days = 90)
  lab_summary     <- aggregate_labs(raw_data)
  obs_summary     <- aggregate_obs(raw_data)
  diag_summary    <- aggregate_diagnoses(raw_data)

  # Columns provided by aggregation summaries — drop from raw non-repeating
  # rows to avoid .x/.y conflicts on join
  summary_cols <- c("dxs01", "hdp01", "hdp02", "qhd20", "is_acute",
                    "qblg9", "qbla9", "qbla4", "qblb1", "qblb4",
                    "qblb9", "qble1", "qblf1", "qblg3", "qblg4",
                    "qblg6", "qblg7")

  df <- raw_data |>
    dplyr::filter(
      !is.na(.data$record_id),
      is.na(.data$redcap_repeat_instrument)
    ) |>
    dplyr::mutate(
      record_id   = as.numeric(.data$record_id),
      age         = calculate_age(.data$idn03),
      gender      = dplyr::case_when(
        .data$pat00 == "Male"   ~ "Male",
        .data$pat00 == "Female" ~ "Female",
        TRUE                    ~ NA_character_
      ),
      ethnicity   = as.character(.data$pat25),
      centre_code = as.character(.data$pat01),
      centre_name = map_centre_code(.data$pat01),
      region      = map_centre_region(.data$pat01),
      unit_code   = as.character(.data$pat01),
      unit_name   = map_centre_code(.data$pat01),
      unit_type   = map_centre_type(.data$pat01),
      consultant  = as.character(.data$pat39)
    ) |>
    # Drop raw versions of columns that come from aggregation summaries
    dplyr::select(-dplyr::any_of(summary_cols)) |>
    dplyr::left_join(diag_summary,    by = "record_id") |>
    dplyr::left_join(session_summary, by = "record_id") |>
    dplyr::left_join(lab_summary,     by = "record_id") |>
    dplyr::left_join(obs_summary,     by = "record_id") |>
    dplyr::mutate(
      is_acute = dplyr::if_else(is.na(.data$is_acute), FALSE, .data$is_acute)
    )

  df |>
    dplyr::select(dplyr::all_of(audit_data_columns()))
}

#' Aggregate diagnoses repeating instrument to one row per patient
#'
#' Picks the first valid ERA-EDTA code (0-99) per patient. Falls back to
#' the first available code if no valid ERA-EDTA code exists.
#'
#' @param raw_data Raw tibble from `ikdds::ikdds_redcap_read()`.
#' @return A tibble with one row per `record_id` and column `dxs01`.
#' @keywords internal
aggregate_diagnoses <- function(raw_data) {
  diag <- raw_data |>
    dplyr::filter(
      !is.na(.data$record_id),
      !is.na(.data$redcap_repeat_instrument),
      .data$redcap_repeat_instrument == "diagnoses",
      !is.na(.data$dxs01),
      .data$dxs01 != ""
    ) |>
    dplyr::mutate(
      record_id  = as.numeric(.data$record_id),
      dxs01_num  = suppressWarnings(as.numeric(.data$dxs01)),
      valid_era  = !is.na(.data$dxs01_num) &
                   .data$dxs01_num >= 0 &
                   .data$dxs01_num <= 99
    )

  # First preference: valid ERA-EDTA codes (0-99)
  valid <- diag |>
    dplyr::filter(.data$valid_era) |>
    dplyr::arrange(.data$record_id, .data$redcap_repeat_instance) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(record_id, dxs01 = as.character(.data$dxs01))

  # Fallback: patients with no valid ERA-EDTA code — take first available
  fallback <- diag |>
    dplyr::filter(!.data$record_id %in% valid$record_id) |>
    dplyr::arrange(.data$record_id, .data$redcap_repeat_instance) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(record_id, dxs01 = NA_character_)  # invalid codes → NA

  dplyr::bind_rows(valid, fallback)
}

#' Aggregate hd_sessions repeating instrument to one row per patient
#'
#' @param raw_data Raw tibble from `ikdds::ikdds_redcap_read()`.
#' @param freq_days Lookback window in days for session frequency. Default 90.
#' @return A tibble with one row per `record_id` and columns
#'   `hdp01`, `hdp02`, `qhd20`, `is_acute`.
#' @keywords internal
aggregate_sessions <- function(raw_data, freq_days = 90) {
  sessions <- raw_data |>
    dplyr::filter(
      !is.na(.data$record_id),
      !is.na(.data$redcap_repeat_instrument),
      .data$redcap_repeat_instrument == "hd_sessions"
    ) |>
    dplyr::mutate(
      record_id    = as.numeric(.data$record_id),
      session_date = as.Date(.data$qhd00, format = "%Y-%m-%d"),
      duration     = safe_numeric(.data$qhd31),
      access_raw   = as.character(.data$qhd20)
    )

  # hdp01: sessions per week — Feb 1 to May 1 2026
  freq_window_end   <- as.Date("2026-05-01")
  freq_window_start <- freq_window_end - freq_days
  freq_summary <- sessions |>
    dplyr::filter(
      !is.na(.data$session_date),
      .data$session_date >= freq_window_start,
      .data$session_date <= freq_window_end
    ) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::summarise(
      hdp01 = as.integer(round(dplyr::n() / (freq_days / 7))),
      .groups = "drop"
    )

  # is_acute: fewer than 90 sessions in audit year (May 1 2025 – May 1 2026)
  acute_window_start <- as.Date("2025-05-01")
  acute_window_end   <- as.Date("2026-05-01")
  acute_summary <- sessions |>
    dplyr::filter(
      !is.na(.data$session_date),
      .data$session_date >= acute_window_start,
      .data$session_date <= acute_window_end
    ) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::summarise(
      sessions_this_year = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::mutate(is_acute = .data$sessions_this_year < 90) |>
    dplyr::select("record_id", "is_acute")

  # hdp02: median actual delivered duration across all sessions
  dur_summary <- sessions |>
    dplyr::filter(!is.na(.data$duration)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::summarise(
      hdp02 = as.integer(round(stats::median(.data$duration, na.rm = TRUE))),
      .groups = "drop"
    )

  # qhd20: modal access type across all sessions
  # Falls back to hdp04 (prescribed access) from hd_prescription if qhd20 is empty
  access_summary <- sessions |>
    dplyr::filter(!is.na(.data$access_raw), .data$access_raw != "") |>
    dplyr::group_by(.data$record_id, .data$access_raw) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::arrange(.data$record_id, dplyr::desc(.data$n)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::mutate(qhd20 = map_access_type(.data$access_raw)) |>
    dplyr::select("record_id", "qhd20")

  # If no session-level access data, fall back to hdp04 (prescribed access)
  hdp04_summary <- raw_data |>
    dplyr::filter(
      !is.na(.data$record_id),
      !is.na(.data$redcap_repeat_instrument),
      .data$redcap_repeat_instrument == "hd_prescription",
      !is.na(.data$hdp04),
      .data$hdp04 != ""
    ) |>
    dplyr::mutate(record_id = as.numeric(.data$record_id)) |>
    dplyr::arrange(.data$record_id, dplyr::desc(.data$redcap_repeat_instance)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      record_id,
      qhd20_fallback = map_access_type(as.character(.data$hdp04))
    )

  # Merge: use session access where available, prescribed access as fallback
  access_summary <- hdp04_summary |>
    dplyr::left_join(access_summary, by = "record_id") |>
    dplyr::mutate(
      qhd20 = dplyr::coalesce(.data$qhd20, .data$qhd20_fallback)
    ) |>
    dplyr::select("record_id", "qhd20")

  freq_summary |>
    dplyr::full_join(dur_summary,    by = "record_id") |>
    dplyr::full_join(access_summary, by = "record_id") |>
    dplyr::full_join(acute_summary,  by = "record_id") |>
    dplyr::mutate(
      is_acute = dplyr::if_else(is.na(.data$is_acute), FALSE, .data$is_acute)
    )
}

#' Aggregate lab_results repeating instrument to one row per patient
#'
#' @param raw_data Raw tibble from `ikdds::ikdds_redcap_read()`.
#' @return A tibble with one row per `record_id` and lab columns.
#' @keywords internal
aggregate_labs <- function(raw_data) {
  labs <- raw_data |>
    dplyr::filter(
      !is.na(.data$record_id),
      !is.na(.data$redcap_repeat_instrument),
      .data$redcap_repeat_instrument == "lab_results"
    ) |>
    dplyr::mutate(record_id = as.numeric(.data$record_id))

  latest_lab <- function(data, value_field, date_field) {
    data |>
      dplyr::select(
        record_id,
        value = dplyr::all_of(value_field),
        date  = dplyr::all_of(date_field)
      ) |>
      dplyr::filter(!is.na(.data$value) & .data$value != "") |>
      dplyr::mutate(date = as.POSIXct(.data$date, format = "%Y-%m-%d %H:%M:%S")) |>
      dplyr::arrange(.data$record_id, dplyr::desc(.data$date)) |>
      dplyr::group_by(.data$record_id) |>
      dplyr::slice(1) |>
      dplyr::ungroup() |>
      dplyr::transmute(
        record_id,
        !!value_field := safe_numeric(.data$value)
      )
  }

  lab_pairs <- list(
    c("qblg9", "qblga"),   # URR
    c("qbla9", "qblaa"),   # Serum potassium
    c("qbla4", "qbla5"),   # Serum bicarbonate
    c("qblb1", "qblb2"),   # Serum phosphate
    c("qblb4", "qblbc"),   # Corrected calcium
    c("qblb9", "qblba"),   # PTH
    c("qble1", "qble2"),   # Haemoglobin
    c("qblf1", "qblf2")    # Serum ferritin
  )

  purrr::reduce(
    purrr::map(lab_pairs, ~ latest_lab(labs, .x[1], .x[2])),
    dplyr::full_join,
    by = "record_id"
  )
}

#' Aggregate observations repeating instrument to one row per patient
#'
#' @param raw_data Raw tibble from `ikdds::ikdds_redcap_read()`.
#' @return A tibble with one row per `record_id` and BP columns.
#' @keywords internal
aggregate_obs <- function(raw_data) {
  obs <- raw_data |>
    dplyr::filter(
      !is.na(.data$record_id),
      !is.na(.data$redcap_repeat_instrument),
      .data$redcap_repeat_instrument == "observations"
    ) |>
    dplyr::mutate(record_id = as.numeric(.data$record_id))

  pre_bp <- obs |>
    dplyr::select(record_id,
                  qblg3 = "qblg3", qblg4 = "qblg4",
                  date  = "qblg5") |>
    dplyr::filter(!is.na(.data$qblg3) | !is.na(.data$qblg4)) |>
    dplyr::mutate(date = as.Date(.data$date, format = "%Y-%m-%d")) |>
    dplyr::arrange(.data$record_id, dplyr::desc(.data$date)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      record_id,
      qblg3 = safe_numeric(.data$qblg3),
      qblg4 = safe_numeric(.data$qblg4)
    )

  post_bp <- obs |>
    dplyr::select(record_id,
                  qblg6 = "qblg6", qblg7 = "qblg7",
                  date  = "qblg8") |>
    dplyr::filter(!is.na(.data$qblg6) | !is.na(.data$qblg7)) |>
    dplyr::mutate(date = as.Date(.data$date, format = "%Y-%m-%d")) |>
    dplyr::arrange(.data$record_id, dplyr::desc(.data$date)) |>
    dplyr::group_by(.data$record_id) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      record_id,
      qblg6 = safe_numeric(.data$qblg6),
      qblg7 = safe_numeric(.data$qblg7)
    )

  dplyr::full_join(pre_bp, post_bp, by = "record_id")
}

#' Calculate age from date of birth
#'
#' Ages > 100 are flagged as NA (likely data quality issues).
#'
#' @param dob Character vector of dates in YYYY-MM-DD format.
#' @return Numeric vector of ages in years.
#' @keywords internal
calculate_age <- function(dob) {
  dob_date <- as.Date(dob, format = "%Y-%m-%d")
  age <- as.numeric(difftime(Sys.Date(), dob_date, units = "days")) / 365.25
  dplyr::if_else(age > 100, NA_real_, age)
}

#' Safely convert to numeric
#' @param x Vector to convert.
#' @return Numeric vector.
#' @keywords internal
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Safely convert to integer
#' @param x Vector to convert.
#' @return Integer vector.
#' @keywords internal
safe_integer <- function(x) {
  suppressWarnings(as.integer(x))
}

#' Map pat01 numeric code to centre name
#' @param pat01 Numeric or character vector of centre codes.
#' @return Character vector of centre names.
#' @keywords internal
map_centre_code <- function(pat01) {
  lookup <- c(
    "1"   = "Cavan General Hospital",
    "2"   = "Letterkenny University Hospital",
    "3"   = "Sligo General Hospital",
    "4"   = "Merlin Park University Hospital",
    "5"   = "University Hospital Galway",
    "6"   = "Mayo General Hospital",
    "7"   = "Cork University Hospital",
    "8"   = "Kerry General Hospital",
    "9"   = "Mater Misericordiae University Hospital",
    "10"  = "Beaumont University Hospital",
    "11"  = "University Hospital Waterford",
    "12"  = "Midland Regional Hospital at Tullamore",
    "13"  = "University Hospital Limerick",
    "14"  = "St Vincents University Hospital",
    "15"  = "Beacon Renal Drogheda",
    "16"  = "Beacon Renal Sandyford",
    "17"  = "Beacon Renal Tallaght",
    "18"  = "Tallaght University Hospital",
    "19"  = "Fresenius Dock Road",
    "20"  = "Northern Cross Dialysis Centre",
    "21"  = "Kilkenny Satellite Dialysis Unit",
    "22"  = "Temple Street CUH",
    "23"  = "Wellstone Clinic Galway",
    "24"  = "Wellstone Clinic Wexford",
    "25"  = "Wellstone Clinic Portlaoise",
    "26"  = "Dialysis Away from Base",
    "27"  = "Saolta Region",
    "28"  = "St James Hospital Dublin",
    "100" = "National Transplant Centre Beaumont"
  )
  unname(lookup[as.character(pat01)])
}

#' Map pat01 numeric code to region
#' @param pat01 Numeric or character vector of centre codes.
#' @return Character vector of regions.
#' @keywords internal
map_centre_region <- function(pat01) {
  lookup <- c(
    "1"   = "North West",
    "2"   = "North West",
    "3"   = "North West",
    "4"   = "West",
    "5"   = "West",
    "6"   = "West",
    "7"   = "South",
    "8"   = "South",
    "9"   = "Dublin",
    "10"  = "Dublin",
    "11"  = "South East",
    "12"  = "Midlands",
    "13"  = "Mid West",
    "14"  = "Dublin",
    "15"  = "North East",
    "16"  = "Dublin",
    "17"  = "Dublin",
    "18"  = "Dublin",
    "19"  = "Mid West",
    "20"  = "Dublin",
    "21"  = "South East",
    "22"  = "Dublin",
    "23"  = "West",
    "24"  = "South East",
    "25"  = "Midlands",
    "26"  = "National",
    "27"  = "West",
    "28"  = "Dublin",
    "100" = "Dublin"
  )
  unname(lookup[as.character(pat01)])
}

#' Map pat01 numeric code to unit type
#' @param pat01 Numeric or character vector of centre codes.
#' @return Character vector of unit types.
#' @keywords internal
map_centre_type <- function(pat01) {
  # Satellite units: Beacon, Fresenius, Northern Cross, Wellstone, Kilkenny
  satellite_codes <- c("15", "16", "17", "19", "20", "21", "23", "24", "25")
  dplyr::if_else(as.character(pat01) %in% satellite_codes,
                 "Satellite", "Renal")
}

#' Map vascular access REDCap code to dashboard label
#' @param code Character vector of access codes from REDCap.
#' @return Character vector of access type labels.
#' @keywords internal
map_access_type <- function(code) {
  dplyr::case_when(
    code == "AVF"                     ~ "AVF",
    code == "AVG"                     ~ "AVG",
    code == "TLN"                     ~ "Tunnelled Line",
    code == "NLN"                     ~ "Non-tunnelled Line",
    code == "VLP"                     ~ "Vein Loop",
    code %in% c("PDC", "PDE", "PDT")  ~ "PD Catheter",
    TRUE                              ~ NA_character_
  )
}
