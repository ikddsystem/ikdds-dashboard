#' Get audit data from REDCap
#'
#' Reads the fields the dashboard needs from REDCap, then collapses the
#' longitudinal export to one row per patient.
#'
#' The IKDDS REDCap project stores demographics as a non-repeating form and
#' clinical data (`diagnoses`, `lab_results`, `observations`,
#' `hd_prescription`, `hd_sessions`) as repeating instruments, so each patient
#' arrives as several sparse rows. Each clinical value is taken from the
#' patient's most recent dated result (see [redcap_latest_fields()]).
#'
#' Centre comes from `pat01` (Hospital centre code). Codes without an entry
#' in [load_centres()] (e.g. 26 Dialysis Away from Base, 27 Saolta Region,
#' 100 National Transplant Centre) get `centre_code = NA`: the patient stays
#' in national figures but not centre comparisons.
#'
#' The full export is large and slow, so the collapsed result is cached on
#' disk in `config$cache_dir` and reused until it is older than
#' `config$cache_ttl` seconds. The cache survives app restarts and is shared
#' by every browser session. Set `IKDDS_DASH_CACHE_TTL=0` to force a refresh.
#'
#' @param config A `dashboard_config` object with REDCap credentials.
#'
#' @return A tibble in the same format as [generate_synthetic_data()].
#'
#' @keywords internal
get_redcap_audit_data <- function(config) {
  cache_file <- file.path(config$cache_dir, "redcap_audit_data.rds")

  if (file.exists(cache_file)) {
    age <- difftime(Sys.time(), file.mtime(cache_file), units = "secs")
    if (age <= config$cache_ttl) {
      cli::cli_inform(
        "Using cached REDCap data from {format(file.mtime(cache_file), '%d %b %Y %H:%M')}."
      )
      return(readRDS(cache_file))
    }
  }

  raw <- read_redcap_raw(config, redcap_audit_fields())
  cli::cli_inform("Read {nrow(raw)} rows from REDCap; collapsing to one row per patient...")
  df <- collapse_redcap_audit_data(raw)
  rm(raw)

  dir.create(config$cache_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(df, cache_file)
  cli::cli_inform("Cached {nrow(df)} patients to {.file {cache_file}}.")
  df
}

#' Read raw REDCap export
#'
#' @param config A `dashboard_config` object with REDCap credentials.
#' @param fields Character vector of REDCap field names to export.
#'
#' @return The raw (long, sparse) REDCap export as a data frame.
#'
#' @keywords internal
read_redcap_raw <- function(config, fields) {
  if (!requireNamespace("REDCapR", quietly = TRUE)) {
    cli::cli_abort(c(
      "The {.pkg REDCapR} package is required for REDCap data access.",
      "i" = "Install with: {.code install.packages(\"REDCapR\")}"
    ))
  }

  result <- REDCapR::redcap_read(
    redcap_uri = config$redcap_uri,
    token      = config$redcap_token,
    fields     = fields,
    batch_size = 200L
  )
  if (!isTRUE(result$success)) {
    cli::cli_abort(c(
      "REDCap read failed.",
      "x" = "{result$outcome_message}"
    ))
  }
  result$data
}

#' Value/date field groups collapsed to each patient's latest result
#'
#' Each element names the value fields that are read together from one
#' repeating-instrument row, plus the date field used to order those rows.
#' Paired values (e.g. systolic/diastolic) come from the same row.
#'
#' @return A list of lists with `values` and `date`.
#'
#' @keywords internal
redcap_latest_fields <- function() {
  list(
    list(values = "dxs01",                  date = "dxs06"),  # PRD
    list(values = "qblg9",                  date = "qblga"),  # URR
    list(values = c("hdp01", "hdp02"),      date = "hdp00"),  # HD prescription
    list(values = c("qblg3", "qblg4"),      date = "qblg5"),  # Pre-dialysis BP
    list(values = c("qblg6", "qblg7"),      date = "qblg8"),  # Post-dialysis BP
    list(values = "qblb1",                  date = "qblb2"),  # Phosphate
    list(values = "qblb4",                  date = "qblbc"),  # Corrected calcium
    list(values = "qblb9",                  date = "qblba"),  # PTH
    list(values = "qbla9",                  date = "qblaa"),  # Potassium
    list(values = "qbla4",                  date = "qbla5"),  # Bicarbonate
    list(values = "qble1",                  date = "qble2"),  # Haemoglobin
    list(values = "qblf1",                  date = "qblf2"),  # Ferritin
    list(values = "qhd20",                  date = "qhd00")   # Vascular access
  )
}

#' REDCap fields exported for the dashboard
#'
#' @return Character vector of REDCap field names.
#'
#' @keywords internal
redcap_audit_fields <- function() {
  latest <- redcap_latest_fields()
  c("record_id", "idn03", "pat00", "pat01", "pat25",
    unlist(lapply(latest, `[[`, "values")),
    vapply(latest, `[[`, character(1), "date"))
}

#' Collapse a raw REDCap export to one row per patient
#'
#' @param raw Raw REDCap export from [read_redcap_raw()].
#'
#' @return A tibble with the columns in [audit_data_columns()].
#'
#' @keywords internal
collapse_redcap_audit_data <- function(raw) {
  # Work in place where possible: the live export is millions of rows, and
  # copying every column at each step exhausts memory
  raw <- raw[!is.na(raw$record_id), , drop = FALSE]
  raw$record_id <- as.character(raw$record_id)

  # Fields absent from the export (e.g. no rows yet) are treated as empty
  for (f in setdiff(redcap_audit_fields(), names(raw))) {
    raw[[f]] <- NA_character_
  }
  if (!"redcap_repeat_instance" %in% names(raw)) {
    raw$redcap_repeat_instance <- NA_integer_
  }

  demographics <- raw[, c("record_id", "idn03", "pat00", "pat01", "pat25"),
                      drop = FALSE]
  demographics <- demographics[
    !is.na(demographics$pat01) | !is.na(demographics$idn03) |
      !is.na(demographics$pat00), , drop = FALSE]
  demographics <- tibble::as_tibble(
    demographics[!duplicated(demographics$record_id), , drop = FALSE]
  )

  patients <- tibble::tibble(record_id = unique(raw$record_id)) |>
    dplyr::left_join(demographics, by = "record_id")

  for (grp in redcap_latest_fields()) {
    patients <- dplyr::left_join(
      patients,
      latest_redcap_values(raw, grp$values, grp$date),
      by = "record_id"
    )
  }

  centres <- load_centres()

  patients |>
    dplyr::mutate(
      age         = calculate_age(.data$idn03),
      gender      = map_gender(.data$pat00),
      ethnicity   = dplyr::na_if(as.character(.data$pat25), ""),
      centre_code = map_centre_code(.data$pat01, centres$centre_code),
      consultant  = NA_character_,  # no consultant field in REDCap yet
      is_acute    = FALSE,          # no acute flag in REDCap yet
      dxs01       = as.character(.data$dxs01),
      qblg9       = safe_numeric(.data$qblg9),
      hdp01       = safe_integer(.data$hdp01),
      hdp02       = safe_integer(.data$hdp02),
      qblg3       = safe_numeric(.data$qblg3),
      qblg4       = safe_numeric(.data$qblg4),
      qblg6       = safe_numeric(.data$qblg6),
      qblg7       = safe_numeric(.data$qblg7),
      qblb1       = safe_numeric(.data$qblb1),
      qblb4       = safe_numeric(.data$qblb4),
      qblb9       = safe_numeric(.data$qblb9),
      qbla9       = safe_numeric(.data$qbla9),
      qbla4       = safe_numeric(.data$qbla4),
      qble1       = safe_numeric(.data$qble1),
      qblf1       = safe_numeric(.data$qblf1),
      qhd20       = map_access_type(.data$qhd20)
    ) |>
    dplyr::left_join(centres, by = "centre_code") |>
    dplyr::select(dplyr::all_of(audit_data_columns()))
}

#' Latest non-missing values per patient
#'
#' Keeps rows where the first of `values` is present, then takes the row with
#' the most recent `date` per patient (ties and missing dates broken by the
#' highest repeat instance).
#'
#' @param raw Raw REDCap export.
#' @param values Character vector of value fields read from the same row.
#' @param date Name of the date field used for ordering.
#'
#' @return A tibble with `record_id` and `values` columns.
#'
#' @keywords internal
latest_redcap_values <- function(raw, values, date) {
  # Subset columns before rows so only the fields needed here are copied
  cols <- raw[, c("record_id", "redcap_repeat_instance", values, date),
              drop = FALSE]
  first <- as.character(cols[[values[1]]])
  cols <- cols[!is.na(first) & nzchar(first), , drop = FALSE]

  tibble::as_tibble(cols) |>
    dplyr::mutate(.date = as.Date(as.character(.data[[date]]),
                                  format = "%Y-%m-%d")) |>
    dplyr::arrange(.data$record_id, dplyr::desc(.data$.date),
                   dplyr::desc(.data$redcap_repeat_instance)) |>
    dplyr::filter(!duplicated(.data$record_id)) |>
    dplyr::mutate(dplyr::across(dplyr::all_of(values), as.character)) |>
    dplyr::select("record_id", dplyr::all_of(values))
}

#' Calculate age from date of birth
#'
#' @param dob Character vector of dates in YYYY-MM-DD format.
#'
#' @return Numeric vector of ages in years.
#'
#' @keywords internal
calculate_age <- function(dob) {
  dob_date <- as.Date(as.character(dob), format = "%Y-%m-%d")
  as.numeric(difftime(Sys.Date(), dob_date, units = "days")) / 365.25
}

#' Safely convert to numeric
#'
#' @param x Vector to convert.
#'
#' @return Numeric vector.
#'
#' @keywords internal
safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

#' Safely convert to integer
#'
#' @param x Vector to convert.
#'
#' @return Integer vector.
#'
#' @keywords internal
safe_integer <- function(x) {
  suppressWarnings(as.integer(x))
}

#' Map pat01 hospital centre code to dashboard centre code
#'
#' @param pat01 Vector of REDCap `pat01` dropdown codes.
#' @param known_codes Character vector of valid centre codes, from
#'   [load_centres()].
#'
#' @return Character vector of centre codes; `NA` for codes that are not
#'   dialysis centres (e.g. 26, 27, 100) or are missing.
#'
#' @keywords internal
map_centre_code <- function(pat01, known_codes) {
  code <- trimws(as.character(pat01))
  dplyr::if_else(code %in% known_codes, code, NA_character_)
}

#' Map pat00 sex code to label
#'
#' Accepts the eMed letter codes written by the ETL and NHS numeric codes.
#'
#' @param code Vector of `pat00` values.
#'
#' @return Character vector: `"Male"`, `"Female"` or `NA`.
#'
#' @keywords internal
map_gender <- function(code) {
  code <- toupper(trimws(as.character(code)))
  dplyr::case_when(
    code %in% c("M", "1", "MALE")   ~ "Male",
    code %in% c("F", "2", "FEMALE") ~ "Female",
    TRUE ~ NA_character_
  )
}

#' Map vascular access code to type
#'
#' Groups the REDCap `qhd20` dropdown (codelist RR2) into the audit's three
#' access categories. PD catheters (`PDC`, `PDE`, `PDT`) map to `NA`.
#'
#' @param code Character vector of access codes from REDCap.
#'
#' @return Character vector of access type labels.
#'
#' @keywords internal
map_access_type <- function(code) {
  dplyr::case_when(
    code %in% c("AVF", "1")                     ~ "AVF",
    code %in% c("AVG", "VLP", "2")              ~ "AVG",
    code %in% c("NLN", "TLN", "3", "Catheter",
                "CVC")                          ~ "Catheter",
    TRUE ~ NA_character_
  )
}
