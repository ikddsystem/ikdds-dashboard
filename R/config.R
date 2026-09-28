#' Create dashboard configuration
#'
#' Reads configuration from environment variables with sensible defaults.
#' Secrets (REDCap tokens) are never hardcoded.
#'
#' @param env Environment to read variables from. Defaults to
#'   [Sys.getenv()]. Pass a named list for testing.
#'
#' @return A named list with class `dashboard_config` containing:
#'   - `data_source`: `"synthetic"` or `"redcap"`
#'   - `redcap_uri`: REDCap API endpoint (required if data_source is redcap)
#'   - `redcap_token`: REDCap API token
#'   - `cache_ttl`: Cache time-to-live in seconds (default 3600)
#'   - `cache_dir`: Directory for the on-disk REDCap data cache (default
#'     the user cache directory from [tools::R_user_dir()])
#'   - `app_title`: Dashboard title
#'   - `debug`: Enable debug mode
#'
#' @export
#' @family config
#'
#' @examples
#' config <- dashboard_config()
#' config$data_source
dashboard_config <- function(env = NULL) {

  get_var <- function(name, default = "") {
    if (!is.null(env) && name %in% names(env)) {
      return(env[[name]])
    }
    Sys.getenv(name, unset = default)
  }

  config <- list(
    data_source   = tolower(get_var("IKDDS_DASH_DATA_SOURCE", "synthetic")),
    redcap_uri    = get_var("IKDDS_REDCAP_URI", ""),
    redcap_token  = get_var("IKDDS_REDCAP_TOKEN", ""),
    cache_ttl     = as.integer(get_var("IKDDS_DASH_CACHE_TTL", "3600")),
    cache_dir     = get_var("IKDDS_DASH_CACHE_DIR",
                            tools::R_user_dir("ikdds.dashboard", "cache")),
    app_title     = get_var("IKDDS_DASH_TITLE", "IKDDS Haemodialysis Audit Dashboard"),
    debug         = identical(tolower(get_var("IKDDS_DASH_DEBUG", "false")), "true"),
    user_role     = tolower(get_var("IKDDS_DASH_USER_ROLE", "admin")),
    user_centre   = get_var("IKDDS_DASH_USER_CENTRE", "")
  )

  class(config) <- c("dashboard_config", "list")
  config
}

#' Validate dashboard configuration
#'
#' Checks that all required configuration values are present and valid.
#'
#' @param config A `dashboard_config` object from [dashboard_config()].
#'
#' @return `config` invisibly if valid; errors otherwise.
#'
#' @export
#' @family config
validate_dashboard_config <- function(config) {
  valid_sources <- c("synthetic", "redcap")
  if (!config$data_source %in% valid_sources) {
    cli::cli_abort(c(
      "Invalid data source: {.val {config$data_source}}",
      "i" = "Must be one of: {.val {valid_sources}}"
    ))
  }

  if (config$data_source == "redcap") {
    missing <- character()
    if (!nzchar(config$redcap_uri)) missing <- c(missing, "IKDDS_REDCAP_URI")
    if (!nzchar(config$redcap_token)) missing <- c(missing, "IKDDS_REDCAP_TOKEN")
    if (length(missing) > 0) {
      cli::cli_abort(c(
        "REDCap data source requires credentials:",
        "x" = "Set environment variables: {.envvar {missing}}"
      ))
    }
  }

  if (!is.integer(config$cache_ttl) || config$cache_ttl < 0) {
    cli::cli_abort("{.var cache_ttl} must be a non-negative integer.")
  }

  invisible(config)
}
