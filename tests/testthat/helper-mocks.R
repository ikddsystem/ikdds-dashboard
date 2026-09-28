# Mock helpers for testing without REDCap connections

#' Create a mock dashboard config for testing
#' @keywords internal
mock_dashboard_config <- function(data_source = "synthetic") {
  config <- list(
    data_source  = data_source,
    redcap_uri   = "https://redcap.test.ie/api/",
    redcap_token = "FAKE_TOKEN_FOR_TESTING",
    cache_ttl    = 60L,
    cache_dir    = tempfile("ikdds-cache-"),
    app_title    = "Test Dashboard",
    debug        = FALSE,
    user_role    = "admin",
    user_centre  = ""
  )
  class(config) <- c("dashboard_config", "list")
  config
}

#' Create a mock config missing required REDCap values
#' @keywords internal
mock_dashboard_config_incomplete <- function() {
  config <- list(
    data_source  = "redcap",
    redcap_uri   = "",
    redcap_token = "",
    cache_ttl    = 60L,
    cache_dir    = tempfile("ikdds-cache-"),
    app_title    = "Test Dashboard",
    debug        = FALSE,
    user_role    = "admin",
    user_centre  = ""
  )
  class(config) <- c("dashboard_config", "list")
  config
}
