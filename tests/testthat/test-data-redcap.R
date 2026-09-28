# Tests for REDCap data loading (no network: raw exports are mocked)

mock_redcap_raw <- function() {
  tibble::tibble(
    record_id                = c(1, 1, 1, 1, 1, 2, 2, 3),
    redcap_repeat_instrument = c(NA, "lab_results", "lab_results",
                                 "observations", "hd_sessions",
                                 NA, "lab_results", NA),
    redcap_repeat_instance   = c(NA, 1L, 2L, 1L, 1L, NA, 1L, NA),
    idn03 = c("1960-01-01", NA, NA, NA, NA, "1980-06-15", NA, "1990-01-01"),
    pat00 = c("M", NA, NA, NA, NA, "F", NA, "M"),
    pat01 = c(10, NA, NA, NA, NA, 26, NA, 7),
    pat25 = c("White Irish", NA, NA, NA, NA, NA, NA, NA),
    # Record 1: instance 1 is the more recent Hb despite the lower instance
    qble1 = c(NA, 11.2, 9.1, NA, NA, NA, 10.0, NA),
    qble2 = c(NA, "2026-08-01", "2026-03-01", NA, NA, NA, "2026-05-01", NA),
    # Record 1, instance 2 has a phosphate but no Hb
    qblb1 = c(NA, NA, 1.4, NA, NA, NA, NA, NA),
    qblb2 = c(NA, NA, "2026-03-01", NA, NA, NA, NA, NA),
    qblg3 = c(NA, NA, NA, 135, NA, NA, NA, NA),
    qblg4 = c(NA, NA, NA, 80, NA, NA, NA, NA),
    qblg5 = c(NA, NA, NA, "2026-07-01", NA, NA, NA, NA),
    qhd20 = c(NA, NA, NA, NA, "TLN", NA, NA, NA),
    qhd00 = c(NA, NA, NA, NA, "2026-09-01", NA, NA, NA)
  )
}

test_that("collapse_redcap_audit_data returns one row per patient", {
  df <- collapse_redcap_audit_data(mock_redcap_raw())

  expect_equal(nrow(df), 3)
  expect_equal(sort(df$record_id), c("1", "2", "3"))
  expect_identical(names(df), audit_data_columns())
  expect_silent(validate_audit_data(df))
})

test_that("collapse_redcap_audit_data takes the latest dated value", {
  df <- collapse_redcap_audit_data(mock_redcap_raw())
  p1 <- df[df$record_id == "1", ]

  expect_equal(p1$qble1, 11.2)
  expect_equal(p1$qblb1, 1.4)
  expect_equal(p1$qblg3, 135)
  expect_equal(p1$qblg4, 80)
  expect_equal(p1$qhd20, "Catheter")
  expect_true(is.na(p1$qblf1))
})

test_that("collapse_redcap_audit_data maps pat01 to centres", {
  df <- collapse_redcap_audit_data(mock_redcap_raw())

  p1 <- df[df$record_id == "1", ]
  expect_equal(p1$centre_code, "10")
  expect_equal(p1$centre_name, "Beaumont University Hospital")
  expect_equal(p1$region, "Dublin and North East")
  expect_equal(p1$gender, "Male")
  expect_equal(p1$ethnicity, "White Irish")

  # 26 = Dialysis Away from Base: kept, but with no centre
  p2 <- df[df$record_id == "2", ]
  expect_true(is.na(p2$centre_code))
  expect_true(is.na(p2$centre_name))
  expect_equal(p2$qble1, 10.0)
})

test_that("latest_redcap_values breaks date ties by highest instance", {
  raw <- tibble::tibble(
    record_id = c("1", "1"),
    redcap_repeat_instance = c(1L, 2L),
    qble1 = c(9, 12),
    qble2 = c("2026-01-01", "2026-01-01")
  )
  expect_equal(latest_redcap_values(raw, "qble1", "qble2")$qble1, "12")
})

test_that("map_centre_code keeps only known centres", {
  known <- load_centres()$centre_code
  expect_equal(
    map_centre_code(c(10, "7", 26, 27, 100, 45, NA), known),
    c("10", "7", NA, NA, NA, NA, NA)
  )
})

test_that("map_access_type groups qhd20 codes", {
  expect_equal(
    map_access_type(c("AVF", "AVG", "VLP", "NLN", "TLN", "PDC", NA)),
    c("AVF", "AVG", "AVG", "Catheter", "Catheter", NA, NA)
  )
})

test_that("map_gender handles letter and numeric codes", {
  expect_equal(map_gender(c("M", "F", "1", "2", "9", NA)),
               c("Male", "Female", "Male", "Female", NA, NA))
})

test_that("get_redcap_audit_data requests the audit fields", {
  requested <- NULL
  local_mocked_bindings(
    read_redcap_raw = function(config, fields) {
      requested <<- fields
      mock_redcap_raw()
    }
  )
  df <- suppressMessages(
    get_redcap_audit_data(mock_dashboard_config("redcap"))
  )

  expect_equal(nrow(df), 3)
  expect_true(all(c("record_id", "pat01", "qble1", "qble2") %in% requested))
})

test_that("get_redcap_audit_data reuses the on-disk cache", {
  calls <- 0L
  local_mocked_bindings(
    read_redcap_raw = function(config, fields) {
      calls <<- calls + 1L
      mock_redcap_raw()
    }
  )
  config <- mock_dashboard_config("redcap")
  config$cache_ttl <- 3600L

  first  <- suppressMessages(get_redcap_audit_data(config))
  second <- suppressMessages(get_redcap_audit_data(config))
  expect_equal(calls, 1L)
  expect_identical(first, second)

  config$cache_ttl <- 0L
  Sys.setFileTime(file.path(config$cache_dir, "redcap_audit_data.rds"),
                  Sys.time() - 10)
  suppressMessages(get_redcap_audit_data(config))
  expect_equal(calls, 2L)
})
