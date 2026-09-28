# Tests for synthetic data generation

test_that("generate_synthetic_data returns correct structure", {
  df <- generate_synthetic_data(seed = 1, n_total = 100)

  expect_s3_class(df, "tbl_df")
  expected_cols <- c("record_id", "centre_code", "centre_name",
                     "region", "unit_code", "unit_name", "unit_type",
                     "consultant", "is_acute",
                     "age", "gender", "ethnicity", "dxs01",
                     "qblg9", "hdp01", "hdp02",
                     "qblg3", "qblg4", "qblg6", "qblg7",
                     "qblb1", "qblb4", "qblb9", "qbla9", "qbla4",
                     "qble1", "qblf1", "qhd20")
  expect_true(all(expected_cols %in% names(df)))
})

test_that("generate_synthetic_data produces approximately correct row count", {
  df <- generate_synthetic_data(seed = 1, n_total = 200)
  expect_equal(nrow(df), 200)
})

test_that("generate_synthetic_data covers every centre", {
  df <- generate_synthetic_data(seed = 1, n_total = 200)
  expect_setequal(unique(df$centre_code), load_centres()$centre_code)
})

test_that("generate_synthetic_data is reproducible with same seed", {
  df1 <- generate_synthetic_data(seed = 99, n_total = 50)
  df2 <- generate_synthetic_data(seed = 99, n_total = 50)
  expect_identical(df1, df2)
})

test_that("generate_synthetic_data has realistic value ranges", {
  df <- generate_synthetic_data(seed = 1, n_total = 500)

  # Age: 18-95 (integer)
  expect_true(all(df$age >= 18 & df$age <= 95))
  expect_true(all(df$age == round(df$age)))

  # Gender: Male/Female only
  expect_true(all(df$gender %in% c("Male", "Female")))

  # URR: 30-95 (where not NA)
  urr <- df$qblg9[!is.na(df$qblg9)]
  expect_true(all(urr >= 30 & urr <= 95))

  # HD frequency: 2-4
  expect_true(all(df$hdp01 %in% c(2, 3, 4)))

  # Access types
  access <- df$qhd20[!is.na(df$qhd20)]
  expect_true(all(access %in% c("AVF", "AVG", "Catheter")))
})

test_that("generate_synthetic_data includes missing values", {
  df <- generate_synthetic_data(seed = 1, n_total = 500)

  # At least some NAs in clinical variables
  expect_true(any(is.na(df$qblg9)))
  expect_true(any(is.na(df$qble1)))
  expect_true(any(is.na(df$qblb1)))
})

test_that("generate_synthetic_data has unique record_ids", {
  df <- generate_synthetic_data(seed = 1, n_total = 200)
  expect_equal(length(unique(df$record_id)), nrow(df))
})

test_that("generate_synthetic_data includes acute patients", {
  df <- generate_synthetic_data(seed = 1, n_total = 500)
  expect_true("is_acute" %in% names(df))
  expect_true(any(df$is_acute))
  expect_true(any(!df$is_acute))
})

test_that("generate_synthetic_data includes ethnicity with missingness", {
  df <- generate_synthetic_data(seed = 1, n_total = 500)
  expect_true("ethnicity" %in% names(df))
  expect_true(any(is.na(df$ethnicity)))
  expect_true(any(!is.na(df$ethnicity)))
})

test_that("generate_synthetic_data includes unit information", {
  df <- generate_synthetic_data(seed = 1, n_total = 100)
  expect_true(all(c("unit_code", "unit_name", "unit_type") %in% names(df)))
  expect_true(all(!is.na(df$unit_code)))
})

test_that("generate_synthetic_data has some unmapped consultants", {
  df <- generate_synthetic_data(seed = 1, n_total = 500)
  expect_true(any(is.na(df$consultant)))
})
