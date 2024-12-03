

# ------------------- START TEST sample_and_adjust_years---------------------- #



# Unit tests
test_that("sample_and_adjust_years works with Date columns", {
  dt <- data.table(time = as.Date('2023-01-01') + 0:364, value = rnorm(365))
  sampled_years <- 2023
  new_years <- 2021
  
  result <- sample_and_adjust_years(dt, sampled_years, new_years, "time")
  
  expect_equal(nrow(result), 365)
  expect_equal(unique(year(result$time)), new_years)
})

test_that("sample_and_adjust_years works with numeric columns", {
  dt <- data.table(year = 2000:2009, value = rnorm(10))
  sampled_years <- c(2001, 2003, 2005)
  new_years <- c(2011, 2013, 2015)
  
  result <- sample_and_adjust_years(dt, sampled_years, new_years, "year")
  
  expect_equal(nrow(result), 3)
  expect_equal(sort(unique(result$year)), sort(new_years))
})

test_that("sample_and_adjust_years handles non-existing year in Date column", {
  dt <- data.table(time = as.Date('2023-01-01') + 0:365, value = rnorm(366))
  sampled_years <- c(2023, 2024)
  new_years <- c(2021, 2022)
  
  result <- sample_and_adjust_years(dt, sampled_years, new_years, "time")
  
  expect_equal(nrow(result), 366)
  expect_equal(sort(unique(year(result$time))), sort(new_years))
})

test_that("sample_and_adjust_years handles errors for unsupported column types", {
  dt <- data.table(time = as.character(as.Date('2023-01-01') + 0:365), value = rnorm(366))
  sampled_years <- c(2023, 2024)
  new_years <- c(2021, 2022)
  
  expect_error(sample_and_adjust_years(dt, sampled_years, new_years, "time"), 
               "The date_col_name must be either a Date or numeric type column.")
})



# ------------------- END TEST sample_and_adjust_years---------------------- #


# ------------------- START TEST extract_unique_years---------------------- #

# Unit tests
test_that("extract_unique_years works with Date columns", {
  dt <- data.table(time = as.Date('2023-01-01') + 0:365, value = rnorm(366))
  result <- extract_unique_years(dt, "time")
  
  expect_true(all(result %in% c(2023, 2024)))
  expect_equal(length(result), 2)
})

test_that("extract_unique_years works with numeric columns", {
  dt <- data.table(year = rep(2000:2009, each = 2), value = rnorm(20))
  result <- extract_unique_years(dt, "year")
  
  expect_equal(sort(result), 2000:2009)
})

test_that("extract_unique_years handles mixed Date and numeric columns", {
  dt <- data.table(time = as.Date('2023-01-01') + 0:365, year = rep(2000:2005, each = 61), value = rnorm(366))
  result_dates <- extract_unique_years(dt, "time")
  result_years <- extract_unique_years(dt, "year")
  
  expect_true(all(result_dates %in% c(2023, 2024)))
  expect_equal(length(result_dates), 2)
  expect_equal(sort(result_years), 2000:2005)
})

test_that("extract_unique_years handles errors for unsupported column types", {
  dt <- data.table(time = as.character(as.Date('2023-01-01') + 0:365), value = rnorm(366))
  expect_error(extract_unique_years(dt, "time"), 
               "The date_col_name must be either a Date or numeric type column.")
})

# ------------------- END TEST extract_unique_years---------------------- #


# ------------------- START TEST sample_dt_by_years---------------------- #


# Integration tests
test_that("sample_dt_by_years works with Date columns", {
  dt <- data.table(time = as.Date('2023-01-01') + 0:364, value = rnorm(365))
  result <- sample_dt_by_years(dt, n_years = 2, start_year = 2025, date_col_name = "time", seed = 123)

  expect_true(all(year(result$time) %in% c(2025, 2026)))
  expect_equal(nrow(result), 365 * 2)
})

test_that("sample_dt_by_years works with numeric columns", {
  dt <- data.table(year = rep(2000:2009, each = 2), value = rnorm(20))
  result <- sample_dt_by_years(dt, n_years = 3, start_year = 2010, date_col_name = "year", seed = 123)
  
  expect_equal(sort(unique(result$year)), 2010:2012)
  expect_equal(nrow(result), 6)
})

test_that("sample_dt_by_years works with no seed", {
  dt <- data.table(time = as.Date('2022-01-01') + 0:729, value = rnorm(730))
  result <- sample_dt_by_years(dt, n_years = 2, start_year = 2020, date_col_name = "time")
  
  expect_true(all(year(result$time) %in% c(2020, 2021)))
  expect_equal(nrow(result), 365 * 2)
})

test_that("sample_dt_by_years handles errors for invalid date_col_name", {
  dt <- data.table(time = as.Date('2023-01-01') + 0:365, value = rnorm(366))
  expect_error(sample_dt_by_years(dt, n_years = 2, start_year = 2020, date_col_name = "invalid_col"), 
               "Assertion on 'names\\(dt\\)' failed: Names must include the elements \\{'invalid_col'\\}, but is missing elements \\{'invalid_col'\\}\\.")
})

test_that("sample_dt_by_years handles errors for unsupported column types", {
  dt <- data.table(time = as.character(as.Date('2023-01-01') + 0:365), value = rnorm(366))
  expect_error(sample_dt_by_years(dt, n_years = 2, start_year = 2020, date_col_name = "time"), 
               "The date_col_name must be either a Date or numeric type column.")
})



# ------------------- END TEST sample_dt_by_years---------------------- #


























