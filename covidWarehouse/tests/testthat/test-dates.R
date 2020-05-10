test_that("get_monday_date works", {
  expect_equal(get_monday_date(lubridate::ymd("2020-05-08")), lubridate::ymd("2020-05-04"))
  expect_equal(get_monday_date(lubridate::ymd("2020-05-04")), lubridate::ymd("2020-05-04"))
  expect_equal(get_monday_date(lubridate::ymd("2020-02-02")), lubridate::ymd("2020-01-27"))
})

test_that("min_non_missing", {
  expect_equal(min_non_missing(c(0, 1)), 0)
  expect_equal(min_non_missing(c(NA, NA)), NA)
  expect_equal(min_non_missing(c(lubridate::ymd("2020-05-08"), as.Date(NA))),
               lubridate::ymd("2020-05-08"))
  expect_equal(min_non_missing(c(as.Date(NA), as.Date(NA))), NA)
})
