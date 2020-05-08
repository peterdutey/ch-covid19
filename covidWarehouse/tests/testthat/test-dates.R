test_that("multiplication works", {
  expect_equal(get_monday_date(lubridate::ymd("2020-05-08")), lubridate::ymd("2020-05-04"))
  expect_equal(get_monday_date(lubridate::ymd("2020-05-04")), lubridate::ymd("2020-05-04"))
  expect_equal(get_monday_date(lubridate::ymd("2020-02-02")), lubridate::ymd("2020-01-27"))
})
