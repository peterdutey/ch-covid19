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


test_that("age_groups", {
  expect_equal(cut_age_denary(102), "100-109 years")
  expect_equal(cut_age_denary(150), NA_character_)

  expect_equal(cut_age_quinary(102), "100-104 years")
  expect_equal(cut_age_quinary(150), NA_character_)

  expect_equal(cut_age_pentadecimal(102), "90-104 years")
  expect_equal(cut_age_pentadecimal(150), NA_character_)



})


test_that("resident bed day approximation", {
  test_data <- dplyr::tribble(
    ~date,       ~original_admission_date,~last_admission_date,~last_discharge_date,   ~status,
    "2020-03-20",            "2019-04-18",        "2019-04-18",        "2019-04-20","Deceased",
    "2020-03-20",            "2020-03-16",	      "2020-03-16",                  NA, "In Home",
    "2020-03-20",            "2020-01-16",        "2020-01-16",                  NA, "In Home",
    "2020-03-20",            "2018-11-12",        "2018-11-12",        "2020-01-01","Permanently Discharged",
    "2020-03-20",            "2019-01-15",        "2019-09-02",                  NA,"Deceased",
    "2019-03-20",            "2019-01-15",        "2019-09-02",                  NA,"Deceased",
    "2020-03-20",            "2019-01-11",        "2019-02-21",        "2019-01-30", "In Home",
    "2019-06-02",            "2019-02-19",        "2019-06-03",        "2019-06-01", "In Home"
  )

  expect_equal(resident_days_approx_indicator(test_data)$rday,
               c(0, 1, 1, 0, 1, 0, 1, 0))
})

