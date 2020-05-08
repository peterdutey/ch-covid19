

#' Set timespan of measurement
#'
#' @param data
#'
#' @return a list with two `Date` objects named `"start"` and `"end"`
#' @export
#' @importFrom lubridate %m+% days
get_time_span <- function(data = incidents) {
  dplyr::summarise(data,
            start = get_monday_date(min(incident_date)),
            end = get_monday_date(max(incident_date)) %m+% days(6)) %>%
    as.list

}

#' Build weekly time series table
#'
#' @param timespan a list with a week start date and a week end date.
#' Default is \code{\link{get_time_span}()}.#'
#' @return a data frame with variables `week_number`, `year`, `week_starting`,
#' and `week_ending`
#' @export
#' @importFrom lubridate %m+% days
get_time_series <- function(timespan = get_time_span()) {
  data.frame(list(
    week_starting = seq(timespan$start, timespan$end %m-% days(6), 7)
  )) %>%
    dplyr::mutate(week_number = lubridate::isoweek(week_starting),
           year = lubridate::year(week_starting),
           week_ending = week_starting %m+% days(6)) %>%
    dplyr::transmute(week_number, year, week_starting, week_ending)

}


#' Get the week starting date
#'
#' @param date a `Date` vector
#'
#' @return a `Date` vector corresponding to the preceding Monday
#' @export
get_monday_date <- function(date) {
  if_else(wday(date, week_start = 1) == 1,
          date,
          date %m-% days(wday(date, week_start = 2)))
}




resident_days_approx <- function(residents_data = residents, time_span = get_time_span()) {

  rdays <- data.frame(list(date = seq(time_span$start, time_span$end, by = 1),
                           date_end = time_span$end))
  rdays <- merge(rdays, residents_data, all = TRUE)
  rdays <- resident_days_approx_indicator(rdays)

  rdays
}


#' Approximate presence in care home
#'
#' @param data a data frame containing varibles `date`, `original_admission_date`,
#' `last_admission_date`, `last_discharge_date`, `date_end`, and `status`
#'
#' @return a data.frame with an rday variable indicating whether the resident
#' was in or out of the care me on the date contained in variable `date`
#' @importFrom dplyr mutate if_else between
resident_days_approx_indicator <- function(data){
  mutate(data, rday = if_else(
    is.na(last_discharge_date),
    if_else(date >= last_admission_date, 1L, 0L),
    if_else(last_discharge_date > last_admission_date |
              (last_discharge_date == last_admission_date & status != "In Home"),
            # orig adm      last adm         last disch        now
            # |-- -- -- -- --|----------------|                |
            as.integer(between(date, original_admission_date, last_discharge_date)),
            # orig adm      last disch       last adm          now
            # |-- -- -- -- --|                |----------------|
            as.integer(
              between(date, original_admission_date, last_discharge_date) |
                between(date, last_admission_date, date_end)
            ))
  ))
}


weekly_rate_confirmed <- function() {
  incidents

}

#' Rates of COVID tests
#'
#' @return
#' @export
#' @importFrom lubridate %m+% %m-% days wday
weekly_rate_tests <- function() {
  tests <- dplyr::filter(incidents, infection_covid_19_type_code %in% 1:2) %>%
    dplyr::transmute(
      home_code,
      # if the test date is more than 7 days away from incident date, use the incident date
      date_test = if_else(
          abs(as.numeric(infection_covid_19_test_date - incident_date, unit = "days")) > 7,
          incident_date,
          infection_covid_19_test_date),
      location = infection_covid_19_test_location,
      # if the test result date is more than 7 days away from incident date, assume it's 2 days after test date
      date_result = if_else(
        as.numeric(infection_covid_19_test_result_date - infection_covid_19_test_date, unit = "days") > 7,
        if_else(
          abs(as.numeric(infection_covid_19_test_date - incident_date, unit = "days")) > 7,
          incident_date,
          infection_covid_19_test_date)  %m+%  days(2),
        infection_covid_19_test_result_date),
      incident_rank,
      infection_covid_19_type_code) %>%
    mutate(week_starting =  get_monday_date(date_test)) %>%
    dplyr::filter(!is.na(date_test) | !is.na(date_result)) %>%
    dplyr::group_by(home_code, week_starting) %>%
    dplyr::summarise(
      first_cases = sum(incident_rank %in% c(NA, 1), na.rm = T),
      total_cases = n(),
      confirmed_cases = sum(infection_covid_19_type_code == 2, na.rm = T),
      tests_performed = sum(!is.na(date_test)),
      tests_performed_home = sum(!is.na(date_test) & location %in% c("Care home", NA)),
      tests_performed_hospital_or_other = sum(!is.na(date_test) & !(location %in% c("Care home", NA))),
      tests_reported = sum(!is.na(date_test) & !is.na(date_result))
    )

  timespan <- list(
    start = get_monday_date(
      min(c(incidents$incident_date,
            incidents$infection_covid_19_test_date),
          na.rm = T)),
    end = get_monday_date(min(
      Sys.Date(),
      max(c(incidents$incident_date,
            incidents$infection_covid_19_test_result_date), na.rm = T)))
     %m+% days(6)
  )

  # roll forward occupancy when missing
  output <- merge(
    get_time_series(timespan = timespan),
    data.frame(home_code = unique(tests$home_code), stringsAsFactors = FALSE)) %>%
    dplyr::left_join(occupancy, by = c("home_code", "week_ending")) %>%
    dplyr::group_by(home_code) %>%
    dplyr::mutate(occupancy_imputed = if_else(
      is.na(occupancy),
      dplyr::last(na.omit(occupancy), order_by = week_starting),
      # dplyr::lag(occupancy, order_by = week_starting),
      occupancy
    ))

  output <- dplyr::left_join(output, tests, by = c("week_starting", "home_code")) %>%
    dplyr::group_by(home_code) %>%
    dplyr::arrange(week_starting) %>%
    dplyr::mutate(cases_cumulative = cumsum(if_else(
      is.na(first_cases),
      0L,
      first_cases)) ) %>%
    dplyr::ungroup()

  output
}


#
#•	Confirmed COVID-19 cases
# •	Suspected COVID-19 cases
# •	Hospital admissions (I don’t know if this will be differentiated into COVID-19/non COVID-19)
# •	Deaths (COVID-19 versus non COVID-19 if available)



export_tables <- function() {

  write.csv(weekly_rate_tests(),
            file = file.path(getOption("fshc_files"), "rate_tests.csv"),
            na = "", row.names = FALSE)


}
