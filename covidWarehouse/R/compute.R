
compute_age <- function(dob, date){
  as.integer(trunc(lubridate::time_length(
    lubridate::interval(dob, date)
    , "year")))
}

cut_age_denary <- function(age) {
  as.character(
    cut(
      x = age,
      breaks = c(seq(0, 130, 10), 150),
      include.lowest = FALSE,
      right = FALSE,
      labels = c(paste0(
        seq(0, 129, 10), "-",
        seq(9, 129, 10), " years"),
        "130+ years")
    ))
}


cut_age_quinary <- function(age) {
  as.character(
    cut(
      x = age,
      breaks = c(seq(0, 135, 5), 150),
      include.lowest = FALSE,
      right = FALSE,
      labels = c(paste0(
        seq(0, 130, 5), "-",
        seq(4, 134, 5), " years"),
        "135+ years")
    ))
}

cut_age_pentadecimal <- function(age) {
  as.character(
    cut(
      x = age,
      breaks = c(seq(0, 135, 15), 150),
      include.lowest = FALSE,
      right = FALSE,
      labels = c(paste0(
        seq(0, 129, 15), "-",
        seq(14, 135, 15), " years"),
        "135+ years")
    ))
  }

tbrounding <- function(x, places = 1){
  format(round(x, places), nsmall = places)
}


pvrounding <- function(x){
  if(x < 0.001){
    "$p$ < 0.001"
  } else {
    paste("$p$ =", tbrounding(x, 3))
  }
}

formatbm <- function(x){
  format(x, big.mark = ",")
}


#' Minimum non missing value
#'
#' @description Unlike \code{\link{min}()}, this function returns NA
#' if there is no non-missing element in `x`
#' @param x a vector
#' @return a vector
#' @export
min_non_missing <- function(x) {
  x <- na.omit(x)
  if(length(x)==0){
    return(NA)
  } else {
    return(min(x))
  }
}


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


#' Rates of COVID tests
#'
#' @return
#' @export
#' @importFrom lubridate %m+% %m-% days wday
weekly_rate_tests <- function() {
  tests <- incidents %>%
    dplyr::transmute(
      home_code,
      covid_tested,
      covid_symptomatic,
      covid_test_result,
      covid_confirmed,
      covid_first_confirmed,
      covid_ever_confirmed,
      incident_rank,
      infection_covid_19_type_code,
      incident_date,
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
      ) %>%
    mutate(week_starting =  get_monday_date(date_test)) %>%
    dplyr::group_by(home_code, week_starting) %>%
    dplyr::summarise(
      first_cases = sum(incident_rank %in% c(NA, 1), na.rm = T),
      total_cases = sum(covid_symptomatic),
      suspected_cases = sum(covid_confirmed, na.rm = T),
      tests_performed = sum(covid_tested, na.rm = T),
      tests_performed_home = sum(covid_tested == 1 & location %in% c("Care home", NA)),
      tests_performed_hospital_or_other = sum(covid_tested == 1 & !(location %in% c("Care home", NA))),
      tests_reported = sum(covid_tested & !is.na(covid_test_result)),
      tests_positive = sum(covid_test_result, na.rm = T)
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
    reference_homes[, c("home_code", "home_name_clickview")]) %>%
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
    dplyr::mutate(
      cases_cumulative = cumsum(if_else(is.na(first_cases), 0L, first_cases))) %>%
    dplyr::ungroup()

  output
}



#' Deduplicated cases and events
#'
#' @return
#' @export
#' @importFrom lubridate %m+% %m-% days wday
weekly_deduplicated_cases <- function() {

  restable <- dplyr::transmute(
    residents,
    resident_encryptedid = encrypted_id,
    res_gender = dplyr::case_when(
      gender == "F" ~ "Female",
      gender == "M" ~ "Male",
      TRUE ~ NA_character_
    ),
    res_dob = lubridate::dmy(dob)
  ) %>% dplyr::distinct()

  cases <-  dplyr::left_join(incidents, restable, by = "resident_encryptedid") %>%
    dplyr::mutate(
      gender = if_else(is.na(gender), res_gender, gender),
      age = if_else(!is.na(date_of_birth), compute_age(date_of_birth, incident_date), age),
    ) %>%
    dplyr::transmute(
      resident_encryptedid,
      age_group = cut_age_pentadecimal(age),
      covid_symptomatic,
      covid_tested,
      covid_test_result,
      covid_confirmed,
      covid_first_symptomatic,
      covid_ever_symptomatic,
      covid_first_confirmed,
      covid_ever_confirmed,
      incident_rank,
      infection_covid_19_type_code,
      incident_date
      ) %>%
    mutate(week_starting = get_monday_date(incident_date)) %>%
    dplyr::group_by(week_starting) %>%
    dplyr::summarise(
      first_symptomatic = sum(covid_first_symptomatic, na.rm = T),
      first_suspected = sum(covid_first_confirmed, na.rm = T),
      first_confirmed = sum(covid_first_positive),
      total_symptomatic = sum(covid_ever_symptomatic, na.rm = T)
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
    data.frame(home_code = reference_homes$home_code, stringsAsFactors = FALSE)) %>%
    dplyr::left_join(occupancy, by = c("home_code", "week_ending")) %>%
    dplyr::group_by(home_code) %>%
    dplyr::mutate(occupancy_imputed = if_else(
      is.na(occupancy),
      dplyr::last(na.omit(occupancy), order_by = week_starting),
      # dplyr::lag(occupancy, order_by = week_starting),
      occupancy
    ))

  output <- dplyr::left_join(output, cases, by = c("week_starting", "home_code")) %>%
    dplyr::group_by(home_code) %>%
    dplyr::arrange(week_starting) %>%
    dplyr::mutate(
      cases_cumulative = cumsum(if_else(is.na(first_cases), 0L, first_cases))) %>%
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
