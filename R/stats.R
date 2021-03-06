



#' Function factory for Poisson funnel plot control limits
#'
#' @param y a numeric vector of observed counts
#' @param denom a numeric vector for the denominator (exposed to risk/person-days)
#'
#' @return a function taking the following arguments
#' \describe{
#'   \item{\code{x}}{value of the denominator}
#'   \item{\code{p}}{lower tail probability}
#' }
#' and returns the smallest integer y such that \eqn{Pr(X \ge y) = p}
#' @export
#' @examples
#' funnel_cl <- funnel_poisson_control_limits(10, 100)
#' funnel_cl(x = 100, .975)
funnel_poisson_control_limits <- function(y, denom) {
  param = sum(y, na.rm = T)/sum(denom, na.rm = T)
  eval(parse(text = paste0(
    "function(x, p) {
      qpois(p, lambda = ", param, "* x, lower.tail = TRUE) / x
    }"
  )))
}


#' Classify observation as an outlier based on Poisson control limits
#'
#' @param o the numeric of the observed count
#' @param denom a numeric value for the rate denominator (exposed to risk/person-days)
#' @param fun a function generated by \code{\link{funnel_poisson_control_limits}()}
#' @param confidence_limit a numeric value between 0 and 1 for the confidence level
#' for outlier detection (eg 0.95)
#' @return
#' @export
funnel_outlier <- function(o, denom, fun, confidence_limit = .9) {
  alpha <- (1-confidence_limit)/2
  return(
    (o/denom) >= fun(denom, 1-alpha) |
      (o/denom) <= fun(denom, alpha)
  )
}


multiply_E5 <- function(x) {
  formatbm(x * 1E5)
}


#' Create funnel plot with Poisson
#'
#' @param data a data frame
#' @param y the unquoted name of a numeric variable containing observed counts in `data`
#' @param denom the unquoted name of a numeric variable containing the denominator in `data`
#' @param home_code the unquoted name of a character variable containing the care home code in `data`
#' @param home_name the unquoted name of a character variable containing the care home name in `data`
#'
#' @return An object of class ggplot
#' @export
plot_funnel <- function(data, y, denom, home_code, home_name) {
  y <- enquo(y)
  denom <- enquo(denom)
  home_code <- enquo(home_code)
  home_name <- enquo(home_name)



  data <- data %>%
    dplyr::group_by(!!home_code, !!home_name) %>%
    dplyr::summarise(.,
                     !!denom := sum(!!denom, na.rm = T),
                     !!y := sum(!!y, na.rm = T)
    )

  conf_limit_fun <- funnel_poisson_control_limits(data[[rlang::as_name(y)]],
                                                  data[[rlang::as_name(denom)]])

  data <- data %>%
    dplyr::group_by() %>%
    dplyr::mutate(overall_rate = sum(!!y, na.rm = T)/sum(!!denom, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      outlier95 = funnel_outlier(o = !!y, denom = !!denom,
                                 fun = conf_limit_fun, .95),
      rate = !!y/!!denom,
      observed_cases = !!y
    ) %>%
    dplyr::mutate(
      rate_per_100000 = multiply_E5(rate)
    )

  ggplot2::ggplot(data, aes(x = !!denom, y = rate))  +
    ggplot2::scale_y_continuous(labels = multiply_E5) +
    ggplot2::geom_point(aes(text = paste("care_home:", !!home_name),
                            observed_cases = observed_cases,
                            rate_per_100000 = rate_per_100000), shape = 3)  +
    ggplot2::geom_hline(aes(yintercept = overall_rate)) +
    ggplot2::stat_function(col = "deepskyblue3", geom="line", fun = rlang::as_function("conf_limit_fun"), args = list(p = .975)) +
    ggplot2::stat_function(col = "deepskyblue3", geom="line", fun = rlang::as_function("conf_limit_fun"), args = list(p = .025)) +
    ggplot2::stat_function(col = "orangered4", geom="line", fun = rlang::as_function("conf_limit_fun"), args = list(p = .995)) +
    ggplot2::stat_function(col = "orangered4", geom="line", fun = rlang::as_function("conf_limit_fun"), args = list(p = .005)) +
    ggplot2::labs(x = "Total resident-days", y = "Rate per 100,000")  +
    ggplot2::theme_bw() + ggplot2::theme(text = ggplot2::element_text(size = 14))

}



#' Kaplan-Meier survivor function point and interval estimator for count-time data
#'
#' @description Kaplan-Meier estimator based on Greenwods' formula
#' @param df a data frame
#' @param t unquoted name of a time variable
#' @param d unquoted name of the count of events
#' @param pop unquoted name of the population (inclusive of those who previously
#' experienced the event, but removing those that are censored, e.g. those who
#' left the study without experiencing the event)
#' @param subtract_cases a boolean indicated whether events count `d` should be
#' substracted from pop as time passes in order to calculate the total exposed-to-risk.
#' The default is `TRUE` for when `pop` number includes person-days for past cases.
#' @param overwrite a boolean indicating whether the function can overwrite variables
#' `n_t`, `S_t` and `S_t_SE` in `df` (default is TRUE)
#'
#' @return the input data frame `df` with additional variables `n_t` for the population
#' at risk size, `S_t` for the survivor function point estimator, and `S_t_SE` for its
#' standard error. A further `S_t_SElog` corresponding to an asymptotic estimator of the
#' standard deviation, which can be used to caculate confident interval bounds like so:
#' \itemize{
#'   \item \code{St_min = S_t^exp(-1.96*S_t_SElog)}
#'   \item \code{St_max = S_t^exp(+1.96*S_t_SElog)}
#' }
#'
#' @references section 1.1.4 of: Kalbfleisch JD, Prentice RL. Statistical Analysis of
#' Failure Time Data. New York: Wiley 1980.
#' @export
km_ct_estimator <- function(df, t, d, pop, subtract_cases = TRUE, overwrite = TRUE) {

  overwriting <- c("n_t", "S_t", "S_t_SE")[which(
    sapply(c("n_t", "S_t", "S_t_SE"),
           exists, where = df)
  )]
  if(length(overwriting)>0){
    if(overwrite){
      warning(paste0("Overwriting ", paste(overwriting, collapse = ", ")))
    } else {
      stop(paste0(paste(overwriting, collapse = ", "), " already exist."))
    }

  }

  df <- dplyr::arrange(df, {{t}})
  if(subtract_cases) {
    df <- dplyr::mutate(
      df,
      n_t = {{pop}} - (cumsum({{d}}) - {{d}})
      )
  } else {
    df <- dplyr::mutate(
      df,
      n_t = {{pop}})
  }

  dplyr::mutate(df,
      S_t = (n_t - {{d}})/n_t
    ) %>%
    dplyr::mutate(
      S_t = cumprod(S_t)
    ) %>%
    dplyr::mutate(
      S_t_SE = sqrt(S_t^2 * cumsum( {{d}}/(n_t*(n_t-{{d}})) )),
      S_t_SElog = sqrt(
        cumsum(
          {{d}} / ( n_t * ( n_t - {{d}} ) )
          ) ) /
          ( cumsum(
            log( 1 - {{d}}/n_t)
            ) )

    )

}



relative_risk <- function(a, b, c, d) {
  (a/(a + b))/(c/(c + d))
}

relative_risk_SE <- function(a, b, c, d){
  sqrt(1/a + 1/c + 1/(a + b) + 1/(c + d))
}
