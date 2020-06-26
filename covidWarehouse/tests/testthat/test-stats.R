

test_that("funnel control limits", {
  funnel_cl <- funnel_poisson_control_limits(10, 100)
  expect(funnel_cl(x = 100, .975), 17)
})

test_that("Kaplan-Meier", {
  suppressWarnings(library(survival))

  test_individual <- structure(list(
  patient_id = c(
    12L, 18L, 28L, 3L, 40L, 6L, 16L,
    15L, 45L, 46L, 43L, 47L, 37L, 39L, 44L, 42L, 31L, 23L, 36L
  ),
  survival_time = c(
    4L, 5L, 5L, 6L, 6L, 7L, 10L, 11L, 12L,
    12L, 15L, 17L, 18L, 18L, 18L, 23L, 40L, 51L, 91L
  ), status = c(
    1L,
    1L, 1L, 1L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 0L, 0L, 1L, 1L,
    1L, 1L, 1L
  )
), class = "data.frame", row.names = c(NA, -19L))

km_indiv <- survfit(Surv(survival_time, status)~1, data = test_individual)
km_indiv <- summary(km_indiv, times = test_individual$survival_time)
km_indiv <- data.frame(km_indiv[1:8])
km_indiv$pop <- km_indiv$n - cumsum(dplyr::lag(km_indiv$n.censor, 1, default = 0))
km_indiv$n_t <- km_indiv$n

output_sub <- expect_warning(km_ct_estimator(df = km_indiv, t = time, d = n.event,
                              pop = pop, subtract_cases = TRUE, overwrite = TRUE))
output_nosub<- expect_warning(km_ct_estimator(df = km_indiv, t = time, d = n.event,
                               pop = n.risk, subtract_cases = FALSE, overwrite = TRUE))

expect_equal(output_nosub$surv, output_nosub$S_t)
expect_equal(output_nosub$std.err, output_nosub$S_t_SE)
expect_equal(output_sub$surv, output_sub$S_t)
expect_equal(output_sub$std.err, output_sub$S_t_SE)
})
