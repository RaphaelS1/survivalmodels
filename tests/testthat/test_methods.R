test_that("print", {
  fit <- akritas(Surv(time, status) ~ ., data = rats[1:10, ])
  expect_output(print(fit), "akritas(formula = Surv(time, status) ~ ., data = rats[1:10, ])",
                fixed = TRUE)
  expect_output(summary(fit), "akritas(formula = Surv(time, status) ~ ., data = rats[1:10, ])",
                fixed = TRUE)
})
