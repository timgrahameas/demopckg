testthat::test_that("errors", {
  testthat::expect_error(
    report_p(-1),
    "p cannot be less than 0"
  )
  
  testthat::expect_error(
    report_p(2),
    "p cannot be greater than 1"
  )
  
  testthat::expect_error(
    report_p(0),
    "p cannot be 0"
  )
  
  testthat::expect_warning(
    report_p(0.5, 6),
    "digits should probably be an integer between 1 and 5"
  )
})