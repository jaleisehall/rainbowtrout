test_that("detect_plastic_works", {
  expect_equal(detect_plastic(C = 10, H = 8, O = 4, Cl = 0), "PETE")
})


#testthat::test_file("tests/testthat/test_detect_plastic.R")
