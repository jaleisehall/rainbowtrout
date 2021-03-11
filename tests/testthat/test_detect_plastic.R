test_that("detect_plastic_works", {plastic_table = tribble(~carbon, ~hydrogen, ~oxygen, ~chlorine,
                                                            10, 8, 4, 0,
                                                            2, 4, 0, 0,
                                                            20, 6, 5, 2,
                                                            8, 8, 0, 0,
                                                            3, 6, 0, 0,
                                                            2, 3, 0, 1)
  expect_equal(detect_plastic(C = 10, H = 8, O = 4, Cl = 0), "PETE")
  expect_match(detect_plastic(C = 4, H = 7, O = 3, Cl = 1),
               detect_plastic(C = 17, H = 3, O = 6, Cl = 0))
  expect_length(detect_plastic(C = c(4, 5, 6), H = 8, O = 3, Cl = 0), 3)
  expect_equal(detect_plastic(C = -4, H = -7, O = -3, Cl = 0),
               "Elements atom content must be a value of 0 or greater than 0. Check for NA, negative, or non-numeric values.")
  expect_length(detect_plastic(C = plastic_table$carbon,
                               H = plastic_table$hydrogen,
                               O = plastic_table$oxygen,
                               Cl = plastic_table$chlorine), 6)
})


#testthat::test_file("tests/testthat/test_detect_plastic.R")
