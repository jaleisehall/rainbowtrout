test_that("slr_time_works", {
  expect_equal(slr_fun(sea_level = 2, elevation = 1), "City is already flooded")
  expect_equal(slr_fun(sea_level = 1, elevation = 1), "The city will be flooded in year 2021")
  expect_match(slr_fun(sea_level = 2, elevation = 3), slr_fun(sea_level = 3, elevation = 4))
})


#test_file("tests/test_slr_time.R")
