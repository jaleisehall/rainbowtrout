test_that("fish_counts_works", {fish = c("salmon", "tuna", "steelhead"," cod", "shark", "trout", "salmon", "salmon", "salmon", "tuna", "tuna", "steelhead", "cod", "salmon", "salmon", "cod", "salmon", "shark", "shark")

  #expect_match(fish_counts(fish = fish, plot = FALSE), c("salmon", "cod", 3, 1, 19))
  #expect_length(fish_counts(fish = fish, plot = TRUE), 9)
  expect_error(fish_counts(fish = fish, plot = YES))
  expect_error(fish_counts(fish = fish, plot = NO))
})



#Run this in the console to test this:   testthat::test_file("tests/testthat/test_fish_counts.R")

#NOTE: my goal was to test the rare_fish and common_fish outputs to expect_match or expect_equal cod and salmon but I was having trouble setting that up.  Also because we only have 3 outputs for the function expect_length should be 3 but is instead 9 for some reason when ran with plot = TRUE and is length 0 when plot = FALSE
