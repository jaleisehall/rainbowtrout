
test_that("fish_counts_works", {fish = c("salmon", "tuna", "steelhead","cod", "shark", "trout", "salmon", "salmon", "salmon", "tuna", "tuna", "steelhead", "cod", "salmon", "salmon", "cod", "salmon", "shark", "shark")
  expect_error(fish_counts(fish = fish, plot = YES))
  expect_error(fish_counts(fish = fish, plot = NO))
})

