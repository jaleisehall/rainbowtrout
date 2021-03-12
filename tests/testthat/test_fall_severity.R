test_that(
  "fall_severity works",
  {
    expect_equal(fall_severity(1, 2), "minor")
    expect_equal(fall_severity(2, 1), "very severe")
  }
)
