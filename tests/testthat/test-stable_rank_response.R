# Test stable_rank_response
test_that("stable_rank_response calculates correct response ratios", {
  # Using a small example dataframe similar to your input
  df <- data.frame(
    binding_signal = c(
      0.0411612635618076, 0.0214105449966155,
      0.0453743372531608, 0.0322136499872431,
      0.00237970236223191
    ),
    responsive = c(TRUE, FALSE, TRUE, TRUE, FALSE),
    experiment = "test_experiment",
    source_expr = "test_source_expr"
  )
  result <- stable_rank_response(
    df,
    "test_experiment;test_source_expr", 2, ";"
  )

  expect_equal(result$response_ratio, c(0, 1 / 2, 1 / 2))
})
