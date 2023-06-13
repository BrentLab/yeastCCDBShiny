# Test rank_response_ratio_summarize
test_that("rank_response_ratio_summarize rr and random test", {
  # Using a small example dataframe similar to your input
  df <- data.frame(
    tf_id = c(1, 1, 1, 1, 2, 2, 2, 2),
    experiment = c(
      "exp1", "exp1", "exp2", "exp2",
      "exp1", "exp1", "exp2", "exp2"
    ),
    binding_signal = c(
      0.043410940724425, 0.0212696224823594,
      0.0109933236078359, 0.0379419134766795,
      0.062146974329371, 0.0225317380577326,
      0.0589414950902574, 0.0699747833795846
    ),
    effect_expr = c(
      0.0402821003729988, -0.504356928856001, -1.39675833299841,
      -0.856505523602546, 0.930709218677073, 0.341770719151093,
      1.59561706277361, 0.100902824901006
    ),
    p_expr = c(
      0.0539662160351872, 0.011883190611843, 0.00552017204230651,
      0.0686250562802888, 0.0217834066436626, 0.0213548930757679,
      0.0633223048551008, 0.0466870082798414
    ),
    source_expr = c(
      "src1", "src1", "src1", "src1",
      "src2", "src2", "src2", "src2"
    )
  )

  result <- rank_response_ratio_summarize(df,
    effect_expr_thres = 0.2,
    p_expr_thres = 0.05,
    normalize = FALSE,
    bin_size = 2,
    separator = ";"
  )

  expect_equal(result$rr$response_ratio, c(0.5, 1, 0.5, 0))
  expect_equal(result$random$random, c(0.5, 1, 0.5, 0))
})
