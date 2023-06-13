# Test create_partitions
test_that("create_partitions creates correct partitions", {
  expect_equal(create_partitions(14, 3), c(
    rep(1, 3), rep(2, 3),
    rep(3, 3), rep(4, 3),
    rep(5, 2)
  ))
  expect_equal(create_partitions(10, 4), c(rep(1, 4), rep(2, 4), rep(3, 2)))
  expect_equal(create_partitions(15, 5), c(rep(1, 5), rep(2, 5), rep(3, 5)))
})
