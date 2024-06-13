# Test the ParamsList class
test_that("ParamsList constructor works", {
  params <- ParamsList(list(a = 1, b = 2))
  expect_s3_class(params, "ParamsList")
  expect_equal(names(params$params), c("a", "b"))
})

test_that("Extraction using [ works", {
  params <- ParamsList(list(a = 1, b = 2))
  subset <- params['a']
  expect_s3_class(subset, "ParamsList")
  expect_equal(names(subset$params), "a")
  expect_equal(subset$params$a, 1)
})

test_that("Extraction using [[ works", {
  params <- ParamsList(list(a = 1, b = 2))
  value <- params[['a']]
  expect_equal(value, 1)
})

test_that("Assignment using [<- works", {
  params <- ParamsList(list(a = 1, b = 2))
  params['c'] <- 3
  expect_equal(params$params$c, 3)
  expect_equal(names(params$params), c("a", "b", "c"))
})

test_that("Dropping item using [<- works", {
  params <- ParamsList(list(a = 1, b = 2))
  params['b'] <- NULL
  expect_equal(names(params$params), c("a"))
  expect_equal(params$params$a, 1)
})

test_that("Assignment using [<- with multiple elements works", {
  params <- ParamsList(list(b = 1, a = 2))

  new_items = list(f = 4, z = 5, c = 6)
  params[names(new_items)] <- new_items
  expect_equal(params$params$f, 4)
  expect_equal(params$params$z, 5)
  expect_equal(params$params$c, 6)
  expect_equal(names(params$params), c("a", "b", "c", "f", "z"))
})

test_that("Droping multiple items using [<- works", {
  params <- ParamsList(list(b = 1, a = 2, c = 3))
  params[c('a', 'b')] <- list(NULL, NULL)
  expect_equal(params$params$c, 3)
  expect_equal(names(params$params), "c")
})

test_that("Assignment using [[<- works", {
  params <- ParamsList(list(a = 1, b = 2))
  params[['g']] <- 7
  expect_equal(params$params$g, 7)
  expect_equal(names(params$params), c("a", "b", "g"))
})

test_that("Dropping item using [[<- works", {
  params <- ParamsList(list(a = 1, b = 2))
  params[['b']] <- NULL
  expect_equal(names(params$params), c("a"))
})

test_that("names.ParamsList works", {
  params <- ParamsList(list(a = 1, b = 2))
  expect_equal(names(params), c("a", "b"))
})

test_that("print.ParamsList works", {
  params <- ParamsList(list(a = 1, b = 2))
  expect_output(print(params), "Current params:\na: 1\nb: 2")
})

test_that("as.character.ParamsList works", {
  params <- ParamsList(list(a = 1, b = 2))
  char_rep <- as.character(params)
  expect_equal(char_rep, "a: 1, b: 2")
})

test_that("as.list.ParamsList works", {
  params <- ParamsList(list(a = 1, b = 2))
  list_rep <- as.list(params)
  expect_equal(list_rep, list(a = 1, b = 2))
})

test_that("sort.ParamsList works", {
  params <- ParamsList(list(b = 2, a = 1))
  sorted_params <- sort(params)
  expect_equal(names(sorted_params$params), c("a", "b"))
})
