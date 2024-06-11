test_that("initialize with only one of redis_host or redis_port raises an error", {
  expect_error(AbstractAPI$new(url = "https://api.example.com/resource", redis_host = "localhost"),
               "Both `redis_host` and `redis_port` must be provided, or neither.")
  expect_error(AbstractAPI$new(url = "https://api.example.com/resource", redis_port = 6379),
               "Both `redis_host` and `redis_port` must be provided, or neither.")
})

test_that("initialize without redis_host and redis_port does not set Redis connection", {
  api_instance <- AbstractAPI$new(url = "https://api.example.com/resource")
  expect_null(api_instance$get_redis())
})

test_that("set_redis sets Redis connection correctly", {
  with_mock(
    redux::hiredis = mock_hiredis,
    {
      api_instance <- AbstractAPI$new(url = "https://api.example.com/resource")
      api_instance$set_redis("localhost", 6379)
      expect_equal(api_instance$get_redis()$host, "localhost")
      expect_equal(api_instance$get_redis()$port, 6379)
    }
  )
})

test_that("initialize sets temp directory", {
  temp_dir <- local_temp_dir()
  api_instance <- AbstractAPI$new(url = "https://api.example.com/resource")
  expect_true(dir.exists(api_instance$get_temp_dir()))
})

test_that("finalizer removes temp directory", {
  temp_dir <- local_temp_dir()
  api_instance <- AbstractAPI$new(url = "https://api.example.com/resource")
  temp_dir <- api_instance$get_temp_dir()
  expect_true(dir.exists(temp_dir))

  rm(api_instance)
  gc()

  expect_false(dir.exists(temp_dir))
})

