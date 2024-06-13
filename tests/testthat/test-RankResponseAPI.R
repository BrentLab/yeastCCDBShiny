test_that("RankResponseAPI instantiation success", {
  withr::with_envvar(new = c(
    PROMOTERSETSIG_URL = "http://127.0.0.1:8001/api/promotersetsig",
    TOKEN = 'asdadsf'
  ), {
    httptest::with_mock_api({
      api_instance <- RankResponseAPI$new()
      # assert that the api_instance is of class RankResponseAPI
      expect_true(inherits(api_instance, "RankResponseAPI"))
      rm(api_instance)
      gc()
    })
  })
})

test_that("RankResponseAPI instantiation failure", {
  # Test that RankResponseAPI$new() throws an error when no URL is provided
  expect_error(
    RankResponseAPI$new(url = ""),
    "A valid Rank Response URL must be provided"
  )
})

# to make this work, I had to comment out the httptest::with_mock_api
# and then enter httptest::start_capture() in the console, run
# devtools::test_active_file() and then enter httptest::stop_capture()
# after that, with the uncommented httptest::with_mock_api, it did work
test_that(".get_data_from_api() success", {
  withr::with_envvar(
    new = c(
      PROMOTERSETSIG_URL = "http://127.0.0.1:8001/api/promotersetsig",
      RANKRESPONSE_URL = "http://127.0.0.1:8001/api/promotersetsig/rankresponse",
      TOKEN = 'asdfasdf'
    ),
    {
      httptest::with_mock_api({
        header = httr::add_headers(
          "Authorization" = paste("token", Sys.getenv('TOKEN'), sep = " "),
          "Content-Type" = "application/json"
        )

        output <- .get_data_from_api(
          url = Sys.getenv("RANKRESPONSE_URL"),
          header = header,
          params = list(promotersetsig_id = 8783, expression_id = 4563)
        )

        # Confirm that output is correct structure
        expect_true(is.data.frame(output$metadata))
        expect_true(is.list(output$data))
      })
    }
  )
})

# Note the future::plan(future::sequential) here to make the test run
# sequentially in the same session
test_that("RankResponseAPI$read() success", {
  withr::with_envvar(
    new = c(
      PROMOTERSETSIG_URL = "http://127.0.0.1:8001/api/promotersetsig",
      RANKRESPONSE_URL = "http://127.0.0.1:8001/api/promotersetsig/rankresponse",
      TOKEN = 'asdadf'
    ),
    {
      httptest::with_mock_api({

        future::plan(future::sequential)

        rr_api = RankResponseAPI$new(
          params = list(promotersetsig_id = 8783, expression_id = 4563)
        )

        callback = function(metadata,
                            data,
                            storr,
                            additional_arg1,
                            additional_arg2) {
          list(metadata = metadata,
               data = data,
               additional_arg1 = additional_arg1,
               additional_arg2 = additional_arg2)
        }

        meta_future <- rr_api$read(callback = callback,
                                   additional_arg1 = "value1",
                                   additional_arg2 = "value2")

        meta_df <- future::value(meta_future)

        # Confirm that output is correct structure
        expect_true(is.data.frame(meta_df$metadata))
        expect_true(is.list(meta_df$data))
        expect_equal(meta_df$additional_arg1, "value1")
        expect_equal(meta_df$additional_arg2, "value2")
      })
    }
  )
})


