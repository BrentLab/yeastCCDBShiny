#
# > res = httr::HEAD(url, header)
# > res
# Response [http://127.0.0.1:8001/api/expression/]
# Date: 2024-06-20 18:01
# Status: 200
# Content-Type: application/json
# <EMPTY BODY>
# > httptest::save_response(res, simplify = FALSE)
test_that("ExpressionAPI instantiation success", {
  withr::with_envvar(new = c(
    EXPRESSION_URL = "http://127.0.0.1:8001/api/expression",
    TOKEN = 'asdfasdf'
  ), {
    httptest::with_mock_api({
      api_instance <- ExpressionAPI$new()
      # assert that the api_instance is of class ExpressionAPI
      expect_true(inherits(api_instance, "ExpressionAPI"))
      rm(api_instance)
      gc()
    })
  })
})

test_that("ExpressionAPI$read records only is successful", {
  withr::with_envvar(new = c(
    EXPRESSION_URL = "http://127.0.0.1:8001/api/expression",
    TOKEN = 'asdfasdf'
  ), {
    httptest::with_mock_api({
      future::plan(future::sequential)

      api_instance <- ExpressionAPI$new(
        params = list(
          regulator_symbol = "HAP5",
          source_time = "mcisaac_oe,15"
        )
      )

      out_future <- api_instance$read()

      out <- future::value(out_future)

      records_df <- out[[1]]

      # Confirm that output is correct structure
      expect_true(is.data.frame(records_df))
      expect_true(is.null(out[[2]]))
    })
  })
})

test_that("ExpressionAPI$read with retrieve_files is successful", {
  withr::with_envvar(new = c(
    EXPRESSION_URL = "http://127.0.0.1:8001/api/expression",
    TOKEN = Sys.getenv("TOKEN")
  ), {
    httptest::with_mock_api({
      future::plan(future::sequential)

      api_instance <- ExpressionAPI$new(
        params = list(
          regulator_symbol = "HAP5",
          source_time = "mcisaac_oe,15"
        )
      )

      out_future <- api_instance$read(retrieve_files = TRUE)

      out <- future::value(out_future)

      records_df <- out[[1]]
      data_list <- out[[2]]

      # Confirm that output is correct structure
      expect_true(is.data.frame(records_df))
      expect_true(is.list(data_list))
      expect_true(length(data_list) == 3)

      # Capture the log output
      log_output <- capture.output({
        out_future_storr <- api_instance$read(retrieve_files = TRUE)
        out_storr <- future::value(out_future_storr)
      })

      # Verify the log message -- TODO: make sure there are 5 of these.
      # can test that all of the expected ID are there
      log_message <- paste(log_output, collapse = "\n")
      expect_true(grepl("ExpressionAPI id: \\d+ retrieved from the storr.",
                        log_message))

    })
  })
})
