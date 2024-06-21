#
# > res = httr::HEAD(url, header)
# > res
# Response [http://127.0.0.1:8001/api/regulator/]
# Date: 2024-06-20 18:01
# Status: 200
# Content-Type: application/json
# <EMPTY BODY>
# > httptest::save_response(res, simplify = FALSE)
test_that("RegulatorAPI instantiation success", {
  withr::with_envvar(new = c(
    EXPRESSION_URL = "http://127.0.0.1:8001/api/regulator",
    TOKEN = "asdfasdf"
  ), {
    httptest::with_mock_api({
      api_instance <- RegulatorAPI$new()
      # assert that the api_instance is of class RegulatorAPI
      expect_true(inherits(api_instance, "RegulatorAPI"))
      rm(api_instance)
      gc()
    })
  })
})

test_that("RegulatorAPI$read records only is successful", {
  withr::with_envvar(new = c(
    EXPRESSION_URL = "http://127.0.0.1:8001/api/regulator",
    TOKEN = 'asdfasdf'
  ), {
    httptest::with_mock_api({
      future::plan(future::sequential)

      api_instance <- RegulatorAPI$new(
          params = list(regulator_symbol = "HAP5,MET31")
      )

      out_future <- api_instance$read()

      out <- future::value(out_future)

      records_df <- out[[1]]
      # Confirm that output is correct structure
      expect_true(is.data.frame(records_df))
      expect_true(nrow(records_df) == 2)
      expect_true(is.null(out[[2]]))
    })
  })
})
