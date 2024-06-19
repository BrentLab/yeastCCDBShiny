test_that("PromoterSetSigAPI instantiation success", {
  withr::with_envvar(new = c(
    PROMOTERSETSIG_URL = "http://127.0.0.1:8001/api/promotersetsig",
    TOKEN = 'asdfasdf'
  ), {
    httptest::with_mock_api({
      api_instance <- PromoterSetSigAPI$new()
      # assert that the api_instance is of class PromoterSetSigAPI
      expect_true(inherits(api_instance, "PromoterSetSigAPI"))
      rm(api_instance)
      gc()
    })
  })
})

test_that("PromoterSetSigAPI$read records only is successful", {
  withr::with_envvar(new = c(
    PROMOTERSETSIG_URL = "http://127.0.0.1:8001/api/promotersetsig",
    TOKEN = Sys.getenv("TOKEN")
  ), {
    httptest::with_mock_api({
      future::plan(future::sequential)

      api_instance <- PromoterSetSigAPI$new(
        params = list(
          regulator_symbol = "HAP5",
          workflow = "nf_core_callingcards_dev",
          data_usable = "pass"
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

test_that("PromoterSetSigAPI$read with retrieve_files is successful", {
  withr::with_envvar(new = c(
    PROMOTERSETSIG_URL = "http://127.0.0.1:8001/api/promotersetsig",
    TOKEN = "sadasdfasdf"
  ), {
    httptest::with_mock_api({
      future::plan(future::sequential)

      api_instance <- PromoterSetSigAPI$new(
        params = list(
          regulator_symbol = "HAP5",
          workflow = "nf_core_callingcards_dev",
          data_usable = "pass"
        )
      )

      out_future <- api_instance$read(retrieve_files = TRUE)

      out <- future::value(out_future)

      records_df <- out[[1]]
      data_list <- out[[2]]

      # Confirm that output is correct structure
      expect_true(is.data.frame(records_df))
      expect_true(is.list(data_list))
      expect_true(length(data_list) == 5)

      # out_future_storr <- api_instance$read(retrieve_files = TRUE)
      #
      # out_storr <- future::value(out_future)

      # Capture the log output
      log_output <- capture.output({
        out_future_storr <- api_instance$read(retrieve_files = TRUE)
        out_storr <- future::value(out_future_storr)
      })

      # Verify the log message
      log_message <- paste(log_output, collapse = "\n")
      expect_true(grepl("promotersetsig id: \\d+ retrieved from the storr.",
                        log_message))


    })
  })
})
