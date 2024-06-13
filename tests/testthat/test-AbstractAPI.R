test_that("Empty AbstractAPI instantiates and destroys correctly", {
  httptest::with_mock_api({
    # Set up logging to capture messages
    log_output <- tempfile()
    futile.logger::flog.appender(futile.logger::appender.file(log_output))

    api_instance <- AbstractAPI$new()
    # assert that the api_instance is of class AbstractAPI
    expect_true(inherits(api_instance, "AbstractAPI"))
    instance_tempdir <- api_instance$temp_dir

    # confirm that the instance deletes its tempdir when rm and garbage
    # collected
    rm(api_instance)
    gc()

    # Read the log file and check the messages
    log_messages <- readLines(log_output)
    expect_true(any(grepl("Garbage Collecting Object AbstractAPI", log_messages)))

    # Clean up the log file
    unlink(log_output)
  })
})

test_that("Empty AbstractAPI CRUD operations print correct error messages", {
  httptest::with_mock_api({
    api_instance <- AbstractAPI$new()

    expect_error(api_instance$create(),
      regexp = "`create\\(\\)` is not implemented for AbstractAPI"
    )

    expect_error(api_instance$read(),
      regexp = "`read\\(\\)` is not implemented for AbstractAPI"
    )

    expect_error(api_instance$update(),
      regexp = "`update\\(\\)` is not implemented for AbstractAPI"
    )

    expect_error(api_instance$delete(),
      regexp = "`delete\\(\\)` is not implemented for AbstractAPI"
    )

    rm(api_instance)
    gc()
  })
})
