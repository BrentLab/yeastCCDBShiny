test_that("get_rank_response_table handles successful HTTP response", {
  # Setup webmockr for the test
  webmockr::enable()
  on.exit(webmockr::disable(), add = TRUE)

  # Create a DataFrame
  df <- data.frame(Name = c("A", "B"), Value = c(1, 2))

  # Write the DataFrame to a gzipped CSV in a temporary directory
  temp_dir <- tempdir()
  csv_file_path <- file.path(temp_dir, "data.csv.gz")
  readr::write_csv(df, csv_file_path)

  # Create a tar file from the temp directory
  tar_file_path <- tempfile(fileext = ".tar")
  utils::tar(tar_file_path, files = csv_file_path, tar = "tar")

  # Read the binary content of the tar file to use as mock response body
  raw_content <- readBin(tar_file_path, "raw", file.info(tar_file_path)$size)

  # Stub the HTTP request to return this tar file as the response
  url = httr::parse_url("http://fakeapi.com/api/promotersetsig/rankresponse")
  url$query = list(promotersetsig_id = 123, experiment_id = 456)
  token = 'fake_token'
  browser()
  webmockr::stub_request("get", httr::build_url(url)) %>%
    webmockr::wi_th(headers = list("Authorization" = paste("Token", token))) %>%
    webmockr::to_return(body = raw_content,
                        status = 200,
                        headers = list("Content-Type" = "application/octet-stream"))

  # Call the function that processes the HTTP response
  result <- get_rank_response_table("http://fakeapi.com/", "123", "456", token)

  browser()

  expect_equal(nrow(result), 2)
  expect_equal(ncol(result), 2)
  expect_equal(result$Name[1], "A")
  expect_equal(result$Value[1], 1)

  # Clean up temporary files
  on.exit({
    unlink(tar_file_path)
    unlink(temp_dir, recursive = TRUE)
  }, add = TRUE)

})



