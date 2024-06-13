# Create a test fixture for temporary directories
local_temp_dir <- function(env = parent.frame()) {
  temp_dir <- tempfile()
  dir.create(temp_dir)
  withr::defer(unlink(temp_dir, recursive = TRUE), envir = env)
  temp_dir
}

# Mock AbstractAPI class using httptest
MockedAbstractAPIClass <- function(url_success = TRUE, env = parent.frame()) {
  url <- "https://api.example.com/resource"

  # Create the response content based on url_success
  response_content <- if (url_success) {
    list(status_code = 200, content = "Success")
  } else {
    list(status_code = 401, content = "Unauthorized")
  }

  # Create a mock response in the httptest context
  with_mock_api({
    # Save the response content to a file
    response_file <- tempfile(tmpdir = local_temp_dir(), fileext = ".json")
    writeLines(jsonlite::toJSON(response_content), response_file)

    # Register the mock response
    set_response(
      GET(url),
      response = response_file,
      status_code = response_content$status_code,
      content_type = "application/json"
    )

    # Create an instance of AbstractAPI
    api_instance <- AbstractAPI$new(
      url = url,
      token = 'asdfa1232asdf',
      tmpdir = local_temp_dir()
    )

    # Ensure the instance is cleaned up after tests
    withr::defer({
      rm(api_instance)
      gc()
    }, envir = env)

    return(api_instance)
  })
}

