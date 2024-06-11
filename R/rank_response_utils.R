#' #' Retrieve Rank Response Data from the Database
#' #'
#' #' Get the rank response dataframe from the database promotersetsig/rankresponse
#' #' endpoint. If expression_id is null, return a rank response dataframe for
#' #' each experiment for that TF in the database.
#' #'
#' #' @param promotersetsig_id the id of the promotersetsig record in the database
#' #' @param tar_file the file to write the response contents to. Eg, you may
#' #'   create this with tempfile(fileext = ".tar")
#' #' @param expression_id the id of the experiment record in the database
#' #' @param token the authorization token
#' #' @param base_url the base URL of the API
#' #' @param url_suffix the URL suffix for the rank response endpoint
#' #'
#' #' @return A response object from the GET request. The intended use pattern is
#' #'   to create a tar_file in the calling
#' #'   environment, and then pass it to this function. The contents of the
#' #'   response will be written to the tar_file, if the GET is successful. If
#' #'   the GET fails without returning a response, an error will be raised.
#' #'   Otherwise, the response object is returned -- this should be checked for
#' #'   status_code in the calling environment
#' #'
#' #' @importFrom httr add_headers GET status_code
#' #' @importFrom futile.logger flog.debug
#' #'
#' #' @examples
#' #' \dontrun{
#' #'   rrtarfile = tempfile(fileext = ".tar")
#' #'   get_rank_response_data(8783, rrtarfile, 4563)
#' #' }
#' #'
#' #' @export
#' get_rank_response_data <- function(promotersetsig_id,
#'                                    tar_file,
#'                                    expression_id = NULL,
#'                                    token = Sys.getenv("TOKEN"),
#'                                    base_url = Sys.getenv('BASE_URL'),
#'                                    url_suffix = Sys.getenv("RANKRESPONSE_URL_SUFFIX")) {
#'   # check inputs
#'   promotersetsig_id = tryCatch({
#'      validate_integer_value(promotersetsig_id)
#'   }, error = function(e) {
#'     stop("promotersetsig_id must be an integer: ", e$message)
#'   })
#'   if (!dir.exists(dirname(tar_file))) {
#'     stop("The directory containing the `tar_file`: ", dirname(tar_file), " does not exist")
#'   }
#'   if (!is.null(expression_id)) {
#'     expression_id = tryCatch({
#'       validate_integer_value(expression_id)
#'     }, error = function(e) {
#'       stop("expression_id must be an integer: ", e$message)
#'     })
#'   }
#'   if (is.null(token)) {
#'     stop("No token provided")
#'   }
#'   if (is.null(base_url)) {
#'     stop("No base URL provided")
#'   }
#'   if (is.null(url_suffix)) {
#'     stop("No URL suffix provided")
#'   }
#'
#'   # Construct the full API URL
#'   url <- paste(base_url, url_suffix, sep = "/")
#'
#'   # Set up the authorization headers
#'   headers <- httr::add_headers(
#'     "Content-Type" = "application/json",
#'     "Authorization" = paste("Token", token)
#'   )
#'
#'   # Prepare parameters for the GET request
#'   params <- list(
#'     promotersetsig_id = promotersetsig_id
#'   )
#'   # add the expression_id if it is passed
#'   if (!is.null(expression_id)) {
#'     params$expression_id <- expression_id
#'   }
#'
#'   # Execute the HTTP GET request and handle possible errors
#'   response <- tryCatch(
#'     {
#'       futile.logger::flog.debug(paste0('rankresponse_url: ',
#'                                        url,
#'                                        "; params: ",
#'                                        paste(names(params),
#'                                              params,
#'                                              sep = ": ",
#'                                              collapse = ", ")))
#'       httr::GET(url,
#'                 headers,
#'                 httr::write_disk(tar_file, overwrite = TRUE),
#'                 query = params)
#'     },
#'     error = function(e) {
#'       stop("Error getting rank response table: ", e$message)
#'     }
#'   )
#'
#'   response
#'
#'
#'   #   # tryCatch(
#'   #   #   {
#'   #   #     .extract_rank_response_dataframe(tar_file)
#'   #   #   },
#'   #   #   error = function(e) {
#'   #   #     stop("Error parsing rank response dataframe: ", e$message)
#'   #   #   }
#'   #   # )
#'   #
#'   # # Check the response status and process the data
#'   # if (httr::status_code(response) != 200) {
#'   #   stop("Request failed with status: ", httr::status_code(response),
#'   #        "; message: ", httr::content(response, "text"))
#'   # }
#' }
#'
#'
#'   # output_dir <- tempfile()
#'   #
#'   # # Create a unique subdirectory within this temporary directory
#'   # dir.create(output_dir)
#'   #
#'   # # Extract the tar file to the output directory
#'   # untar(tar_file, exdir = output_dir)
#'   #
#'   # # Check the contents of the directory. Currently, it is required that there
#'   # # be only one file
#'   # data_paths <- list.files(output_dir, recursive = TRUE, full.names = TRUE)
#'   # if (length(data_paths) != 1) {
#'   #   stop("Expected a single file in the tarball, found ", length(data_paths))
#'   # }
#'   #
#'   # # Read the CSV file into a dataframe
#'   # readr::read_csv(data_paths)
#' # }
#'
#' .extract_rr_tarball_csv_data = function(tar_file, filename, ...){
#'   tar_contents <- untar(tar_file, list = TRUE)
#'   futile.logger::flog.debug(paste("tar_contents: ", tar_contents))
#'
#'   # Check if 'metadata.json' is in the tar file
#'   if (!filename %in% tar_contents) {
#'     stop(filename, " not found in tarball")
#'   # if metadata.json does exist, read it in as a dataframe and return
#'   }
#'   # Extract the tar file to a temporary directory
#'   temp_dir <- tempfile()
#'   dir.create(temp_dir)
#'   on.exit(unlink(temp_dir, recursive = TRUE), add = TRUE)
#'   untar(tar_file, files = filename, exdir = temp_dir)
#'
#'   # Read the CSV file into a dataframe
#'   readr::read_csv(file.path(temp_dir, filename), ...)
#' }
#'
#' #' Create rank response plot
#' #'
#' #' This function creates a rank response plot using the ggplot2 package. It
#' #' takes a rank response summary as input and returns a ggplot object.
#' #'
#' #' @importFrom plotly plot_ly add_trace layout
#' #' @importFrom dplyr select ungroup group_by group_map filter pull
#' #' @importFrom purrr map
#' #'
#' #' @param rank_response_summary A rank response summary object
#' #' @param confidence_intervals Boolean. Default to FALSE. Set to TRUE to plot
#' #'   points with confidence interval ranges rather than lines. Note that
#' #'   the line plots have the conf.int info in the hovertip
#' #'
#' #' @return A ggplot object representing the rank response plot
#' #'
#' #' @examples
#' #' \dontrun{
#' #' # Assuming rank_response_summary is a properly formatted object
#' #' plot <- plot_rank_response(rank_response_summary)
#' #' print(plot)
#' #' }
#' #'
#' #' @export
#' plot_rank_response <- function(tarfile_metadata_row,
#'                                confidence_intervals = FALSE) {
#'
#'       plot_title <- paste(
#'         "Expression Source:", expression_source,
#'         "\nResponsive : Total (CC gene set): ",
#'         n_responsive_expression, " : ",
#'         n_total_expression
#'       )
#'
#'       p <- plotly::plot_ly(
#'         data = data,
#'         x = ~rank,
#'         y = ~response_ratio,
#'         color = ~binding_src,
#'         text = ~ format(p_value, digits = 3),
#'         hoverinfo = "text",
#'         hovertext = ~ paste(
#'           "experiment: ", binding_src,
#'           "\nrank_bin: ", rank,
#'           "\nresponse_ratio: ", response_ratio,
#'           "\nconf.int: ", ci_lower, " - ", ci_upper,
#'           "\np-value:", p_value
#'         )
#'       )
#'
#'       if (confidence_intervals) {
#'         p <- p %>% plotly::add_trace(
#'           type = "scatter",
#'           mode = "markers",
#'           error_y = list(
#'             type = "data",
#'             symmetric = FALSE,
#'             array = ~ ci_upper - response_ratio,
#'             arrayminus = ~ response_ratio - ci_lower
#'           )
#'         )
#'       } else {
#'         p <- p %>% plotly::add_trace(
#'           type = "scatter",
#'           mode = "lines"
#'         )
#'       }
#'       p <- p %>%
#'         plotly::add_trace(
#'           y = ~random,
#'           type = "scatter",
#'           mode = "lines",
#'           color = I("black"),
#'           linetype = I("dot"),
#'           name = "random",
#'           hoverinfo = "y"
#'         ) %>%
#'         plotly::layout(
#'           title = plot_title,
#'           xaxis = list(
#'             title = "Rank",
#'             range = c(0, 150),
#'             tickmode = "array",
#'             tickvals = seq(0, 150, 5)
#'           ),
#'           yaxis = list(
#'             title = "Response Ratio",
#'             range = c(0, 1),
#'             tickmode = "array",
#'             tickvals = seq(0, 1, .1)
#'           ),
#'           legend = list(
#'             orientation = "h",
#'             xanchor = "center",
#'             x = 0.5,
#'             y = -0.2
#'           ),
#'           showlegend = TRUE
#'         )
#'
#'       list(name = keys$source_expr, plot = p)
#'     })
#'
#'   output <- purrr::map(plot_list, ~ .$plot)
#'   names(output) <- unlist(purrr::map(plot_list, ~ .$name))
#'
#'   output$random_df <- rank_response_summary$random
#'
#'   output
#' }
