#' @title Generate Rank Response Plot Data
#' @description A helper function that creates and summarizes rank response data
#'   for a specific transcription factor gene, and prepares a color-coded
#'   rank response plot.
#' @param rank_response_df DataFrame that includes rank response data.
#'   It should contain columns for 'binding_src', 'exp_ratio', 'bind_ratio', etc.
#' @param tf_gene A character string that specifies the transcription factor gene
#'   to be used for the rank response plot.
#' @return A list containing a ggplot2 plot object ('plot') and a DataFrame ('df').
#' @details This function summarizes the rank response ratio, determines unique
#'   binding source levels, creates a color palette for the different binding
#'   levels, and prepares a rank response plot for the specified transcription
#'   factor gene.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   # Generate plot data for transcription factor gene "myGene"
#'   result = generate_plot_data(rank_response_df, "myGene")
#'   # Access the plot object
#'   plot = result$plot
#'   plot
#'  }
#' }
#'
#' @seealso
#'  \code{retrieve_rank_response_data}, \code{\link[yeastCCDBShiny]{rank_response_ratio_summarize}}, \code{\link[yeastCCDBShiny]{plot_rank_response}}
#'
#' @rdname generate_plot_data
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#' @importFrom futile.logger flog.warn flog.error
#'
#' @export
generate_plot_data = function(rank_response_df, tf_gene){
  plt = NULL
  tryCatch(
    {
      rank_response_summary <-
        yeastCCDBShiny::rank_response_ratio_summarize(
          rank_response_df
        )

      # get the unique binding_src levels
      binding_levels <- unique(rank_response_summary$rr$binding_src)

      # create a color palette with the same number of
      # colors as binding_levels
      # max number of colors for set1 is 9
      color_palette <- RColorBrewer::brewer.pal(
        max(min(length(binding_levels), 9), 3), "Set1"
      )

      if (length(binding_levels) > 9) {
        color_palette <-
          grDevices::colorRampPalette(color_palette)(length(binding_levels))
      } else {
        # color brewer returns a minimum of 3 -- use this to set to less
        color_palette <- color_palette[seq(length(binding_levels))]
      }


      # create a named vector with the binding_levels as
      # names and the color_palette as values
      color_vector <- setNames(color_palette, binding_levels)

      # set harbison to black
      color_vector[names(color_vector) == "harbison"] <- "black"
      color_vector[names(color_vector) == "chipexo_yiming"] <- "black"

      rank_response_plot <- yeastCCDBShiny::plot_rank_response(
        rank_response_summary,
        color_vector,
        tf_gene
      )
      # update the plot object
      plt = rank_response_plot
    },
    warning = function(w) {
      futile.logger::flog.warn(w)
    },
    error = function(e) {
      futile.logger::flog.error(e)
    },
    finally = {
      plt
    }
  )
}

#' Retrieve and process rank response data from a remote server.
#'
#' This function fetches data related to a specific transcription factor from
#' a remote server, processes the data, and returns a ggplot object
#' representing the rank response of the transcription factor.
#'
#' @importFrom shiny withProgress incProgress
#' @importFrom httr GET add_headers http_status content
#' @importFrom readr read_csv
#' @importFrom futile.logger flog.error flog.warn
#' @importFrom labretriever retrieve create_url
#' @importFrom dplyr arrange
#' @importFrom RColorBrewer brewer.pal
#' @importFrom grDevices colorRampPalette
#'
#' @param tf_gene Character. The name of the transcription factor gene.
#' @param tf_id Numeric. The id of the transcription factor.
#' @param background_source Character. The source of the background dataset.
#' @param promoter_source Character. The source of the promoter dataset.
#' @param token Character. The authorization token for the API request.
#'
#' @rdname retrieve_rank_response_data
#'
#' @return ggplot. A plot representing the rank response of the
#'   transcription factor.
#'
#' @examples
#' \dontrun{
#' retrieve_rank_response_data(
#'   tf_gene = "my_gene", tf_id = 123,
#'   background_source = "source1",
#'   promoter_source = "source2",
#'   token = "my_token"
#' )
#' }
#'
#' @export
retrieve_rank_response_data <- function(tf_gene,
                                        tf_id,
                                        background_source,
                                        promoter_source,
                                        token) {
  N_STEPS <- 5
  result <- shiny::withProgress(
    message = "Fetching data...",
    detail = paste0(
      "Please wait while the data for TF ",
      tf_gene,
      " is being retrieved."
    ),
    value = 0,
    {
      shiny::incProgress(1 / N_STEPS,
        message = paste0(
          "retrieving calling cards data.",
          "Note: if data for any one of the ",
          "TFs has not previously been computed ",
          "this can take minutes to return"
        )
      )

      url <- paste0(
        yeastCCDBShiny::base_url,
        "/api/v1/promoterregions/callingcards/"
      )

      headers <- httr::add_headers(
        "Content-Type" = "application/json",
        "Authorization" = paste("Token", token)
      )

      query_params <- list(
        tf_id = tf_id,
        background_source = background_source,
        promoter_source = promoter_source
      )

      response <- httr::GET(url, headers, query = query_params)

      # Check if the response is successful
      if (httr::http_status(response)$category == "Success") {
        # Get the content of the response as text
        response_content <- httr::content(response,
          as = "text",
          encoding = "UTF-8"
        )

        # Read the CSV content into a tibble
        cc_df <- readr::read_csv(response_content)
      } else {
        futile.logger::flog.error(paste(
          "Request failed with status:",
          http_status(response)$message
        ))
      }

      shiny::incProgress(1 / N_STEPS,
        message = "retrieving expression data"
      )

      expr_df <- labretriever::retrieve(
        labretriever::create_url("expression",
          base_url = yeastCCDBShiny::base_url
        ),
        token,
        filter_list = list(
          tf_id = tf_id
        )
      )

      shiny::incProgress(1 / N_STEPS,
        message = "retrieving harbison data",
        detail = "If it exists for this TF..."
      )

      binding_df <- labretriever::retrieve(
        labretriever::create_url("harbisonchip_with_annote",
          base_url = yeastCCDBShiny::base_url
        ),
        token,
        filter_list = list(tf_id = tf_id)
      )

      shiny::incProgress(1 / N_STEPS,
                         message = "retrieving ChipExo data",
                         detail = "If it exists for this TF..."
      )

      chipexo_df <- labretriever::retrieve(
        "http://ec2-18-118-133-191.us-east-2.compute.amazonaws.com/api/v1/chipexo/with_annote/",
        token,
        filter_list = list(tf_id = tf_id)
      )

      shiny::incProgress(1 / N_STEPS,
        message = "Creating Plot"
      )

      tryCatch(
        {
          rank_response_df <- yeastCCDBShiny::process_tf_data(
            expr_df,
            binding_df,
            chipexo_df,
            cc_df
          )
        },
        warning = function(w) {
          futile.logger::flog.warn(w)
        },
        error = function(e) {
          futile.logger::flog.error(e)
        }
      )
      rank_response_df
    }
  )
}

#' @title Test Retrieval of Rank Response Data
#' @description A function that launches a Shiny application to test and visualize
#'   the retrieval of rank response data.
#' @return This function does not return a value; it runs a Shiny application.
#' @details The Shiny application provides an interface for entering various parameters
#'   related to the transcription factor and data sources, and displays the rank response
#'   plot based on the entered parameters.
#' @examples
#' \dontrun{
#' if(interactive()){
#'   # Launch the Shiny application
#'   testRetriveRankResponseData()
#'  }
#' }
#' @seealso
#'  \code{generate_plot_data}, \code{retrieve_rank_response_data}
#' @rdname testRetriveRankResponseData
#' @export
#' @importFrom shiny fluidPage titlePanel sidebarLayout sidebarPanel textInput numericInput mainPanel plotOutput
testRetriveRankResponseData <- function() {
  ui <- shiny::fluidPage(
    shiny::titlePanel("Retrieve Rank Response Data"),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::textInput("tf_gene", "Transcription Factor Gene", "HAP5"),
        shiny::numericInput("tf_id", "Transcription Factor ID", 6498),
        shiny::textInput("background_source", "Background Source", "adh1"),
        shiny::textInput("promoter_source", "Promoter Source", "yiming"),
        shiny::textInput("token", "token", Sys.getenv("TOKEN"))
      ),
      shiny::mainPanel(
        shiny::plotOutput("rankResponsePlot")
      )
    )
  )

  server <- function(input, output) {
    output$rankResponsePlot <- renderPlot(
      {
        retrieve_rank_response_data(
          tf_gene = input$tf_gene,
          tf_id = input$tf_id,
          background_source = input$background_source,
          promoter_source = input$promoter_source,
          token = input$token
        )
      },
      res = 96
    )
  }

  shinyApp(ui = ui, server = server)
}
