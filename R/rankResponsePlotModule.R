#' @title Rank Response Plot Module UI
#'
#' @description A UI object which contains the plotly output for a rank
#'   response plot
#'
#' @param id Element ID. see \url{https://mastering-shiny.org/scaling-modules.html}
#'
#' @return the module UI
#'
#' @seealso
#'  \code{rankResponsePlotServer}, \code{testRankResponsePlotModule}
#'
#' @rdname rankResponsePlotUI
#'
#' @importFrom shiny NS tagList
#' @importFrom plotly plotlyOutput
#' @export
rankResponsePlotUI <- function(id) {
  ns <- shiny::NS(id)

  tagList(
    div(
      id = ns("plots_container"), class = "plots-container",
      div(plotly::plotlyOutput(ns("kemmeren_tfko")), id = ns("kemmeren_tfko_container")),
      div(plotly::plotlyOutput(ns("mcisaac_zev")), id = ns("mcisaac_zev_container"))
    )
  )
}



#' @title Rank Response Plot Module Server
#'
#' @description Server for the rank response plot module
#'
#' @param id Element ID. see \url{https://mastering-shiny.org/scaling-modules.html}
#' @param tf_id Reactive Character. The transcription factor ID from the database
#'   backend -- this is the ID of that gene in the genes table. Though this is
#'   an integer, it is passed as a Character vector, eg "1234".
#' @param rank_response_df Reactive DataFrame. this is passed in from the
#'   outside so that it can be updated in this server function and shared
#'   with other elements in the app.
#' @param tf_gene Reactive Character. The common name of the gene, eg 'EDS1'
#' @param background_source Reactive Character. The background against which the hops'
#'   significance is calculated. Suggested default is 'adh1'
#' @param promoter_source Reactive Character. The promoter regions definition source
#'   against which the hops' significance is calculated. Suggested default
#'   is 'yiming'
#' @param normalize Reactive Boolean. Set to TRUE to normalize the number of responsive
#'   genes across (gene expression) data sets. Suggested default is False
#' @param bin_size Reactive Integer. The rank response bin size. Suggested default is 5
#' @param confidence_intervals Reactive Boolean. Set to TRUE to display the range of
#'   values a given rank response bin may take with a chance of 95% or greater
#' @param table_row_click Reactive. This reactive is meant to facilitate actions
#'   on the plot, eg highlighting, when the user clicks a given row in the QC
#'   table
#' @param token Reactive Character. User authorization token
#' @param test Reactive Boolean. Set to TRUE to pull data from the package
#'   resources rather than from the database
#'
#' @return The module server function, but otherwise no explicit reactive
#'   vlaues are returned
#'
#' @seealso
#'  \code{\link{rankResponsePlotUI}}, \code{\link{testRankResponsePlotModule}}
#'
#' @rdname rankResponsePlotServer
#'
#' @importFrom shiny moduleServer is.reactive observeEvent renderPlot
#' @importFrom futile.logger flog.error
#' @importFrom ggplot2 ggplot_build
#' @importFrom plotly renderPlotly ggplotly layout
#'
#' @export
rankResponsePlotServer <- function(id,
                                   tf_id,
                                   tf_gene,
                                   rank_response_df,
                                   table_row_click,
                                   hops_source,
                                   background_source,
                                   promoter_source,
                                   bin_size,
                                   normalize_selector,
                                   confidence_intervals,
                                   token,
                                   test = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    input_check_list <- list(
      tf_id = tf_id,
      tf_gene = tf_gene,
      rank_response_df = rank_response_df,
      table_row_click = table_row_click,
      hops_source = hops_source,
      background_source = background_source,
      promoter_source = promoter_source,
      bin_size = bin_size,
      normalize_selector = normalize_selector,
      confidence_intervals = confidence_intervals,
      token = token
    )
    for (i in names(input_check_list)) {
      if (!shiny::is.reactive(input_check_list[[i]])) {
        futile.logger::flog.error(sprintf("input %s must be reactive", i))
      }
    }

    if (shiny::is.reactive(test)) {
      futile.logger::flog.error(sprintf("input `test` should not be reactive"))
    }

    rr_plots <- reactiveValues(
      kemmeren_tfko = NULL,
      mcisaac_zev = NULL
    )
    rr_df <- reactiveVal(NULL)

    shiny::observeEvent(tf_id(), {
      if (test) {
        if (tf_gene() == "HAP5") {
          rr_df(yeastCCDBShiny::hap5_rr_data)
        }
      } else {
        df <- retrieve_rank_response_data(
          tf_gene(),
          tf_id(),
          hops_source(),
          background_source(),
          promoter_source(),
          token()
        )
        rr_df(df)
      }
    })

    shiny::observeEvent(hops_source(),
      {
        if (test) {
          if (tf_gene() == "HAP5") {
            rr_df(yeastCCDBShiny::hap5_rr_data)
          }
        } else {
          df <- retrieve_rank_response_data(
            tf_gene(),
            tf_id(),
            hops_source(),
            background_source(),
            promoter_source(),
            token()
          )
          rr_df(df)
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(background_source(),
      {
        if (test) {
          if (tf_gene() == "HAP5") {
            rr_df(yeastCCDBShiny::hap5_rr_data)
          }
        } else {
          df <- retrieve_rank_response_data(
            tf_gene(),
            tf_id(),
            hops_source(),
            background_source(),
            promoter_source(),
            token()
          )
          rr_df(df)
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(promoter_source(),
      {
        if (test) {
          if (tf_gene() == "HAP5") {
            rr_df(yeastCCDBShiny::hap5_rr_data)
          }
        } else {
          df <- retrieve_rank_response_data(
            tf_gene(),
            tf_id(),
            hops_source(),
            background_source(),
            promoter_source(),
            token()
          )
          rr_df(df)
        }
      },
      ignoreInit = TRUE
    )

    shiny::observeEvent(rr_df(), {
      gen_plot_data_out <- generate_plot_data(
        rr_df(),
        tf_gene(),
        normalize_selector(),
        bin_size()
      )
      flog.info(paste0('names of gen_plot_data_out: ',
                       paste(names(gen_plot_data_out), collapse = '", "')))
      rr_plots$kemmeren_tfko <- gen_plot_data_out$plt$kemmeren_tfko
      rr_plots$mcisaac_zev <- gen_plot_data_out$plt$mcisaac_zev
      rank_response_df(gen_plot_data_out$rank_response_df)
    })

    shiny::observeEvent(rr_plots,
      {
        #   # Get the plot data
        #   pb <- ggplot2::ggplot_build(rr_plot_harbison())
        #   # Get the data for the first layer (geom_point)
        #   layer_data <- pb$data[[1]]
        #   # extract the color vector
        #   color_vector <- unique(layer_data$colour)
        #   output$rank_response_plot_harbison <- plotly::renderPlotly({
        #     rr_plot_harbison() %>%
        #       plotly::ggplotly() %>%
        #       plotly::layout(
        #         colorway = color_vector,
        #         legend = list(
        #           traceorder = "normal",
        #           font = list(
        #             family = "sans-serif",
        #             size = 11,
        #             color = "black"
        #           ),
        #           bgcolor = "#E2E2E2",
        #           bordercolor = "#FFFFFF",
        #           borderwidth = 2
        #         )
        #       )
        #   })
        output$kemmeren_tfko <- plotly::renderPlotly(
          rr_plots$kemmeren_tfko
        )
        output$mcisaac_zev <- plotly::renderPlotly(
          rr_plots$mcisaac_zev
        )
      },
      ignoreNULL = TRUE
    )

    # whether or not to normalize the rank response plots by reducing the
    # number of genes to the minimum of the set of responsive data
    shiny::observeEvent(normalize_selector(),
      {
        gen_plot_data_out <- generate_plot_data(
          rr_df(),
          tf_gene(),
          normalize_selector(),
          bin_size()
        )
        rr_plots$kemmeren_tfko <- gen_plot_data_out$plt$kemmeren_tfko
        rr_plots$mcisaac_zev <- gen_plot_data_out$plt$mcisaac_zev
        rank_response_df(gen_plot_data_out$rank_response_df)
      },
      ignoreInit = TRUE
    )

    # adjust the bin size of the
    shiny::observeEvent(bin_size(),
      {
        gen_plot_data_out <- generate_plot_data(
          rr_df(),
          tf_gene(),
          normalize_selector(),
          bin_size(),
          confidence_intervals()
        )
        rr_plots$kemmeren_tfko <- gen_plot_data_out$plt$kemmeren_tfko
        rr_plots$mcisaac_zev <- gen_plot_data_out$plt$mcisaac_zev
        rank_response_df(gen_plot_data_out$rank_response_df)
      },
      ignoreInit = TRUE
    )

    # turn on and off the 95% binomial CI
    shiny::observeEvent(confidence_intervals(),
      {
        gen_plot_data_out <- generate_plot_data(
          rr_df(),
          tf_gene(),
          normalize_selector(),
          bin_size(),
          confidence_intervals()
        )
        rr_plots$kemmeren_tfko <- gen_plot_data_out$plt$kemmeren_tfko
        rr_plots$mcisaac_zev <- gen_plot_data_out$plt$mcisaac_zev
        rank_response_df(gen_plot_data_out$rank_response_df)
      },
      ignoreInit = TRUE
    )

    # when a table row is clicked, highlight the corresponding line and increase
    # the transparency of the other lines
    shiny::observeEvent(table_row_click,
      {
        message("responding to the table row click!")
      },
      ignoreInit = TRUE
    )
  })
}

#' @title Rank Response Plot Module Test App
#' @description A function to launch an app which displays only the rank
#'   response module UI and demonstrates the server functionality
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' if (interactive()) {
#'   testRankResponsePlotModule()
#' }
#' }
#' @rdname testRankResponsePlotModule
#'
#' @importFrom shiny fluidPage reactive shinyApp HTML
#'
#' @export
testRankResponsePlotModule <- function() {
  ui <- shiny::fluidPage(
    tags$head(tags$style(
      shiny::HTML("
         .plots-container {
           display: flex;
           justify-content: space-between;
         }
         #rr_plot-kemmeren_tfko_container, #rr_plot-mcisaac_zev_container {
           width: 50%;
         }
         ")
    )),
    rankResponsePlotUI("rr_plot")
  )


  server <- function(input, output, session) {
    tf_id <- reactiveVal("")
    tf_gene <- reactiveVal("")
    hops_source <- shiny::reactiveVal(NULL)
    background_source <- shiny::reactiveVal(NULL)
    promoter_source <- shiny::reactiveVal(NULL)
    bin_size <- shiny::reactiveVal(NULL)
    normalize_selector <- shiny::reactiveVal(NULL)
    confidence_intervals <- shiny::reactiveVal(NULL)
    rankResponsePlotServer("rr_plot",
      tf_id = tf_id,
      tf_gene = tf_gene,
      table_row_click = shiny::reactive(NULL),
      hops_source = hops_source,
      background_source = background_source,
      promoter_source = promoter_source,
      bin_size = bin_size,
      normalize_selector = normalize_selector,
      confidence_intervals = confidence_intervals,
      token = shiny::reactive(Sys.getenv("TOKEN")),
      test = TRUE
    )
    observe({
      tf_id("6498")
      tf_gene("HAP5")
    })
  }
  shiny::shinyApp(ui, server)
}
