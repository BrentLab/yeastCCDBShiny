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
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::radioButtons(ns("background_source"),
                            "Select a Background Set",
                            choices = c("adh1", "dSir4"),
                            selected = "adh1",
                            inline = TRUE
        ),
        shiny::radioButtons(ns("promoter_source"),
                            "Select a Promoter Set",
                            choices = c("yiming", "not_orf"),
                            selected = "yiming",
                            inline = TRUE
        )
      ),
      shiny::column(
        width = 4,
        shiny::sliderInput(
          ns("bin_size"),
          "Set a Rank Response Bin Size",
          min = 5,
          max = 50,
          value = 5,
          step = 5
        ),
        shiny::sliderInput(
          ns("qvalue_threshold"),
          "Set qvalue threshold",
          min = 5,
          max = 50,
          value = 5,
          step = 5
        )
      ),
      shiny::column(
        width = 4,
        shiny::checkboxInput(ns("normalize"),
                             "Normalize",
                             value = FALSE
        ),
        shiny::checkboxInput(ns("confidence_intervals"),
                             "Confidence Intervals",
                             value = FALSE
        )
      )
      # create a input dropdown selector
    ),
    shiny::tabsetPanel(
      shiny::tabPanel("CC vs Harbison",
                      shiny::plotOutput(ns("rank_response_plot_harbison"),
                      )
      ),
      shiny::tabPanel("CC vs Chipexo",
                      shiny::plotOutput(ns("rank_response_plot_chipexo"),
                      )
      ),
      shiny::tabPanel("Responsive Summary",
                      shiny::tableOutput(ns("random_df"))
      )

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
#' @param tf_gene Reactive Character. The common name of the gene, eg 'EDS1'
#' @param background_source Reactive Character. The background against which the hops'
#'   significance is calculated. Suggested default is 'adh1'
#' @param promoter_source Reactive Character. The promoter regions definition source
#'   against which the hops' significance is calculated. Suggested default
#'   is 'yiming'
#' @param normalize Reactive Boolean. Set to TRUE to normalize the number of responsive
#'   genes across (gene expression) data sets. Suggested default is False
#' @param bin_size Reactive Integer. The rank response bin size. Suggested default is 5
#' @param qvalue_thres Reactive Decimal. This will control the vertical line which
#'   partitions the rank response plot between values above and below a given
#'   qvalue threshold. See \url{https://github.com/StoreyLab/qvalue}
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
#' @export
rankResponsePlotServer <- function(id,
                                   tf_id,
                                   tf_gene,
                                   table_row_click,
                                   token,
                                   test = FALSE) {
  shiny::moduleServer(id, function(input, output, session) {
    input_check_list <- list(
      tf_id = tf_id,
      tf_gene = tf_gene,
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

    rr_plot_harbison <- reactiveVal(NULL)
    rr_plot_chipexo <- reactiveVal(NULL)
    random_df <- reactiveVal(NULL)
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
          input$background_source,
          input$pormoter_source,
          token()
        )
        rr_df(df)
      }
    })

    shiny::observeEvent(rr_df(), {
      gen_plot_data_out <- yeastCCDBShiny::generate_plot_data(
        rr_df(),
        tf_gene()
      )
      rr_plot_harbison(gen_plot_data_out$harbison)
      rr_plot_chipexo(gen_plot_data_out$chipexo)
      random_df(gen_plot_data_out$random_df)
    })

    shiny::observeEvent(rr_plot_harbison(), {
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
      output$rank_response_plot_harbison <- shiny::renderPlot({
        rr_plot_harbison()
      })
    })

    shiny::observeEvent(rr_plot_chipexo(), {
      # # Get the plot data
      # pb <- ggplot2::ggplot_build(rr_plot_chipexo())
      # # Get the data for the first layer (geom_point)
      # layer_data <- pb$data[[1]]
      # # extract the color vector
      # color_vector <- unique(layer_data$colour)

      # output$rank_response_plot_chipexo <- plotly::renderPlotly({
      #   rr_plot_chipexo() %>%
      #     plotly::ggplotly() %>%
      #     plotly::layout(
      #       colorway = color_vector,
      #       legend = list(
      #         traceorder = "normal",
      #         font = list(
      #           family = "sans-serif",
      #           size = 11,
      #           color = "black"
      #         ),
      #         bgcolor = "#E2E2E2",
      #         bordercolor = "#FFFFFF",
      #         borderwidth = 2
      #       )
      #     )
      # })
      output$rank_response_plot_chipexo <- shiny::renderPlot(
        rr_plot_chipexo()
      )
    })

    shiny::observeEvent(random_df(), {
      output$random_df = shiny::renderTable(random_df())
    })

    # whether or not to normalize the rank response plots by reducing the
    # number of genes to the minimum of the set of responsive data
    shiny::observeEvent(input$normalize,
      {
        message("(un)normalizing!")
      },
      ignoreInit = TRUE
    )

    # this will need to re-pull data -- cache it
    shiny::observeEvent(input$promoter_source,
      {
        message("changing promoter source!")
      },
      ignoreInit = TRUE
    )

    # this will need to re-pull data -- cache it
    shiny::observeEvent(input$background_source,
      {
        message("changing background source!")
      },
      ignoreInit = TRUE
    )

    # adjust the bin size of the
    shiny::observeEvent(input$bin_size,
      {
        message("Changing bin size!")
      },
      ignoreInit = TRUE
    )

    # turn on and off the 95% binomial CI
    shiny::observeEvent(input$confidence_intervals,
      {
        message("turning on/off confidence intervals!")
      },
      ignoreInit = TRUE
    )

    # move a verticle line on the plot left/right by changing the qvalue
    # threshold. Adjust the qvalue threshold and consider providing dist of
    # effect/pvalue of expression and binding
    shiny::observeEvent(input$qvalue_thres,
      {
        message("changing qvalue thres!")
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
#' @importFrom shiny fluidPage reactive shinyApp
#'
#' @export
testRankResponsePlotModule <- function() {
  ui <- shiny::fluidPage(
    rankResponsePlotUI("rr_plot")
  )
  server <- function(input, output, session) {
    tf_id <- reactiveVal("")
    tf_gene <- reactiveVal("")
    rankResponsePlotServer("rr_plot",
      tf_id = tf_id,
      tf_gene = tf_gene,
      table_row_click = shiny::reactive(NULL),
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
