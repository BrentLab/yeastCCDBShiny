library(shiny)
library(labretriever)

server <- function(input, output, session) {
  auth_token <- reactiveVal()
  tf_choice_list <- reactiveValues(tfs = list())
  tf_manual_review_reactive <- reactiveValues(df = data.frame(), trigger = TRUE)
  # Initialize an empty list to store the changes
  tf_manual_review_changes <- reactiveValues(data = list())

  # initialize the DT proxy
  dt_proxy <- DT::dataTableProxy("tf_manual_review_table")


  observeEvent(input$show_login_modal, {
    showModal(modalDialog(
      id = "login_module",
      title = "Login",
      textInput("username", "Username"),
      passwordInput("password", "Password"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("login", "Log In")
      ),
      size = "m",
      easyClose = FALSE,
      fade = TRUE
    ))
  })

  observeEvent(input$login, {
    req(input$username, input$password)

    token <-
      labretriever::get_user_auth_token(
        labretriever::create_url("auth_token",
          base_url = yeastCCDBShiny::base_url
        ),
        input$username,
        input$password
      )

    if (!is.null(token)) {
      removeModal()
      shinyjs::html("user_info", paste("User:", input$username))
      shinyjs::show("user_info")
      shinyjs::hide("show_login_modal")
      session$sendCustomMessage("loginState", 1)
      auth_token(token)
    } else {
      removeModal()
      showModal(modalDialog(
        id = "auth_failed_modal",
        title = "Authentication Failed",
        "Incorrect username or password.",
        footer = modalButton("Close")
      ))
    }
  })

  # this observer executes when the token is updated -- use this to do the
  # initial setup
  observeEvent(auth_token(), {
    if (!is.null(auth_token())) {
      # retrieve list of active TFs
      # TODO add error handling
      # TODO use the user group attr to retrieve the right list of TFs here
      tf_list_df <- labretriever::retrieve(
        labretriever::create_url("cctf_tf_list",
          base_url = yeastCCDBShiny::base_url
        ),
        auth_token()
      )

      choice_list <- setNames(
        tf_list_df$tf_id,
        tf_list_df$tf_gene
      )
      # update the reactive
      tf_choice_list$tfs <- choice_list

      updateSelectInput(session, "tf_selector",
        choices = choice_list,
        selected = ""
      )
    }
  })

  # rank response plot and tf set review table --------------------------------
  observeEvent(input$tf_selector,
    {
      if (!is.null(input$tf_selector)) {
        TF_ID <- input$tf_selector
        TF_GENE <- names(tf_choice_list$tfs)[which(tf_choice_list$tfs ==
          input$tf_selector)]
        SOURCE <- "yiming"
        BACKGROUND <- "adh1"

        n_steps <- 5

        token <- auth_token()

        shiny::withProgress(
          message = "Fetching data...",
          detail = paste0(
            "Please wait while the data for TF ",
            TF_GENE,
            " is being retrieved."
          ),
          value = 0,
          {
            shiny::incProgress(1 / n_steps,
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
              tf_id = TF_ID,
              background_source = BACKGROUND,
              promoter_source = SOURCE
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

            shiny::incProgress(1 / n_steps,
              message = "retrieving review data"
            )

            tf_manual_review_df <- labretriever::retrieve(
              labretriever::create_url("qcreview",
                base_url = yeastCCDBShiny::base_url
              ),
              token,
              filter_list = list(tf_id = TF_ID)
            )

            tf_manual_review_reactive$df <- tf_manual_review_df %>%
              dplyr::arrange(experiment_id)

            shiny::incProgress(1 / n_steps,
              message = "retrieving expression data"
            )

            expr_df <- labretriever::retrieve(
              labretriever::create_url("expression",
                base_url = yeastCCDBShiny::base_url
              ),
              token,
              filter_list = list(
                tf_id = TF_ID
              )
            )

            shiny::incProgress(1 / n_steps,
              message = "retrieving outside binding data",
              detail = "currently: Harbison ChIP"
            )

            binding_df <- labretriever::retrieve(
              labretriever::create_url("harbisonchip_with_annote",
                base_url = yeastCCDBShiny::base_url
              ),
              token,
              filter_list = list(tf_id = TF_ID)
            )

            shiny::incProgress(1 / n_steps,
              message = "Creating Plot"
            )

            tryCatch(
              {
                rank_response_df <- yeastCCDBShiny::process_tf_data(
                  expr_df,
                  binding_df,
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
                  max(min(length(binding_levels),9),3), "Set1"
                )

                if(length(binding_levels) > 9){
                  color_palette =
                    colorRampPalette(color_palette)(length(binding_levels))
                } else{
                  # color brewer returns a minimum of 3 -- use this to set to less
                  color_palette <- color_palette[seq(length(binding_levels))]
                }


                # create a named vector with the binding_levels as
                # names and the color_palette as values
                color_vector <- setNames(color_palette, binding_levels)

                # set harbison to black
                color_vector[names(color_vector) == "harbison"] <- "black"

                rank_response_plot <- yeastCCDBShiny::plot_rank_response(
                  rank_response_summary,
                  color_vector,
                  TF_GENE
                )

                output$rank_response_plot <- plotly::renderPlotly({
                  rank_response_plot %>%
                    plotly::ggplotly() %>%
                    plotly::layout(
                      colorway = color_vector,
                      line_dash = c("solid", "solid"),
                      legend = list(
                        traceorder = "normal",
                        font = list(
                          family = "sans-serif",
                          size = 11,
                          color = "black"
                        ),
                        bgcolor = "#E2E2E2",
                        bordercolor = "#FFFFFF",
                        borderwidth = 2
                      )
                    )
                })
              },
              warning = function(w) {
                futile.logger::flog.warn(w)
              },
              error = function(e) {
                futile.logger::flog.error(e)
              }
            )
          }
        )
      }
    },
    ignoreInit = TRUE
  )

  observe({
    tmp <- tf_manual_review_reactive$trigger
    output$tf_manual_review_table <-
      DT::renderDataTable(tf_manual_review_reactive$df %>%
        DT::datatable(
          editable = TRUE,
          selection = "multiple",
          options = list(
            scrollX = TRUE,
            width = "100%"
          )
        ))
  })

  # TODO: labretriever::send() with update=TRUE requires a column
  # names `id`. the table happens have have experiment_id which is the id
  # we want to use. But, this is very detail oriented, buried hard coding --
  # it needs to be removed to a config file
  observeEvent(input$tf_manual_review_table_cell_edit,
    {
      # Capture the changes in the DataTable
      info <- input$tf_manual_review_table_cell_edit
      i <- info$row
      # NOTE this seems like it * should be * off by 1 -- DT is 0 indexed,
      # R is 1 indexed. Not sure why this seems not to need to be incremented --
      # need to check data table
      j <- info$col
      value <- info$value
      experiment_id <- tf_manual_review_reactive$df[i, "experiment_id"]
      update_col <- colnames(tf_manual_review_reactive$df)[j]
      df <- data.frame(id = experiment_id, value = value)
      df <- setNames(df, c("id", update_col))
      # Append the changes to the list with the corresponding id
      new_index <- length(tf_manual_review_changes$data) + 1
      tf_manual_review_changes$data[[new_index]] <- df
      # and update the session state
      session$sendCustomMessage(
        "updateManualReviewChangesLength",
        length(tf_manual_review_changes$data)
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(input$cancel_update_button, {
    if (length(tf_manual_review_changes$data) > 0) {
      # Clear the changes list
      tf_manual_review_changes$data <- list()
      # update the session variable
      session$sendCustomMessage(
        "updateManualReviewChangesLength",
        length(tf_manual_review_changes$data)
      )
      # reset the table
      shiny::withProgress(
        message = "Resetting table...",
        detail = "Please wait while the review table is reset... ",
        value = 0,
        {
          shiny::incProgress(1 / 1,
            message = "Retriving review table data ..."
          )
          updated_data <- labretriever::retrieve(
            labretriever::create_url("qcreview",
              base_url = yeastCCDBShiny::base_url
            ),
            auth_token(),
            filter_list = list(tf_id = input$tf_selector)
          )
          # update the review reactive
          tf_manual_review_reactive$df <- updated_data
          tf_manual_review_reactive$trigger <- !tf_manual_review_reactive$trigger
        }
      )
    }
  })

  observeEvent(input$update_button,
    {
      # Check if there are any changes
      if (length(tf_manual_review_changes$data) > 0) {
        # Process the changes and send updates to the database
        tryCatch({
          # Iterate through the changes and send updates for each changed field
          for (change in tf_manual_review_changes$data) {
            labretriever::send(
              df = change,
              url = labretriever::create_url("qcreview",
                base_url = yeastCCDBShiny::base_url
              ),
              token = auth_token(),
              update = TRUE
            )
          }
        }, error = function(err) {
          # If an error occurs, show a modal with the error message
          showModal(modalDialog(
            title = "Error",
            paste0(
              "An error occurred while updating the data: ",
              err$message, "\n\n",
              "ALL CHANGES CLEARED"
            ),
            easyClose = TRUE
          ))
        }, finally = {
          # retrieve the updated data and update the reactive
          updated_data <- labretriever::retrieve(
            labretriever::create_url("qcreview",
              base_url = yeastCCDBShiny::base_url
            ),
            auth_token(),
            filter_list = list(tf_id = input$tf_selector)
          )
          # update the review reactive
          tf_manual_review_reactive$df <- updated_data
          tf_manual_review_reactive$trigger <- !tf_manual_review_reactive$trigger

          # Clear the changes list
          tf_manual_review_changes$data <- list()
        })
      }
    },
    ignoreInit = TRUE
  )
} # end server
