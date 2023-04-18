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
        labretriever::create_url("auth_token"),
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
      tf_list_df <- labretriever::retrieve(
        labretriever::create_url("cctf_tf_list"),
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

  observeEvent(input$tf_selector,
    {
      if (!is.null(input$tf_selector)) {
        TF_ID <- input$tf_selector
        TF_GENE <- names(tf_choice_list$tfs)[which(tf_choice_list$tfs == input$tf_selector)]
        SOURCE <- "yiming"
        BACKGROUND <- "adh1"

        n_steps = 6

        token <- auth_token()

        shiny::withProgress(
          message = "Fetching data...",
          detail = paste0("Please wait while the data for TF ",
                          TF_GENE,
                          " is being retrieved."),
          value = 0,
          {
            shiny::incProgress(1/n_steps,
                        message="retrieving calling cards data")
            cc_df <- labretriever::retrieve(
              labretriever::create_url("hopsreplicatesig_with_annote"),
              token,
              filter_list = list(
                tf_id = TF_ID,
                background = BACKGROUND,
                promoter_source = SOURCE
              )
            )

            shiny::incProgress(1/n_steps,
                        message="retrieving review data")
            tf_manual_review_df <- labretriever::retrieve(
              labretriever::create_url("qcreview"),
              token,
              filter_list = list(tf_id = TF_ID)
            )

            tf_manual_review_reactive$df <- tf_manual_review_df %>%
              dplyr::arrange(experiment_id)

            shiny::incProgress(1/n_steps,
                        message="retrieving promoter details")
            promoter_df <- labretriever::retrieve(
              labretriever::create_url("promoterregions_targets"),
              token,
              filter_list = list(source = SOURCE)
            )

            shiny::incProgress(1/n_steps,
                        message="retrieving expression data")
            expr_df <- labretriever::retrieve(
              labretriever::create_url("expression"),
              token,
              filter_list = list(
                tf_id = TF_ID
              )
            )

            shiny::incProgress(1/n_steps,
                        message="retrieving outside binding data",
                        detail = "currently: Harbison ChIP")
            binding_df <- labretriever::retrieve(
              labretriever::create_url("harbisonchip_with_annote"),
              token,
              filter_list = list(tf_id = TF_ID)
            )

            shiny::incProgress(1/n_steps,
                        message="Creating Plot")
            rank_response_df <- process_tf_data(
              promoter_df,
              expr_df,
              binding_df,
              cc_df
            )

            rank_response_summary <-
              rankresponse::rank_response_ratio_summarize(
                rank_response_df)

            # get the unique binding_src levels
            binding_levels <- unique(rank_response_summary$rr$binding_src)

            # create a color palette with the same number of
            # colors as binding_levels
            color_palette <- RColorBrewer::brewer.pal(
              length(binding_levels), "Set1")

            # create a named vector with the binding_levels as
            # names and the color_palette as values
            color_vector <- setNames(color_palette, binding_levels)

            rank_response_plot <- plot_rank_response(rank_response_summary,
                                                     color_vector,
                                                     TF_GENE)

            output$rank_response_plot <- plotly::renderPlotly({
              rank_response_plot %>%
                plotly::ggplotly() %>%
                plotly::layout(colorway = color_vector,
                               line_dash = c('solid', 'solid'),
                               legend = list(
                                 traceorder = "normal",
                                 font = list(family = "sans-serif",
                                             size = 11,
                                             color = "black"),
                                 bgcolor = "#E2E2E2",
                                 bordercolor = "#FFFFFF",
                                 borderwidth = 2))
            })
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
        shiny::incProgress(1/1,
                           message = "Retriving review table data ...")
        updated_data <- labretriever::retrieve(
          labretriever::create_url("qcreview"),
          auth_token(),
          filter_list = list(tf_id = input$tf_selector)
        )
        # update the review reactive
        tf_manual_review_reactive$df <- updated_data
        tf_manual_review_reactive$trigger <- !tf_manual_review_reactive$trigger
        })
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
              url = labretriever::create_url("qcreview"),
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
            labretriever::create_url("qcreview"),
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
