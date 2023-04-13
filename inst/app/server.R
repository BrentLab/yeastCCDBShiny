library(shiny)
library(labretriever)

server <- function(input, output, session) {

  auth_token <- reactiveVal()
  tf_manual_review_reactive <- reactiveValues(df=data.frame(), trigger=TRUE)
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
      showModal(modalDialog(id = "auth_failed_modal",
                            title = "Authentication Failed",
                            "Incorrect username or password.",
                            footer = modalButton("Close")))
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

      updateSelectInput(session, "tf_selector",
        choices = choice_list,
        selected = ''
      )
    }
  })

  observeEvent(input$tf_selector, {
    if (!is.null(input$tf_selector)) {
      TF_ID <- input$tf_selector
      SOURCE <- "yiming"
      BACKGROUND <- "adh1"

      token <- auth_token()

      cc_df <- labretriever::retrieve(
        labretriever::create_url("hopsreplicatesig_with_annote"),
        token,
        filter_list = list(
          tf_id = TF_ID,
          background = BACKGROUND,
          promoter_source = SOURCE
        )
      )

      tf_manual_review_df = labretriever::retrieve(
          labretriever::create_url('qcreview'),
          token,
          filter_list=list(tf_id = TF_ID)
        )

        tf_manual_review_reactive$df = tf_manual_review_df

      promoter_df <- labretriever::retrieve(
        labretriever::create_url("promoterregions_targets"),
        token,
        filter_list = list(source = SOURCE)
      )

      expr_df <- labretriever::retrieve(
        labretriever::create_url("expression"),
        token,
        filter_list = list(
          tf_id = TF_ID
        )
      )

      binding_df <- labretriever::retrieve(
        labretriever::create_url("harbisonchip_with_annote"),
        token,
        filter_list = list(tf_id = TF_ID)
      )

      rank_response_df <- process_tf_data(
        promoter_df,
        expr_df,
        binding_df,
        cc_df
      )

      rank_response_summary <- rankresponse::rank_response_ratio_summarize(
        rank_response_df
      )

      rank_response_plot = plot_rank_response(rank_response_summary)
      output$rank_response_plot = plotly::renderPlotly({rank_response_plot})
    }
  }, ignoreInit = TRUE)

  observe({
    tmp = tf_manual_review_reactive$trigger
    output$tf_manual_review_table =
      DT::renderDataTable(tf_manual_review_reactive$df %>%
                            DT::datatable(editable=TRUE))
  })

  # TODO: labretriever::send() with update=TRUE requires a column
  # names `id`. the table happens have have experiment_id which is the id
  # we want to use. But, this is very detail oriented, buried hard coding --
  # it needs to be removed to a config file
  observeEvent(input$tf_manual_review_table_cell_edit, {
    # Capture the changes in the DataTable
    info <- input$tf_manual_review_table_cell_edit
    i <- info$row
    # NOTE this seems like it * should be * off by 1 -- DT is 0 indexed,
    # R is 1 indexed. Not sure why this seems not to need to be incremented --
    # need to check data table
    j <- info$col
    value <- info$value
    experiment_id <- tf_manual_review_reactive$df[i, "experiment_id"]
    update_col = colnames(tf_manual_review_reactive$df)[j]
    df <- data.frame(id = experiment_id, value = value)
    df <- setNames(df, c("id", update_col))
    # Append the changes to the list with the corresponding id
    new_index <- length(tf_manual_review_changes$data) + 1
    tf_manual_review_changes$data[[new_index]] <- df

  }, ignoreInit = TRUE)

  observeEvent(input$update_button, {
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
            update=TRUE
          )
        }

      }, error = function(err) {
        # If an error occurs, show a modal with the error message
        showModal(modalDialog(
          title = "Error",
          paste0("An error occurred while updating the data: ",
                 err$message, "\n\n",
                 "ALL CHANGES CLEARED"),
          easyClose = TRUE
        ))
      }, finally = {
        # retrieve the updated data and update the reactive
        updated_data = labretriever::retrieve(
          labretriever::create_url("qcreview"),
          auth_token(),
          filter_list = list(tf_id = input$tf_selector)
        )
        # update the review reactive
        tf_manual_review_reactive$df = updated_data
        tf_manual_review_reactive$trigger = !tf_manual_review_reactive$trigger

        # Clear the changes list
        tf_manual_review_changes$data <- list()
      })
    }
  }, ignoreInit = TRUE)


} # end server




# auth_token = login_server('user_auth', auth_token)

# table_detail_server('db_table',
#                     shiny::reactive(input$db_tables),
#                     auth_token)

# Define the graph, dataTable1, and dataTable2 outputs based on the user's input.
# Replace with actual data and rendering logic.
# output$graph <- renderPlot({...})
# output$dataTable1 <- renderDataTable({...})
# output$dataTable2 <- renderDataTable({...})
