ui <- shiny::navbarPage(
  "Yeast Calling Cards DB",
  shiny::tabPanel("DB Tables",
                  shiny::selectInput(
                    "db_tables",
                    "Choose a table",
                    choices = names(raw_db_tables)),
                  table_detail_ui('db_table')),
  shiny::tabPanel("User Auth", login_ui("user_auth")),
  header = tags$head(
    tags$script(shiny::HTML('// In the client side JavaScript code

                $(document).on("shiny:connected", function() {
                console.log("hello world!!")

                    // Retrieve the authentication token from localStorage
                    var authToken = localStorage.getItem("authToken");

                    // If the authentication token is null, do nothing
                    if (authToken == null) {
                        return;
                    }

                    // Otherwise, set the authentication token in the Shiny input
                    Shiny.setInputValue("authToken", authToken);
                });

                // Custom message handler to save the authentication token in localStorage
                Shiny.addCustomMessageHandler("saveAuthToken", function(token) {
                    console.log(token);
                    localStorage.setItem("authToken", token);
                    console.log(localStorage.getItem("authToken"));
                });'))
  )
)
