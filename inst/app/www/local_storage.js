// In the client side JavaScript code
$(document).on("shiny:connected", function() {
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
  localStorage.setItem("authToken", token);
});
