# Load required libraries
library(shiny)
library(rpart)  # Replace with your specific machine learning library
library(generics)
# library(dplyr)

# Load your pre-trained model
tree_model <- readRDS("password_tree.rda")  # Replace with your model's file path

# Define the Shiny UI
ui <- fluidPage(
  titlePanel("Password Strength Classifier"),

  sidebarLayout(
    sidebarPanel(
      passwordInput("password", "Enter a Password:", placeholder = "Password"),
      actionButton("predictButton", "Classify"),
      verbatimTextOutput("predictionOutput")
    ),

    mainPanel(
      HTML("<p>Welcome to the Password Strength Classifier.</p>
        <p>Im a little decision tree that has been trained on 669640 passwords in order to tell you whether it's weak, medium, or strong.</p>
        <p>No, I don't take Quantum Computing into consideration.</p>
        <p>Insert your password here to find out! It will not be saved or sent anywhere.</p>
        <p>DISCLAMER</p>
        <p>Please note that password strength assessment is a complex subject with multiple factors to consider. The decision tree provided here is a simplified representation for educational purposes.</p>
        <p> Real-world password security relies on a combination of factors, including character complexity, length, uniqueness, and protection against various attacks.
        <p>This decision tree should not be used as the sole method for real-time password strength assessment or security decisions.</p>"),
    )
  )
)

# Define the Shiny server logic
server <- function(input, output) {

  observeEvent(input$predictButton, {
    # Extract the input password
    password <- input$password

    # Check if the password is empty or contains only whitespace
    if (nchar(trimws(password)) == 0) {
      # If the password is empty, display a message to the user
      output$predictionOutput <- renderText({
        "Please enter a password."
      })
    } else {
      # Otherwise, proceed with the password strength classification as before
      num_characters <- nchar(password)
      num_uppercase <- sum(grepl("[A-Z]", password))
      num_lowercase <- sum(grepl("[a-z]", password))
      num_special <- num_characters - num_uppercase - num_lowercase

      input_data <- data.frame(
        TotalChars = num_characters,
        UppercaseCount = num_uppercase,
        LowercaseCount = num_lowercase,
        SpecialCharCount = num_special
      )

      # Make predictions using your pre-trained model
      prediction <- predict(tree_model, new_data = input_data, type = "class")

      # Map the model's prediction to password strength categories
      password_strength <- dplyr::case_when(
        prediction == 0 ~ "Weak",
        prediction == 1 ~ "Medium",
        prediction == 2 ~ "Strong",
        TRUE ~ "Unknown"  # Handle other cases if needed
      )

      # Output the prediction as text
      output$predictionOutput <- renderText({
        paste("Strength:", password_strength)
      })
    }
  })
}

# Run the Shiny application
shinyApp(ui, server)
