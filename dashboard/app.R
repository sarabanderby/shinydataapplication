library(shiny)
library(skimr)

setwd("/application") 
missing_person_claims <- read.csv("datafile.csv", header = TRUE, sep = ";")

ui <- fluidPage(
  h2("Missing Person Claims"),
    h3("This is built as a demo and not intended for production use!"),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    skim(missing_person_claims)
  })
  
  output$table <- renderTable({
    missing_person_claims
  })
}

shinyApp(ui = ui, server = server)
