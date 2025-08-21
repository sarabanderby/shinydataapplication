library(shiny)
library(dplyr)
library(shinyjs)
library(leaflet)

# Load your data
merged_claims <- read.csv("merged_claims.csv", header = TRUE, sep = ',')

# ---- Aggregate by location including city ----
location_counts <- merged_claims %>%
  group_by(longitude, latitude, city) %>%
  summarise(reports = n(), .groups = "drop")

ui <- fluidPage(
  useShinyjs(), # needed for JS interactions
  
  # ---- Custom CSS for ICRC style ----
  tags$head(
    tags$style(HTML("
      body { background-color: white; color: #333; font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; }
      h2.app-title { color: #333; font-weight: 400; font-size: 28px; margin: 0; line-height: 60px; }
      .sidebar { background-color: #f7f7f7; border-right: 1px solid #ddd; padding: 15px; }
      .sidebar h3 { color: #ED1C24; }
      a { color: #ED1C24; }
      a:hover { color: #b50f1a; }
      .btn { background-color: #ED1C24; color: white; border: none; }
      .btn:hover { background-color: #b50f1a; }
      .leaflet-popup-content { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif; font-size: 14px; color: #333; }
      .leaflet-popup-content strong { color: #ED1C24; }
      .popup-link { color: #ED1C24; cursor: pointer; text-decoration: underline; }
      footer { text-align: center; color: gray; padding: 10px; font-size: 12px; margin-top: 10px; border-top: 1px solid #ddd; }
    "))
  ),
  
  # ---- Header with logo + title ----
  titlePanel(
    div(
      style = "display:flex; align-items:center; border-bottom: 3px solid #ED1C24; padding-bottom:5px;",
      img(src = "icrc.png", height = "60px", style = "margin-right:10px; background:white; padding:2px;"),
      h2("International Federation of Red Cross", class = "app-title")
    )
  ),
  
  # ---- Sidebar + main layout ----
  sidebarLayout(
    sidebarPanel(
      class = "sidebar",
      h3("Missing People"),
      helpText("Current geographical location with people reported missing."),
      actionButton("report_btn", "Report missing person", class = "btn",
                   style = "width: 200px; margin-bottom: 5px;"),
      actionButton("find_btn", "Find missing person", class = "btn",
                   style = "width: 200px; margin-top: 5px;")
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  ),
  
  # ---- Footer ----
  tags$footer("This is built as a demo and not intended for production use!")
)

server <- function(input, output, session) {
  
  # ---- Open ICRC links in new tabs ----
  observeEvent(input$report_btn, {
    runjs("window.open('https://familylinks.icrc.org/directory', '_blank')")
  })
  
  observeEvent(input$find_btn, {
    runjs("window.open('https://familylinks.icrc.org/online-tracing', '_blank')")
  })
  
  # ---- Render map with scaled circle markers, hover labels, and popup link ----
  output$map <- renderLeaflet({
    leaflet(location_counts) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        radius = ~sqrt(reports) * 2,
        color = "#ED1C24",
        fillColor = "#ED1C24",
        fillOpacity = 0.7,
        stroke = TRUE,
        weight = 1,
        layerId = ~paste0(longitude, "_", latitude),
        # Hover label showing city
        label = ~paste0(city),
        labelOptions = labelOptions(
          direction = "auto",
          textsize = "14px",
          opacity = 0.9,
          style = list(
            "color" = "#ED1C24",
            "font-family" = "'Helvetica Neue', Helvetica, Arial, sans-serif",
            "font-weight" = "bold",
            "background" = "white",
            "padding" = "5px",
            "border-radius" = "5px",
            "border" = "1px solid #ED1C24"
          )
        ),
        # Popup with clickable link for missing person list
        popup = ~paste0(
          "<strong>Missing Person Reports</strong><br/>",
          "City: ", city, "<br/>",
          "Reports: ", reports, "<br/>",
          "<span class='popup-link' onclick='Shiny.setInputValue(\"show_list\", \"",
          longitude, "_", latitude, "\", {priority: \"event\"})'>Click for missing person list</span>"
        )
      )
  })
  
  # ---- Show modal with all missing people at clicked location ----
  observeEvent(input$show_list, {
    coords <- strsplit(input$show_list, "_")[[1]]
    lng <- as.numeric(coords[1])
    lat <- as.numeric(coords[2])
    
    people_here <- merged_claims %>%
      filter(longitude == lng & latitude == lat)
    
    showModal(modalDialog(
      title = paste("Missing people in", people_here$city[1]),
      renderTable(people_here[, c("Missing.Person.Name", "Claim.Description")]),
      easyClose = TRUE,
      footer = modalButton("Close")
    ))
  })
}

shinyApp(ui, server)


