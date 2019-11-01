library(shiny)
library(shinyFiles)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(leaflet)
source("./src/PedigreeEngine.R")

# Define UI ----
ui <- fluidPage(
  titlePanel("Pedigree Engine"),

  sidebarLayout(
    sidebarPanel(
       # textInput("var", label = "Enter Command", value = "")
      fileInput("file1", "Choose input text File", accept = c(".txt"))
    ),
    mainPanel(
      imageOutput("image")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  output$image <- renderImage({
    relation_file <- input$file1
    if (is.null(relation_file)) return(NULL)
    out_jpg_path <- producePED(relation_file)
    pedigree <- normalizePath(file.path(out_jpg_path))
    list(src = pedigree)
  })

}

# Run the app ----
shinyApp(ui = ui, server = server)