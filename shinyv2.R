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
      div(HTML("<b>Choose Directory Containing PED file:</b>"), style = "margin-bottom: 5px;"),
        shinyDirButton('pedLocation', 'Browse...', title = 'Select a directory to save file in'),
        br(),
        htmlOutput('pedDirectory', inline = TRUE),  
        tags$hr(),
        textInput("pedtext", label = "Name PED", value = ".ped"),
        tags$hr(),
        fileInput("file1", "Choose input text File", accept = c(".txt")),
        tags$hr(),
        textAreaInput("command", "Enter Commands", "", height = "150px"),
        tags$hr(),
        actionButton("action1", "Update", class = "btn-primary")
      ),
  mainPanel(
    fluidRow(
      column(3, offset = 1,
      textInput("pedigreeText", label = "Name Graph", value = ".png")
      ),
      column(3, offset = 2, style="padding-top:25px",
      downloadButton('pedigreeDownload', 'Download Pedigree')
      )
    ),
    hr(),
    imageOutput("image")
    )
  )
)

# Define server logic ----
server <- function(input, output, session) {

  dataUpload = reactiveValues(
    ped_folder = NULL
  )
  
  volumes <- getVolumes()
  
  shinyDirChoose(input, 'pedLocation', roots=volumes, session=session)
 
  observeEvent(input$pedLocation, {
    dataUpload$ped_folder = toString(parseDirPath(volumes, input$pedLocation))
  })
  
  output$pedDirectory <- renderUI({
    if (dataUpload$ped_folder != '' && ! is.null(dataUpload$ped_folder)) {
      helpText(HTML(paste0("<b>Selected directory:</b> ", dataUpload$ped_folder)))
    }
  })

  output$image <- renderImage({
    relation_file <- input$file1
    if (is.null(relation_file)) return(NULL)
    file_name <- as.character(relation_file['name'])
    file_path <- file.path(getwd(), "sample_input", file_name)
    out_jpg_path <- producePED(file_path)
    pedigree <- normalizePath(file.path(out_jpg_path))
    list(src = pedigree)
  })

  output$pedigreeDownload <- downloadHandler(
      filename = input$pedigreeText,
      content = function(file) {
        ggsave(input$pedigreeText, plot = input$image, device = "png")
      }
    )

}


# Run the app ----
shinyApp(ui = ui, server = server)