library(shiny)
library(shinyFiles)
library(shinyjs)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(leaflet)
source("./src/PedigreeEngine.R")
if(!require(shinythemes)) install.packages("shinythemes")


# Define UI ----
ui <- fillPage(
  padding = 5,
  theme = shinytheme("journal"),
  shinythemes::themeSelector(),
  titlePanel("Pedigree Engine"),

  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = 'newbutton',label = 'New FILE', width = '200px'),
      hr(),
      fileInput("file1", "Choose input text File", accept = c(".txt")),
      textAreaInput('inputbox','command',height = '7cm'),
      sliderInput('distance','Distance between nodes',min = 0.01,max = 2,value = 0.7),
      actionButton(inputId = 'generatebutton',label = 'generate graph', icon = icon("refresh")),
      

      
      width = 4
      
    ),
    mainPanel(
      fluidRow(
        column(3, offset = 1,
               textInput("pedigreeText", label = "Name .ped file", value = ".ped")
        ),
        column(3, offset = 2, style="padding-top:25px",
               downloadButton('downloadbutton', 'Download Pedigree file')
        )
      ),
      hr(),
      imageOutput("image"),
      
      width = 4
    )
  )
)

# Define server logic ----
server <- function(input, output,session) {
  
  
  #rendering image 
  output$image <- renderImage({
    #print(getwd())
    relation_file <- input$file1

    if (is.null( relation_file)){ 
      blank <- paste(getwd(),'/src/blank.png',sep = '')
      pedigree <- normalizePath(blank)
      list(src = pedigree)
    } else{
      text <- preview(input$file1$datapath)
      updateTextAreaInput(session,'inputbox',
                          value = text                        
      )
      
      out_jpg_path <- producePED(relation_file$datapath)
      pedigree <- normalizePath(file.path(out_jpg_path))
      list(src = pedigree)
      }

  }, deleteFile = FALSE)
  
  #update preview box
  observeEvent(input$generatebutton,{
    
    text <- preview(input$file1$datapath)
    updateTextAreaInput(session,'inputbox',
      value = text                        
                        )
  })
  
  observeEvent(input$newbutton,{
    updateTextAreaInput(session,'inputbox',
                        value = 'new'                        
    )
  })
  
  
  #download event
#  output$downloadbutton <- downloadHandler(
 #   filename = input$pedigreeText$value,
  #  content = function(con){
   #   
    #}
    
  #)

}


# Run the app ----
shinyApp(ui = ui, server = server)