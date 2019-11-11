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
ui <- fluidPage(
  shinyjs::useShinyjs(),
  padding = 5,
  theme = shinytheme("journal"),
  shinythemes::themeSelector(),
  titlePanel("Pedigree Engine"),

  sidebarLayout(
    sidebarPanel(
      actionButton(inputId = 'newbutton',label = 'New FILE', width = '200px'),
      downloadButton ( outputId =  'savebutton',label = 'Save FILE'),
      hr(),
      fileInput("file1", "Choose input text File", accept = c(".txt")),
      textAreaInput('inputbox','command',height = '7cm'),
      selectizeInput(inputId = "legendPosition",'Legend position', choices = c("Top right" = "topright",  "Top left" = "topleft", "Bottom right" = "bottomright", "Bottom left" = "bottomleft")),
      sliderInput('distance','Distance between nodes',min = 0.01,max = 2,value = 0.7),
      textAreaInput('console','console',height = '5cm'),
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
  
  #output$table <- renderDataTable()
  
  #rendering image 
  output$image <- renderImage({
    
    #print(getwd())
    relation_file <- input$file1
    shinyjs::disable("console")

    if (is.null( relation_file)){ 
      blank <- paste(getwd(),'/src/blank.png',sep = '')
      pedigree <- normalizePath(blank)
      list(src = pedigree)
    } else{
      text <- preview(input$file1$datapath)
      

      ped <- producePED(relation_file$datapath)
      
      out_jpg_path <- producegraph(ped,input$distance,input$legendPosition)
      pedigree <- normalizePath(file.path(out_jpg_path))
      logtext <- getlog()
      updateTextAreaInput(session,'inputbox',
                          value = text                        
      )
      updateTextAreaInput(session,'console',
                          value = logtext                        
      )
      list(src = pedigree)
      
      }

  }, deleteFile = FALSE)
  
  #update preview box
  observeEvent(input$generatebutton,{
    print(input$file1)
    text <- preview(input$file1$datapath)
    logtext <- getlog()
    updateTextAreaInput(session,'inputbox',
      value = text                        
                        )
    updateTextAreaInput(session,'console',
                        value = logtext                        
    )
  })
  
  observeEvent(input$newbutton,{
    updateTextAreaInput(session,'inputbox',
                        value = 'new'                        
    )
  })
  
  output$savebutton <- downloadHandler(
    filename = input$file1$name,
    
    content = function(con){
      tt <- input$inputbox
      print(tt)
      writeLines(tt, con)
      
    }
    
  )
  
  
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