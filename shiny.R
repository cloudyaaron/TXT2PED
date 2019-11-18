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
  shinyjs::useShinyjs(),
  padding = 5,
  theme = shinytheme("yeti"),
  #shinythemes::themeSelector(),
  div(style="text-align: center; padding-bottom:5px;", titlePanel("TXT2PED")),
  tags$head(tags$script(HTML("
        // Enable navigation prompt
        window.onbeforeunload = function() {
            return 'Your changes will be lost!';
        };
    "))),
  sidebarLayout(
    sidebarPanel(
      #helpText("ZOOM out if needed\n"),
      div(style="display:inline-block;width:45%;text-align: center;", downloadButton(outputId = 'newbutton',label = 'New FILE', width = '200px')),
      div(style="display:inline-block;width:53%;text-align: center;", downloadButton ( outputId =  'savebutton',label = 'Save FILE')),
      hr(),
      fileInput("file1", "Load input text File", accept = c(".txt"),buttonLabel = "Current File"),
      div(style="display:inline-block;width:99%;text-align: center;", helpText("Find input format at:","https://github.com/cloudyaaron/6112project/")),
      hr(),
      textAreaInput('inputbox','Command',height = '1.5cm'),
      hr(),
      textAreaInput('console','Console',height = '2cm'),
      hr(),
      div(style="display:inline-block;width:99%;text-align: center;", actionButton(inputId = 'generatebutton',label = 'generate graph', icon = icon("refresh"), align="center")),
      

      
      width = 3
      
    ),
    
    mainPanel(
      fluidRow(
        column(3, offset = 0,
          div(style="text-align: center; width: 200px", sliderInput('distance','Node Distance',min = 0.01,max = 2,value = 0.7))
        ),

        column(3, offset = 0,
          div(style="text-align: center; width: 200px", sliderInput('size','Legend Size',min = 0.001,max = 0.5,value = 0.05))
        ),

        column(3, offset = 0,
          div(style="text-align: center; width: 200px;", selectizeInput(inputId = "legendPosition",'Legend position', choices = c("Top right" = "topright",  "Top left" = "topleft", "Bottom right" = "bottomright", "Bottom left" = "bottomleft")))
        ),

        column(3, offset = 0,              
          div(style="text-align: center; padding-right: 15px", checkboxGroupInput("variable", "Variables to show:",
                                                c("ID" = "id",
                                                  "Real name" = "name",
                                                  "Date of birth" = "dob",
                                                  "Affected" = "affect",
                                                  "Addtional Text" = "ad"), inline = TRUE)))
      ),
      hr(),
      div(style="padding-left: 100px", imageOutput("image")),
      div(style="text-align: center; padding-left: 800px", downloadButton('exportbutton', 'Export Pedigree File')),
      
      width = 9
    ),
    
    fluid = TRUE
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
      updateTextAreaInput(session,'inputbox',
                          value = text                        
      )

      ped <- producePED(relation_file$datapath)
      #print(ped)
      out_jpg_path <- producegraph(ped,input$distance,input$legendPosition,input$variable,input$size)
      pedigree <- normalizePath(file.path(out_jpg_path))
      logtext <- getlog()

      updateTextAreaInput(session,'console',
                          value = logtext                        
      )
      list(src = pedigree, contentType = "image/png")
      
      }

  }, deleteFile = FALSE)
  
  #update preview box
  observeEvent(input$generatebutton,{
    print(input$file1)
    if (is.null(input$file1)){
      blank <- paste(getwd(),'/src/blank.png',sep = '')
      pedigree <- normalizePath(blank)
      list(src = pedigree)
      return()
    } else{
      con <- file(input$file1$datapath)
    }
    writeLines(input$inputbox,con)
    close(con)
    text <- preview(input$file1$datapath)
    logtext <- getlog()
    updateTextAreaInput(session,'inputbox',
      value = text                        
                        )
    updateTextAreaInput(session,'console',
                        value = logtext                        
    )
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
        
        
        
        out_jpg_path <- producegraph(ped,input$distance,input$legendPosition,input$variable,input$size)
        pedigree <- normalizePath(file.path(out_jpg_path))
        logtext <- getlog()
        updateTextAreaInput(session,'inputbox',
                            value = text                        
        )
        updateTextAreaInput(session,'console',
                            value = logtext                        
        )
        list(src = pedigree, contentType = "image/png" )
        
      }
      
    }, deleteFile = FALSE)
  })
  
  
  
  
  observeEvent(input$newbutton,{
    updateTextAreaInput(session,'inputbox',
                        value = 'new'                        
    )
  })
  
  output$savebutton <- downloadHandler(
    
    filename = function(){
      
      paste(input$file1$name)
    },
    
    content = function(con){
      tt <- input$inputbox
      #print(tt)
      writeLines(tt, con)
      
    },
    
    contentType = "text/csv"
    
  )
  
  output$exportbutton <- downloadHandler(
    filename = function(){
      paste(gsub('.{0,3}$', '', input$file1$name),"ped",sep = "")
    },
    content = function(con){
      ped <- producePED(input$file1$datapath)
      write.csv(ped[,1:7],con)
      
    },
    contentType = "ped"
  )
  
  rv <- reactiveValues(download_flag = 0)
  
  observeEvent(rv$download_flag, {
    shinyjs::alert("When New File created, Please load the new file!")
  }, ignoreInit = TRUE)
  
  output$newbutton <- downloadHandler(
    filename = function(){
      paste("Newfile.txt")
    },
    content = function(con){
      writeLines("new",con)
      rv$download_flag <- rv$download_flag + 1
    },
    contentType = "txt/csv"
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