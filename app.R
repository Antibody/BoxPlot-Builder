library(shiny)
library(rhandsontable)
library(dplyr)


ui <- shinyUI(fluidPage(
titlePanel("Boxplot builder"),
textOutput('result'),
sidebarLayout(
  sidebarPanel(
    wellPanel(
      plotOutput("plot")
    )    ,
    
    br(), 
    
    wellPanel(
      h3("Build BoxPlot"), 
      actionButton("build", "Build")
    ) ,   
    
    br(), 
    
    wellPanel(
      h3("Export Plot"),
      downloadButton('ExportPlot', 'Export as png')
    ) , 
    
    br(), 
    
    wellPanel(
      h3("Erase data"), 
      actionButton("recalc", "Erase")
    ) ,  
    
    
    
  ),
  
  mainPanel(textInput("colnames", "Enter Column Names (separated by comma)", 
                      value="", placeholder = "e.g. Var1, Var2, etc"),
            h5(tags$b("Enter data")),
    
    rHandsontableOutput('table')
    
  )
)
))



rowNames <- c(sprintf("[%d]",seq(1:20)), "Means")
defaultDF <- data.frame(
  row.names = rowNames,
  A = rep(NA_integer_, nrow=21),
  B = rep(NA_integer_, 21),
  C = rep(NA_integer_, 21),
  stringsAsFactors = FALSE
)

server <- function(input, output, session)
  ({
    values <- reactiveValues(data = defaultDF) ## assign it with NULL
    
    ## button press resets now the data frame
    observeEvent(input$recalc, {
      values$data[] <- NA_integer_
    })
    
    observe({
      req(input$table)
      DF <- hot_to_r(input$table)
      DF[setdiff(rowNames, "Means"),]
      DF["Means",] <- colMeans(DF[setdiff(rowNames, "Means"),], na.rm = TRUE)
      values$data <- DF
    })
    
    output$table <- renderRHandsontable({
      req(values$data)
      rhandsontable(values$data, rowHeaderWidth = 100, colHeaders=str_trim(unlist(strsplit(input$colnames,","))),) %>%
        hot_row(nrow(values$data), readOnly = TRUE)
    })
    
   
    
    
    
    
    observeEvent(input$build, {
      
      output$plot <- renderPlot({
        
        boxplot(values$data)
        
      })
    })
    
    
   #Export PNG
    output$ExportPlot = downloadHandler(
      filename = 'BoxPlot.png',
      content = function(file) {
        
          png(file) # open the png device
        boxplot(values$data) # draw the plot
        dev.off()  # turn the device off
        
      })
                                            
  
  
    
    
  })

shinyApp(ui = ui, server = server)
