library(shiny)
library(rhandsontable)
library(dplyr)
library(stringr)


ui <- shinyUI(fluidPage(  
  titlePanel("A Simple Online Boxplot Generator App with ANOVA"),

  textOutput('result'),
  sidebarLayout(
    sidebarPanel(
      
      wellPanel(
      plotOutput("plot"),
      br(),
      ) ,
      
    
      wellPanel(
        h4("Build boxplot"), 
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
    
    mainPanel(textInput("colnames", span("Enter column names (separated by comma)", style = "color:darkblue"), 
                        value="", placeholder = "e.g. Var1, Var2, Var3, etc"),
              h5(tags$b("Enter data", style = "color:darkblue")),
              
              rHandsontableOutput('table'),
              br(),
              br(),
              h4('One-way ANOVA summary'),
              verbatimTextOutput('aovSummary'),
              actionButton("anova", "Calculate ANOVA"),
              
              br(),
              br(),
              
              
              h4('Tukey HSD summary'),
              verbatimTextOutput('TukeySummary'),
              actionButton("Tukey", "Calculate Tukey"),
              
              hr(),
              print("~~~ Web Box Plot Builder With One-Way Anova ~~~~")
              
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
      req(input$colnames)
       DF <- hot_to_r(input$table)
       colnames(DF) = str_trim(unlist(strsplit(isolate(input$colnames),",")))
       DF[setdiff(rowNames, "Means"),]
       DF["Means",] <- colMeans(DF[setdiff(rowNames, "Means"),], na.rm = TRUE)
        values$data <- DF
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
      rhandsontable(values$data, rowHeaderWidth = 50, useTypes = FALSE, colHeaders=str_trim(unlist(strsplit(input$colnames,",")))) %>%
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
        dev.off() # turn the device off
        
      })
    
    
    meltDF <- reactive({
      row.names.remove <- c("Means") #to remove "means" row
    
    DF1 <- DF1[!(row.names(DF1) %in% row.names.remove), ] # removes "means" row
    
    
    DF1$id = 1:nrow(DF1)
    
    require(reshape2)
    DF1 = melt(DF1, id.vars = "id")
                                    })
    
    observeEvent(input$anova, {
      DF1 <- isolate(values$data)
      
    output$aovSummary = renderPrint({
        #to change names in plot, alternatively, put (see comment below)
      
      row.names.remove <- c("Means") #to remove "means" row
      
      DF1 <- DF1[!(row.names(DF1) %in% row.names.remove), ] # removes "means" row
      
      
      DF1$id = 1:nrow(DF1)
      
      require(reshape2)
      DF1 = melt(DF1, id.vars = "id")
      print(summary(aov(value ~ variable, data = DF1 )))
     
      
     
      
    })
    
    })
    
   
    observeEvent(input$Tukey, {
      
      output$TukeySummary = renderPrint({
        DF1 <- isolate(values$data)  #to change names in plot, alternatively, put (see comment below)
          row.names.remove <- c("Means") #to remove "means" row
          DF1 <- DF1[!(row.names(DF1) %in% row.names.remove), ] # removes "means" row
          DF1$id = 1:nrow(DF1)
          require(reshape2)
          DF1 = melt(DF1, id.vars = "id")
          res.aov <- (aov(value ~ variable, data = DF1 ))
          print(TukeyHSD(res.aov))
              })
      
    })
    
  })





shinyApp(ui = ui, server = server)
