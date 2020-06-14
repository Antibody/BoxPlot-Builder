library(shiny)
library(rhandsontable)
library(dplyr)
library(stringr)


ui <- shinyUI(fluidPage(
  titlePanel("Boxplot builder"),
  textOutput('result'),
  sidebarLayout(
    sidebarPanel(
      wellPanel(
        plotOutput("plot")
      ),
      
      
    
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
    
    mainPanel(textInput("colnames", "Enter Column Names (separated by comma), names are required for ANOVA calculation", 
                        value="", placeholder = "e.g. Var1, Var2, Var3, etc"),
              h5(tags$b("Enter data")),
              
              rHandsontableOutput('table'),
              br(),
              br(),
              h4('One-way ANOVA summary'),
              h5("requires column names input"),
              verbatimTextOutput('aovSummary'),
              actionButton("anova", "Calculate Anova"),
              
              br(),
              br(),
              
              h4('Tukey HSD summary'),
              h5("requires column names input"),
              verbatimTextOutput('TukeySummary'),
              actionButton("Tukey", "Calculate Tukey")
              
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
      rhandsontable(values$data, rowHeaderWidth = 50, colHeaders=str_trim(unlist(strsplit(input$colnames,",")))) %>%
        hot_row(nrow(values$data), readOnly = TRUE)
    })
    
    
    change_col_names <- reactive({
      req(input$colnames)
      colnames(values$data) = str_trim(unlist(strsplit(isolate(input$colnames),",")))
    })
    
    observeEvent(input$build, {
      
      output$plot <- renderPlot({
        
        if(!is.null(input$colnames)) { #depending on column names input the headers will be substituted with new names, or not
          DF1 <- isolate(values$data)
          colnames(DF1) = str_trim(unlist(strsplit(isolate(input$colnames),","))) 
          }
        
        else {DF1 <- isolate(values$data)
        
        }
        
        boxplot(DF1) 
        
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
    
    observeEvent(input$anova, {
    
    output$aovSummary = renderPrint({
      DF1 <- isolate(values$data)  #to change names in plot, alternatively, put (see comment below)
      colnames(DF1) = str_trim(unlist(strsplit(isolate(input$colnames),",")))
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
        colnames(DF1) = str_trim(unlist(strsplit(isolate(input$colnames),",")))
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
