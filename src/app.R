# Load libraries!
library(shiny)
library(xts)
library(tools)
library(tidyverse)
library(ggplot2)
library(plotly)
library(changepoint)
library(changepoint.np)
library(shinycssloaders)
library(shinyBS)
library(openxlsx)
library(shinyjs)
library(shinythemes)

options(shiny.maxRequestSize = 1 * 1024^2)

###### UI #######
ui <- fluidPage(header = "",       div("Time Series Segmentation & Changepoint Detection", style = 'padding-left: 15px; padding-right: 15px; margin-bottom: 20px; padding-bottom: 14px; padding-top: 14px; width: 100%;
 background-color: #2c3e50; color: #ffffff; font-size: 19px; font-family: "Lato","Helvetica Neue",Helvetica,Arial,sans-serif;'),
                
                
  theme = shinytheme("flatly"),
  shinyjs::useShinyjs(),
  tryCatch(includeHTML(("consent.html")),warning = function(e){}),
  tryCatch(tags$head(includeHTML(("google-analytics.html"))),warning = function(e){}),
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }",
             ".shiny-output-error-validation {color: red;} "
  ),
  tags$head(
    tags$style(HTML("hr {border-top: 1px solid #000000;}"))
  ),
    
  sidebarLayout(
    
    sidebarPanel(

      selectInput(inputId = "data", label = "Choose / Upload dataset", choices = c("financial-time-series",
                                                                                   'bitcoin-market-price-and-transactions',
                                                                                   "monthly-sunspots",
                                                                                   'covid-19-cases-and-deaths-worldwide',
                                                                                   'Upload Dataset...'
                                                                                   )),
  
      
      
      
      conditionalPanel(condition = "input.data == 'Upload Dataset...'",
                         fileInput('file', 'Upload Excel file',accept=c('application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
                                                                               '.xlsx',
                                                                               ".xls",
                                                                               "application/vnd.ms-excel")),
                       checkboxInput('header','Use first row for the column names',T)),
                       
      
      
      selectInput("vary","Select changepoint column", choices = c("")),
      
      selectInput(inputId = "algorithm", label = "Select changepoint measure",
                  choices = c(   "Changepoint in mean" = "cpt.mean",
                                "Changepoint in variance" = "cpt.var",
                                "Changepoint in both mean and variance" = "cpt.meanvar",
                              
                              "Non-parametric changepoint" = "cpt.np"
                              )),
      
      # common parameters
      conditionalPanel(condition = "input.algorithm != 'None'", 
                       
                       radioButtons(inputId = "method", label = "Select method",
                                    choices = c("Binary Segmentation" = "BinSeg",
                                                "PELT" = "PELT",
                                                "Segmentation Neighborhoods" = "SegNeigh")
                       ),
                       
                        selectInput(inputId = "penalty", label = "Select penalty", 
                                    choices = c("MBIC", "SIC", "BIC", "AIC", "Hannan-Quinn", "Asymptotic", "Manual"), multiple = F),
                        
                        
                        conditionalPanel(condition = "input.penalty == 'Manual'",
                                         
                                         numericInput(inputId = "pen.val", label = "Add numeric pentalty", min = 1, max = 1000, value = 1)
                                         
                        ),
                        
                       conditionalPanel(condition = "input.method != 'PELT'",

                                        sliderInput(inputId = "Q", label = "Maximum number of changepoints", min = 1, max=100, value = 12)

                       ),


                       numericInput(inputId = "minseglen", label = "Minimum segment length", min = 1, value = 10)

      ),
      # cpt.mean
      conditionalPanel(condition = "input.algorithm == 'cpt.mean'",
                        selectInput(inputId = "distribution", label = "Data distribution", choices = c("Normal", "CUSUM"))
       ),
      # cpt.var
      conditionalPanel(condition = "input.algorithm == 'cpt.var'",

                       selectInput(inputId = "distribution", label = "Data distribution", choices = c("Normal", "CSS")),

                       selectInput(inputId = "known_mean", label = "Known mean?", choices = c("TRUE", "FALSE"), selected = "FALSE"),

                       conditionalPanel(condition = "input.known_mean == 'TRUE'",
                                        numericInput(inputId = "mu", label = "Mean", value = 0)
                       )
      ),
      # cpt.meanvar
      conditionalPanel(condition = "input.algorithm == 'cpt.meanvar'",

                       selectInput(inputId = "distribution", label = "Data distribution",
                                   choices = c("Normal", "Gamma", "Exponential", "Poisson")
                       ),

                       conditionalPanel(condition = "input.distribution == 'Gamma'",
                                        numericInput(inputId = "shape", label = "Assumed shape of parameter for Gamma", value = 1)
                       )

      ),
      # cpt.np
      conditionalPanel(condition = "input.algorithm == 'cpt.np'",

                       selectInput(inputId = "distribution", label = "Data distribution",
                                   choices = c("empirical distribution")),

                       textInput(inputId = "nquantiles", label = "Number of quantiles to use", value = "10")
      ),

splitLayout(cellWidths = c("100","35","35"),
  tags$a(
  tags$img(src="medoid-ai-logo.png",height = "80%", width="80%"),
  href="https://www.medoid.ai/",target="_blank"),
  tags$a(href="https://github.com/medoidai/ts-changepoint-detection-app", icon("github"),target="_blank",style = "font-size:15px;color: black"),
  width = 3,
  tags$a(href = "https://www.linkedin.com/company/medoid-ai/", icon("linkedin"),target="_blank",style = "font-size:15px;color: black"),
  tags$a(href = "https://www.researchgate.net/publication/264485551_Changepoint_An_R_Package_for_Changepoint_Analysis",icon("book"),target="_blank", style = "font-size:15px;color: black")
  
  ),
width = 3
    ),
        
    
    mainPanel(
      br(),
uiOutput("all"),


      splitLayout(
               cellWidths = 150,
          checkboxInput(inputId = "changepoints", label = "Show changepoints", value = T),
          checkboxInput(inputId = "segments", label = "Show segments", value = F),
            checkboxInput(inputId = 'show_console', label = 'Show console',value = F)),

      #uncomment this for download functionality
      #downloadButton("downloadData", "Download changepoints"),
      
      br(),
      verbatimTextOutput(outputId = "console"),
     
    )
  )
)

###### SERVER #######

server <- function(input, output,session){
  
  shinyjs::hideElement("tabs")
  values <- reactiveValues()

  values$display_upload_instructions <- TRUE
  values$file_error_msg <- ""
  values$display_algorithm_error <- FALSE 

  
  observe({
    if((values$display_algorithm_error == FALSE)){
      shinyjs::hideElement("error_algorithm")
    }
  })
  observe({
    if((values$display_algorithm_error == TRUE)){
      shinyjs::showElement("error_algorithm")
    }
    
  })
  
  # error handling when not displaying plot
  observe({
    
    if((values$display_upload_instructions == FALSE) ){
      shinyjs::hideElement("show_console")
      shinyjs::hideElement("changepoints")
      shinyjs::hideElement("segments")

      output$image <- renderUI({
        if((input$data) == 'Upload Dataset...')
           column(6,
                  p(values$file_error_msg,
                    style = "font-size:15px;color: red",height = "100%", width="100%"),
                   p("The Excel file format required by this app is the following:",
                     style = "font-size:18px",height = "100%", width="100%"),
                   tags$img(src="data-format-screenshot.png",height = "100%", width="100%"),
                   br(),
                   br(),
                   br(),
                   br(),
                  
                   tags$ul(
                               tags$li("The maximum file size is 1MB"),
                               tags$li("The first column in data will be used for the x-axis in plot"),
                               tags$li("Only Excel (xlsx/xls) file type is supported")
                              )
                   )
          })
   
    } 
  })
  
  # show checkboxes when needed
  observe({
    if(values$display_upload_instructions ==TRUE){
      shinyjs::showElement("show_console")
      shinyjs::showElement("changepoints")
      shinyjs::showElement("segments")
      values$file_error_msg <- ""
      
    }
  })
  
  true_data <- reactive({
    
    input$data
  })
  
  # data reading/manipulation
  df <- reactive({
    
      df_temp <- if(true_data() == 'Upload Dataset...'){
        values$display_algorithm_error <- FALSE 
        
      values$display_upload_instructions <- FALSE
      req(input$file)
      inFile <- input$file
      values$file_error_msg <- "The file format is not correct!"
      
      df_temp <- readxl::read_excel(inFile$datapath,col_names = input$header)

    } else  {
      df_temp <- readxl::read_excel(paste0('../data/',true_data(),".xlsx"))
    }
    
      df_temp$Timestamp <- 1:nrow(df_temp)
    
    if(length(df_temp)>0){
      df_temp$Timestamp <- 1:nrow(df_temp)
      if(length(df_temp)>2){
        ts_column <- setdiff(sapply(df_temp,class) %>% .[.%in% c("numeric","integer")] %>% names(),c("Timestamp",colnames(df_temp)[1])) 
      }else {
        ts_column <- setdiff(sapply(df_temp,class) %>% .[.%in% c("numeric","integer")] %>% names(),"Timestamp")
        
      }
      
    
      updateSelectInput(session, 'vary', choices = ts_column)
    }  else {
      df_temp <- NULL
    }
     

  

    return(df_temp)
  })
  
  # change point aglorithm
  cpt_res <- reactive({
    df <- df()

    tryCatch({
      df <- df()[,c("Timestamp",input$vary)]
      
    }, error = function(){
      output$error <- renderPrint("Your dataset must not be empty..")
    })
  
  
    df <-na.omit(df)
    df <- as.data.frame(df)

    tryCatch({
      if(input$algorithm %in% c("cpt.mean", "cpt.var", "cpt.meanvar") & length(df>0)){
        
        res <- get(input$algorithm)(data = df[,input$vary], 
                                    penalty = input$penalty,
                                    pen.value = input$pen.val,
                                    method = input$method,
                                    Q = input$Q,
                                    minseglen = input$minseglen,
                                    test.stat = input$distribution)
        values$display_algorithm_error <- FALSE 
        
      } 
      else if(input$algorithm == "cpt.np" & length(df)>0) {
        
        res <- get(input$algorithm)(data = df[,input$vary],
                                    penalty = input$penalty,
                                    pen.value = input$pen.val,
                                    method = input$method,
                                    nquantiles = as.numeric(input$nquantiles),
                                    test.stat = "empirical_distribution",
                                    minseglen = input$minseglen,
                                    class = T
                                    )
        values$display_algorithm_error <- FALSE 
       
      } else{
        res <- "Select an Algorithm to see the results here..."
      }
      
      return(res)},error = function(e){
        e
        }
      )
  
    })
  
  # console outputs 
  output$console <- renderPrint({
    if(input$show_console) cpt_res()
  })
  
  # Render Timestamp series plot with segments
  output$cpt_plot <- renderPlotly({
      
      df <- df()
      data <- df()
      req(true_data())
      # readxl imports data as tible so we must convert in in data.frame, otherwise everything below will brake.
      data_df <- data.frame(data[,1])
      for (column in c(2:ncol(data))){
        data_df <- cbind(data_df, data[,column])
      }
      data <- data_df
      if(length(data >2)){
        data<-data[,c(colnames(data)[1],input$vary,"Timestamp")]
      } else {
        data <-data[,c(input$vary,"Timestamp")]
      }
      
      data <- na.omit(data)
      cpt_res <- cpt_res()
      
      if(!isS4(cpt_res)){
        
        values$display_algorithm_error <- TRUE 
        output$error_algorithm <- renderUI( p("Please try different algorithm parameters!",
                                              style = "font-size:15px;color: red",height = "100%", width="100%"))
        p <- ggplot(data) +
          geom_line(aes_string("Timestamp", input$vary))+
          scale_x_continuous(breaks = data$Timestamp[round(seq(1,nrow(data), length.out = 10))], labels = data[round(seq(1,nrow(data), length.out = 10)),1]) +
          theme(axis.text.x=element_text(angle=60, vjust=.5)) +
          xlab(colnames(df)[1]) 
        
        
      } else {
        
        # Define segments id
        segment_id <- rep(1:length(cpt_res@cpts), times = c(cpt_res@cpts[1], diff(cpt_res@cpts)))
        
        data$segment_id <- segment_id
        
        # Calculate mean mid_price in every segment
        segment_mean_mid_value<- data %>% 
          group_by(segment_id) %>% 
          summarise(mean_mid_value = mean(get(input$vary)))
        
        segments_df <- data[cpt_res@cpts,]
        segments_df$start_time <- lag(segments_df[,"Timestamp"] +1)
        segments_df$start_time[1] <- data[1,"Timestamp"]
        segments_df$segment_mean_mid_value <- segment_mean_mid_value$mean_mid_value
        segments_df$segment_id <- unique(segment_id)
        df <- df()
       
          if(length(df) > 2){
            p <- ggplot(data) +
              geom_line(aes_string("Timestamp", input$vary)) +
              {if(input$segments) geom_segment(data = segments_df, aes_string(x = "start_time" , y = "segment_mean_mid_value", xend = "Timestamp", yend = "segment_mean_mid_value"), col = "red")}+
              {if(input$changepoints) geom_point(data = segments_df[-nrow(segments_df),], aes_string("Timestamp" , input$vary), col = "red")} +
              scale_x_continuous(breaks = data$Timestamp[round(seq(1,nrow(data), length.out = 10))], labels = data[round(seq(1,nrow(data), length.out = 10)),1]) +
              theme(axis.text.x=element_text(angle=60, vjust=.5)) +
              xlab(colnames(df)[1])    
            
          } else {
            
            p <- ggplot(data) +
              geom_line(aes_string("Timestamp", input$vary)) +
              {if(input$segments) geom_segment(data = segments_df, aes_string(x = "start_time" , y = "segment_mean_mid_value", xend = "Timestamp", yend = "segment_mean_mid_value"), col = "red")}+
              {if(input$changepoints) geom_point(data = segments_df[-nrow(segments_df),], aes_string("Timestamp" , input$vary), col = "red")} 
            
          }
        
        
        
        
      }
      pltly <- ggplotly(p)
      values$display_upload_instructions <- TRUE

      return(pltly)
      
    
   
  
    
    
  })

  
  output$all <- renderUI({
    
    if(values$display_upload_instructions == TRUE){
      
      if (values$display_algorithm_error == TRUE)
      tagList(
        uiOutput("error_algorithm"),
        plotlyOutput(outputId = "cpt_plot",width = "auto", height = "auto",inline = F) %>% withSpinner(type = 5,color = "blue"))
      else
        tagList(
          plotlyOutput(outputId = "cpt_plot",width = "auto", height = "auto",inline = F) %>% withSpinner(type = 5,color = "blue"))
      
      
    }else{
      
      tagList(
        
        uiOutput("image"),
        uiOutput("text"),
        plotlyOutput(outputId = "cpt_plot",width = "auto", height = "auto",inline = F) %>% withSpinner(type = 5,color = "blue"))
    }
    

  })
  # uncomment this for download functionality
  # download_df <- reactive({
  #   data <- df()
  #   cpt_res <- cpt_res()
  #   cpt_points <- cpt_res@cpts
  #   data$change_point <- ifelse(data$Timestamp %in% cpt_points,T,F)
  #   return(data)
  # })
  
  # output$downloadData <- downloadHandler(
  #   filename = function(){true_data()},
  #   content = function(file) {
  #     write.table(download_df(), file, row.names = FALSE)
  #   }
  #     
  # )

}

shinyApp(ui = ui, server = server)
