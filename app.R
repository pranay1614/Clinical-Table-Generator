
library(shiny)
library(bslib)
library(dplyr)
library(DT)


source("R/demographics_functions.R")

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    primary = "#176B87"
  ),
  
  titlePanel("Clinical Trial Table Generator"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      h4("Data Management"),
      
      fileInput(
        inputId = "adsl_file",
        label = "Upload ADSL Dataset",
        accept = c(".csv", ".xpt", ".sas7bdat")
      ),
      
      hr(),
      
      uiOutput("variable_controls"),
      
      actionButton(
        inputId = "generate",
        label = "Generate Demographics Table",
        class = "btn-primary",
        width = "100%"
      ),
      
      br(),
      br(),
      
      downloadButton(
        outputId = "download_table",
        label = "Download Table CSV",
        width = "100%"
      )
    ),
    
    mainPanel(
      
      h3("Dataset Information"),
      
      verbatimTextOutput("dataset_info"),
      
      hr(),
      
      h3("Dataset Preview"),
      
      DTOutput("dataset_preview"),
      
      hr(),
      
      h3("Demographics Summary Table"),
      
      DTOutput("demographics_table")
    )
  )
)

server <- function(input, output, session) {
  
  adsl_data <- reactiveVal(NULL)
  
  table_result <- reactiveVal(NULL)
  
  observeEvent(input$adsl_file, {
    
    req(input$adsl_file)
    
    tryCatch({
      
      data <- read_adam_dataset(
        file_path = input$adsl_file$datapath,
        file_name = input$adsl_file$name
      )
      
      if (nrow(data) == 0 || ncol(data) == 0) {
        stop("Uploaded dataset is empty.")
      }
      
      adsl_data(data)
      
      table_result(NULL)
      
      showNotification(
        "ADSL dataset uploaded successfully.",
        type = "message"
      )
      
    }, error = function(e) {
      
      adsl_data(NULL)
      table_result(NULL)
      
      showNotification(
        paste("Upload failed:", e$message),
        type = "error",
        duration = NULL
      )
      
    })
    
  })
  
  output$dataset_info <- renderPrint({
    
    req(adsl_data())
    
    data <- adsl_data()
    
    cat("Number of subjects:", nrow(data), "\n")
    
    cat("Number of variables:", ncol(data), "\n")
    
    cat(
      "Variables:",
      paste(names(data), collapse = ", "),
      "\n"
    )
    
  })
  
  output$dataset_preview <- renderDT({
    
    req(adsl_data())
    
    datatable(
      head(adsl_data(), 10),
      
      options = list(
        scrollX = TRUE,
        pageLength = 5
      ),
      
      rownames = FALSE
    )
    
  })
  
  output$variable_controls <- renderUI({
    
    req(adsl_data())
    
    data <- adsl_data()
    
    vars <- names(data)
    
    tagList(
      
      h4("Table Configuration"),
      
      selectInput(
        inputId = "trt_var",
        label = "Treatment Variable",
        choices = vars,
        selected = if ("TRT01A" %in% vars) {
          "TRT01A"
        } else {
          vars[1]
        }
      ),
      
      selectInput(
        inputId = "usubjid",
        label = "Subject ID Variable",
        choices = vars,
        selected = if ("USUBJID" %in% vars) {
          "USUBJID"
        } else {
          vars[1]
        }
      ),
      
      hr(),
      
      h5("Categorical Variables"),
      
      selectInput(
        inputId = "cat_vars",
        label = "Select Variables",
        choices = vars,
        selected = intersect(
          c("SEX", "RACE", "ETHNIC"),
          vars
        ),
        multiple = TRUE
      ),
      
      h5("Continuous Variables"),
      
      selectInput(
        inputId = "cont_vars",
        label = "Select Variables",
        choices = vars,
        selected = intersect(
          c("AGE", "HEIGHT", "WEIGHT", "BMI"),
          vars
        ),
        multiple = TRUE
      )
      
    )
    
  })
  
  observeEvent(input$generate, {
    
    req(adsl_data())
    req(input$trt_var)
    req(input$usubjid)
    
    tryCatch({
      
      result <- create_demographics_table(
        
        data = adsl_data(),
        
        trt_var = input$trt_var,
        
        usubjid = input$usubjid,
        
        cat_vars = input$cat_vars,
        
        cont_vars = input$cont_vars
        
      )
      
      table_result(result)
      
      showNotification(
        "Demographics table generated successfully.",
        type = "message"
      )
      
    }, error = function(e) {
      
      table_result(NULL)
      
      showNotification(
        paste("Table generation failed:", e$message),
        type = "error",
        duration = NULL
      )
      
    })
    
  })
  
  output$demographics_table <- renderDT({
    
    req(table_result())
    
    datatable(
      
      table_result(),
      
      rownames = FALSE,
      
      options = list(
        scrollX = TRUE,
        pageLength = 25,
        searching = TRUE
      )
      
    )
    
  })
  
  output$download_table <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "Demographics_Table_",
        Sys.Date(),
        ".csv"
      )
      
    },
    
    content = function(file) {
      
      req(table_result())
      
      write.csv(
        table_result(),
        file,
        row.names = FALSE,
        na = ""
      )
      
    }
    
  )
  
}

shinyApp(
  ui = ui,
  server = server
)
