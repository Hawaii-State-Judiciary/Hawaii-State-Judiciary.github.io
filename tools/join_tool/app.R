
library(shiny)
library(readxl)
library(dplyr)

#default file size upload limit is 5MB; change to 30MB
options(shiny.maxRequestSize=30*1024^2) 

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("SID Matcher"),
  sidebarLayout(
    
    sidebarPanel(width = 3,
      fileInput("leftFile", "Upload File with Client/Case List (must have an SID column)", accept=c("csv","xls","xlsx")),
      fileInput("righttFile", "Upload CYZAP File to Extract Data from (must have an SID column)", accept=c("csv","xls","xlsx")),
      HTML("<em>Note: There is a 30MB file size upload limit</em>
           <br>
           <br>
           <br>"),
      downloadButton("download", "Download Output File")
    ),
    
    mainPanel(width = 9,
              h3("Preview of the first five rows..."),
      tableOutput("tableSample")
    )
  )#sidebarLayout
)

# Define server logic
server <- function(input, output) {
  
  #use validate/need b/c fileInput is initialized to NULL on page load, this ensures code waits until the files are uploaded
  df_merged <- reactive({

    validate(
      need(input$leftFile != "", "No left data has been uploaded"),
      need(input$righttFile != "", "No right data has been uploaded")
    )
    
    #check for valid extensions
    ext_left <- tools::file_ext(input$leftFile$name)
    ext_right <- tools::file_ext(input$righttFile$name)
    
    validate(
      need(ext_left %in% c("csv","xls","xlsx"), "Invalid left file; Please upload a csv, xls, or xlsx file"),
      need(ext_right %in% c("csv","xls","xlsx"), "Invalid right file; Please upload a csv, xls, or xlsx file")
    )
    
    #check csv vs xls/xlsx, import conditioned on extension
    leftFile_tmp <- input$leftFile
    if(ext_left == "csv"){
      leftFile <- read.csv(leftFile_tmp$datapath)
    }else if(ext_left %in% c("xls","xlsx")){
      leftFile <- read_excel(leftFile_tmp$datapath, col_names = T)
    }
    
    rightFile_tmp <- input$righttFile
    if(ext_right == "csv"){
      rightFile <- read.csv(rightFile_tmp$datapath)
    }else if(ext_right %in% c("xls","xlsx")){
      rightFile <- read_excel(rightFile_tmp$datapath, col_names = T)
    }
    
    # browser()
    
    #validate SID column exists
    leftFile_colnames <- toupper(colnames(leftFile))
    rightFile_colnames <- toupper(colnames(rightFile))
    validate(
      need("SID" %in% leftFile_colnames, "Invalid left file; No column named SID."),
      need("SID" %in% rightFile_colnames, "Invalid right file; No column named SID.")
    )
    
    #find index of column with SID; since we don't know case, make everything uppercase
    left_SID_index <- match("SID", toupper(colnames(leftFile)))
    right_SID_index <- match("SID", toupper(colnames(rightFile)))
    
    #custom rename SID column so that it is in all CAPS (e.g., changes  "sid" and "Sid" to "SID")
    colnames(leftFile)[left_SID_index] <- "SID"
    colnames(rightFile)[right_SID_index] <- "SID"
    
    #left join data
    df_merged <- leftFile %>% 
      left_join(rightFile, by ="SID")
    
  })
  
  output$tableSample <- renderTable({
    head(df_merged())
  })

  output$download <- downloadHandler(

    filename = function() {
      paste0("joined_data_", format(Sys.time(),"%Y-%m-%d_%H%M%S"),".csv")
    },
    content = function(file) {
      write.csv(df_merged(), file)
    }
    
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
