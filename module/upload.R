uploadUI <- function(id, label = "Upload") {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"),
              "Upload CSV or Excel workbook",
              buttonLabel = "Browse...",
              placeholder = "No file selected"),
    selectInput(ns("fileSelect"),
                "Uploaded files",
                choices = NULL)
  )
}

uploadServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # increase max request size to upload files up to 100mb
      options(shiny.maxRequestSize=100*1024^2)
      
      uploaded_files <- list.files(path = "./data/Uploads")
      updateSelectInput(session, "fileSelect",
                        choices = uploaded_files)
      
      observeEvent(input$file, {
        file <- input$file
        req(file)
        ext <- tools::file_ext(file$datapath)
        
        if (is.null(file)) {
          return(NULL)
        } else if (ext == "csv") {
          newFile <- read.csv(file$datapath)
          write.csv(newFile, file = paste0("./data/Uploads/", file$name), row.names = FALSE)
          
          # currently only writes one sheet for workbooks
        } else if (ext == "xlsx" || ext == "xls") { 
          dataframes <- list()
          sheets <- excel_sheets(file$datapath)
          for (sheet in sheets) {
            newSheet <- read_excel(file$datapath, sheet = sheet)
            dataframes[[sheet]] <- newSheet
            write.xlsx(dataframes, file = paste0("./data/Uploads/", file$name), rowNames = FALSE)
          }
        }
        
        uploaded_files <- list.files("./data/Uploads/")
        updateSelectInput(session, "fileSelect",
                          choices = uploaded_files)
      })
      
      return(
        dataset <- reactive({
          session$input$fileSelect
        })
      )
    }
  )
}