uploadUI <- function(id, label = "Upload") {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"),
              "Upload CSV or Excel workbook",
              buttonLabel = "Browse...",
              placeholder = "No file selected"),
  )
}

uploadServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # increase max request size to upload files up to 100mb
      options(shiny.maxRequestSize=100*1024^2)
      
      observeEvent(input$file, {
        file <- input$file
        req(file)
        ext <- tools::file_ext(file$datapath)
        
        if (is.null(file)) {
          return(NULL)
        } else if (ext == "csv") {
          newFile <- read.csv(file$datapath)
          write.csv(newFile, file = paste0("./data/Uploads/", file$name))
          
          # currently only writes one sheet for workbooks
        } else if (ext == "xlsx" || ext == "xls") { 
          newFile <- read_excel(file$datapath)
          write.xlsx(newFile, file = paste0("./data/Uploads/", file$name))
        }
      })
    }
  )
}