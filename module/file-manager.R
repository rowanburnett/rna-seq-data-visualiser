fileManagerUI <- function(id, label = "Upload") {
  ns <- NS(id)
  
  tagList(
    fileInput(ns("file"),
              "Upload CSV or Excel workbook",
              buttonLabel = "Browse...",
              placeholder = "No file selected"),
    checkboxGroupInput(ns("fileSelect"),
                "Uploaded files",
                choices = NULL),
    actionButton(ns("deleteFiles"),
                 "Delete selected files")
  )
}

fileManagerServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # increase max request size to upload files up to 100mb
      options(shiny.maxRequestSize=100*1024^2)
      
      uploadedFiles <- list.files(path = "./data/Uploads")
      updateCheckboxGroupInput(session, "fileSelect", "Uploaded files",
                        choices = uploadedFiles)
      
      observeEvent(input$file, {
        file <- input$file
        req(file)
        ext <- tools::file_ext(file$datapath)
        
        if (is.null(file)) {
          return(NULL)
        } else if (ext == "csv") {
          newFile <- read.csv(file$datapath)
          write.csv(newFile, 
                    file = paste0("./data/Uploads/", file$name), 
                    row.names = FALSE)
          
        } else if (ext == "xlsx" || ext == "xls") { 
          dataframes <- list()
          sheets <- excel_sheets(file$datapath)
          for (sheet in sheets) {
            newSheet <- read_excel(file$datapath, 
                                   sheet = sheet, 
                                   .name_repair = "minimal")
            
            dataframes[[sheet]] <- newSheet
            write.xlsx(dataframes,
                       file = paste0("./data/Uploads/", file$name), 
                       rowNames = FALSE)
          }
        }
        
        uploadedFiles <- list.files("./data/Uploads/")
        updateCheckboxGroupInput(session, "fileSelect",
                            choices = uploadedFiles)
      })
      
      observeEvent(input$deleteFiles, {
        unlink(paste0("./data/Uploads/", input$fileSelect))
        
        uploadedFiles <- list.files("./data/Uploads/")
        updateCheckboxGroupInput(session, "fileSelect",
                            choices = uploadedFiles)
      })
      
      return(
        dataset <- reactive({
          session$input$fileSelect
        })
      )
    }
  )
}