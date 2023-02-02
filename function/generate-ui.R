generateCheckboxes <- function(dataset, ns) {
  tagList(
    lapply(dataset, function(file) {
      ext <- tools::file_ext(file)
      
      if (ext == "csv") {
        checkboxGroupInput(ns(file), 
                           tools::file_path_sans_ext(file), 
                           choices = file)
        
      } else if (ext == "xls" || ext == "xlsx") {
        sheets <- excel_sheets(paste0("./data/Uploads/", file))
        choices <- c()
        
        for (sheet in sheets) {
          choices <- append(choices, sheet)
        }
        
        checkboxGroupInput(ns(file), 
                           tools::file_path_sans_ext(file), 
                           choices = choices)
      }
    })
  )
}

generatePlotDownload <- function(name, extension, plot) {
  downloadHandler(
    filename = function() {
      paste0(name, extension)
    },
    
    content = function(file) {
      tryCatch(
        ggsave(
          filename = file,
          plot = plot
        )
      ) 
    }
  )
}