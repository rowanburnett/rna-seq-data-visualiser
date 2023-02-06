generateRadioButtons <- function(dataset, ns) {
  tagList(
    lapply(dataset, function(file) {
      ext <- tools::file_ext(file)
      
      if (ext == "csv") {
        radioButtons(ns(file), 
                           tools::file_path_sans_ext(file), 
                           choices = file)
        
      } else if (ext == "xls" || ext == "xlsx") {
        sheets <- excel_sheets(paste0("./data/Uploads/", file))
        choices <- c()
        
        for (sheet in sheets) {
          choices <- append(choices, sheet)
        }
        
        radioButtons(ns(file), 
                           tools::file_path_sans_ext(file), 
                           choices = choices)
      }
    })
  )
}

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


generatePlotTabs <- function(data, title, ns) {
  do.call(tabBox, 
    c(title = title,
      side = "right", 
      lapply(seq(data), function(i) {
        tabPanel(
          title = names(data[i]),
          plotOutput(ns(paste0("plot", i)),
                     height = "500px",
                     click = ns(paste0("plotClick", i))),
          textInput(ns(paste0("plotFileName", i)),
                    "File name"),
          selectInput(ns(paste0("plotExtension", i)),
                      "File extension",
                      choices = c(".png", ".jpeg", ".bpm", ".pdf")),
          downloadButton(ns(paste0("plotDownload", i)),
                         "Download plot")
        )
    }))
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