boxPlotUI <- function(id, label = "Box plot matrix") {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Box plot", collapsible = TRUE,
      plotOutput(
        ns("boxPlot")
      )
    ),
    
    box(
      htmlOutput(ns("dataChoices")),
      checkboxGroupInput(ns("sampleChoices"),
                         "Samples"),
      textInput(ns("plotFileName"),
                "File name"),
      selectInput(ns("plotExtension"),
                  "File extension",
                  choices = c(".png", ".jpeg", ".bpm", ".pdf")),
      downloadButton(ns("plotDownload"),
                     "Download plot")
    )
  )
}

boxPlotServer <- function(id, dataset) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # need to use session namespace for ui elements created in server function
      ns <- session$ns
      
      # create checkboxes from selected files in sidebar
      output$dataChoices <- renderUI({
        generateRadioButtons(dataset(), ns)
      })
      
      # adds numbers to replicate column names
      makeUnique = function(x, sep='.'){
        ave(x, x, FUN=function(a){if(length(a) > 1){paste(a, 1:length(a), sep=sep)} else {a}})
      }
      
      # gets the selected data
      data <- reactive({
        dataList <- list()
        lapply(dataset(), function(data) {
          for (file in input[[data]]) {
            ext <- tools::file_ext(data) # file extension
            
            df <- data.frame()
            # different read functions depending on file format
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data),
                               file,
                               na = "na",
                               .name_repair = "minimal")
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            # format data
            tryCatch({
              # convert columns to numeric because excel files save in the wrong format
              df[,-1] <- lapply(df[,-1], as.numeric)
              
              colnames(df) <- str_replace_all(colnames(df), "_", "")
              
              oldNames <- colnames(df)
              newNames <- makeUnique(oldNames, sep = ".")
              
              updateCheckboxGroupInput(session, 
                                       "sampleChoices", 
                                       choices = unique(oldNames[-1]))
              
              names(df) <- newNames
              
              dataList[[paste0(data, " ", file)]] <<- df
              
            })
          }
        })
        return(dataList)
      })
      
      df <- reactive({
        replicateList <- data.frame()
        
        lapply(input$sampleChoices, function(name) {
          df <- data()[[1]]
          duplicateNames <- str_extract(colnames(df[-1]), regex(paste0(name, ".+"), ignore_case = TRUE, dotall = TRUE))
          duplicateNames <- na.omit(duplicateNames)
          
          replicates <- data.frame()
          lapply(duplicateNames, function(duplicate) {
            replicate <- df %>% dplyr::select(duplicate)
            replicates <<- append(replicates, replicate)
            
          })
          replicates <- bind_cols(replicates)
         # replicates[rowSums(replicates) < 30, ] <- NA
          replicateList <<- append(replicateList, replicates)
        })
        return(replicateList)
      })
      
      output$boxPlot <- renderPlot({
        req(length(data()) > 0)
        df <- bind_rows(df())
        df <- na.omit(df)
        df <- varianceStabilizingTransformation(round(as.matrix(df)))
       # df <- rlog(round(as.matrix(df)))
        df <- as.data.frame(df)
        print(min(df))
        
        boxPlot <- ggplot(stack(df), aes(x = ind, y = values)) + 
          geom_boxplot(na.rm = TRUE) +
          xlab("Sample") +
          ylab("Count") 
        
        # download current plot
        output$plotDownload <- generatePlotDownload(input$plotFileName, 
                                                    input$plotExtension,
                                                    boxPlot)
        
        return(boxPlot)
      })
    })
}

