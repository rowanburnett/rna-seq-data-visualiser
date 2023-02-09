scatterPlotUI <- function(id, label = "Scatter plot matrix") {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Scatter plot matrix", collapsible = TRUE,
      plotOutput(
        ns("scatterplot"),
        height = "700px",
        click = ns("plotClick")
      )
    ),
    
    box(
      htmlOutput(ns("dataChoices")),
      checkboxGroupInput(ns("sampleChoices"),
                         "Samples"),
      textInput(ns("title"),
                "Plot title"),
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

scatterPlotServer <- function(id, dataset) {
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
        req(data())
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
          replicates[rowSums(replicates) < 30, ] <- NA
          replicateList <<- append(replicateList, replicates)
        })
        return(replicateList)
      })
      
      plotLine <- function(data, mapping) {
        maxVal <- max(data[,-1])
        minVal <- min(data[,-1])
        maxRange <- c(minVal, maxVal)
        
        p <- ggplot(data = data, mapping = mapping) +
          geom_point(shape = ".") +
          geom_abline(color = "red") +
          coord_cartesian(xlim = c(maxRange[1], maxRange[2]),
                          ylim = c(maxRange[1], maxRange[2]))
      }
      
      output$scatterplot <- renderPlot({
        df <- bind_rows(df())
        df <- varianceStabilizingTransformation(round(as.matrix(df)))
        df <- as.data.frame(df)
     #   df <- log2(df + 0.8)
        scatterplot <- ggpairs(df, progress = FALSE,
                title = input$title,
                lower = list(continuous = wrap(plotLine)))
        
        # download current plot
        output$plotDownload <- generatePlotDownload(input$plotFileName, 
                                                    input$plotExtension,
                                                    scatterplot)
        
        return(scatterplot)
      })
  })
}

