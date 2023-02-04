scatterplotUI <- function(id, label = "Scatterplot matrix") {
  ns <- NS(id)
  
  fluidRow(
    box(
      title = "Scatterplot matrix", collapsible = TRUE,
      plotOutput(
        ns("scatterplot"),
        height = "700px",
        click = ns("plotClick")
      )
    ),
    
    box(
      htmlOutput(ns("dataChoices"))
    ),
    
    box(
      title = "Extra information",
      htmlOutput(ns("geneInfo"))
    )
  )
}

scatterplotServer <- function(id, dataset) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # need to use session namespace for ui elements created in server function
      ns <- session$ns
      
      # create checkboxes from selected files in sidebar
      output$dataChoices <- renderUI({
        generateCheckboxes(dataset(), ns)
      })
      
      # adds numbers to replicate column names
      make_unique = function(x, sep='.'){
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
              newNames <- make_unique(oldNames, sep = ".")
              
              names(df) <- newNames
              
              replicateList <- data.frame()
              
              lapply(unique(oldNames), function(name) {
                
                duplicateNames <- str_extract(newNames, regex(paste0(name, ".+"), ignore_case = TRUE, dotall = TRUE))
                duplicateNames <- na.omit(duplicateNames)
                
                replicates <- data.frame()
                lapply(duplicateNames, function(duplicate) {
                  replicate <- df %>% dplyr::select(duplicate)
                  replicates <<- append(replicates, replicate)
                  
                })
                replicates <- bind_cols(replicates)
                replicates <- filter(replicates, rowSums(replicates) < 30)
                replicateList <<- append(replicateList, replicates)
              })
              print(replicateList)
             dataList[[paste0(data, " ", file)]] <<- replicateList

            }, error = function(cond) {
              print(cond)
              showModal(modalDialog(
                title = "Incorrect format",
                "Please check that data is formatted correctly",
                easyClose = TRUE
              ))
            })
          }
        })
        return(dataList)
      })
      
     output$scatterplot <- renderPlot({
      # print(data())
       df <- bind_rows(data())
       print(df)
       scatterplot <- ggpairs(df)
       
     })
  })
}

