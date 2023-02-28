heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)

      fluidPage(
        column(7,
          box(
            title = "Heat map",
            width = "100%",
            plotlyOutput(
              ns("heatMap"),
              height = "700px"
            )
          )
        ),
        
        column(5,
          box(
            title = "Select data",
            width = "100%",
            htmlOutput(ns("dataChoices")),
            
            textInput(ns("title"),
                      "Plot title"),
            
            textAreaInput(ns("geneList"),
                          label = "Enter a list of FlyBase IDs:",
                          placeholder = "FBgn0000123..."),
            
            checkboxInput(ns("showGenes"),
                          "Show gene IDs"),
            
            wellPanel("Scale for heat map colours",
              numericInput(ns("scaleMin"),
                           "Minimum",
                           value = -3),
              numericInput(ns("scaleMax"),
                           "Maximum",
                           value = 3)
            )
          )
        ),
      )
}

heatMapServer <- function(id, dataset) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # need to use session namespace for ui elements created in server function
      ns <- session$ns
      
      # create checkboxes from selected files in sidebar
      output$dataChoices <- renderUI({
        generateCheckboxes(dataset(), ns)
      })
      
      # gets the selected data
      data <- reactive({
        dataList <- list()
        
        # use regular expression to get gene IDs out of input
        geneList <- as.list(str_split(input$geneList, regex("\\s", ignore_case = TRUE)))[[1]]
        print(geneList)
        
        lapply(dataset(), function(data) {
          for (file in input[[data]]) {
            ext <- tools::file_ext(data)
            
            df <- data.frame()
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data), 
                               file, 
                               na = "NA", 
                               .name_repair = "minimal")
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            colnames(df)[1] <- "id"
            df[,-1] <- lapply(df[,-1], as.numeric)
            
            tryCatch({
              print(colnames(df))
              df <- df[order(df$id),]
              df <- dplyr::filter(df, id %in% geneList)
              if ("log2FoldChange" %in% colnames(df)) {
                df <- dplyr::select(df, id, log2FoldChange)
                colnames(df)[2] <- file
              } 
              df <- column_to_rownames(df, "id")
              print(df)
              dataList[[paste0(tools::file_path_sans_ext(file))]] <<- df
              
            }, error = function(cond) {
                showModal(modalDialog(
                  title = "Incorrect format",
                  "Please check that data is formatted correctly",
                  easyClose = TRUE
                ))
            })
          }
        })
        
        dataList <- bind_cols(dataList)
        return(dataList)
      })
      
      output$heatMap <- renderPlotly({
        req(input$geneList)

        if (input$showGenes) {
          showLabels <<- c(TRUE, TRUE)
        } else {
          showLabels <<- c(TRUE, FALSE)
        }

        heatMap <- heatmaply(data(),
                            label_names = c("Gene", "Sample", "Log2 fold change"),
                            main = input$title,
                            scale_fill_gradient_fun = scale_fill_gradientn(
                              colours = c("blue", "white", "red"),
                              limits = c(input$scaleMin, input$scaleMax),
                              oob = scales::squish
                            ),
                            showticklabels = showLabels,
                            margins = c(150, 50, 100, 0))
        
        return(heatMap)
      })
    })
}
