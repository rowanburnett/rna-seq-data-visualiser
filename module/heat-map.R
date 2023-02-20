heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)

      fluidRow(
        box(
          title = "Heat map", collapsible = TRUE,
          plotlyOutput(
            ns("heatMap"),
            height = "700px"
          )
        ),
        
        box(
          title = "Select data",
          htmlOutput(ns("dataChoices")),
          
          textInput(ns("title"),
                    "Plot title"),
          textInput(ns("plotFileName"),
                    "File name"),
          selectInput(ns("plotExtension"),
                      "File extension",
                      choices = c(".png", ".jpeg", ".bpm", ".pdf")),
          downloadButton(ns("plotDownload"),
                         "Download plot"),
          
          textAreaInput(ns("geneList"),
                        label = "Enter a list of FlyBase IDs:",
                        placeholder = "FBgn0000123..."),
          
          checkboxInput(ns("showGenes"),
                        "Show gene IDs")
        )
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
        regFilter <- regex("FBGN\\d\\d\\d\\d\\d\\d\\d", ignore_case = TRUE, )
        geneList <- as.list(str_extract_all(input$geneList, regFilter))[[1]]
        
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
            
            tryCatch({
              if ("log2FoldChange" %in% colnames(df)) {
                df[,-1] <- lapply(df[,-1], as.numeric)
                df <- dplyr::select(df, id, log2FoldChange) %>%
                  dplyr::filter(id %in% geneList)
                colnames(df)[2] <- file
                
              } else {
                df[,-1] <- lapply(df[,-1], as.numeric)
                df <- dplyr::filter(df, id %in% geneList)
              }
              
              df <- df[order(df$id),]
              df <- column_to_rownames(df, "id")
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
                              limits = c(-3, 3),
                              oob = scales::squish
                            ),
                            showticklabels = showLabels,
                            margins = c(150, 50, 100, 0))
        
        return(heatMap)
      })
    })
}
