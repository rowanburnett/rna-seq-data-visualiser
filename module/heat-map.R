heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)

      fluidRow(
        box(
          title = "Heat map", collapsible = TRUE,
          plotOutput(
            ns("heatMap"),
            height = "700px"
          )
        ),
        
        box(
          title = "Select data",
          htmlOutput(ns("dataChoices")),
          
          textInput(ns("title"),
                    "Plot title"),
          
          textAreaInput(ns("gene_list"),
                        label = "Enter a list of FlyBase IDs:",
                        placeholder = "FBgn0000123...")
        ),
        
        box(
          title = "Extra information",
          htmlOutput(ns("geneInfo"))
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
        gene_list <- as.list(str_extract_all(input$gene_list, regFilter))[[1]]
        
        lapply(dataset(), function(data) {
          for (file in input[[data]]) {
            ext <- tools::file_ext(data)
            
            df <- data.frame()
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data), 
                               file, 
                               na = "NA", 
                               .name_repair = "minimal")
              print(colnames(df))
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            colnames(df)[1] <- "id"
            
            tryCatch({
              if ("log2FoldChange" %in% colnames(df)) {
                df[,-1] <- lapply(df[,-1], as.numeric)
                df <- dplyr::select(df, id, log2FoldChange) %>%
                  na.omit() %>%
                  dplyr::filter(id %in% gene_list)
                colnames(df)[2] <- file
                df <- column_to_rownames(df, "id")
                
              } else {
                df[,-1] <- lapply(df[,-1], as.numeric)
                df <- dplyr::filter(df, id %in% gene_list) %>%
                  column_to_rownames("id") %>%
                  na.omit()
              }
              
              dataList[[paste0(data, " ", tools::file_path_sans_ext(file))]] <<- df
              
            }, error = function(cond) {
                showModal(modalDialog(
                  title = "Incorrect format",
                  "Please check that data is formatted correctly",
                  easyClose = TRUE
                ))
            })
          }
        })
        
        dataList <- bind_cols(dataList) %>%
          as.matrix()

        return(dataList)
      })
      
      output$heatMap <- renderPlot({
        # colours for heat map
        colorScale <- colorRamp2(c(3, 0, -3), c("yellow", "white", "blue"))
        
        heatmap <- Heatmap(data(),
                           col = colorScale,
                           column_title = input$title,
                           column_title_gp = gpar(fontsize = 20, fontface = "bold"))
        draw(heatmap)
      })
    })
}
