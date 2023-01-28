heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)

      fluidRow(
        box(
          title = "Heat map", collapsible = TRUE,
          plotOutput(
            ns("heatMap"),
            height = "500px",
            click = ns("plot_click")
          )
        ),
        
        box(
          checkboxGroupInput(ns("tissues"),
                             label = "Tissues",
                             choiceNames = tissue_names,
                             choiceValues = list("SG_LFC_DATABASE",
                                                 "WD_LFC_DATABASE",
                                                 "BRAIN_LFC_DATABASE")),
          
          htmlOutput(ns("dataChoices")),
          
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
      
      output$dataChoices <- renderUI({
        tagList(
          lapply(dataset(), function(file) {
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
      })
      
      data <- reactive({
        dataList <- list()
        colorScale <- colorRamp2(c(3, 0, -3), c("blue", "white", "red"))
        regFilter <- regex("FBGN\\d\\d\\d\\d\\d\\d\\d", ignore_case = TRUE, )
        gene_list <- as.list(str_extract_all(input$gene_list, regFilter))[[1]]
        
        lapply(dataset(), function(data) {
          print(data)
          for (file in input[[data]]) {
            ext <- tools::file_ext(data)
            
            df <- data.frame()
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data), file, na = "NA")
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            colnames(df)[1] <- "id"
            
            if ("log2FoldChange" %in% colnames(df)) {
              df[,-1] <- lapply(df[,-1], as.numeric)
              df <- dplyr::select(df, id, log2FoldChange) %>%
                na.omit() %>%
                dplyr::filter(id %in% gene_list)
              colnames(df)[2] <- file
              df <- column_to_rownames(df, "id")
              print(df)
              dataList[[paste0(data, " ", file)]] <<- df
              
            } else {
              df <- dplyr::filter(df, id %in% gene_list) %>%
                column_to_rownames("id") %>%
                na.omit()
              dataList[[paste0(data, " ", file)]] <<- df
            }
          }
        })
        dataList <- bind_cols(dataList) %>%
          as.matrix()
        heatmap <- Heatmap(dataList,
                           col = colorScale)
        return(heatmap)
      })
      
      output$heatMap <- renderPlot({
        heatmap <- data()
        draw(heatmap)
      })
    }
  )
}
