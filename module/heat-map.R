heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)

      fluidRow(
        box(
          title = "Heat map", collapsible = TRUE,
          plotlyOutput(
            ns("heatMap"),
            height = "700px"
          #  click = ns("plotClick")
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
      
      obs <- list()
      val <- reactiveValues()
      
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
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            colnames(df)[1] <- "id"
            
            tryCatch({
              if ("log2FoldChange" %in% colnames(df)) {
                df[,-1] <- lapply(df[,-1], as.numeric)
                df <- dplyr::select(df, id, log2FoldChange, padj, lfcSE) %>%
                  na.omit() %>%
                  dplyr::filter(id %in% gene_list)
                
              } else {
                df[,-1] <- lapply(df[,-1], as.numeric)
                df <- dplyr::filter(df, id %in% gene_list) %>%
                  na.omit()
              }
              
              df <- mutate(df, "symbol" = getGeneNames(df))
              dataList[[paste0(tools::file_path_sans_ext(data), " ", tools::file_path_sans_ext(file))]] <<- df
              
            }, error = function(cond) {
                showModal(modalDialog(
                  title = "Incorrect format",
                  "Please check that data is formatted correctly",
                  easyClose = TRUE
                ))
            })
          }
        })
        
      #  dataList <- bind_cols(dataList)
        dataList <- bind_rows(dataList, .id = "Sample")

        return(dataList)
      })
      
      # get gene symbols from gene ids
      getGeneNames <- function(df) {
        symbols <- mapIds(org.Dm.eg.db, 
                          keys = df$id, 
                          keytype = "FLYBASE", 
                          column = "SYMBOL")
      }
      
      output$heatMap <- renderPlotly({
        print(data())
        dataMatrix <- select(data(), Sample, id, log2FoldChange)
     #   dataMatrix <- as.matrix(dataMatrix)
        print(dataMatrix)
      #  clusters <- as.dendrogram(hclust(dist(dataMatrix)))
        
        heatMap <- ggplot(data(), aes(Sample, id, fill = log2FoldChange)) +
          geom_tile() +
          theme(legend.position = "bottom",
                axis.text.x = element_text(angle = 90))
        
    #    dendro <- ggplot(data(), aes(Sample, id)) +
    #      ggdendrogram(clusters)
        
    #    print(dendro)
        
        heatmaply(data())
     #   barPlot <- ggplot(data(), aes(log2FoldChange, id)) +
    #      geom_col() +
     #     theme(axis.text.y = element_blank())
        
       # ggarrange(heatMap, barPlot, ncol = 2, widths = c(3, 1))
    #    subplot(dendro, heatMap, barPlot)
      })
      
      # extra gene information
      observeEvent(input$plotClick, {
            data <- data()
            
            tryCatch({
              print(input$plotClick)
              gene <- nearPoints(
                data,
                input$plotClick,
                xvar = "Sample",
                yvar = "id",
                threshold = 100,
                maxpoints = 1
              )
              
              print(gene)
              
              # get gene summary from flybase api
           #   summary <- GET(paste0("https://api.flybase.org/api/v1.0/gene/summaries/auto/", gene$id))
            #  summary <- fromJSON(rawToChar(summary$content))
             # summary <- summary$resultset$result$summary
              
              link <- paste0("https://flybase.org/reports/", gene$id, ".html")
              
              output$geneInfo <- renderUI(
                tagList(
                  div(
                    span("Symbol:", style = "font-weight: bold;"),
                    span(gene$symbol)),
                  div(
                    span("ID:", style = "font-weight: bold;"),
                    span(gene$id)),
                  div(
                    span("Log2 fold change:", style = "font-weight: bold;"),
                    span(gene$log2FoldChange)),
                  div(
                    span("p-value:", style = "font-weight: bold;"),
                    span(gene$padj)),
          #        div(
           #         span("Gene summary:", style = "font-weight: bold;"),
            #        span(summary)),
                  a("FlyBase Gene Report", 
                    href = link,
                    target = "_blank")
                )
              )
            }, error = function(cond) {
              print(cond)
            })
            
            val[[id]] <- input[[id]]$x
          }, ignoreInit = TRUE)

    })
}
