geneEnrichmentUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
    fluidPage(
      htmlOutput(ns("plots")),
      box(
        title = "Select data",
        htmlOutput(ns("dataChoices")),
        
        textInput(ns("title"),
                  "Plot title"),
        
        wellPanel(
          "Significance cutoff values",
          
          numericInput(ns("pvalue"),
                       label = "p-value",
                       value = 0.05,
                       min = 0,
                       step = 0.01),
          
          numericInput(ns("log2foldchange"),
                       label = "Log2 fold change",
                       value = 1.4,
                       min = 0,
                       step = 0.01)
        )
      ),
      box(
        title = "Convert GO term IDs to FlyBase gene IDs",
        textAreaInput(ns("convertGOInput"),
                      "Enter GO term IDs"),
        textInput(ns("convertGOFileName"),
                  "File name"),
        downloadButton(ns("convertGOButton"),
                     "Download results")
      ),
      htmlOutput(ns("geneTable"))
    )
}

geneEnrichmentServer <- function(id, dataset) {
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
        lapply(dataset(), function(data) {
          for (file in input[[data]]) {
            ext <- tools::file_ext(data)
            
            df <- data.frame()
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data), 
                               file, 
                               na = "na",
                               .name_repair = "minimal")
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            tryCatch({
              df[,-1] <- lapply(df[,-1], as.numeric)
              colnames(df)[1] <- "id"
              df <- dplyr::select(df, id, padj, log2FoldChange) %>%
                na.omit() %>%
                filter(padj < input$pvalue)
              
              df <- column_to_rownames(df, "id")
  
              downregulatedGenes <- filter(df, log2FoldChange < -input$log2foldchange)
              upregulatedGenes <- filter(df, log2FoldChange > input$log2foldchange)
              
              gostres <- gost(query = list("Upregulated" = rownames(upregulatedGenes), 
                                           "Downregulated" = rownames(downregulatedGenes)), 
                              organism = "dmelanogaster",
                              multi_query = FALSE, evcodes = TRUE)
              
              gostres <- gostres$result[,c("query", "source", "term_id",
                                           "term_name", "p_value", "query_size", 
                                           "intersection_size", "term_size", 
                                           "effective_domain_size", "intersection")]
              
              gostres$GeneRatio <- paste0(gostres$intersection_size, "/", gostres$query_size)
              gostres$BgRatio <- paste0(gostres$term_size, "/", gostres$effective_domain_size)
              
              names(gostres) <- c("Cluster", "Category", "ID", "Description", "p.adjust", 
                                  "query_size", "Count", "term_size", "effective_domain_size", 
                                  "geneID", "GeneRatio", "BgRatio")
              
              gostres$geneID <- gsub(",", "/", gostres$geneID)
              gostres <- gostres[!duplicated(gostres$ID),]
              
              dataList[[paste0(tools::file_path_sans_ext(data), " ", file)]] <<- gostres
            }, error = function(cond) {
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
      
      # render plots dynamically 
      observe({
        lapply(seq(data()), function(i){
          
          output[[paste0("plot", i) ]] <- renderPlot({
            gostres <- data()[[i]]
            row.names(gostres) <- gostres$ID
            
            gostresCluster <- new("compareClusterResult", compareClusterResult = gostres)
            
            dotPlot <- enrichplot::dotplot(gostresCluster, 
                                           title = input$title,
                                           by = "Count")
            
            # download current plot
            output[[paste0("plotDownload", i)]] <- generatePlotDownload(input[[paste0("plotFileName", i)]], 
                                                                        input[[paste0("plotExtension", i)]],
                                                                        dotPlot)
            
            return(dotPlot)
          })
        })
      })
      
      # create tabBox with tab for each plot
      output$plots <- renderUI ({
        generatePlotTabs(data(), "GSEA dot plot", ns)
      })
      
      # create tabBox with tab for each table
      output$geneTable <- renderUI ({
        do.call(tabBox, 
          c(title = "Results", 
            side = "right", 
            lapply(seq(data()), function(i) {
              tabPanel(
                title = names(data()[i]),
                textInput(ns(paste0("tableFileName", i)),
                          "File name"),
                downloadButton(ns(paste0("tableDownload", i)),
                             "Download data"),
                tableOutput(ns(paste0("table", i)))
              )
          }))
        )
      })
      
      # render tables dynamically
      observe({
        lapply(seq(data()), function(i){
          data <- data()[[i]]
          
          # download current dataset
          output[[paste0("tableDownload", i)]] <- downloadHandler(
            file = function() {
              paste0(input[[paste0("tableFileName", i)]], ".csv")
            },
            
            content = function(file) {
              write.csv(data, file, row.names = FALSE)
            }
          )
        
          output[[paste0("table", i) ]] <- renderTable(data)
        })
      })
      
      # convert GO term IDs to flybase gene IDs
      output$convertGOButton <- downloadHandler(
        filename = function() {
          paste0(input$convertGOFileName, ".csv")
        },
          
        content = function(file) {
          tryCatch({
          # use regex to match all inputted terms
            termsToConvert <- str_extract_all(input$convertGOInput, ".+(?=[:space:]+)")[[1]]
            
            # use gconvert to get gene IDs
            conversion <- gconvert(termsToConvert, organism = "dmelanogaster")
            
            conversion <- dplyr::select(conversion, -input_number, -namespace)
            names(conversion) <- c("TermID", "TargetNumber", "GeneID", "Name", "Description")
            conversion <- conversion[!duplicated(conversion$GeneID),]
            
            write.csv(conversion, file, row.names = FALSE)
            
          }, error = function(cond) {
            showModal(modalDialog(
              title = "No results",
              "Please check that GO term IDs were entered correctly",
              easyClose = TRUE
            ))
          })
        }
      )
    }
  )}
