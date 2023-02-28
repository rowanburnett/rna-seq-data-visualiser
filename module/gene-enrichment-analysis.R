geneEnrichmentUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
    fluidPage(
     column(8,
        box(
          width = "100%",
          title = "GSEA dot plot",
          plotOutput(ns("dotPlot"),
                     height = "100%"),
          textInput(ns("plotFileName"),
                    "File name"),
          selectInput(ns("plotExtension"),
                      "File extension",
                      choices = c(".png", ".jpeg", ".bpm", ".pdf")),
          downloadButton(ns("plotDownload"))
        )
      ),
      column(4,
        box(
          width = "100%",
          title = "Convert GO term IDs to FlyBase gene IDs",
          textAreaInput(ns("convertGOInput"),
                        "Enter GO term IDs"),
          textInput(ns("convertGOFileName"),
                    "File name"),
          downloadButton(ns("convertGOButton"),
                         "Download results")
        )
      ),
      column(4,
        box(
          width = "100%",
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
          ),
          wellPanel(
            "Plot options",
            
            textInput(ns("plotWidth"),
                      "Plot width in pixels",
                      value = 700),
            textInput(ns("plotHeight"),
                      "Plot height in pixels",
                      value = 700),
            numericInput(ns("GOLimit"),
                         label = "Maximum number of terms to show",
                         value = 10,
                         min = 1,
                         step = 1)
          ),
          checkboxGroupInput(ns("sources"),
                             "Data sources to query against",
                             choices = c("GO:BP", "GO:MF", "GO:CC",
                                         "KEGG", "REAC", "TF",
                                         "MIRNA", "CORUM", "HP", 
                                         "HPA", "WP")
          )
        )
      ),
      column(12,
        htmlOutput(ns("gseaTable"))
      )
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
                              multi_query = FALSE, evcodes = TRUE,
                              sources = input$sources)
              
              gostres <- gostres$result[,c("query", "source", "term_id",
                                           "term_name", "p_value", "query_size", 
                                           "intersection_size", "term_size", 
                                           "effective_domain_size", "intersection")]
              
              
              gostres$GeneRatio <- paste0(gostres$intersection_size, "/", gostres$query_size)
              gostres$BgRatio <- paste0(gostres$term_size, "/", gostres$effective_domain_size)
              
              names(gostres) <- c("Cluster", "Category", "ID", "Description", "p.adjust", 
                                  "query_size", "Count", "term_size", "effective_domain_size", 
                                  "geneID", "GeneRatio", "BgRatio")
              
              gostres$Cluster <- paste0(gostres$Cluster, "\n", file)
              
              gostres$geneID <- gsub(",", "/", gostres$geneID)
              gostres <- gostres[!duplicated(gostres$ID),]
              
              
              dataList[[paste0(tools::file_path_sans_ext(data), " ", file)]] <<- gostres
              
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
      
      observe({
        output$dotPlot <- renderPlot({
          gostres <- bind_rows(data())
          gostresCluster <- new("compareClusterResult", compareClusterResult = gostres)
          
          dotPlot <- enrichplot::dotplot(gostresCluster, 
                                         title = input$title,
                                         by = "Count",
                                         showCategory = input$GOLimit)
          
          # download current plot
          output$plotDownload <- downloadHandler(
            filename = function() {
              paste0(input$plotFileName, input$plotExtension)
            },
            
            content = function(file) {
              tryCatch(
                ggsave(
                  filename = file,
                  plot = dotPlot,
                  units = "px",
                  width = as.numeric(input$plotWidth),
                  height = as.numeric(input$plotHeight)
                )
              ) 
            }
          )
          
          return(dotPlot)
        }, 
        width = as.numeric(input$plotWidth),
        height = as.numeric(input$plotHeight)
        )
      })
      
     
      
      # create tabBox with tab for each table
      output$gseaTable <- renderUI ({
        do.call(tabBox, 
          c(title = "Results", 
            side = "right", 
            width = "100%",
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
          gostres <- data()[[i]]
          
          # download current dataset
          output[[paste0("tableDownload", i)]] <- downloadHandler(
            file = function() {
              paste0(input[[paste0("tableFileName", i)]], ".csv")
            },
            
            content = function(file) {
              write.csv(gostres, file, row.names = FALSE)
            }
          )
          output[[paste0("table", i) ]] <- renderTable(gostres)
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
              "Did you enter a list of GO terms?",
              easyClose = TRUE
            ))
          })
        }
      )
    }
  )}
