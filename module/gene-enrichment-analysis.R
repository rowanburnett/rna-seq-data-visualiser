geneEnrichmentUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
    fluidPage(
      htmlOutput(ns("plots")),
      box(
        title = "Select data",
        htmlOutput(ns("dataChoices")),
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
      
      # get gene symbols from gene ids
      get_gene_names <- function(df) {
        symbols <- mapIds(org.Dm.eg.db, 
                          keys = df$id, 
                          keytype = "FLYBASE", 
                          column = "SYMBOL")
      }
      
      # gets the selected data
      data <- reactive({
        dataList <- list()
        lapply(dataset(), function(data) {
          for (file in input[[data]]) {
            ext <- tools::file_ext(data)
            
            df <- data.frame()
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data), file, na = "na")
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            df[,-1] <- lapply(df[,-1], as.numeric)
            colnames(df)[1] <- "id"
            df <- dplyr::select(df, id, padj, log2FoldChange) %>%
              na.omit() %>%
          #    slice_sample(n = 500) %>%
              filter(padj < 0.05)
            df <- mutate(df, "symbol" = get_gene_names(df))
            df <- column_to_rownames(df, "id")

            downregulatedGenes <- filter(df, log2FoldChange < -1.4)
            upregulatedGenes <- filter(df, log2FoldChange > 1.4)
            
            multiGP <- gost(query = list("Upregulated" = rownames(upregulatedGenes), 
                                         "Downregulated" = rownames(downregulatedGenes)), 
                            organism = "dmelanogaster",
                            multi_query = FALSE, evcodes = TRUE)
            
            dataList[[paste0(data, " ", file)]] <<- multiGP
          }
        })
        return(dataList)
      })
      
      # render plots dynamically 
      observe({
        lapply(seq(data()), function(i){
          
          output[[paste0("plot", i) ]] <- renderPlot({
            data <- data()[[i]]
            gp_mod = data$result[,c("query", "source", "term_id",
                                    "term_name", "p_value", "query_size", 
                                    "intersection_size", "term_size", 
                                    "effective_domain_size", "intersection")]
            
            
            gp_mod$GeneRatio <- paste0(gp_mod$intersection_size, "/", gp_mod$query_size)
            gp_mod$BgRatio <- paste0(gp_mod$term_size, "/", gp_mod$effective_domain_size)
            names(gp_mod) <- c("Cluster", "Category", "ID", "Description", "p.adjust", 
                               "query_size", "Count", "term_size", "effective_domain_size", 
                               "geneID", "GeneRatio", "BgRatio")
            gp_mod$geneID <- gsub(",", "/", gp_mod$geneID)
            gp_mod <- gp_mod[!duplicated(gp_mod$ID),]
            row.names(gp_mod) <- gp_mod$ID
            
            
            gp_mod_cluster <- new("compareClusterResult", compareClusterResult = gp_mod)
            
            dotPlot <- enrichplot::dotplot(gp_mod_cluster, 
                                x = "GeneRatio", 
                                group = TRUE, 
                                by = "Count", 
                                includeAll = TRUE)
            
            # download current plot
            observeEvent(input[[paste0("plotDownload", i)]], {
              ggsave(
                filename = paste0(names(data()[i]), ".png"),
                plot = dotPlot,
                device = "png",
                path = ("./data/GSEA/Plots/")
              )
            })
            
            return(dotPlot)

          })
        })
      })
      
      # create tabBox with tab for each plot
      output$plots <- renderUI ({
        do.call(tabBox, 
                c(title = "GSEA plot", 
                  side = "right", 
                  lapply(seq(data()), function(i) {
                    tabPanel(
                      title = names(data()[i]),
                      plotOutput(ns(paste0("plot", i)),
                                    height = "500px"),
                                 actionButton(ns(paste0("plotDownload", i)),
                                              "Download plot"),
                    )
                }))
              )
      })
      
      # create tabBox with tab for each table
      output$geneTable <- renderUI ({
        do.call(tabBox, 
                c(id = ns("tableTabs"), 
                  title = "Results", 
                  side = "right", 
                  lapply(seq(data()), function(i) {
                    tabPanel(
                      title = names(data()[i]),
                      actionButton(ns(paste0("tableDownload", i)),
                                   "Download data"),
                      tableOutput(ns(paste0("table", i)))
                    )
                }))
              )
      })
      
      tableData <- reactive({
        lapply(seq(data()), function(i) {
          data <- as.data.frame(data()[[i]]$result)
          
          data <- data[,c("term_id", 
                          "p_value", 
                          "source", 
                          "term_name", 
                          "term_size", 
                          "intersection_size")]
          
          data <- data[!duplicated(data$term_id),]
          names(data) <- c("ID", 
                           "p-value", 
                           "Source", 
                           "Term", 
                           "Term size", 
                           "Intersection")
          
          row.names(data) <- data$ID
          return(data)
        })
      })
      
      # render tables dynamically
      observe({
        lapply(seq(tableData()), function(i){
          data <- tableData()[[i]]
          # download current dataset
          observeEvent(input[[paste0("tableDownload", i)]], {
            print(data)
            print(input$tableTabs)
            write.csv(data, paste0("./data/GSEA/Data/", input$tableTabs), row.names = FALSE)
          })
          
          output[[paste0("table", i) ]] <- renderTable(data)
        })
      })
    }
  )}
