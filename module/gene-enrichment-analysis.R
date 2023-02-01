geneEnrichmentUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
    fluidPage(
      htmlOutput(ns("plots")),
      box(
        title = "Select data",
        htmlOutput(ns("dataChoices")),
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
            
            tryCatch({
              df[,-1] <- lapply(df[,-1], as.numeric)
              colnames(df)[1] <- "id"
              df <- dplyr::select(df, id, padj, log2FoldChange) %>%
                na.omit() %>%
                filter(padj < input$pvalue)
              
              df <- mutate(df, "symbol" = get_gene_names(df))
              df <- column_to_rownames(df, "id")
  
              downregulatedGenes <- filter(df, log2FoldChange < -input$log2foldchange)
              upregulatedGenes <- filter(df, log2FoldChange > input$log2foldchange)
              
              gostres <- gost(query = list("Upregulated" = rownames(upregulatedGenes), 
                                           "Downregulated" = rownames(downregulatedGenes)), 
                              organism = "dmelanogaster",
                              multi_query = FALSE, evcodes = TRUE)
              
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
            
            dotPlot <- enrichplot::dotplot(gp_mod_cluster, by = "Count")
            
            # download current plot
            observeEvent(input[[paste0("plotDownload", i)]], {
              fileName <- paste0(input[[paste0("plotFileName", i)]])
              fileExtension <- paste0(input[[paste0("plotExtension", i)]])
              
              ggsave(
                filename = paste0(fileName, fileExtension),
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
                c(title = "GSEA dot plot", 
                  side = "right", 
                  lapply(seq(data()), function(i) {
                    tabPanel(
                      title = names(data()[i]),
                      plotOutput(ns(paste0("plot", i)),
                                    height = "500px"),
                      textInput(ns(paste0("plotFileName", i)),
                                "File name"),
                      selectInput(ns(paste0("plotExtension", i)),
                                  "File extension",
                                  choices = c(".png", ".jpeg", "bpm", ".pdf")),
                       actionButton(ns(paste0("plotDownload", i)),
                                    "Download plot")
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
                      textInput(ns(paste0("tableFileName", i)),
                                "File name"),
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
            fileName <- paste0(input[[paste0("tableFileName", i)]])
            
            write.csv(data, paste0("./data/GSEA/Data/", paste0(fileName, ".csv")), row.names = FALSE)
          })
          
          output[[paste0("table", i) ]] <- renderTable(data)
        })
      })
    }
  )}
