geneEnrichmentUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
    fluidPage(
      plotOutput(ns("plots")),
      box(
        htmlOutput(ns("dataChoices")),
      ),
      box(
        tableOutput(ns("geneTable"))
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
      
      
      output$plots <- renderPlot({
        lapply(data(), function(data) {
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
          
          enrichplot::dotplot(gp_mod_cluster, x = "Count", group = TRUE, by = "GeneRatio")
        })
      })
      
      output$geneTable <- renderTable({
        lapply(data(), function(data) {
          table(data$result[,c("term_id", "p_value", "source", "term_name", "term_size", "intersection_size")])
        })
      })
      
    }
  )}
