geneEnrichmentUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  fluidPage(
    plotOutput(ns("plots")),
    box(
      htmlOutput(ns("dataChoices")),
      
      numericInput(ns("pvalue"),
                   label = "p-value",
                   value = 0.05,
                   min = 0,
                   step = 0.01),
      
      numericInput(ns("log2foldchange"),
                   label = "Log2 fold change",
                   value = 1.4,
                   min = 0,
                   step = 0.01),
      
      numericInput(ns("lfcLimit"),
                   label = "Maximum log2 fold change to display",
                   value = 10,
                   min = 1,
                   step = 0.01),
      textAreaInput(ns("gene_list"),
                    label = "Enter a list of gene symbols to label",
                    placeholder = "")
    ),
    
    box(
      title = "Extra information",
      htmlOutput(ns("geneInfo"))
    )
  )
}

geneEnrichmentServer <- function(id, dataset) {
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
      
      # get gene symbols from gene ids
      get_gene_names <- function(df) {
        symbols <- mapIds(org.Dm.eg.db, 
                          keys = df$id, 
                          keytype = "FLYBASE", 
                          column = "SYMBOL")
      }
      
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

            dataList[[paste0(data, " ", file)]] <<- df
          }
        })
        return(dataList)
      })
      
      output$plots <- renderPlot({
        for (genes in data()) {
          downregulatedGenes <- filter(genes, log2FoldChange < -1.4)
          upregulatedGenes <- filter(genes, log2FoldChange > 1.4)

          multiGP <- gost(query = list("Upregulated" = rownames(upregulatedGenes), 
                                       "Downregulated" = rownames(downregulatedGenes)), 
                          organism = "dmelanogaster",
                          multi_query = FALSE, evcodes = TRUE)
      
          
          gp_mod = multiGP$result[,c("query", "source", "term_id",
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
          row.names(gp_mod) = gp_mod$ID
          
          gp_mod_cluster = new("compareClusterResult", compareClusterResult = gp_mod)
          print(enrichplot::dotplot(gp_mod_cluster, x = "Count", group = TRUE, by = "GeneRatio"))
        }
      })
    }
  )}
