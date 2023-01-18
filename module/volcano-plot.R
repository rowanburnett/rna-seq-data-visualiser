volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "Volcano plot"),
      dashboardSidebar(
        checkboxGroupInput(ns("genotypes"), 
                           label = "Genotypes",
                           choiceNames = genotype_names,
                           choiceValues = genotype_values),
        checkboxGroupInput(ns("tissues"),
                           label = "Tissues",
                           choiceNames = tissue_names,
                           choiceValues = tissue_values)
      ),
      
      dashboardBody(
        fluidRow(
          box(
            # could maybe create tabs to view each dataset on a different graph?
            title = "Volcano plot", collapsible = TRUE,
            plotOutput(
              ns("volcanoPlot"),
              height = "500px",
              click = ns("plot_click")
            ),
            tableOutput(ns("geneTable"))
          ),
          
          box(
            title = "Differentially expressed gene thresholds", collapsible = TRUE,
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
          
          box(
            title = "Extra information",
            htmlOutput(ns("geneInfo"))
          )
        )
      )
    )
}

volcanoPlotServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      # get gene symbols from gene ids
      get_gene_names <- function(data) {
        symbols <- mapIds(org.Dm.eg.db, keys = data$id, keytype = "FLYBASE", column = "SYMBOL")
        unname(symbols)
      }
      
      # get data from excel files based on provided parameters
      get_data <- function(path, sheet) {
        df <- read_excel(path, sheet, na = "NA") %>%
          dplyr::select(-baseMean, -lfcSE, -stat) %>%
          na.omit() %>%
          mutate("-log10(padj)" = -log10(padj))
      }
      
      data <- reactive({
        deseq2_names <- c()
        deseq2_data <- data.frame()
        
        # gather tissues and genotype to get from excel files - this isn't very good
        for (tissue in input$tissues) {
          for (genotype in input$genotypes) {
            tryCatch({
              sheets <- excel_sheets(tissue)
              for (sheet in sheets) {
                index <- which(tissue_values == tissue)
                name <- tissue_names[index]
                if (grepl(genotype, sheet)) {
                  deseq2_names <- append(deseq2_names, paste(name, ": ", genotype))
                  deseq2_data <- append(deseq2_data, lapply(tissue, get_data, sheet = sheet))
                }
              }
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        
        names(deseq2_data) <- c(deseq2_names) # add names to data frame to tell data apart
        deseq2_data <- bind_rows(deseq2_data)
        deseq2_data <- mutate(deseq2_data, symbol = get_gene_names(deseq2_data))
      })
      
      
      output$volcanoPlot <- renderPlot({
        req(input$tissues, input$genotypes)

        # create volcano plot
         EnhancedVolcano(data(), lab = data()$symbol, x = "log2FoldChange", y = "padj",
           pCutoff = input$pvalue,
           FCcutoff = input$log2foldchange)
      })
      
      point <- function(){
        deseq2_data <- data()
        gene <- nearPoints(
          deseq2_data, 
          input$plot_click,
          xvar = "log2FoldChange",
          yvar = "-log10(padj)",
          maxpoints = 1
        )
      }
      
      summary <- function(id) {
        res <- GET(glue("https://api.flybase.org/api/v1.0/gene/summaries/auto/{id}"))
        summary <- fromJSON(rawToChar(res$content))
        summary <- summary$resultset$result$summary
      }
      
      output$geneInfo <- renderUI({
        print(input$plot_click)
        gene <- nearPoints(
          data(), 
          input$plot_click,
          xvar = "log2FoldChange",
          yvar = "-log10(padj)",
          maxpoints = 1
        )
        
        if (length(gene$id) > 0) {
          summ = summary(gene$id)
          print(summ[1])
        
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
          div(
            span("Gene summary:", style = "font-weight: bold;"), 
            span(summ))
        )
        }
      })
    }
  )
}
