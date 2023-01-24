volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
      
    fluidRow(
      htmlOutput(ns("plots")),
      
      box(
        checkboxGroupInput(ns("genotypes"), 
                           label = "Genotypes",
                           choiceNames = genotype_names,
                           choiceValues = genotype_values),
        
        checkboxGroupInput(ns("tissues"),
                           label = "Tissues",
                           choiceNames = tissue_names,
                           choiceValues = tissue_values),
    
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
}

volcanoPlotServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # need to use session namespace for ui elements created in server function
      ns <- session$ns
      
      # increase max request size to upload files up to 100mb
      options(shiny.maxRequestSize=100*1024^2)
      
      # get gene symbols from gene ids
      get_gene_names <- function(df) {
        symbols <- mapIds(org.Dm.eg.db, 
                          keys = df$id, 
                          keytype = "FLYBASE", 
                          column = "SYMBOL")
      }
      
      # get data from excel files based on provided parameters
      get_data <- function(path, sheet) {
        df <- read_excel(path, sheet, na = "NA") %>%
          dplyr::select(-baseMean, -lfcSE, -stat) %>%
          na.omit() %>%
          mutate("-log10(padj)" = -log10(padj))
      }
      
      # render plots dynamically 
      observeEvent(data(), {
        lapply(seq(data()), function(i) {
          output[[paste0("plot", i) ]] <- renderPlot({
            data <- data()[[i]]
            data <- mutate(data, "symbol" = get_gene_names(data))
            print(head(data))
            EnhancedVolcano(data, lab = data$symbol, x = "log2FoldChange", y = "padj",
                            pCutoff = input$pvalue,
                            FCcutoff = input$log2foldchange)
          })
        })
      })
      
      output$plots <- renderUI ({
        do.call(tabBox, c(lapply(seq(data()), function(i) {
          tabPanel(
            title = names(data()[i]),
            plotOutput(ns(paste0("plot", i)), 
                       click = ns(paste0("plot_click", i)))
          )
        })))
      })
      
      # gather tissues and genotypes to get from excel files
      data <- reactive({
        data <- list()

        for (tissue in input$tissues) {
          for (genotype in input$genotypes) {
            tryCatch({
              sheets <- excel_sheets(tissue)
              for (sheet in sheets) {
                
                # get tissue name for plot title
                tissue_index <- which(tissue_values == tissue)
                tissue_name <- tissue_names[tissue_index]
                
                # check if genotype in sheet name - need to do this a better way
                if (grepl(genotype, sheet)) {
                  data[[paste0(tissue_name, " ", genotype)]] <- get_data(tissue, sheet)
                }
              }
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        return(data)
      })
      
      # extra gene info box
      output$geneInfo <- renderUI({
        lapply(seq(data()), function(i) {
          gene <<- nearPoints(
            data()[[i]], 
            input[[paste0("plot_click", i)]],
            xvar = "log2FoldChange",
            yvar = "-log10(padj)",
            maxpoints = 1
          )
        })
        
        if (length(gene$id) > 0) {
          summ = summary(gene$id)
        
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
      
      # get gene summary from flybase api
      summary <- function(id) {
        res <- GET(glue("https://api.flybase.org/api/v1.0/gene/summaries/auto/{id}"))
        summary <- fromJSON(rawToChar(res$content))
        summary <- summary$resultset$result$summary
      }
    }
)}
