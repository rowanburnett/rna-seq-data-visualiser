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
                     step = 0.01),
        fileInput(ns("file"),
                  "Upload CSV or Excel workbook",
                  buttonLabel = "Browse...",
                  placeholder = "No file selected")
      ),
      
      dashboardBody(
        fluidRow(
          tabBox(
            id = ns("tabset"),
            title = "Volcano plot"
          ),
        
          box(
            title = "Extra information",
            htmlOutput(ns("geneInfo"))
          ),
          
          box(
            title = "File",
            textOutput(ns("fileInfo"))
          )
        )
      )
    )
}

volcanoPlotServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # increase max request size to upload files up to 100mb
      options(shiny.maxRequestSize=100*1024^2)
      
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
      
      observeEvent(data(), {
        print(data())
        for (data in data()) {
            appendTab("tabset",
                        tabPanel("Tab",
                          renderPlot({
                            EnhancedVolcano(data, lab = "id", x = "log2FoldChange", y = "padj",
                                               pCutoff = input$pvalue,
                                               FCcutoff = input$log2foldchange)
                             })
                        )
                      )
                    }
      })
      
      data <- reactive({
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
                  deseq2_data <- append(deseq2_data, lapply(tissue, get_data, sheet = sheet))
                }
              }
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        return(deseq2_data)
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
        gene <- nearPoints(
          data(), 
          input$plot_click,
          xvar = "log2FoldChange",
          yvar = "-log10(padj)",
          maxpoints = 1
        )
        
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
      
      output$fileInfo <- renderPrint({
        file <- input$file
        req(file)
        print(file)
        ext <- tools::file_ext(file$datapath)
        
        
        if (ext == "csv") {
          print("csv file")
        } else {
          print("not csv file")
        }
        
      })
    }

)}
