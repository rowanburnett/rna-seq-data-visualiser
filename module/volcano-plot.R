volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  fluidPage(
    htmlOutput(ns("plots"),
               label = "Volcano plot"),
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

volcanoPlotServer <- function(id, dataset) {
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
      
      # genes to label on the plot (optional)
      geneList <- reactive({
        geneList <- as.list(el(strsplit(input$gene_list, " ")))
      })
      
      # render plots dynamically 
      observeEvent(data(), {
        lapply(seq(data()), function(i) {
          output[[paste0("plot", i) ]] <- renderPlot({
            data <- data()[[i]]
            
            # if list of genes is provided then use it
            if (length(geneList()) > 0) {
              labelledGenes <<- geneList()
            } else {
              labelledGenes <<- NULL
            }
            
            EnhancedVolcano(data, lab = data$symbol, x = "log2FoldChange", y = "padj",
                            pCutoff = input$pvalue,
                            FCcutoff = input$log2foldchange,
                            xlim = c(-input$lfcLimit, input$lfcLimit),
                            selectLab = labelledGenes)
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
            df <- dplyr::select(df, -baseMean, -lfcSE, -stat) %>%
              mutate("-log10(padj)" = -log10(padj)) %>%
              na.omit()
            df <- mutate(df, "symbol" = get_gene_names(df))
              print(df)
              
            dataList[[paste0(data, " ", file)]] <<- df
          }
        })
        return(dataList)
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
