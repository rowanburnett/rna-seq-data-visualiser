volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  fluidPage(
    column(7,
      htmlOutput(ns("plots"),
                 label = "Volcano plot")
    ),
    column(5,
    box(title = "Extra information",
        width = "100%",
        htmlOutput(ns("geneInfo"))
    )),
    column(5,
      box(
        title = "Select data",
        width = "100%",
        htmlOutput(ns("dataChoices")),
        
        textInput(ns("title"),
                  "Plot title"),
        
        checkboxInput(ns("useNonAdjusted"),
                      "Use non-adjusted p-values"),
        
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
          "Axis limits",
          textInput(ns("lfcLimit"),
                       label = "Maximum log2 fold change to display"),
          textInput(ns("pvalueLimit"),
                       label = "Smallest p-value to display")
        ),
        
        wellPanel(
          "Gene labels",
          textAreaInput(ns("geneList"),
                        label = "Enter a list of gene IDs/symbols to label",
                        placeholder = ""),
          checkboxInput(ns("convertIDs"),
                        "Convert gene IDs to gene symbols")
        )
      )
    )
  )
}

volcanoPlotServer <- function(id, dataset) {
  moduleServer(
    id,
    
    function(input, output, session) {
      # need to use session namespace for ui elements created in server function
      ns <- session$ns
      
      obs <- list()
      val <- reactiveValues()

      # create checkboxes from selected files in sidebar
      output$dataChoices <- renderUI({
        generateCheckboxes(dataset(), ns)
      })
      
      # gets the selected data
      data <- reactive({
        dataList <- list()
        lapply(dataset(), function(data) {
          for (file in input[[data]]) {
            ext <- tools::file_ext(data) # file extension
            
            df <- data.frame()
            # different read functions depending on file format
            if (ext == "xls" || ext == "xlsx") {
              df <- read_excel(paste0("./data/Uploads/", data), 
                               file, 
                               na = "na",
                               .name_repair = "minimal")
              
            } else if (ext == "csv") {
              df <- read.csv(paste0("./data/Uploads/", data))
            }
            
            # format data
            tryCatch({
              # convert columns to numeric because excel files save in the wrong format
              df[,-1] <- lapply(df[,-1], as.numeric)
              colnames(df)[1] <- "id"
              df <- mutate(df, "-log10(padj)" = -log10(padj))
              df <- mutate(df, "-log10(pvalue)" = -log10(pvalue)) %>%
                na.omit()
              df <- mutate(df, "symbol" = getGeneNames(df))
              
              dataList[[paste0(data, " ", file)]] <<- df
              
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
      
      # get gene symbols from gene ids
      getGeneNames <- function(df) {
        symbols <- mapIds(org.Dm.eg.db, 
                          keys = df$id, 
                          keytype = "FLYBASE", 
                          column = "SYMBOL")
      }
      
      # render plots dynamically 
      observe({
        lapply(seq(data()), function(i){
          output[[paste0("plot", i) ]] <- renderPlot({
            df <- data()[[i]]

            # list of genes to label on the plot (optional)
            geneList <- as.list(str_split(input$geneList, regex("\\s", ignore_case = TRUE)))
            
            # if list of genes is provided then use it
            if (geneList[[1]][1] != "") {
              labelledGenes <<- geneList[[1]]
              overlap <<- Inf
              connectors <<- TRUE
            } else {
              labelledGenes <<- NULL
              overlap <<- 10
              connectors <<- FALSE
            }
            
            if (input$useNonAdjusted) {
              pvals <<- "pvalue"
              pmaximum <<- max(-log10(df$pvalue), na.rm = TRUE)
            } else {
              pvals <<- "padj"
              pmaximum <<- max(-log10(df$padj), na.rm = TRUE)
            }
            
            if (input$convertIDs) {
              label <<- df$symbol
            } else {
              label <<- df$id
            }
            
            if (input$lfcLimit != "") {
              xLimit <<- c(-as.numeric(input$lfcLimit), as.numeric(input$lfcLimit))
            } else {
              xLimit <<- c(min(df$log2FoldChange, na.rm=TRUE) - 1.5,
                       max(df$log2FoldChange, na.rm=TRUE) + 1.5)
            }
            
            if (input$pvalueLimit != "") {
              yLimit <<- c(0, -log10(as.numeric(input$pvalueLimit)))
            } else {
              yLimit <<- c(0, pmaximum + 5)
            }
            
            # draw volcano plot
            volcanoPlot <- EnhancedVolcano(df, lab = label, x = "log2FoldChange", y = pvals,
                            title = input$title,
                            pCutoff = input$pvalue,
                            FCcutoff = input$log2foldchange,
                            xlim = xLimit,
                            ylim = yLimit,
                            selectLab = labelledGenes,
                            legendLabels = c('Not sig.','Log2 FC','p-value',
                                             'p-value & Log2 FC'),
                            legendPosition = "bottom",
                            legendLabSize = 12,
                            legendIconSize = 3.0,
                            drawConnectors = connectors,
                            widthConnectors = 0.75,
                            max.overlaps = overlap,
                            caption = paste0("Log2 fold change cutoff, ", 
                                             input$log2foldchange, "; ", 
                                             "p-value cutoff, ", 
                                             input$pvalue))
            
            
            # download current plot
            output[[paste0("plotDownload", i)]] <- generatePlotDownload(input[[paste0("plotFileName", i)]], 
                                                                        input[[paste0("plotExtension", i)]],
                                                                        volcanoPlot)
            
            return(volcanoPlot)
          })
        })
      })
      
      # extra gene information
      observe({
        lapply(seq(data()), function(i){
          id <- paste0("plotClick", i)
          
          # necessary to allow clicking multiple plots correctly
          if (!is.null(obs[[id]])) {
            obs[[id]]$destroy()
          }
          val[[id]] <- NULL
          obs[[id]] <<- observeEvent(input[[id]], {
            data <- data()
            tryCatch({
              gene <- nearPoints(
                data[[i]], 
                input[[paste0("plotClick", i)]],
                xvar = "log2FoldChange",
                yvar = "-log10(padj)",
                maxpoints = 1
              )
              
              # get gene summary from flybase api
              summary <- GET(paste0("https://api.flybase.org/api/v1.0/gene/summaries/auto/", gene$id))
              summary <- fromJSON(rawToChar(summary$content))
              summary <- summary$resultset$result$summary
              
              link <- paste0("https://flybase.org/reports/", gene$id, ".html")
              
              output$geneInfo <- renderUI(
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
                    span(summary)),
                  a("FlyBase Gene Report", 
                    href = link,
                    target = "_blank")
                )
              )
            }, error = function(cond) {
              print("No gene selected")
            })
            
            val[[id]] <- input[[id]]$x
          }, ignoreInit = TRUE)
          
        })
      })
      
      # create tabBox with tab for each plot
      output$plots <- renderUI ({
        do.call(tabBox, 
        c(title = "Volcano plot",
          width = "100%",
          height = "700px",
          side = "right", 
          lapply(seq(data()), function(i) {
            tabPanel(
              title = names(data()[i]),
              plotOutput(ns(paste0("plot", i)),
                         height = "700px",
                         click = ns(paste0("plotClick", i))),
              textInput(ns(paste0("plotFileName", i)),
                        "File name"),
              selectInput(ns(paste0("plotExtension", i)),
                          "File extension",
                          choices = c(".png", ".jpeg", ".bpm", ".pdf")),
              downloadButton(ns(paste0("plotDownload", i)),
                             "Download plot")
            )
          }))
        )
      })
    }
)}
