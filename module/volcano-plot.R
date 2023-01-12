

volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  
  tabPanel("Volcano plot",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(ns("genotypes"), 
                           label = "Genotypes",
                           choiceNames = genotype_names,
                           choiceValues = genotype_values),
        checkboxGroupInput(ns("tissues"),
                           label = "Tissues",
                           choiceNames = tissue_names,
                           choiceValues = tissue_values),
        wellPanel(
          p("Differentially expressed gene thresholds"),
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
         )
      ),
      
      mainPanel(
        h1("Volcano Plot"),
        plotOutput(
          ns("volcanoPlot"),
          click = clickOpts(id = "plot_click")
        ),
        tableOutput(ns("geneTable"))
      )
    )
  )
}

volcanoPlotServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      # get data from excel files based on provided parameters
      get_data <- function(path, sheet) {
        data <- read_excel(path, sheet, na = "NA") %>%
          select(-baseMean, -lfcSE, -stat, -pvalue) %>%
          replace(is.na(.), 0)
        return(data)
      }
      
      deseq2_data <- c()
      deseq2_names <- c()
      
      output$volcanoPlot <- renderPlot({
        req(input$tissues, input$genotypes)
        deseq2_data <<- c()
        deseq2_names <<- c()
        
        # gather tissues and genotype to get from excel files - this isn't very good
        for (tissue in input$tissues) {
          for (genotype in input$genotypes) {
            tryCatch({
              sheets <- excel_sheets(tissue)
              for (sheet in sheets) {
                index <- which(tissue_values == tissue)
                name <- tissue_names[index]
                if (grepl(genotype, sheet)) {
                  deseq2_names <<- append(deseq2_names, paste(name, ": ", genotype))
                  deseq2_data <<- append(deseq2_data, lapply(tissue, get_data, sheet = sheet))
                }
              }
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        
        # need to add names to data frame to tell data apart
        names(deseq2_data) <- c(deseq2_names)
        deseq2_data <- bind_rows(deseq2_data, .id = "Genotype")
        
        # create volcano plot
        ggplot(data = deseq2_data, aes(x = log2FoldChange, y = -log10(padj), colour = Genotype)) +
          geom_point() +
          theme_minimal() +
          geom_vline(xintercept = c(-(input$log2foldchange), input$log2foldchange), col = "red") +
          geom_hline(yintercept = -log10(input$pvalue), col = "red")
      })
    }
  )
}
