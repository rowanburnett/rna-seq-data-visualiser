volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  
  tabPanel("Volcano plot",
    sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(ns("deseq2"), 
                           label = "DeSEQ2 data",
                           choices = list("Salivary gland" = deseq2_salivary_gland,
                                          "Brain" = deseq2_brain,
                                          "Wing disc" = deseq2_wing_disc)),
        checkboxGroupInput(ns("day"),
                           label = "Day",
                           choices = list("D5" = 1,
                                          "D6" = 2,
                                          "D8" = 3)),
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
        req(input$deseq2, input$day)
        deseq2_data <<- c()
        deseq2_names <<- c()
        
        # gather days and tissues to get from excel files - this isn't very good
        for (tissue in input$deseq2) {
          for (day in input$day) {
            tryCatch({
              day <- as.numeric(day)
              name <- excel_sheets(tissue)[day]
              deseq2_names <<- append(deseq2_names, name)
              deseq2_data <<- append(deseq2_data, lapply(tissue, get_data, sheet = day))
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        
        # need to add names to data frame to tell data apart
        names(deseq2_data) <- c(deseq2_names)
        deseq2_data <- bind_rows(deseq2_data, .id = "Tissue")
        
        # create volcano plot
        ggplot(data = deseq2_data, aes(x = log2FoldChange, y = -log10(padj), colour = Tissue)) +
          geom_point() +
          theme_minimal() +
          geom_vline(xintercept = c(-(input$log2foldchange), input$log2foldchange), col = "red") +
          geom_hline(yintercept = -log10(input$pvalue), col = "red")
      })
    }
  )
}
