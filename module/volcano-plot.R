volcanoPlotUI <- function(id, label = "Volcano plot") {
  ns <- NS(id)
  
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
                                        "D8" = 3))
    ),
    mainPanel(
      h1("Volcano Plot"),
      plotOutput(
        ns("volcanoPlot"),
        click = clickOpts(id = "plot_click")
      )
    )
  )
}

volcanoPlotServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      get_data <- function(path, sheet) {
        data <- read_excel(path, sheet, na = "NA") %>%
          select(-baseMean, -lfcSE, -stat, -pvalue) %>%
          replace(is.na(.), 0)
        return(data)
      }
      
      output$volcanoPlot <- renderPlot({
        req(input$deseq2, input$day)
        
        deseq2_data <- list()
        deseq2_names <- list()
        
        for (tissue in input$deseq2) {
          for (day in input$day) {
            tryCatch({
              day <- as.numeric(day)
              name <- excel_sheets(tissue)[day]
              deseq2_names <- append(deseq2_names, name)
              deseq2_data <- append(deseq2_data, lapply(tissue, get_data, sheet = day))
              
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        
        names(deseq2_data) <- c(deseq2_names)
        deseq2_data <- bind_rows(deseq2_data, .id = "Tissue")
        
        ggplot(data = deseq2_data, aes(x = log2FoldChange, y = -log10(padj), colour = Tissue)) +
          geom_point() +
          theme_minimal() +
          geom_vline(xintercept=c(-1.4, 1.4), col="red") +
          geom_hline(yintercept=-log10(0.05), col="red")
      })
    
    }
  )
}