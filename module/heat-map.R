heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)
  
  dashboardPage(
    dashboardHeader(title = "Heat map"),
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
        box(
          # could maybe create tabs to view each dataset on a different graph?
          title = "Heat map", collapsible = TRUE,
          plotOutput(
            ns("heatMap"),
            height = "500px",
            click = ns("plot_click")
          ),
          tableOutput(ns("geneTable"))
        ),
      
        box(
          title = "Extra information",
          htmlOutput(ns("geneInfo"))
        )
      )
    )
  )
}

heatMapServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      # get data from excel files based on provided parameters
      get_data <- function(path, sheet) {
        df <- read_excel(path, sheet, na = "NA") %>%
          dplyr::select(-baseMean, -lfcSE, -stat, -padj, -pvalue) %>%
          na.omit()
      }
      
      heatmaps <- reactive({
        deseq2_names <- c()
        heatmaps <- NULL
        
        # gather tissues and genotype to get from excel files - this isn't very good
        for (tissue in input$tissues) {
          for (genotype in input$genotypes) {
            tryCatch({
              sheets <- excel_sheets(tissue)
              for (sheet in sheets) {
                index <- which(tissue_values == tissue)
                name <- tissue_names[index]
                if (grepl(genotype, sheet)) {
                  deseq2_data <- data.frame()
                  deseq2_data <- append(deseq2_data, lapply(tissue, get_data, sheet = sheet)) %>%
                    bind_rows(deseq2_data) %>%
                    dplyr::slice(10:20) %>%
                    column_to_rownames(var = "id") %>%
                    rename(log2FoldChange = paste(name, genotype)) %>%
                    
                    as.matrix()
                  heatmap <- Heatmap(deseq2_data)
                  heatmaps <- heatmaps + heatmap
                  
                }
              }
            },
            error = function(cond) {
              print(cond)
            })
          }
        }
        return(heatmaps)
      })
      
      
      output$heatMap <- renderPlot({
        req(input$tissues, input$genotypes)
        draw(heatmaps())
      })
    }
  )
}
