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
                         choiceValues = list("SG_LFC_DATABASE",
                                        "WD_LFC_DATABASE",
                                        "BRAIN_LFC_DATABASE")),
      
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
        ),
        
        box(
          title = "Genes to include",
          textAreaInput(ns("gene_list"),
                        "Enter a list of FlyBase IDs:",
                        placeholder = "Fbgn0000123...")
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
      get_data <- function(sheet, genes) {
        print(sheet)
        df <- read_excel("C:/Users/puppy/OneDrive/Documents/projects/rna-seq-data-visualiser/data/CoreData/ALL_Tissues_LFC_Database.xlsx", sheet, na = "NA") %>%
          dplyr::filter(GeneID %in% genes) %>%
          na.omit()
      }
      
      heatmap <- reactive({
        deseq2_data <- data.frame()

        regFilter <- regex("FBGN\\d\\d\\d\\d\\d\\d\\d", ignore_case = TRUE, )
        gene_list <- as.list(str_extract_all(input$gene_list, regFilter))[[1]]
      
        deseq2_data <- append(deseq2_data, get_data(input$tissues[1], gene_list)) %>%
          bind_rows() %>%
          column_to_rownames("GeneID")
      })
      
      
      output$heatMap <- renderPlot({
        req(input$tissues, input$genotypes)
        colorScale <- colorRamp2(c(3,0,-3), c("blue", "white", "red"))
        Heatmap(as.matrix(heatmap()), 
                col = colorScale)
      })
    }
  )
}
