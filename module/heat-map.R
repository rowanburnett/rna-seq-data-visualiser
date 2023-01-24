heatMapUI <- function(id, label = "Heat map") {
  ns <- NS(id)

      fluidRow(
        box(
          title = "Heat map", collapsible = TRUE,
          plotOutput(
            ns("heatMap"),
            height = "500px",
            click = ns("plot_click")
          )
        ),
        
        box(
          checkboxGroupInput(ns("tissues"),
                             label = "Tissues",
                             choiceNames = tissue_names,
                             choiceValues = list("SG_LFC_DATABASE",
                                                 "WD_LFC_DATABASE",
                                                 "BRAIN_LFC_DATABASE")),
          
          textAreaInput(ns("gene_list"),
                        label = "Enter a list of FlyBase IDs",
                        placeholder = "FBgn0000123...")
        ),
        
        box(
          title = "Extra information",
          htmlOutput(ns("geneInfo"))
        )
      )
}

heatMapServer <- function(id) {
  moduleServer(
    id,
    
    function(input, output, session) {
      
      get_data <- function(sheet, genes) {
        print(sheet)
        df <- read_excel("C:/Users/puppy/OneDrive/Documents/projects/rna-seq-data-visualiser/data/CoreData/ALL_Tissues_LFC_Database.xlsx", sheet, na = "NA") %>%
          dplyr::filter(GeneID %in% genes) %>%
          column_to_rownames("GeneID") %>%
          na.omit()
      }
      
      heatmaps <- reactive({
        heatmap_list <- NULL
        regFilter <- regex("FBGN\\d\\d\\d\\d\\d\\d\\d", ignore_case = TRUE, )
        gene_list <- as.list(str_extract_all(input$gene_list, regFilter))[[1]]
      
        
        for (tissue in input$tissues) {
          index <- which(tissue_values == tissue)
          name <- tissue_names[index]
          
          data <- get_data(tissue, gene_list) %>%
            as.matrix()
          heatmap <- Heatmap(data, 
                             col = colorScale,
                             column_title = tissue)
          heatmap_list <- heatmap_list + heatmap
        }
        
        return(heatmap_list)
      })
      
      
      output$heatMap <- renderPlot({
        req(input$tissues)
        colorScale <- colorRamp2(c(3, 0, -3), c("blue", "white", "red"))
        draw(heatmaps())
      })
    }
  )
}
