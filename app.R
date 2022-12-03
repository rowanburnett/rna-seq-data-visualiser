library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(httr)
library(jsonlite)

# deseq2 file paths
deseq2_salivary_gland <- "./data/DESEQ2 Results/Salivary Gland/deseq2_salivary_gland.xlsx"
deseq2_brain <- "./data/DESEQ2 Results/Brain/deseq2_brain.xlsx"
deseq2_wing_disc <- "./data/DESEQ2 Results/Wing Disc/deseq2_wing_disc.xlsx"

ui <- fluidPage(
  
  titlePanel("Title"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("deseq2", 
                         label = "DeSEQ2 data",
                         choices = list("Salivary gland" = deseq2_salivary_gland,
                                        "Brain" = deseq2_brain,
                                        "Wing disc" = deseq2_wing_disc))
    ),
    
    mainPanel(
      h1("Volcano Plot"),
      plotOutput(
        outputId = "volcanoPlot",
        click = clickOpts(id = "plot_click")
        ),
      textOutput(outputId = "coordinates"),
      tableOutput(outputId = "geneTable")
      
    ),
  )
  
)

server <- function(input, output) {
  
  get_data <- function(path) {
    print(path)
    data <- read_excel(path, 1, na = "NA") %>%
      select(-baseMean, -lfcSE, -stat, -pvalue) %>%
      replace(is.na(.), 0)
  }
                        
  output$volcanoPlot <- renderPlot({
    deseq2_data <- lapply(input$deseq2, get_data)
    deseq2_data <- bind_rows(deseq2_data)
    ggplot(data = deseq2_data, aes(x = log2FoldChange, y = -log10(padj))) + 
      geom_point() +
      theme_minimal() +
      geom_vline(xintercept=c(-1.4, 1.4), col="red") +
      geom_hline(yintercept=-log10(0.05), col="red")
  })
  
  output$geneTable <- renderTable({
    table(PtcG4_D6_V_Fer12OG_D6$id)
  })
  output$coordinates <- renderText({
    paste("coordinates", input$plot_click)
  })
  

}

shinyApp(ui = ui, server = server)