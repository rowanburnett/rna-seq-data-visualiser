library(shiny)
library(readxl)
library(ggplot2)
library(tidyverse)
library(httr)
library(jsonlite)

ui <- fluidPage(
  
  titlePanel("Title"),
  
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("genes", 
                         label = "Genes",
                         choices = list("Test1" = 1,
                                        "Test2" = 2))
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

  PtcG4_D6_V_Fer12WT_D6 = read_excel("./data/DESEQ2 Results/Salivary Gland/deseq2_salivary_gland.xlsx", sheet = 1, range = "A1:G50") %>%
                          select(-baseMean, -lfcSE, -stat, -pvalue)
  PtcG4_D6_V_RasYki_D5 = read_excel("./data/DESEQ2 Results/Salivary Gland/deseq2_salivary_gland.xlsx", sheet = 2, range = "A1:G50") %>%
                          select(-baseMean, -lfcSE, -stat, -pvalue)
  PtcG4_D6_V_RasYki_D8 = read_excel("./data/DESEQ2 Results/Salivary Gland/deseq2_salivary_gland.xlsx", sheet = 3, range = "A1:G50") %>%
                          select(-baseMean, -lfcSE, -stat, -pvalue)
  
  output$volcanoPlot <- renderPlot({
    ggplot(data = PtcG4_D6_V_Fer12OG_D6, aes(x = log2FoldChange, y = -log10(padj))) + 
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