library(shiny)
library(shinydashboard)
library(readxl)
library(tidyverse)
library(httr)
library(jsonlite)
library(clusterProfiler)
library(AnnotationDbi)
library(org.Dm.eg.db)
library(EnhancedVolcano)
library(glue)
library(ComplexHeatmap)

tissue_names <- list("Salivary gland", "Wing disc", "Brain")

tissue_values <- list("./data/DESEQ2 Results/Salivary Gland/deseq2_salivary_gland.xlsx", 
                      "./data/DESEQ2 Results/Wing Disc/deseq2_wing_disc.xlsx", 
                      "./data/DESEQ2 Results/Brain/deseq2_brain.xlsx")

genotype_names <- list("RasYki (D5)","RasYki (D8)","Feritin (D6)",
                  "Feritin (D8)", "Feritin WT looking (D6)",
                  "ImpL2 RNAi (D6)", "ImpL2 RNAi (D8)")

genotype_values <- list("RasYki_D5","RasYki_D8","Fer12OG_D6",
                    "Fer12OG_D8","Fer12WT_D6","ImpL2i_D6",
                    "ImpL2i_D8") 

source("./module/volcano-plot.R", local = TRUE)
source("./module/heat-map.R", local = TRUE)

ui <- fluidPage(
    volcanoPlotUI("volcanoPlot", "Volcano plot"),
    heatMapUI("heatMap", "Heat map")
  )

server <- function(input, output, session) {
  volcano <- volcanoPlotServer("volcanoPlot")
  heatmap <- heatMapServer("heatMap")
}


shinyApp(ui = ui, server = server)
