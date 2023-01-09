library(shiny)
library(readxl)
library(tidyverse)
library(httr)
library(jsonlite)

# deseq2 file paths
deseq2_salivary_gland <- "./data/DESEQ2 Results/Salivary Gland/deseq2_salivary_gland.xlsx"
deseq2_brain <- "./data/DESEQ2 Results/Brain/deseq2_brain.xlsx"
deseq2_wing_disc <- "./data/DESEQ2 Results/Wing Disc/deseq2_wing_disc.xlsx"

source("./module/volcano-plot.R", local = TRUE)

ui <- fluidPage(
  navbarPage("RNA-seq data",
      volcanoPlotUI("volcanoPlot", "Volcano plot")
    )
  )

server <- function(input, output, session) {
  volcano <- volcanoPlotServer("volcanoPlot")
}


shinyApp(ui = ui, server = server)
