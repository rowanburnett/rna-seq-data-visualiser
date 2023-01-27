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
library(circlize)
library(openxlsx)

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
source("./module/upload.R", local = TRUE)

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(title = "Data visualisation"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Volcano plot", tabName = "volcanoPlot"),
        menuItem("Heat map", tabName = "heatMap"),
        uploadUI("upload")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "volcanoPlot",
                volcanoPlotUI("volcanoPlot")),
        tabItem(tabName = "heatMap",
                heatMapUI("heatMap"))
      )
    )
  )
)

server <- function(input, output, session) {
  upload <- uploadServer("upload")
  volcano <- volcanoPlotServer("volcanoPlot", dataset = upload)
  heatMap <- heatMapServer("heatMap", dataset = upload)
  
  # increase max request size to upload files up to 100mb
  options(shiny.maxRequestSize=100*1024^2)
  
}


shinyApp(ui = ui, server = server)
