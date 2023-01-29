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
library(ComplexHeatmap)
library(circlize)
library(openxlsx)
library(gprofiler2)
library(enrichplot)
library(DOSE)

source("./module/volcano-plot.R", local = TRUE)
source("./module/heat-map.R", local = TRUE)
source("./module/upload.R", local = TRUE)
source("./module/gene-enrichment-analysis.R", local = TRUE)

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(
      title = "RNA-seq data visualisation",
      titleWidth = "300px"
      ),
    dashboardSidebar(
      width = "300px",
      sidebarMenu(
        menuItem("Volcano plot", tabName = "volcanoPlot"),
        menuItem("Heat map", tabName = "heatMap"),
        menuItem("Gene enrichment analysis", tabName = "geneEnrichment"),
        uploadUI("upload")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "volcanoPlot",
                volcanoPlotUI("volcanoPlot")),
        tabItem(tabName = "heatMap",
                heatMapUI("heatMap")),
        tabItem(tabName = "geneEnrichment",
                geneEnrichmentUI("geneEnrichment")),
      )
    )
  )
)

server <- function(input, output, session) {
  upload <- uploadServer("upload")
  volcano <- volcanoPlotServer("volcanoPlot", dataset = upload)
  heatMap <- heatMapServer("heatMap", dataset = upload)
  geneEnrichment <- geneEnrichmentServer("geneEnrichment", dataset = upload)
}


shinyApp(ui = ui, server = server)
