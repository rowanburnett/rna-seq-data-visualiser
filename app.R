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
source("./module/file-manager.R", local = TRUE)
source("./module/gene-enrichment-analysis.R", local = TRUE)
source("./function/generate-ui.R")

ui <- fluidPage(
  dashboardPage(
    dashboardHeader(
      title = "RNA-seq data visualisation",
      titleWidth = "280px"
      ),
    dashboardSidebar(
      width = "280px",
      sidebarMenu(
        menuItem("Volcano plot", tabName = "volcanoPlot"),
        menuItem("Heat map", tabName = "heatMap"),
        menuItem("Gene set enrichment analysis (GSEA)", tabName = "geneEnrichment"),
        fileManagerUI("fileManager")
      )
    ),
    dashboardBody(
      tabItems(
        tabItem(tabName = "volcanoPlot",
                volcanoPlotUI("volcanoPlot")),
        tabItem(tabName = "heatMap",
                heatMapUI("heatMap")),
        tabItem(tabName = "geneEnrichment",
                geneEnrichmentUI("geneEnrichment"))
      )
    )
  )
)

server <- function(input, output, session) {
  options(shiny.reactlog=TRUE)
  files <- fileManagerServer("fileManager")
  volcano <- volcanoPlotServer("volcanoPlot", dataset = files)
  heatMap <- heatMapServer("heatMap", dataset = files)
  geneEnrichment <- geneEnrichmentServer("geneEnrichment", dataset = files)
}

shinyApp(ui = ui, server = server)
