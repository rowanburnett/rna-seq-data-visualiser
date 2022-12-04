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
                                        "Wing disc" = deseq2_wing_disc)),
      checkboxGroupInput("day",
                         label = "Day",
                         choices = list("D5" = 1,
                                        "D6" = 2,
                                        "D8" = 3))
    ),
    
    mainPanel(
      h1("Volcano Plot"),
      plotOutput(
        outputId = "volcanoPlot",
        click = clickOpts(id = "plot_click")
        ),
      textOutput("coordinates"),
      tableOutput("geneTable")
      
    ),
  )
  
)

server <- function(input, output) {
  
  get_data <- function(path, sheet) {
    data <- read_excel(path, sheet, na = "NA") %>%
      select(-baseMean, -lfcSE, -stat, -pvalue) %>%
      replace(is.na(.), 0)
    return(data)
  }
                        
  output$volcanoPlot <- renderPlot({
    req(input$deseq2, input$day)
    deseq2_data <- list()
    deseq2_names <- list()
    for (tissue in input$deseq2) {
      for (day in input$day) {
        tryCatch({
          day <- as.numeric(day)
          name <- excel_sheets(tissue)[day]
          deseq2_names <- append(deseq2_names, name)
          deseq2_data <- append(deseq2_data, lapply(tissue, get_data, sheet = day))
          
        },
        error = function(cond) {
          print(cond)
        })
      }
    }
    names(deseq2_data) <- c(deseq2_names)
    deseq2_data <- bind_rows(deseq2_data, .id = "Tissue")
    
    ggplot(data = deseq2_data, aes(x = log2FoldChange, y = -log10(padj), colour = Tissue)) +
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