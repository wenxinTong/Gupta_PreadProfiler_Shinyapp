library(qs)
library(shiny)
library(tidyverse)
library(cowplot)
library(dplyr)
library(ggpubr)
library(shinyjs)
#load functions
source('helpers.R')

#load data
multiomics_data <- qs::qread("data/multiomics_data.qs")
cold_data <- qs::qread("data/cold_response_data.qs")
# Define UI for application that draws a plot
ui <- fluidPage(
  tabsetPanel(
    tabPanel(
      titlePanel("Adipocyte Precursor Profile"),
      sidebarLayout(
        sidebarPanel(
          textInput("name", "Gene Name"),
          actionButton("search", "Search"),
          actionButton("clear", "Clear"),
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("multiomics_plot",width = "100%", height = "800px"),
        )
      )
    ),
    tabPanel(
      titlePanel("Cold Exposure Profile"),
      sidebarLayout(
        sidebarPanel(
          textInput("gene", "Gene Name"),
          actionButton("start", "Search"),
          actionButton("reset", "Clear"),
        ),
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("cold_plot",width = "80%", height = "500px"),
        )
      )
    ),
    tabPanel("About",
             titlePanel("Multilayer-omic Analysis of Adipose Tissue Progenitor Cell Heterogeneity"),
             br(),
             fluidRow(tags$img(src="Murine WAT description.png",width="1400px",height="1000px"),align="center"),
             tags$h4(strong("Reference"),tags$div(
               br(),
             p("  Multilayered omics reveal sex- and depot-dependent adipose progenitor cell heterogeneity:",em("Cell Metab. 2022 May 3;34(5):783-799.e7."),style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                      p("  Cold-responsive adipocyte progenitors couple adrenergic signaling to immune cell activation to promote beige adipocyte accrual:",em("Genes Dev. 2021 Oct 1;35(19-20):1333-1338."),style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
             ),
             br(),
             tags$h4("The room-temperature transcriptomic and proteomic profile is developed by the Wu Lab and Gupta Lab that provides the expression information of murine PDGFRβ+ adipose tissue progenitors at three levels:"),tags$div()),
             tags$h5(
               em("  - Cell subpopulation (four functionally distinct subpopulations)"), 
               br(),
               em("  - localization (intraabdominal and subcutaneous adipose depots)"), 
               br(),
               em("  - Sex (male and female mice)"),
               br(),
               br(),
               tags$div(
               p("  In this Room-temperature landscape dataset we have identified:"),
              p("   - 15,477 transcripts quantified by RNA-seq"),
              p("   - 4,870 proteins quantified by DIA Mass Spectrometry"),
             p("   - 4,540 genes represented at both the protein and mRNA level")
             ),
             br()),
             tags$h4("The cold-response transcriptomic dataset is developed by the Gupta Lab that provides the transcriptomic expression information of murine PDGFRβ+ progenitors in male inguinal white adipose tissue"),
             tags$div(
               p("  In this Cold-response dataset we have identified:"),
               p("- 54,358 transcripts quantified by RNA-seq")
             ),
            
             br(),
             tags$h4("Acknowledgement:"),
             tags$div(
               "We thank Matthew Hirschey and Wenxin Tong for developing and maintaining this website."
             ),
             br(),
             tags$h4("Contact the Gupta lab: "),
             tags$div(
               "Rana K. Gupta",br(),em("rana.gupta@duke.edu"),
                                       br(),
               tags$a(href="https://dmpi.duke.edu/rana-gupta-phd-0", 
                      "Gupta lab link")
               ),

             tags$h4("Contact the Wu lab: "),
             tags$div(
               "Yibo Wu",br(),em("yibo.wu@unige.ch"),
               br(),
               tags$a(href="https://www.ims.riken.jp/labo/59/cv.html", 
                      "Wu lab link")
             )
             
    )))




# Define server logic required to draw a boxplot
server <- function(input, output) {
  observeEvent(input$search, {
    gene_string <- unlist(stringr::str_split(input$name, pattern = ", "))
    output$multiomics_plot<-renderPlot({
       multiomics_boxplot(dataframe = multiomics_data,
                 fav_gene = gene_string)
  })
  })
  
  observeEvent(input$clear, {
    output$multiomics_plot<-NULL
  })
  
  observeEvent(input$start, {
    gene_name <- unlist(stringr::str_split(input$gene, pattern = ", "))
    output$cold_plot<-renderPlot({
      cold_boxplot(dataframe = cold_data,
                         fav_gene = gene_name)
    })
  })
  
  observeEvent(input$reset, {
    output$cold_plot<-NULL
  })
  }

# Run the application
shinyApp(ui = ui, server = server)