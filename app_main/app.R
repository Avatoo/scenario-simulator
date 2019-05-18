#-----------------------------
# Web app for clustering, scenario simulation and data view
#-----------------------------

library(shiny)
library(shinydashboard)
library(shinyWidgets)

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(gridExtra)
library(lubridate)

source('../utilities.R')

theme_set(theme_bw())

ui <- dashboardPage(
    skin = 'black',
    dashboardHeader(
        title = 'Senario Simulator'
    ),
    dashboardSidebar(
        width = '300px',
        sidebarMenu(
            menuItem("Cluster", tabName = "cluster", icon = icon("dashboard")),
            menuItem('Senario', tabName = "senario", icon = icon("bar-chart-o")),
            menuItem('View', tabName = "view",icon = icon("table"))
        )
    ),
    dashboardBody(
        
        tabItems(
            tabItem(tabName = "cluster", h2("Perform Clustering"),
                    box(title = 'Clustering Specification',
                        status = 'info',
                        width = 3,
                        sliderInput('k', 'Number of Clusters', min = 1, max = 20, value = 10)),
                    box(title = 'Cluster',
                        status = 'info',
                        width = 9,
                        plotOutput("plt"))),
            tabItem(tabName = "senario", h2("Senario Simulator"),
                    fluidRow(
                      column(width = 4,  
                             box(title = 'Senario Specification #1',
                                 status = 'info',
                                 width = 12,
                                 selectInput('this_asset', 'Asset Filter', 
                                             selected = 'PROJECT-ASSET-210',
                                             choices = unique(dat$Asset_ID), multiple = T),
                                 #selectInput('standard_var', 'Standardization Variable Filter', 
                                 #            choices = c('All', names(dat))),
                                 #textInput('standard_method', 'Standardization Method', ''),
                                 actionButton('se1', 'Go senario 1 (Baseline)')
                             )),
                      column(width = 4,  
                             box(title = 'Senario Specification #2',
                                 status = 'info',
                                 width = 12,
                                 selectInput('this_asset_2', 'Asset Filter', 
                                             selected = 'PROJECT-ASSET-210',
                                             choices = unique(dat$Asset_ID), multiple = T),
                                 selectInput('standard_var_2', 'Standardization Variable Filter', 
                                             selected = 'Depth',
                                             choices = c('All', names(dat))),
                                 textInput('standard_method_2', 'Standardization Method', 'max'),
                                 actionButton('se2', 'Go senario 2')
                             )),
                      column(width = 4,  
                             box(title = 'Senario Specification #3',
                                 status = 'info',
                                 width = 12,
                                 selectInput('this_asset_3', 'Asset Filter', 
                                             selected = 'PROJECT-ASSET-210',
                                             choices = unique(dat$Asset_ID), multiple = T),
                                 selectInput('standard_var_3', 'Standardization Variable Filter', 
                                             selected = 'Price',
                                             choices = c('All', names(dat))),
                                 textInput('standard_method_3', 'Standardization Method', 'min'),
                                 actionButton('se3', 'Go senario 3')
                             ))
                    ),
                    fluidRow(
                        column(width = 4,
                               valueBox(
                                   width = 12,
                                   uiOutput("cost_se1"),
                                   subtitle = tags$p("Cost of Senario #1", style = "font-size: 150%;"),
                                   icon = icon("users"), color = "light-blue")),
                        column(width = 4,
                               valueBox(
                                   width = 12,
                                   uiOutput("cost_se2"),
                                   subtitle = tags$p("Cost of Senario #2", style = "font-size: 150%;"),
                                   icon = icon("users"), color = "light-blue")),
                        column(width = 4,
                               valueBox(
                                   width = 12,
                                   uiOutput("cost_se3"),
                                   subtitle = tags$p("Cost of Senario #3", style = "font-size: 150%;"),
                                   icon = icon("users"), color = "light-blue"))
                    )
                    ),
            tabItem(tabName = "view", h2("View Asset"),
                    box(title = 'View',
                        status = 'info',
                        width = 12,
                        DT::DTOutput("tbl1")))
            )
    )
)


server <- function(input, output, session) {
    
    
    # data <- reactive({
    #   dat
    # })
    
    rvs <- reactiveValues()
    
    # observeEvent(data(), {
    #   rvs$a = data()
    # })
    
    observe({
        rvs$dat = dat
    })
    
    cluser <- reactive({
      cluster_mm(dat, k = input$k)
    })
    
    observeEvent(input$k, {
      rvs$a = cluser()$data
    })
    
    output$plt <- renderPlot({
      cluser()$p
    })
    
    observeEvent(input$se1, {
      rvs$a = run_senario_0(rvs$dat, p = cluser(), this_asset = input$this_asset)
    })
    
    observeEvent(input$se2, {
      rvs$b =  run_senario_2(rvs$a, this_asset = input$this_asset_2,
                             standard_var = input$standard_var_2, standard_method = input$standard_method_2)
    })
    
    observeEvent(input$se3, {
      rvs$c =  run_senario_2(rvs$a, this_asset = input$this_asset_3,
                             standard_var = input$standard_var_3, standard_method = input$standard_method_3)
    })
    
    
    output$tbl1 <- DT::renderDT({
        
      rvs$a #%>% 
        #select(ID, everything())
    }, server = T)
    
    output$cost_se1 <- renderText({
        prettyNum(as.numeric(sum(rvs$a$Price)), big.mark=",")
    })
     
    output$cost_se2 <- renderText({
        prettyNum(as.numeric(sum(rvs$b$Price)), big.mark=",")
    })

    output$cost_se3 <- renderText({
        prettyNum(as.numeric(sum(rvs$c$Price)), big.mark=",")
    })
}

shinyApp(ui, server)
