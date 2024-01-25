library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(reshape2)
library(haven)
library(shinydashboard)

# for melt function

# UI Definition
ui <- dashboardPage(
  dashboardHeader(title = "Comprehensive Analysis - Zambia Height Data"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Analysis", tabName = "dataAnalysis", icon = icon("chart-line"),
               menuSubItem("Data Overview", tabName = "dataOverview"),
               menuSubItem("Histogram of zscore", tabName = "zscoreHist"),
               menuSubItem("Correlation Matrix", tabName = "correlationMatrix"),
               menuSubItem("Bar Plot of c_gender", tabName = "cGenderBarPlot"),
               menuSubItem("Histograms of zscore by Gender", tabName = "zscoreGenderHist"),
               menuSubItem("Histogram of c_breastf", tabName = "cBreastfHist"),
               menuSubItem("Marginal Plot", tabName = "zscoreBreastfMarginalPlot"),
               menuSubItem("Correlation Test", tabName = "corTestZscoreBreastf")
               # Add more sub-menu items for additional data analysis parts
      ),
      menuItem("Predictive Model Building", tabName = "modelBuilding", icon = icon("cogs"),
               # Add sub-menu items for model building
               # Add sub-menu items here. Example:
               menuSubItem("Sub-item 1", tabName = "subItem1"),
               menuSubItem("Sub-item 2", tabName = "subItem2")
               # Add more sub-menu items as needed
      )
      # Additional main menu items can be added here
    )
  ),
  dashboardBody(
    tabItems(
      # Data Analysis Tabs
      tabItem(tabName = "dataOverview", DT::dataTableOutput("dataOverview")),
      tabItem(tabName = "zscoreHist", plotlyOutput("zscoreHist")),
      tabItem(tabName = "correlationMatrix", plotlyOutput("correlationMatrix")),
      tabItem(tabName = "cGenderBarPlot", plotlyOutput("cGenderBarPlot")),
      tabItem(tabName = "zscoreGenderHist", plotlyOutput("zscoreGenderHist")),
      tabItem(tabName = "cBreastfHist", plotlyOutput("cBreastfHist")),
      tabItem(tabName = "zscoreBreastfMarginalPlot", plotlyOutput("zscoreBreastfMarginalPlot")),
      tabItem(tabName = "corTestZscoreBreastf", verbatimTextOutput("corTestZscoreBreastf")),
      # Predictive Model Building Tabs
      # Add tabItems for model building
      tabItem(tabName = "subItem1", verbatimTextOutput("subItem1")),
      tabItem(tabName = "subItem2", verbatimTextOutput("subItem2"))
      
      
      # More tabItems can be added for additional sections
    )
  )
)


# Server Logic
server <- function(input, output) {
  # Load and prepare the data
  data <- read_dta("data/zambia_height92.dta")
  set.seed(123)
  train_indices <- sample(1:nrow(data), 0.85 * nrow(data))
  train_data <- data[train_indices, ]
  
  # Data Overview
  output$dataOverview <- DT::renderDataTable({
    DT::datatable(data, options = list(pageLength = 5))
  })
  
  # Interactive Histogram of zscore
  output$zscoreHist <- renderPlotly({
    p <- ggplot(train_data, aes(x = zscore)) +
      geom_histogram(binwidth = 1, fill = "blue", alpha = 0.7) +
      labs(title = "Histogram of zscore", x = "zscore", y = "Count")
    ggplotly(p)
  })
  
  # Interactive Correlation Matrix
  output$correlationMatrix <- renderPlotly({
    cor_matrix <- cor(train_data[, c("zscore", "c_breastf", "c_age")]) # Add your variables here
    p <- ggplot(melt(cor_matrix), aes(Var1, Var2, fill = value)) +
      geom_tile() +
      scale_fill_gradient2(limits = c(-1, 1), mid = "white", high = "blue", low = "red") +
      theme_minimal() +
      coord_fixed()
    ggplotly(p)
  })
  
  # Bar Plot of c_gender
  output$cGenderBarPlot <- renderPlotly({
    p <- ggplot(train_data, aes(x = c_gender)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Bar Plot of Gender", x = "Gender", y = "Count")
    ggplotly(p)
  })
  
  # Histograms of zscore by Gender
  output$zscoreGenderHist <- renderPlotly({
    # Implement histograms of zscore by gender
  })
  
  # Histogram of c_breastf
  output$cBreastfHist <- renderPlotly({
    # Implement histogram of c_breastf
  })
  
  # Marginal Plot of zscore and c_breastf
  output$zscoreBreastfMarginalPlot <- renderPlotly({
    # Implement marginal plot of zscore and c_breastf
  })
  
  # Correlation Test Between zscore and c_breastf
  output$corTestZscoreBreastf <- renderPrint({
    cor.test(train_data$zscore, train_data$c_breastf)
  })
  
  # Predictive Model Building - Placeholder for Implementation
  output$modelBuilding <- renderPrint({
    # Placeholder for Predictive Model Building logic
    # Actual implementation will go here
  })
  
  # Placeholder for Predictive Model Building
  # Implement server logic for model building if needed
  # Example:
  # output$modelOutput <- renderPrint({
  #   # Model building logic
  # })
}

# Run the application
shinyApp(ui = ui, server = server)

