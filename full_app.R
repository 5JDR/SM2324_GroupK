library(shiny)
library(leaflet)
library(dplyr)
library(sf)
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
               menuSubItem("Correlation Test", tabName = "corTestZscoreBreastf"),
               menuSubItem("Regional Analysis", tabName = "regionalAnalysis")
      ),
      menuItem("Predictive Model Building", tabName = "modelBuilding", icon = icon("cogs"),
               menuSubItem("Model Overview", tabName = "modelOverview"),
               menuSubItem("Model Performance", tabName = "modelPerformance"),
               menuSubItem("Model Comparison", tabName = "modelComparison")
      )
  )),
  dashboardBody(
    tabItems(
      # Data Analysis Tabs
      tabItem(tabName = "dataOverview", 
               HTML("<p>This dataset provides a comprehensive overview of 
               malnutrition among children aged 0-5 in Zambia, based on a survey
               conducted in 1992. It focuses on the nutritional condition measured 
               by the Z-score, which compares a child's anthropometric status to 
               a reference population of white US-American children from wealthy
               families with high socioeconomic status. Key variables in the dataset 
               include:</p>
             <ul>
               <li><b>Z-score:</b> The child's nutritional status measurement.</li>
               <li><b>Gender:</b> Coded as 1 for male and 0 for female.</li>
               <li><b>Duration of Breastfeeding:</b> Recorded in months.</li>
               <li><b>Child's Age:</b> Noted in months.</li>
               <li><b>Mother's Age at Birth:</b> Indicated in years.</li>
               <li><b>Mother's Height:</b> Measured in centimeters.</li>
               <li><b>Mother's BMI:</b> Body Mass Index of the mother.</li>
               <li><b>Mother's Education Level:</b> Ranges from no education to
               higher education.</li>
               <li><b>Mother's Work Status:</b> Indicates whether the mother is
               working (1) or not (0).</li>
               <li><b>Region of Residence:</b> Including Central, Copperbelt,
               Eastern, Luapula, Lusaka, Northern, North Western, Southern, and
               Western regions of Zambia.</li>
               <li><b>District of Residence:</b> Covering 55 districts in Zambia.</li>
             </ul>
             <p>This dataset serves as a tool for understanding the scale
                    and specifics of child malnutrition in Zambia, providing 
                    valuable insights.</p>")),
      tabItem(tabName = "zscoreHist", plotlyOutput("zscoreHist"),
              HTML("<p style='font-size:16px; color: #333333;'><b>Z-Score 
                   Distribution of Children in Zambia:</b> <span style='color:
                   #2E86C1;'>This histogram vividly illustrates the Z-score
                   distribution</span> for children aged 0-5, based on a 1992
                   survey. Observing the <span style='color: #28B463; font-weight:
                   bold;'>normal distribution curve</span>, it's striking to note
                   the <span style='background-color: #FADBD8;'><b>mean Z-score at 
                   -172.8</b></span>, a stark indicator of the nutritional gap when 
                   compared to the reference population of US-American children. 
                   This significant deviation paints a picture of the disparities
                   in child nutrition.</p>")),
      tabItem(tabName = "correlationMatrix", plotlyOutput("correlationMatrix")),
      tabItem(tabName = "cGenderBarPlot", plotlyOutput("cGenderBarPlot")),
      tabItem(tabName = "zscoreGenderHist", plotlyOutput("zscoreGenderHist")),
      tabItem(tabName = "cBreastfHist", plotlyOutput("cBreastfHist")),
      tabItem(tabName = "zscoreBreastfMarginalPlot", plotlyOutput("zscoreBreastfMarginalPlot")),
      tabItem(tabName = "corTestZscoreBreastf", verbatimTextOutput("corTestZscoreBreastf")),
      tabItem(tabName= "regionalAnalysis", leafletOutput("regionalAnalysis")),
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
  library(haven)
  data <- read_dta("data/zambia_height92.dta")
  #split data in train and test set
  set.seed(123)
  train <- sample(1:nrow(data), 0.85*nrow(data))
  
  test <- setdiff(1:nrow(data), train)
  
  train_data <- data[train,]
  test_data <- data[test,]
  
  
  # Data Overview
  output$dataOverview <- DT::renderDataTable({
    DT::datatable(data, options = list(pageLength = 5))
  })
  
  # Interactive Histogram of zscore
  output$zscoreHist <- renderPlotly({
    p <- ggplot(train_data, aes(x = zscore)) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd ),geom = "polygon", color = "red",fill = "red",alpha = 0.3, size = 1, text = paste("Normal Distribution"))+
      #histogram of density instead of count
      geom_histogram(aes(y = ..density..),binwidth = 30, fill = "blue", alpha = 0.7) +
      labs(title = "Histogram of zscore", x = "zscore", y = "Density")
    #add normal curve
    mean <- mean(train_data$zscore)
    sd <- sd(train_data$zscore)
    #add a vertical line for the mean 
    p <- p + geom_vline(aes(xintercept = mean), color = "green", linetype = "dashed",alpha = 0.7, size = 1)
    
    ggplotly(p)
  })
  
  # Interactive Correlation Matrix
  output$correlationMatrix <- renderPlotly({
    cor_matrix <- cor(train_data[, c("zscore", "c_breastf", "c_age", "m_agebirth", "m_height", "m_bmi")]) # Add your variables here
    melted_cor_matrix <- melt(cor_matrix)
    
    p <- ggplot(melted_cor_matrix, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white") +  # Adds a border around each tile
      scale_fill_gradient2(limits = c(-1, 1), mid = "white", high = "blue", low = "red", midpoint = 0) +
      geom_text(aes(label = sprintf("%.2f", value)), vjust = 1) +  # Adds correlation values to the tiles
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),  # Improves readability of x-axis labels
            axis.title = element_blank(),  # Removes axis titles
            panel.grid.major = element_blank(),  # Removes major grid lines
            panel.grid.minor = element_blank(),  # Removes minor grid lines
            panel.background = element_blank(),  # Removes panel background
            plot.background = element_blank()) +  # Removes plot background
      coord_fixed()
    
    ggplotly(p, tooltip = "text")  # Enhances interactivity with tooltips
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
  
  
    # Implement regional analysis
    # Prepare map data
    zambia_map <- st_read("data/columbia_fewsn_1996_zambiaadmn2.shp")
    zambia_map$region <- c("Northern", "Copperbelt", "Luapula", "Eastern", "Central", "North Western", "Western", "Southern", "Lusaka")
    zambia_map$zscore <- data %>% 
      
      mutate(region = case_when(
        region == 6 ~ "Northern",
        region == 2 ~ "Copperbelt",
        region == 4 ~ "Luapula",
        region == 3 ~ "Eastern",
        region == 1 ~ "Central",
        region == 7 ~ "North Western",
        region == 9 ~ "Western",
        region == 8 ~ "Southern",
        region == 5 ~ "Lusaka"
      )) %>% 
      group_by(region) %>% 
      summarise(zscore = round(mean(zscore),2)) %>% 
      pull(zscore)
    
    # Add color gradient based on zscore, with a scale of grey
    zambia_map$color <- colorRampPalette(c("aquamarine","aquamarine4"),alpha = TRUE)(length(zambia_map$zscore))[rank(zambia_map$zscore)]
    output$regionalAnalysis <- renderLeaflet({
      # Create basic map with region boundaries
      leaflet(zambia_map) %>%
        addPolygons(
          color = ~zambia_map$color,
          fillOpacity = 1,
          #popup of region name and zscore
          popup = ~paste0("Region: ", zambia_map$region, "<br>",
                          "Z-score: ", zambia_map$zscore)
          #add regions name to the map
        ) 
        
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

