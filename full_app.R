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
library(splines)
library(mgcv)

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
               menuSubItem("Gender Analysis", tabName = "genderAnalysis"),
               
               #menuSubItem("Breast-feedings", tabName = "Breast-feedings"),
               menuSubItem("Breast-feedings", tabName = "zscoreBreastfMarginalPlot"),
               menuSubItem("Age Analysis", tabName = "zscoreAgeMarginalPlot"),
               menuSubItem("Mother Height Analysis", tabName = "mheightMarginalPlot"),
               menuSubItem("Mother BMI Analysis", tabName = "mbmiMarginalPlot"),
               menuSubItem("Mother Work Analysis", tabName = "mworkAnalysis"),
               
               menuSubItem("Regional Analysis", tabName = "regionalAnalysis")
      ),
      menuItem("Predictive Model Building", tabName = "modelBuilding", icon = icon("cogs"),
               menuSubItem("Linear and Gam Models", tabName = "linearmodel"),
               menuSubItem("Other Models", tabName = "othermodels"),
               menuSubItem('Conclusion', tabName = 'conclusion')
               
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
      tabItem(tabName = "zscoreHist", plotlyOutput("zscoreHist",width = "100%", height = "800px"),
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
      tabItem(tabName = "correlationMatrix", plotlyOutput("correlationMatrix",height = "800px")),
      tabItem(tabName = "genderAnalysis",
              radioButtons("genderSelect", "Select Gender:",
                           choices = list("Both Genders" = "both",
                                          "Female" = "0",
                                          "Male" = "1"),
                           selected = "both"),
              plotlyOutput("genderAnalysis", width = "100%", height = "800px"),
              HTML("<p style='font-size:16px; color: #333333;'><b>Z-Score 
                   Distribution of Children in Zambia, divided by gender:</b>
                   There isn't a substantial difference between the two sub-populations,
                   the zscore distribution seems quite balanced for males and females.</p>")),
      #tabItem(tabName = "Breast-feedings", plotlyOutput("cBreastfHist",height = "800px")),
      
      tabItem(tabName = "zscoreBreastfMarginalPlot",
              checkboxInput("hideZeroBreastf", "Hide Breast-feedings <= 2", FALSE),
              fluidRow(
                column(8, plotlyOutput("hist_c_breastf"))
              ),
              fluidRow(
                column(8, plotlyOutput("scatterPlot")),
                column(4, plotlyOutput("hist_zscore",width = '250px'))
              ) 
      ),
      
      tabItem(tabName = "zscoreAgeMarginalPlot",
              
              fluidRow(
                column(8, plotlyOutput("hist_c_age"))
              ),
              fluidRow(
                column(8, plotlyOutput("scatterPlot_2")),
                column(4, plotlyOutput("hist_zscore_2",width = '250px'))
              ) 
      ),
      tabItem(tabName = "mheightMarginalPlot",
              
              fluidRow(
                column(8, plotlyOutput("hist_m_height"))
              ),
              fluidRow(
                column(8, plotlyOutput("scatterPlot_3")),
                column(4, plotlyOutput("hist_zscore_3",width = '250px'))
              ) 
      ),
      
      tabItem(tabName= "mbmiMarginalPlot",
              fluidRow(
                column(8, plotlyOutput("hist_m_bmi"))
              ),
              fluidRow(
                column(8, plotlyOutput("scatterPlot_4")),
                column(4, plotlyOutput("hist_zscore_4",width = '250px'))
              ) 
      ),
      
      tabItem(tabName = "mworkAnalysis",
              radioButtons("working_select", "Select:",
                           choices = list("Both" = "both",
                                          "Not working" = "0",
                                          "Working" = "1"),
                           selected = "both"),
              plotlyOutput("mworkAnalysis", width = "100%", height = "800px"),
      ),
      
      
      
      tabItem(tabName= "regionalAnalysis", leafletOutput("regionalAnalysis",height = "800px")),
      # Predictive Model Building Tabs
      # Add tabItems for model building
      tabItem(tabName = "linearmodel",
              selectInput("modelStep", "Select Model Building Step:",
                          choices = list("Step 1: Initial Model" = "step1",
                                         "Step 2: High-correlation variables" = "step2",
                                         "Step 3: Saturated Model" = "step3",
                                         "Step 4: Final Linear Model" = "step4",
                                         "Step 7: Best splines model" = "step7",
                                         "Step 8: Most sig. GAM model" = "step8"),),
              
              
              uiOutput("linearModelOutput")
              
      )
      
      
      
      
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
  
  #cast of zscore to integer
  train_data$zscore <- as.integer(train_data$zscore)
  test_data$zscore <- as.integer(test_data$zscore)
  
  #factorize gender
  train_data$c_gender <- as.factor(train_data$c_gender)
  test_data$c_gender <- as.factor(test_data$c_gender)
  
  #cast c_breastf as integer
  train_data$c_breastf <- as.integer(train_data$c_breastf)
  test_data$c_breastf <- as.integer(test_data$c_breastf)
  #cast c_age as integer
  train_data$c_age <- as.integer(train_data$c_age)
  test_data$c_age <- as.integer(test_data$c_age)
  
  #NOTE: to have the month of birth in m_agebirth calculate: decimal_part * 12
  
  #factorize m_education
  train_data$m_education <- as.factor(train_data$m_education)
  test_data$m_education <- as.factor(test_data$m_education)
  
  #factorize m_work
  train_data$m_work <- as.factor(train_data$m_work)
  test_data$m_work <- as.factor(test_data$m_work)
  
  #factorize region
  train_data$region <- as.factor(train_data$region)
  test_data$region <- as.factor(test_data$region)
  
  #factorize district
  train_data$district <- as.factor(train_data$district)
  test_data$district <- as.factor(test_data$district)
  
  #for splines we need to convert the categorical variables to factors
  #train_data_cat <- train_data  # Make sure to copy the original data
  #train_data_cat$c_breastf <- as.factor(train_data$c_breastf)  # Convert to factor if it's categorical
  
  #test_data_cat <- test_data  # Copy the original test data
  #test_data_cat$c_breastf <- as.factor(test_data$c_breastf)  # Convert to factor for test data as well
  
  train_data_cat <- train_data
  test_data_cat <- test_data
  train_data_cat$c_breastf <- ifelse(train_data$c_breastf == 0 | train_data$c_breastf == 1 | train_data$c_breastf == 2, 0, 1)
  test_data_cat$c_breastf <- ifelse(test_data$c_breastf == 0 | test_data$c_breastf == 1 | test_data$c_breastf == 2, 0, 1)
  train_data_cat$c_breastf <- as.factor(train_data_cat$c_breastf)
  test_data_cat$c_breastf <- as.factor(test_data_cat$c_breastf)
  
  
 
  
  # Data Overview
  output$dataOverview <- DT::renderDataTable({
    DT::datatable(data, options = list(pageLength = 5))
  })
  
  # Interactive Histogram of zscore
  output$zscoreHist <- renderPlotly({
    
    mean <- mean(train_data$zscore)
    sd <- sd(train_data$zscore)
    
    p <- ggplot(train_data, aes(x = zscore)) +
      stat_function(fun = dnorm, args = list(mean = mean, sd = sd ),geom = "polygon", color = "red",fill = "red",alpha = 0.3, size = 1, text = paste("Normal Distribution"))+
      #histogram of density instead of count
      geom_histogram(aes(y = ..density..),binwidth = 30, fill = "blue", alpha = 0.7) +
      labs(title = "Histogram of zscore", x = "zscore", y = "Density")
    
    p <- p + geom_vline(aes(xintercept = mean), color = "green", linetype = "dashed",alpha = 0.7, size = 1)
    
    ggplotly(p)
  })
  
  # Interactive Correlation Matrix
  output$correlationMatrix <- renderPlotly({
    cor_matrix <- cor(train_data[, c("zscore", "c_breastf", "c_age", "m_agebirth", "m_height", "m_bmi")]) # Add your variables here
    cor_matrix[upper.tri(cor_matrix)] <- NA  # Set the upper triangle of the correlation matrix to NA
    
    melted_cor_matrix <- melt(cor_matrix, na.rm = TRUE)  # Melt the matrix, removing NA values
    
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
  
  output$genderAnalysis <- renderPlotly({
    # Filter data based on selected gender
    if (input$genderSelect == "0") {  # Female
      data_to_plot <- subset(train_data, c_gender == 0)
      bar_color <- "pink"  # Set color to pink for females
    } else if (input$genderSelect == "1") {  # Male
      data_to_plot <- subset(train_data, c_gender == 1)
      bar_color <- "blue"  # Choose a different color for males
    } else {  # Both genders
      data_to_plot <- train_data
      bar_color <- "grey"  # Neutral color for both genders
    }
    
    # Plot relative frequency histogram
    plot_ly(data_to_plot, x = ~zscore, type = 'histogram', histnorm = "probability",
            marker = list(
              color = bar_color,
              opacity = 0.6,
              line = list(color = 'black', width = 2)  # Add border with specified color and width
            )) %>%
      layout(title = 'Relative Frequency Histogram of zscore by Gender',
             xaxis = list(title = 'zscore'),
             yaxis = list(title = 'Relative Frequency'))
  })
  
  
  
  
  
  
  # Histograms of zscore by Gender
  output$zscoreGenderHist <- renderPlotly({
    # Implement histograms of zscore by c_gender = 0 and c_gender = 1
    p <- ggplot(train_data, aes(x = zscore, y = after_stat(count / sum(count)))) +
      geom_histogram(data = subset(train_data, c_gender == 0), aes(fill = "Female"), colour = 'black') +
      geom_histogram(data = subset(train_data, c_gender == 1), aes(fill = "Male"), colour = 'black', alpha = 0.5) +
      scale_fill_manual(name = "Gender", values=c("pink1", "steelblue1")) +
      labs(title = "Histograms of zscore by gender", x = "zscore", y = NULL)
    ggplotly(p)
  })
    
   
   filtered_data <- reactive({
    if (input$hideZeroBreastf) {
      subset(train_data, c_breastf > 2)
    } else {
      train_data
    }
  })
   #make filtered data a dataframe
   
  
   # Marginal Plot of zscore and c_breastf
   output$scatterPlot <- renderPlotly({
     # Use the filtered data for the scatter plot
     scatterPlot <- plot_ly(filtered_data(), x = ~c_breastf, y = ~zscore, type = 'scatter', mode = 'markers', showlegend = F) %>%
       layout(title = '',
              xaxis = list(title = 'Breast-feedings months', range = c(0, 48)),
              yaxis = list(title = 'zscore'),
              legend = list(c = 0.1, y = 0.9))
     fit <- lm(zscore ~ c_breastf, data = filtered_data())
     scatterPlot %>% 
       add_trace(x = filtered_data()$c_breastf, y = fitted(fit), mode = "lines", name = "Regression Line")
   })
   
   # Histogram for c_breastf
   output$hist_c_breastf <- renderPlotly({
     hist_c_breastf <- plot_ly(filtered_data(), x = ~c_breastf, type = 'histogram', histnorm = "probability",
                               marker = list(
                                 color = 'rgba(102,194,165,0.5)',
                                 line = list(color = 'black', width = 2)  # Adding border
                               )) %>%
       layout(showlegend = FALSE, 
              xaxis = list(title = "", range = c(0, 48)),
              yaxis = list(title = "Relative Frequency", range = c(0, 0.25)))
     
     hist_c_breastf
   })
   
   # Histogram for zscore
   output$hist_zscore <- renderPlotly({
     hist_zscore <- plot_ly(filtered_data(), y = ~zscore, type = 'histogram', histnorm = "probability",
                            marker = list(color = 'rgba(252,141,98,0.5)',line = list(color = 'black', width = 2))) %>%
       layout(showlegend = FALSE, 
              xaxis = list(title = "Relative Frequency", range = c(0, 0.075)),
              yaxis = list(title = ""))
     
     hist_zscore
   })
  
   # Marginal Plot of zscore and c_age
   output$scatterPlot_2 <- renderPlotly({
     # Use the filtered data for the scatter plot
     scatterPlot_2 <- plot_ly(train_data, x = ~c_age, y = ~zscore, type = 'scatter', mode = 'markers', showlegend = F) %>%
       layout(title = '',
              xaxis = list(title = 'age-months', range = c(0, 48)),
              yaxis = list(title = 'zscore'),
              legend = list(c = 0.1, y = 0.9))
     fit <- lm(zscore ~ c_age, data = train_data)
     scatterPlot_2 %>% 
       add_trace(x = train_data$c_age, y = fitted(fit), mode = "lines", name = "Regression Line")
   })
   
   # Histogram for c_age
   output$hist_c_age <- renderPlotly({
     hist_c_age <- plot_ly(train_data, x = ~c_age, type = 'histogram', histnorm = "probability",
                               marker = list(
                                 color = 'rgba(102,194,165,0.5)',
                                 line = list(color = 'black', width = 2)  # Adding border
                               )) %>%
       layout(showlegend = FALSE, 
              xaxis = list(title = ""),
              yaxis = list(title = "Relative Frequency", range = c(0, 0.25)))
     
     hist_c_age
   })
   
   # Histogram for zscore
   output$hist_zscore_2 <- renderPlotly({
     hist_zscore_2 <- plot_ly(train_data, y = ~zscore, type = 'histogram', histnorm = "probability",
                            marker = list(color = 'rgba(252,141,98,0.5)',line = list(color = 'black', width = 2))) %>%
       layout(showlegend = FALSE, 
              xaxis = list(title = "Relative Frequency"),
              yaxis = list(title = ""))
     
     hist_zscore_2
   })
   
   #######################################################################################################
   
   
   # Marginal Plot of zscore and m_height
   output$scatterPlot_3 <- renderPlotly({
     # Use the filtered data for the scatter plot
     scatterPlot_3 <- plot_ly(train_data, x = ~m_height, y = ~zscore, type = 'scatter', mode = 'markers', showlegend = F) %>%
       layout(title = '',
              xaxis = list(title = 'Mother height'),
              yaxis = list(title = 'zscore'),
              legend = list(c = 0.1, y = 0.9))
     fit <- lm(zscore ~ m_height, data = train_data)
     scatterPlot_3 %>% 
     add_trace(x = train_data$m_height, y = fitted(fit), mode = "lines", name = "Regression Line")
   })
   
   # Histogram for m_height
   output$hist_m_height <- renderPlotly({
     hist_m_height <- plot_ly(train_data, x = ~m_height, type = 'histogram', histnorm = "probability",
                               marker = list(
                                 color = 'rgba(102,194,165,0.5)',
                                 line = list(color = 'black', width = 2)  # Adding border
                               )) %>%
       layout(showlegend = FALSE, 
              xaxis = list(title = ""),
              yaxis = list(title = "Relative Frequency", range = c(0, 0.25)))
     
     hist_m_height
   })
   
   # Histogram for zscore
   output$hist_zscore_3 <- renderPlotly({
     hist_zscore_3 <- plot_ly(train_data, y = ~zscore, type = 'histogram', histnorm = "probability",
                            marker = list(color = 'rgba(252,141,98,0.5)',line = list(color = 'black', width = 2))) %>%
       layout(showlegend = FALSE, 
              xaxis = list(title = "Relative Frequency", range = c(0, 0.075)),
              yaxis = list(title = ""))
     
     hist_zscore_3
   })
   
  
  # Predictive Model Building - Placeholder for Implementation
  output$modelBuilding <- renderPrint({
    # Placeholder for Predictive Model Building logic
    # Actual implementation will go here
  })
  
  #########################################################################################
  
  # Marginal Plot of mother bmi and zscore
  output$scatterPlot_4 <- renderPlotly({
    # Use the filtered data for the scatter plot
    scatterPlot_4 <- plot_ly(train_data, x = ~m_bmi, y = ~zscore, type = 'scatter', mode = 'markers', showlegend = F) %>%
      layout(title = '',
             xaxis = list(title = 'Mother BMI'),
             yaxis = list(title = 'zscore'),
             legend = list(c = 0.1, y = 0.9))
    fit <- lm(zscore ~ m_bmi, data = train_data)
    scatterPlot_4 %>% 
      add_trace(x = train_data$m_bmi, y = fitted(fit), mode = "lines", name = "Regression Line")
  })
  
  # Histogram for m_bmi
  output$hist_m_bmi <- renderPlotly({
    hist_m_bmi <- plot_ly(train_data, x = ~m_bmi, type = 'histogram', histnorm = "probability",
                          marker = list(
                            color = 'rgba(102,194,165,0.5)',
                            line = list(color = 'black', width = 2)  # Adding border
                          )) %>%
      layout(showlegend = FALSE, 
             xaxis = list(title = ""),
             yaxis = list(title = "Relative Frequency", range = c(0, 0.25)))
    
    hist_m_bmi
  })
  
  # Histogram for zscore
  output$hist_zscore_4 <- renderPlotly({
    hist_zscore_4 <- plot_ly(train_data, y = ~zscore, type = 'histogram', histnorm = "probability",
                             marker = list(color = 'rgba(252,141,98,0.5)',line = list(color = 'black', width = 2))) %>%
      layout(showlegend = FALSE, 
             xaxis = list(title = "Relative Frequency", range = c(0, 0.075)),
             yaxis = list(title = ""))
    
    hist_zscore_4
  })
  
  ########################################################################################
  
  output$mworkAnalysis <- renderPlotly({
    # Filter data based on selected gender
    if (input$working_select == "0") {  # Not working
      data_to_plot <- subset(train_data, m_work == 0)
      bar_color <- "pink"  # Set color to pink for females
    } else if (input$working_select == "1") {  # Working
      data_to_plot <- subset(train_data, m_work == 1)
      bar_color <- "blue"  # Choose a different color for males
    } else {  # Both genders
      data_to_plot <- train_data
      bar_color <- "grey"  # Neutral color for both genders
    }
    
    # Plot relative frequency histogram
    plot_ly(data_to_plot, x = ~zscore, type = 'histogram', histnorm = "probability",
            marker = list(
              color = bar_color,
              opacity = 0.6,
              line = list(color = 'black', width = 2)  # Add border with specified color and width
            )) %>%
      layout(title = 'Relative Frequency Histogram of zscore by working status',
             xaxis = list(title = 'zscore'),
             yaxis = list(title = 'Relative Frequency'))
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
  pal<-colorNumeric(palette = "RdYlGn", domain = zambia_map$zscore, alpha = 0.7)
  output$regionalAnalysis <- renderLeaflet({
    # Create basic map with region boundaries
    leaflet(zambia_map) %>%
      addPolygons(
        color = "black", # Set border color to black
        fillColor = ~pal(zscore),
        fillOpacity = 1,
        weight = 1, # Adjust the border width if needed
        #popup of region name and zscore
        popup = ~paste0("Region: ", zambia_map$region, "<br>",
                        "Z-score: ", zambia_map$zscore)
      ) %>%
      # Add legend
      addLegend(
        position = "bottomright", # Can be "topright", "bottomright", "bottomleft", or "topleft".
        pal = colorNumeric(palette = "RdYlGn", domain = zambia_map$zscore),
        values = zambia_map$zscore,
        title = "Z-SCORE",
        opacity = 1
      )
  })
  
  
  # Placeholder for Predictive Model Building
  # Implement server logic for model building if needed
  # Example:
  # output$modelOutput <- renderPrint({
  #   # Model building logic
  # })
  # Define renderPrint for modelStep1 outside of any other reactive context
  output$modelStep1 <- renderPrint({
    # Fit the null model
    lm_null <- lm(zscore ~ 1, data = train_data)
    
    # Display the summary of the null model
    summary(lm_null)
  })
  
  # Calculate and display test error metrics
  output$modelTestError <- renderText({
    # Predict on test data
    lm_null <- lm(zscore ~ 1, data = train_data)
    predicted <- predict(lm_null, newdata = test_data)
    
    # Calculate errors
    MSE <- mean((predicted - test_data$zscore)^2)
    RMSE <- sqrt(MSE)
    MAE <- mean(abs(predicted - test_data$zscore))
    
    # Create a text output
    paste("Test Set Results - MSE: ", MSE, ", RMSE: ", RMSE, ", MAE: ", MAE)
  })
  
  output$qqPlotNullModel <- renderPlot({
    # Fit the null model
    lm_null <- lm(zscore ~ 1, data = train_data)
    
    # Generate Q-Q plot for the null model
    qqnorm(resid(lm_null))
    qqline(resid(lm_null), col = "red")
  })
  #######################################################
  
  output$modelStep2 <- renderPrint({
    # Fit the linear model with selected variables
    lm_all_correlated <- lm(zscore ~ c_breastf + c_age + m_height, data = train_data)
    
    # Display the summary of the model
    model_summary <- summary(lm_all_correlated)
    print(model_summary)
    
    # Predict on test data and calculate errors
    predicted <- predict(lm_all_correlated, newdata = test_data)
    MSE <- mean((predicted - test_data$zscore)^2)
    RMSE <- sqrt(MSE)
    MAE <- mean(abs(predicted - test_data$zscore))
    
    # Calculate AIC and BIC
    AIC_value <- AIC(lm_all_correlated)
    BIC_value <- BIC(lm_all_correlated)
    
    # Display test error metrics and AIC, BIC
    cat("\nTest Set Error Metrics:\n")
    cat("MSE: ", MSE, "\nRMSE: ", RMSE, "\nMAE: ", MAE, "\n")
    cat("\nModel Criteria:\n")
    cat("AIC: ", AIC_value, "\nBIC: ", BIC_value, "\n")
  })
  
  output$residualsPlotStep2 <- renderPlot({
    # Fit the model (ensure this is done outside of renderPlot if it's used elsewhere)
    lm_all_correlated <- lm(zscore ~ c_breastf + c_age + m_height, data = train_data)
    
    # Set up the plot area for a 2x2 grid of plots
    par(mfrow = c(2, 2))
    
    # Generate diagnostic plots
    plot(lm_all_correlated)
    
    # Add a main title for all plots
    mtext("All Correlated Variables Model", side = 3, line = -2, outer = TRUE)
  })
  
  #########################################################################################
  output$modelStep3 <- renderPrint({
    # Fit the full model with all variables
    lm_full <- lm(zscore ~ ., data = train_data)
    
    # Display the summary of the full model
    print(summary(lm_full))
    
    # Predict on test data and calculate errors
    predicted_full <- predict(lm_full, newdata = test_data)
    MSE_full <- mean((predicted_full - test_data$zscore)^2)
    RMSE_full <- sqrt(MSE_full)
    MAE_full <- mean(abs(predicted_full - test_data$zscore))
    
    # Calculate AIC and BIC
    AIC_value_full <- AIC(lm_full)
    BIC_value_full <- BIC(lm_full)
    
    # Display test error metrics
    cat("\nTest Set Error Metrics (Full Model):\n")
    cat("MSE: ", MSE_full, "\nRMSE: ", RMSE_full, "\nMAE: ", MAE_full, "\n")
    cat("\nModel Criteria (Full Model):\n")
    cat("AIC: ", AIC_value_full, "\nBIC: ", BIC_value_full, "\n")
    
  })
 
  output$residualsPlotStep3 <- renderPlot({
    # Fit the full model with all variables
    lm_full <- lm(zscore ~ c_breastf + c_age + m_height + c_gender + m_education + m_work + region + district, data = train_data)
    
    # Set up the plot area for a 2x2 grid of plots
    par(mfrow = c(2, 2))
    
    # Generate diagnostic plots
    plot(lm_full)
    
    # Add a main title for all plots
    mtext("Full Model Residuals Analysis", side = 3, line = -2, outer = TRUE)
    
    # Reset plot settings to default (optional, but good practice)
    par(mfrow = c(1, 1))
  })
  
  #########################################################################################
  spline_cat_model <- reactive({
    # Fit the spline_cat model
    spline_cat <- lm(zscore ~ ns(c_gender) + ns(c_breastf) + ns(c_age, df = 3) + ns(m_agebirth, df = 3) + ns(m_height, df = 3) + ns(m_bmi, df = 3) + ns(m_education) + ns(region) + ns(district), data=train_data_cat)
    return(spline_cat)
  })
  # Reactive expression for model predictions and error metrics
  model_diagnostics <- reactive({
    model <- spline_cat_model()
    predicted <- predict(model, newdata = test_data_cat)
    actual <- test_data_cat$zscore
    
    MSE <- mean((predicted - actual)^2)
    RMSE <- sqrt(MSE)
    MAE <- mean(abs(predicted - actual))
    AIC_value <- AIC(model)
    BIC_value <- BIC(model)
    
    list(
      MSE = MSE,
      RMSE = RMSE,
      MAE = MAE,
      AIC = AIC_value,
      BIC = BIC_value,
      predicted = predicted
    )
  })
  
  # To predict and compute test error metrics
  spline_cat_predictions <- reactive({
    predict(spline_cat_model(), newdata = test_data_cat)
  })
  
  # Output the summary of spline_cat model
  output$splineCatModelSummary <- renderPrint({
    summary(spline_cat_model())
  })
  
  # Output the model diagnostics
  output$splineCatModelDiagnostics <- renderText({
    diag <- model_diagnostics()
    paste(    "\nTest Set Error Metrics :\n",
              "MSE: ", diag$MSE, "\nRMSE: ", diag$RMSE, "\nMAE: ", diag$MAE, "\n",
              "AIC: ", diag$AIC, "\nBIC: ", diag$BIC, "\n")
  })
  
  # Render diagnostic plots for the spline_cat model
  output$splineCatDiagnosticPlots <- renderPlot({
    par(mfrow = c(2, 2))
    plot(spline_cat_model())
  })
  
  # Render actual vs. predicted plot for spline_cat model
  output$splineCatActualVsPredicted <- renderPlot({
    predicted_values <- spline_cat_predictions()
    diag <- model_diagnostics()
    comparison_data <- data.frame(Actual = test_data_cat$zscore, Predicted = predicted_values)
    
    ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
      geom_point() +
      geom_abline(intercept = 0, slope = 1, color = "red") +
      labs(title = "Actual vs. Predicted for spline_cat Model",
           x = "Actual zscore", y = "Predicted zscore")
  })
  
  #########################################################################
  
  # Fit the GAM model
  gam_most_significant <- reactive({
    gam(zscore ~ c_gender + s(c_breastf) + s(c_age) + s(m_agebirth) + s(m_height) + 
          s(m_bmi) + m_education + region, data = train_data)
  })
  
  # Reactive for generating predictions and handling potential issues
  gam_predictions <- reactive({
    if (is.null(gam_most_significant())) {
      return(NULL)  # Early exit if the model hasn't been successfully created
    }
    
    tryCatch({
      predict(gam_most_significant(), newdata = test_data)
    }, error = function(e) {
      return(NULL)  # Handle prediction errors by returning NULL
    })
  })
  
  # Calculate test errors and criteria
  gam_test_results <- reactive({
    model <- gam_most_significant()
    predicted <- predict(model, newdata = test_data)
    actual <- test_data$zscore
    
    MSE <- mean((predicted - actual)^2)
    RMSE <- sqrt(MSE)
    MAE <- mean(abs(predicted - actual))
    AIC_value <- AIC(model)
    BIC_value <- BIC(model)
    
    list(MSE = MSE, RMSE = RMSE, MAE = MAE, AIC = AIC_value, BIC = BIC_value)
  })
  
  # Output the summary of GAM model
  output$gamModelSummary <- renderPrint({
    summary(gam_most_significant())
  })
  
  # Output the model diagnostics
  output$gamModelDiagnostics <- renderText({
    diag <- gam_test_results()
    paste("MSE:", diag$MSE, "RMSE:", diag$RMSE, "MAE:", diag$MAE, "AIC:", diag$AIC, "BIC:", diag$BIC)
  })
  
  # Render diagnostic plots for the GAM model
  output$gamDiagnosticPlots <- renderPlot({
    req(gam_most_significant()) # Ensure the gam model is available
    
    par(mfrow = c(2, 2)) # Set up the plotting area to display 4 plots (2x2 grid)
    plot(gam_most_significant(), pch = 20, cex = 0.5, xlab = "Theoretical quantiles of Z-score", ylab = "Obseverved quantiles of Z-score") # Residuals vs Fitted plot 
    plot(gam_most_significant(), resid = TRUE, xlab = "Fitted values of Z-score", ylab = "Residuals of Z-score") #
    plot(gam_most_significant(), se = TRUE, xlab = "Residuals of Z-score", ylab = "Frequency") # 
    plot(gam_most_significant(), scheme = 1, shade = TRUE, pch = 20, cex = 0.5, xlab = "Fitted Values of Z-score", ylab = "Observed Z-score") # 
    gam.check(gam_most_significant()) # Additional checks for the GAM model
    
    # Reset the graphical parameters
    par(mfrow = c(1, 1))
  }, height = 800)
  
  # Render actual vs. predicted plot for GAM model
  output$gamActualVsPredicted <- renderPlot({
    req(!is.null(gam_predictions()))  # Ensure predictions are not NULL
    
    # Create comparison data frame
    comparison_data <- data.frame(
      Actual = test_data$zscore,
      Predicted = gam_predictions()
    )
    
    # Check for data integrity before plotting
    if (nrow(comparison_data) > 0 && !anyNA(comparison_data$Predicted)) {
      ggplot(comparison_data, aes(x = Actual, y = Predicted)) +
        geom_point() +
        geom_abline(intercept = 0, slope = 1, color = "red") +
        labs(title = "Actual vs. Predicted for GAM Model",
             x = "Actual zscore", y = "Predicted zscore")
    } else {
      plot.new()
      text(0.5, 0.5, "Unable to generate plot due to data issues")
    }
  })
  
  
  ################################################################################
   
  
  output$linearModelOutput <- renderUI({
    step <- input$modelStep
    
    if (step == "step1") {
      tagList(
        HTML("<p>Step 1: Initial Model (Null Model)</p>
           <p>In this step, we start with a null model. A null model predicts the outcome (zscore) using only the intercept. This model doesn't include any predictors and serves as a baseline to compare against more complex models. It helps us understand the mean of the response variable when no predictors are included.</p>"),
        verbatimTextOutput("modelStep1"),
        verbatimTextOutput("modelTestError"),
        plotOutput("qqPlotNullModel")
        
      )
     
    } else if (step == "step2") {
      tagList(
        HTML("<p>Step 2: Correlated Variables Model</p>
           <p>In this step, we consider a linear model that includes variables which are highly correlated with 'zscore': 'c_breastf', 'c_age', and 'm_height'. This model helps us understand the combined influence of these variables on 'zscore'.</p>"),
        verbatimTextOutput("modelStep2"),
        plotOutput("residualsPlotStep2", width = "100%", height = "600px") 
      )
    } else if (step == "step3") {
      tagList(
        HTML("<p>Step 3: Full Model</p>
           <p>In this step, we consider a full linear model that includes all available variables. This model helps us understand the combined influence of all variables on 'zscore' and to assess the overall fit of the model.</p>"),
        verbatimTextOutput("modelStep3"),
        plotOutput("residualsPlotStep3", width = "100%", height = "600px")
      )
    }else if (step == "step4") {
      tagList(
        HTML("<p>Step 4: Explanation of final model...</p>"),
        verbatimTextOutput("modelStep4")
      )
    }else if (step == "step7") {
      tagList(
        HTML("<p>Splines: The best Spline model found is the one found with step AIC, it's the one that uses all predictors except 'm_work' and uses the categorical version of 'c_breastf' :</p>"),
        verbatimTextOutput("splineCatModelSummary"),
        textOutput("splineCatModelDiagnostics"),
        plotOutput("splineCatDiagnosticPlots"),
        plotOutput("splineCatActualVsPredicted")
        
      )
    }else if (step == "step8") {
      tagList(
        HTML("<p>GAM: To represent the best Generalized Additive Model (GAM), we'll take the gam_most_significant model which uses the following variables: child's gender, duration of breastfeeding, child's age, mother's age at birth, mother's height, and mother's BMI, as well as mother's education and the region of residence. , this seems to have the best performance based on the analysis. We'll include the model fitting, the calculation of test errors, and the plotting of residuals. <p>"),
        verbatimTextOutput("gamModelSummary"),
        textOutput("gamModelDiagnostics"),
        plotOutput("gamDiagnosticPlots", height = "800px"),
        plotOutput("gamActualVsPredicted")
        
        
      )
    }
  })
}



# Run the application
shinyApp(ui = ui, server = server)
