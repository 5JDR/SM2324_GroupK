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
               menuSubItem("Gender Analysis", tabName = "genderAnalysis"),
               
               #menuSubItem("Breast-feedings", tabName = "Breast-feedings"),
               menuSubItem("Breast-feedings", tabName = "zscoreBreastfMarginalPlot"),
               menuSubItem("Age Analysis", tabName = "zscoreAgeMarginalPlot"),
               menuSubItem("Mother Height Analysis", tabName = "mheightMarginalPlot"),
               
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
      
      tabItem(tabName= "regionalAnalysis", leafletOutput("regionalAnalysis",height = "800px")),
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
  
  #cast of zscore to integer
  train_data$zscore <- as.integer(train_data$zscore)
  
  #factorize gender
  train_data$c_gender <- as.factor(train_data$c_gender)
  
  #cast c_breastf as integer
  train_data$c_breastf <- as.integer(train_data$c_breastf)
  
  #cast c_age as integer
  train_data$c_age <- as.integer(train_data$c_age)
  
  #NOTE: to have the month of birth in m_agebirth calculate: decimal_part * 12
  
  #factorize m_education
  train_data$m_education <- as.factor(train_data$m_education)
  
  #factorize m_work
  train_data$m_work <- as.factor(train_data$m_work)
  
  #factorize region
  train_data$region <- as.factor(train_data$region)
  
  #factorize district
  train_data$district <- as.factor(train_data$district)
  
 
  
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
}

# Run the application
shinyApp(ui = ui, server = server)
