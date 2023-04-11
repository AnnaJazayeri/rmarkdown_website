library(shiny)
library(ggplot2)
library(dplyr)
library(readxl)

# Read and preprocess data
mydata <- read_excel("Penguin data.xlsx")
# [Preprocessing steps from your code]
# install.packages("readxl")
library(readxl)
mydata <- read_excel("Penguin data.xlsx")
mydata<- mydata[, -17]
miss_count <- colSums(is.na(mydata))
mydata <- na.omit(mydata)
mydata$"Culmen Length (mm)" <- as.numeric(mydata$"Culmen Length (mm)")
mydata$"Culmen Depth (mm)" <- as.numeric(mydata$"Culmen Depth (mm)")
mydata$"Flipper Length (mm)" <- as.numeric(mydata$"Flipper Length (mm)")
mydata$"Body Mass (g)" <- as.numeric(mydata$"Body Mass (g)")
mydata$"Delta 15 N (o/oo)" <- as.numeric(mydata$"Delta 15 N (o/oo)")
mydata$"Delta 13 C (o/oo)" <- as.numeric(mydata$"Delta 13 C (o/oo)")

mydata <- mydata %>% mutate(Sex = ifelse(Sex == "MALE", "MALE", "FEMALE"))

# Define UI
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("selected_island", "Select Island:", choices = c("All", unique(mydata$Island))),
      sliderInput("body_mass_range", "Select Body Mass (g) Range:",
                  min = min(mydata$`Body Mass (g)`),
                  max = max(mydata$`Body Mass (g)`),
                  value = c(min(mydata$`Body Mass (g)`), max(mydata$`Body Mass (g)`)))
      
    ),
    mainPanel(
      plotOutput("bar_chart"),
      plotOutput("scatter_plot"),
      plotOutput("scatter_plot_species"),
      plotOutput("scatter_plot_culmen_length_depth"),
      plotOutput("scatter_plot_culmen_depth_by_island"),
      plotOutput("scatter_plot_flipper_length"),
      plotOutput("scatter_plot_body_mass_delta_15_n"),
      plotOutput("scatter_plot_body_mass_delta_13_c")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  output$bar_chart <- renderPlot({
    if (input$selected_island != "All") {
      data_filtered <- mydata %>% filter(Island == input$selected_island)
    } else {
      data_filtered <- mydata
    }
    
    ggplot(data_filtered, aes(x = Island, fill = Species)) +
      geom_bar(position = "stack") +
      labs(title = "Penguin Species Counts by Island", x = "Island", y = "Count", fill = "Species")
  })
  
  output$scatter_plot <- renderPlot({
    data_filtered <- mydata %>% filter(`Body Mass (g)` >= input$body_mass_range[1] & 
                                         `Body Mass (g)` <= input$body_mass_range[2])
    
    ggplot(data_filtered, aes(x = `Culmen Length (mm)`, y = `Body Mass (g)`, color = Sex)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relationship between Culmen Length and Body Mass by Sex", x = "Culmen Length (mm)", y = "Body Mass (g)", color = "Sex")
  })
  output$scatter_plot_species <- renderPlot({
    ggplot(mydata, aes(x = `Culmen Length (mm)`, y = `Body Mass (g)`, color = Species)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relationship between Culmen Length and Body Mass by Species", x = "Culmen Length (mm)", y = "Body Mass (g)", color = "Species")
  })
  output$scatter_plot_culmen_length_depth <- renderPlot({
    ggplot(mydata, aes(x = `Culmen Length (mm)`, y = `Culmen Depth (mm)`, color = Species)) +
      geom_point(alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(title = "Relationship between Culmen Length and Depth by Species", x = "Culmen Length (mm)", y = "Culmen Depth (mm)", color = "Species")
  })
  output$scatter_plot_culmen_depth_by_island <- renderPlot({
    mydata$`Date Egg` <- as.Date(mydata$`Date Egg`)
    ggplot(mydata, aes(x = `Date Egg`, y = `Culmen Depth (mm)`)) +
      geom_point(alpha = 0.2) +
      geom_smooth() +
      labs(title = "Trend of Culmen Depth (mm) by Island over Time", x = "Date", y = "Culmen Depth (mm)") +
      facet_grid(. ~ Island) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y")
  })
  output$scatter_plot_flipper_length <- renderPlot({
    ggplot(mydata, aes(x = `Flipper Length (mm)`, y = `Body Mass (g)`, color = Species)) +
      geom_point(size = 2) +
      labs(x = "Flipper Length (mm)", y = "Body Mass (g)", color = "Species") +
      ggtitle("Body Mass vs. Flipper Length by Species")
  })
  output$scatter_plot_body_mass_delta_15_n <- renderPlot({
    ggplot(mydata, aes(x = `Body Mass (g)`, y = `Delta 15 N (o/oo)`)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Body Mass (g)", y = "Delta 15 N (o/oo)", 
           title = "Relationship between Body Mass and Delta 15 N") +
      theme_bw()
  })
  output$scatter_plot_body_mass_delta_13_c <- renderPlot({
    ggplot(mydata, aes(x = `Body Mass (g)`, y = `Delta 13 C (o/oo)`)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      labs(x = "Body Mass (g)", y = "Delta 13 C (o/oo)", 
           title = "Relationship between Body Mass and Delta 13 C (o/oo)") +
      theme_bw()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
