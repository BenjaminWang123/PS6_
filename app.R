library(shiny)
library(tidyverse)
library(plotly)

data <- read_delim("exams.csv")

ui <- fluidPage(
  titlePanel("Simple Exam Analyzation"),
  tabsetPanel(
    tabPanel("General Information",
             h1("About This Data:"),
             p("This dataset contains information on the performance of high school students in mathematics, including their grades and demographic information. The data was collected from three high schools in the United States."),
             p("Source Link: https://www.kaggle.com/datasets/rkiattisak/student-performance-in-mathematics"),
             p("Source Name: Student performance prediction")),
    tabPanel("Overall Performance",
             h2("GPA VS Students"),
             fluidPage(
               titlePanel("General distribution of students overall grades"),
               p("This page shows general distribution of students grades of math, reading, and writing"),
               p("There are ", nrow(data), "students in the dataset"),
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("n",
                               "Number of students:",
                               min = 0,
                               max = nrow(data),
                               value = 100)
                 ),
                 mainPanel(
                   fluidRow(
                     splitLayout(cellWidths = c("33%", "33%", "33%"), plotlyOutput("plot1_1"), plotlyOutput("plot1_2"), plotlyOutput("plot1_3"))
                   )
                 )
               )
             )),
    tabPanel("By Family Background",
             h3("GPA VS Parents' Education Level"),
             fluidPage(
               titlePanel("Do family's education level influence students' GPA?"),
               p("This page let us find the insight of family background. We want to explore whether family backgroud can influence students grade and do they influence significantly?"),
               sidebarLayout(
                 sidebarPanel(
                   uiOutput("checkboxFamily")
                 ),
                 mainPanel(
                   fluidRow(
                     splitLayout(cellWidths = c("33%", "33%", "33%"), plotlyOutput("plot2_1"), plotlyOutput("plot2_2"), plotlyOutput("plot2_3"))
                   )
                 )
               )
             )),
    tabPanel("By Race",
             h4("GPA VS Race"),
             fluidPage(
               titlePanel("Grades by Race"),
               p("This page displays a table that shows all grades data based on different racial group"),
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput(
                     inputId = "race_filter",
                     label = "Racial group:",
                     choices = unique(data$`race/ethnicity`),
                     selected = unique(data$`race/ethnicity`)
                   )
                 ),
                 mainPanel(
                   tableOutput('table')
                 )
               )
             ))
  )
)
  

server <- function(input, output) {
  sample <- reactive({
    s1 <- data %>% 
      sample_n(input$n)
  })
  output$plot1_1 <- renderPlotly({
    plot_ly(data = sample(),
            x = ~`math score`,
            type = "histogram",
            color = "red")
  })
  output$plot1_2 <- renderPlotly({
    plot_ly(data = sample(),
            x = ~`reading score`,
            type = "histogram",
            color = "green")
  })
  output$plot1_3 <- renderPlotly({
    plot_ly(data = sample(),
            x = ~`writing score`,
            type = "histogram",
            color = "black")
  })
  output$checkboxFamily <- renderUI ({
    checkboxGroupInput("family", "choose Family Education Level",
                       choices = unique(data$`parental level of education`))
  })
  sample2 <- reactive({
    s2 <- data %>% 
      filter(`parental level of education` %in% input$family)
  })
  output$plot2_1 <- renderPlotly({
    plot_ly(data = sample2(),
            x = ~`math score`,
            type = "histogram",
            color = ~`parental level of education`)
  })
  output$plot2_2 <- renderPlotly({
    plot_ly(data = sample2(),
            x = ~`reading score`,
            type = "histogram",
            color = ~`parental level of education`)
  })
  output$plot2_3 <- renderPlotly({
    plot_ly(data = sample2(),
            x = ~`writing score`,
            type = "histogram",
            color = ~`parental level of education`)
  })
  filtered_race <- reactive({
    data %>%
      filter(`race/ethnicity` %in% input$race_filter)
  })
  
  # Create a table of grades by race
  output$table <- renderTable({
    filtered_race() %>%
      select(`test preparation course`, `writing score`, `reading score`, `math score`)
  })
}


shinyApp(ui = ui, server = server)
