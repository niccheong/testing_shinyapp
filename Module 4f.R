# Load packages ----------------------------------------------------------------

library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(shinythemes)

# Load data --------------------------------------------------------------------

load("movies.RData")

# Define UI --------------------------------------------------------------------

ui <- fluidPage(
  shinythemes::themeSelector(),
  #consider selected themes:
  #theme=shinytheme("united") i.e., selecting "united" as the theme
  titlePanel("Module 3 to 4"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "y",
                  label = "Y-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "audience_score"
      ),
      selectInput(inputId = "x",
                  label = "X-axis:",
                  choices = c("imdb_rating", "imdb_num_votes", "critics_score", "audience_score", "runtime"),
                  selected = "critics_score"
      ),
      textInput(inputId = "plot_title",
                label = "Insert Plot Title",
                placeholder = "Title Here"
      ),
      radioButtons(inputId = "filetype",
                   label = "Select filetype:",
                   choices = c("csv", "tsv"),
                   selected = "csv"
      ),
      checkboxGroupInput(inputId = "selected_title_type",
                         label = "Select title type:",
                         choices=levels(movies$title_type),
                         #let default be all boxes are TRUE
                         selected=levels(movies$title_type)
      )
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          title = "Plot",
          plotOutput(outputId= "scatterplot", brush="plot_brush")
          ),
        tabPanel(
          title="Table", dataTableOutput(outputId = "datatable")
          ),
        tabPanel(
          title="Reference",
          tags$p(
            "There data were obtained from",
            tags$a("IMDB", href = "http://www.imdb.com/"), "and",
            tags$a("Rotten Tomatoes", href = "https://www.rottentomatoes.com/"), "."
          ),
          tags$p(
            "The data represent", nrow(movies),
            "randomly sampled movies released between 1972 to 2014 in the United States.")
        )
      ),

      downloadButton(outputId="download_data", 
                     label="Download data")
      
    )
  )
)
# Define server ----------------------------------------------------------------

server <- function(input, output, session){
  
  movies_subset <- reactive({
    req(input$selected_title_type)
    filter(movies, title_type %in% input$selected_title_type)
  })
  
  movies_brushed <- reactive({
    brushedPoints(movies_subset(), input$plot_brush) %>%
      select(title, audience_score, critics_score)
  })
  
  output$scatterplot <- renderPlot({
    ggplot(data = movies_subset(), 
           aes_string(x = input$x, y = input$y)) +
      geom_point() +
      labs(title = isolate({input$plot_title})) #Isolate, will only update if others update
  })
  
  output$datatable <- renderDataTable({
    movies_brushed()
    #helper function brushedPoints to only select points inside the rectangle
    #brushedPoints(movies_subset(), input$plot_brush) %>%
    #select(title, audience_score, critics_score)
    #for hover, the helper function is nearPoint(data, input$<name>)
  })
  
  # Download file
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("movies.", input$filetype)
    },
    content = function(file) { 
      if(input$filetype == "csv"){ 
        write_csv(movies_brushed(), file) 
      }
      if(input$filetype == "tsv"){ 
        write_tsv(movies_brushed(), file) 
      }
    }
  )
  
}
# Create a Shiny app object ----------------------------------------------------

shinyApp(ui = ui, server = server)