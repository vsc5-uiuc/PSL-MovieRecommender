## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)
library(dplyr)

source('functions/helpers.R')

shinyUI(
    dashboardPage(
          skin = "blue",
          dashboardHeader(title = "Movie Recommender"),
          
          # sidebar
          dashboardSidebar(
            sidebarMenu(
              menuItem(text =  "Popular Movies", tabName = "system1"),
              menuItem(text = "Personalized Movie Recommender", tabName = "system2")
            )
          ),

          # Content
          dashboardBody(includeCSS("css/movies.css"),
              tabItems(
                # First Tab
                tabItem(tabName = "system1",
                  fluidRow(
                    box(
                      selectInput(inputId = "genre", 
                                  label = "Choose a Genre:",
                                  choices = c("Action", "Adventure", "Comedy", "Crime", "Drama", "Horror", "Romance", "Sci-Fi", "Thriller"), 
                                  selected = "Action"),
                      withBusyIndicatorUI(
                        actionButton("showTopBtn", "Show Popular Movies", class = "btn-success")
                      ),
                    )
                  ),
                  
                  fluidRow(
                      useShinyjs(),
                      box(
                        width = 12, status = "info", solidHeader = TRUE,
                        title = "Popular Movies",
                        br(),
                        tableOutput("topMovieResults")
                      )
                  )
                ),
                
                # Second Tab
                tabItem(tabName = "system2", 
                        fluidRow(
                          box(width = 12, title = "Step 1: Rate as many movies as possible (atleast 3)", status = "info", solidHeader = TRUE, collapsible = TRUE,
                              div(class = "rateitems",
                                  uiOutput('ratings')
                              )
                          )
                        ),
                        fluidRow(
                          useShinyjs(),
                          box(
                            width = 12, status = "info", solidHeader = TRUE,
                            title = "Step 2: Discover movies you might like",
                            br(),
                            withBusyIndicatorUI(
                              actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
                            ),
                            br(),
                            tableOutput("results")
                          )
                        )
                )
              )
          )
    )
)