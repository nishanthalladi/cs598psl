## ui.R
library(shiny)
library(shinydashboard)
library(recommenderlab)
library(data.table)
library(ShinyRatingInput)
library(shinyjs)

source('helpers.R')

shinyUI(
  navbarPage("Navbar!",
             tabPanel("By Genre",
                      dashboardPage(
                            skin = "blue",
                            dashboardHeader(title = "Movie Recommender"),
                            dashboardSidebar(disable = TRUE),
                            
                            dashboardBody(
                              includeCSS("movie.css"),
                              selectInput("genre", "Choose a genre:", choices=genres),
                              actionButton("genre_submit", "Get Recommendations")
                            )
                      )
              ),
             tabPanel("By Rating",
                      dashboardPage(
                            skin = "blue",
                            dashboardHeader(title = "Movie Recommender"),
                            
                            dashboardSidebar(disable = TRUE),
                            
                            dashboardBody(includeCSS("movie.css"),
                                          fluidRow(
                                            box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
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

# shinyUI(
#     dashboardPage(
#           skin = "blue",
#           dashboardHeader(title = "movie Recommender"),
#           
#           dashboardSidebar(disable = TRUE),
# 
#           dashboardBody(includeCSS("movie.css"),
#               fluidRow(
#                   box(width = 12, title = "Step 1: Rate as many movies as possible", status = "info", solidHeader = TRUE, collapsible = TRUE,
#                       div(class = "rateitems",
#                           uiOutput('ratings')
#                       )
#                   )
#                 ),
#               fluidRow(
#                   useShinyjs(),
#                   box(
#                     width = 12, status = "info", solidHeader = TRUE,
#                     title = "Step 2: Discover movies you might like",
#                     br(),
#                     withBusyIndicatorUI(
#                       actionButton("btn", "Click here to get your recommendations", class = "btn-warning")
#                     ),
#                     br(),
#                     tableOutput("results")
#                   )
#                )
#           )
#     )
# ) 