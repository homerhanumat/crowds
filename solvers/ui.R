library(shinydashboard)
library(shinyjs)

source("globals.R")

header <- dashboardHeader(
  title = "Creepy Solver"
)

sidebar <- dashboardSidebar(
  textInput(
    inputId = "heuristic",
    label = "Solver Moves (separate by commas)",
    value = "1,10,20"
  ),
  HTML(paste0(
    "<p style='margin-left: 10px; margin-right: 1opx;'>",
    "The solver tries approaches beyond the curren t",
    "best solution, moving forward by one of the ",
    "values you provide above.</p>"
    )
  ),
  numericInput(
    inputId = "smoothing_factor",
    label = "Smoothing Factor",
    value = 10,
    min = 1,
    max = settings$max_smoothness
  ),
  HTML(paste0(
    "<p style='margin-left: 10px; margin-right: 10px;'>",
    "The bigger the smoothing factor, the ",
    "less 'choppy' the epistemic landscape wiil be.</p>"
    )
  ),
  br(),
  actionButton(
    inputId = "go",
    label = "Make New Landscape"
  ),
  actionButton(
    inputId = "try",
    label = "Try Another Option"
  )
)

body <- dashboardBody(
  fluidRow(
    plotOutput("landscape_plot")
  ),
  fluidRow(
    tableOutput("process")
  )
)

dashboardPage(
  useShinyjs(),
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)