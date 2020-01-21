library(shinydashboard)
library(shinyjs)

source("globals.R")

## header ----
header <- dashboardHeader(
  title = "Creepy Solvers"
)

## sidebar ----
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Solving", tabName = "solving"),
    menuItem("Ability", tabName = "ability"),
    menuItem("Teams", tabName = "teams")
  )
)

## body ----
body <- dashboardBody(
  tabItems(
    ## tab item "solving" ----
    tabItem(
      tabName = "solving",
      fluidRow(
        box(
          solidHeader = TRUE,
          collapsible = TRUE,
          status = "primary",
          width = 9,
          title = "Inputs",
          column(
            width = 7,
            textInput(
              inputId = "heuristic",
              label = "Solver Moves (separate by commas)",
              value = "1,10,20"
            ),
            HTML(paste0(
              "<p>",
              "The solver tries approaches beyond the current ",
              "best solution, moving forward by one of the ",
              "values you provide above.</p>"
            ))
          ),
          column(
            width = 5,
            numericInput(
              inputId = "smoothing_factor",
              label = "Smoothing Factor",
              value = 10,
              min = 1,
              max = settings$max_smoothness
            ),
            HTML(paste0(
              "<p>",
              "The bigger the smoothing factor, the ",
              "less 'choppy' the epistemic landscape wiil be.</p>"
            ))
          )
        ),
        box(
          solidHeader = TRUE,
          status = "primary",
          width = 3,
          title = "Actions",
          actionButton(
            inputId = "go",
            label = HTML("Make New <br>Landscape")
          ),
          br(), br(),
          actionButton(
            inputId = "try",
            label = HTML("Try Another<br>Option")
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title = "Epistemic Landscape and Search Status",
          column(
            width = 9,
            plotOutput("landscape_plot")
          ),
          column(
            width = 3,
            tableOutput("process")
          )
        ) # end box
      ) # end row
    ), # end  solving tab item
    ## tab item "ability" ----
    tabItem(
      tabName = "ability",
      fluidRow(
        box(
          solidHeader = TRUE,
          status = "primary",
          width = 9,
          title = "Inputs",
          column(
            width = 7,
            textInput(
              inputId = "heuristic2",
              label = "Solver Moves (separate by commas)",
              value = "1,10,20"
            ),
            HTML(paste0(
              "<p>",
              "The solver tries approaches beyond the current ",
              "best solution, moving forward by one of the ",
              "values you provide above.</p>"
            ))
          ),
          column(
            width = 5,
            numericInput(
              inputId = "smoothing_factor2",
              label = "Smoothing Factor",
              value = 10,
              min = 1,
              max = settings$max_smoothness
            ),
            HTML(paste0(
              "<p>",
              "The bigger the smoothing factor, the ",
              "less 'choppy' the epistemic landscape wiil be.</p>"
            ))
          )
        ),
        box(
          numericInput(
            inputId = "sims",
            label = HTML("Number of<br>Simulations<br>(max 100000)"),
            value = 1000,
            min = 1,
            max = 100000
          ),
          br(),
          solidHeader = TRUE,
          status = "primary",
          width = 3,
          title = "Actions",
          actionButton(
            inputId = "go2",
            label = HTML("Simulate")
          )
        ) # end box
      ), # end fluid row
      fluidRow(
        box(
          width = 12,
          title = "Simulation Results",
          column(
            width = 3,
            tableOutput("simresults")
          ),
          column(
            width = 9,
            plotOutput("simsplot")
          )
        )
      )
    ),  # end ability tab item
    ## tab item "teams"
    tabItem(
      tabName = "teams",
      fluidRow(
        box(
          width = 9,
          title = "Inputs",
          status = "primary",
          column(
            width = 3,
            numericInput(inputId = "expert_size", label = "Expert Team Size",
                         min = 1, value = 6, step = 1),
            numericInput(inputId = "random_size", label = "Random Team Size",
                         min = 1, value = 6, step = 1)
          ),
          column(
            width = 9,
            sliderInput(
              inputId = "move_limit",
              label = "Maximum Heuristic",
              value = 12,
              min = 1, max = 30,
              step = 1
            ),
            sliderInput(
              inputId = "sf_range",
              label = "Smoothing-Factor Range",
              value = c(1, 20),
              min = 1, max = 20,
              step = 1
            )
          )
        ),
        box(
          width = 3,
          title = "Simulation",
          status = "primary",
          numericInput(
            inputId = "team_sims",
            label = HTML("Number of<br>Simulations<br>(max 10000)"),
            value = 100,
            min = 1,
            max = 10000
          ),
          actionButton(
            inputId = "simulate_teams",
            label = "Simulate"
          )
        )
      ),
      fluidRow(
        box(
          width = 12,
          title = "Mean Difference in Score (Expert - Random) vs. Smoothing-Factor",
          plotOutput("team_results")
        )
      )
    )
  ) # end tab items
  
) # end body

## page ----
dashboardPage(
  useShinyjs(),
  header = header,
  sidebar = sidebar,
  body = body,
  skin = "blue"
)
