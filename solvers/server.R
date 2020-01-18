library(shiny)
library(shinyjs)
library(ggrepel)

source("globals.R")

function(input, output) {
  
  hide("try")
  
  rv <- reactiveValues(
    landscape = NULL,
    solving = FALSE,
    moved = NA,
    current = 1,
    possibility = 1,
    best = 0,
    strikes = 0,
    steps = 0
  )

  observeEvent(input$go, {
    rv$landscape <-
      make_landscape(
        n = settings$options,
        smoothing_factor = input$smoothing_factor,
        max_value = settings$max_value
      )
    rv$current = 1
    rv$best = 0
    rv$strikes = 0
    rv$steps = 0
    rv$solving = TRUE
    rv$moved = NA
    rv$possibility = 1
    show("try")
  })
  
  observeEvent(input$try, {
    current <- rv$current
    strikes <- rv$strikes
    steps <- rv$steps
    best <- rv$best
    landscape <- rv$landscape
    heuristic <- str_split(
      input$heuristic, pattern = ",\\s*") %>% 
      unlist() %>% 
      as.numeric()
    h_length <- length(heuristic)
    try <- steps %% h_length + 1
    rv$moved <- heuristic[try]
    possibility <- ifelse(
      (current + heuristic[try]) %% length(landscape) == 0,
      length(landscape),
      (current + heuristic[try]) %% length(landscape)
    )
    rv$possibility <- possibility
    if (landscape[possibility] > best) {
      rv$current <- possibility
      rv$best <- landscape[possibility]
      rv$strikes <- 0
    } else {
      rv$strikes <- strikes + 1
    }
    rv$steps <- steps + 1
    if (rv$strikes == h_length) {
      rv$solving = FALSE
      hide("try")
    }
  })

  output$landscape_plot <- renderPlot({
    if (!is.null(rv$landscape)) {
      landscape <- rv$landscape
      df <- data.frame(
        approach = 1:length(landscape),
        value = landscape
      )
      present <- data.frame(
        x = c(rv$current, rv$possibility),
        y = c(rv$landscape[rv$current], rv$landscape[rv$possibility]),
        point = c("current best", "trying")
      )
      p <- 
        ggplot(df, aes(x = approach, y = value)) +
        geom_line() +
        geom_point(data = present, 
                   aes(x = x, y = y,
                      color = point), 
                   size = 3) +
        scale_color_manual(values = c("blue", "red")) +
        geom_label_repel(
          data = present,
          mapping = aes(x = x, y = y, label = point),
          #arrow = arrow(length = unit(0.03, "npc"), type = "closed", ends = "last"),
          force = 10
        ) +
        theme(legend.position = "none")
      print(p)
    }
  })
  
  output$process <- renderTable({
    rv$steps
    if (!is.null(rv$landscape)) {
      data.frame(
        moved = as.integer(isolate(rv$moved)),
        trying = as.integer(isolate(rv$possibility)),
        `best so far` = isolate(rv$best),
        strikes = isolate(as.integer(rv$strikes)),
        stuck = !isolate(rv$solving)
      )
    }
  })
  
}