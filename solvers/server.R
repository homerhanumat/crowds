## packages and globals ----

library(shiny)
library(shinyjs)
library(ggrepel)

source("globals.R")
load("data/results_3_36_200_1to20_3030.rda")

## server function ----
function(input, output) {
  
  hide("try")
  
  ## reactive values ----
  rv <- reactiveValues(
    landscape = NULL,
    solving = FALSE,
    moved = NA,
    current = 1,
    possibility = 1,
    best = 0,
    strikes = 0,
    steps = 0,
    simulations = NULL,
    team_results = NULL
  )

  ## observers ----
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
  
  observeEvent(input$go2, {
    reps <- input$sims
    simulations <- numeric(reps)
    heuristic <- str_split(
      input$heuristic2, pattern = ",\\s*") %>% 
      unlist() %>% 
      as.numeric()
    withProgress(message = "Simulating ...", 
                 min = 1, max = input$sims, {
      for (i in 1:reps) {
        landscape <- make_landscape(
          n = settings$options,
          smoothing_factor = input$smoothing_factor2,
          max_value = settings$max_value
        )
        simulations[i] <- solve_landscape(
          heuristic = heuristic,
          landscape = landscape
        )$val
        incProgress(1/reps * 100, detail = paste(ceiling(i/reps * 100), "% done"))
      }
    })
    
    rv$simulations <- simulations
  })
  
  observeEvent(input$simulate_teams, {
    lst <- get_bounded_all(bound = input$move_limit, lst = results_3_36_200_1to20_3030)
    lower <- isolate(input$sf_range[1])
    upper <- isolate(input$sf_range[2])
    rv$team_results <- withProgress(
      message = "Simulating ...", 
      min = lower, max = upper, 
      comparison(
        sims = input$team_sims,
        seed = NULL,
        expert_size = input$expert_size,
        expert_limit = input$move_limit,
        random_size = input$random_size,
        set = lst)
      )
  })

  ## output ----
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
      results <- c(
        moved = as.character(isolate(rv$moved)),
        trying = as.character(isolate(rv$possibility)),
        `best so far` = as.character(round(isolate(rv$best), 3)),
        strikes = as.character(isolate(as.integer(rv$strikes))),
        stuck = as.character(!isolate(rv$solving))
        )
      result_names <- c(
        "moved", "trying", "best so far", "strikes", "stuck?"
      )
      df <- data.frame(results)
      names(df) <- ""
      row.names(df) <- result_names
      df
    }
  }, rownames = TRUE)
  
  output$simsplot <- renderPlot({
    if (!is.null(rv$simulations)) {
      data.frame(value = rv$simulations) %>% 
        ggplot(aes(x = value)) +
        geom_density(fill = "burlywood") +
        geom_rug() +
        labs(x = "value at stopping solution")
    }
  })
  
  output$simresults <- renderTable({
    if (!is.null(rv$simulations)) {
      sims <- round(rv$simulations, 3)
      results <- c(
        as.character(min(sims)),
        as.character(median(sims)),
        as.character(mean(sims)),
        as.character(max(sims))
      )
      result_names <- c(
        "minimum", "median", "mean", "maximum"
      )
      df <- data.frame(results)
      names(df) <- ""
      row.names(df) <- result_names
      df
    }
  }, rownames = TRUE)
  
  output$team_results <- renderPlot({
    if (!is.null(rv$team_results)) {
      lower <- isolate(input$sf_range[1])
      upper <- isolate(input$sf_range[2])
      sims <- isolate(input$team_sims)
      ggplot(rv$team_results, aes(x = factor, y = mean_diff)) +
        geom_ribbon(aes(ymin = mean_diff - 2 * sd_diff/sqrt(sims), 
                        ymax = mean_diff + 2 * sd_diff/sqrt(sims)),
                    fill = "grey70", alpha = 0.5) +
        geom_line(aes(y = mean_diff)) +
        geom_hline(yintercept = 0) +
        scale_x_continuous(breaks = lower:upper) +
        labs(x = "smoothing factor",
             y = "mean of differences (experts - randoms)")
    }
  })
  
}