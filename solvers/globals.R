library(gtools)
library(tidyverse)

## Settings ----

## options = number of x-values in the epistemic landscape
## max_smoothness = maximum smoothness factor in input widgets
## max_value = maximum y-value in an epistemic landscape (min = 0, always)
settings <- list(
  options = 200,
  max_smoothness = 100,
  max_value = 100,
  ability_sim_max = 100000,
  teams_sim_max = 10000
)

## Making a set of heuristics ----

## construct all permutation sof l things, taken k at a time
## (currently not needed in main paper)
make_heuristics <- function(k = 3, l = 12) {
  gtools::permutations(n = l, r = k)
}

## Making an Epistemic Landscape ----

make_landscape <- function(n = 2000, smoothing_factor = 6,
                           max_value = 100, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (smoothing_factor < 1) {
    stop("smoothing_factor must be an integer >= 1")
  }
  if (smoothing_factor == 1) {
    vals <- runif(n, 0, max_value)
    return(vals)
  }
  to_pick <- round(n / smoothing_factor, 0)
  xvals <- c(1, sort(sample(2:n, size = to_pick)), n + 1)
  start_y <- runif(1, 0, max_value)
  yvals <- c(start_y, runif(to_pick, 0, max_value), start_y)
  vals <- approx(
    x = xvals,
    y = yvals,
    xout = 1:(n + 1)
  )
  vals <- vals$y[-(n + 1)]
  move <- sample(2:(n - 1), size = 1)
  vals <- c(vals[move:n], vals[1:(move - 1)])
  ## now scale
  vals <- vals / max(yvals) * max_value
  vals
}

## Solving a Landscape ----

solve_landscape <- function(heuristic, landscape, start = 1) {
  h_length <- length(heuristic)
  val <- landscape
  current <- start
  best <- val[current]
  alternative <- 0
  strikes <- 0
  steps <- 0
  while (TRUE) {
    # set the heuristics the solver will use in this iteration:
    try <- steps %% h_length + 1
    # tentative new option:
    possibility <- ifelse(
      (current + heuristic[try]) %% length(landscape) == 0,
      length(landscape),
      (current + heuristic[try]) %% length(landscape)
    )
    if (val[possibility] > best) {
      current <- possibility
      best <- val[possibility]
      strikes <- 0
    } else {
      strikes <- strikes + 1
      if (strikes == h_length) break
    }
    steps <- steps + 1
  }
  return(list(solution = current, steps = steps + 1, val = val[current]))
}

## Team Solving Effort ----

## a team (or some subset thereof, as indicated by value of parameter: team)
## each member works from the current best option:
all_try <- function(team, member_numbers, landscape, start) {
  val <- landscape
  solutions <- numeric(length(member_numbers))
  steps <- 0
  for (i in 1:length(member_numbers)) {
    member <- team[[i]]
    res <- solve_landscape(member, landscape, start)
    solutions[i] <- res$solution
    steps <- steps + res$steps
  }
  highest <- max(val[solutions])
  best_solution <- solutions[val[solutions] == highest][1]
  proposer <- member_numbers[val[solutions] == highest][1]
  list(
    solution = best_solution,
    steps = steps,
    proposals = proposer
  )
}

## "tournament-style" team-solving procedure:
solve_tournament <- function(team, landscape) {
  current <- 1
  steps <- 0
  solving <- TRUE
  debug_limit <- Inf
  counter <- 0
  previous_proposer <- NA
  while (solving & counter <= debug_limit) {
    members_to_try <- setdiff(1:length(team), previous_proposer)
    if (length(members_to_try) > 0) {
      res <- all_try(
        team = team[members_to_try],
        member_numbers = members_to_try,
        landscape = landscape,
        start = current
      )
      steps <- steps + res$steps
      if (res$solution == current) {
        solving <- FALSE
      }
      current <- res$solution
    } else {
      solving <- FALSE
    }
    counter <- counter + 1
  }
  list(
    solution = current,
    steps = steps
  )
}

## Comparing Teams ----

## sims = number of simulations to perform
## expert_size = number of member sin the team of experts
## expert_limit:  team of expert_size experts is drawn randomly
## from the 1st, 2md, ..., expert_limit-th highest-ranking experts
## set = list of ranked solvers, at various smoothing levels
comparison <- function(
                       sims = 100,
                       seed = NULL,
                       expert_size = 9,
                       expert_limit = 9,
                       random_size = 9,
                       set,
                       manual_experts = NULL) {
  mean_experts <- mean_randoms <- sd_diffs <- mean_diffs <- numeric(length(set))
  factors <- as.numeric(names(set))
  for (i in 1:length(set)) {
    incProgress(i, detail = paste("working on smoothing factor", factors[i]))
    solvers <- set[[i]]
    expert_pool <- solvers$ranked[1:expert_limit, , drop = FALSE]
    expert_scores <- randoms_scores <- differences <- numeric(sims)
    for (m in 1:sims) {
      land <- make_landscape(
        n = 200, smoothing_factor = factors[i],
        max_value = 100, seed = NULL
      )
      if (!is.null(manual_experts)) {
        experts <- manual_experts
      } else {
        rows_to_pick <- sample(1:nrow(expert_pool), expert_size)
        experts_matrix <- expert_pool[rows_to_pick, , drop = FALSE]
        experts <- lapply(
          1:nrow(experts_matrix),
          function(i) experts_matrix[i, ]
        )
      }
      expert_sol <- solve_tournament(
        team = experts,
        landscape = land
      )
      rows_to_pick <- sample(1:nrow(solvers$ranked), random_size)
      randoms_matrix <- solvers$ranked[rows_to_pick, , drop = FALSE]
      randoms <- lapply(
        1:nrow(randoms_matrix),
        function(i) randoms_matrix[i, ]
      )
      random_sol <- solve_tournament(
        team = randoms,
        landscape = land
      )
      differences[m] <- land[expert_sol$solution] - land[random_sol$solution]
      expert_scores[m] <- land[expert_sol$solution]
      randoms_scores[m] <- land[random_sol$solution]
    }
    mean_experts[i] <- mean(expert_scores)
    mean_randoms[i] <- mean(randoms_scores)
    mean_diffs[i] <- mean(differences)
    sd_diffs[i] <- sd(differences)
  }
  data.frame(
    factor = factors,
    mean_diff = mean_diffs,
    sd_diff = sd_diffs,
    mean_experts = mean_experts,
    mean_randoms = mean_randoms
  )
}

## For Team-Sim Graphs ----

make_team_sim_graph <- function(df, sims, lower, upper) {
  df$mean_experts_nodisplay <- df$mean_experts
  df$mean_experts <- round(df$mean_experts, 2)
  df$mean_randoms_nodisplay <- df$mean_randoms
  df$mean_randoms <- round(df$mean_randoms, 2)
  df$mean_diff_nodisplay <- df$mean_diff
  df$mean_diff <- round(df$mean_diff, 2)
  df$lower_nodisplay <-
    with(
      df,
      mean_diff_nodisplay - 2 * sd_diff / sqrt(sims)
    )
  df$lower <-
    with(
      df,
      round(mean_diff_nodisplay - 2 * sd_diff / sqrt(sims), 2)
    )
  df$upper_nodisplay <-
    with(
      df,
      mean_diff_nodisplay + 2 * sd_diff / sqrt(sims)
    )
  df$upper <-
    with(
      df,
      round(mean_diff_nodisplay + 2 * sd_diff / sqrt(sims), 2)
    )
  ## because for some reason plotly needs two datasets:
  df2 <- df

  p <-
    ggplot() +
    geom_ribbon(
      data = df, aes(
        x = factor,
        ymin = lower_nodisplay,
        ymax = upper_nodisplay,
        label1 = upper,
        label2 = lower
      ),
      fill = "grey70",
      alpha = 0.3
    ) +
    geom_line(data = df2, aes(
      x = factor,
      y = mean_diff_nodisplay,
      label1 = mean_experts,
      label2 = mean_randoms,
      label3 = mean_diff
    )) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = lower:upper) +
    labs(
      x = "smoothing factor",
      y = "mean of differences (experts - randoms)",
      title = "Mean Difference in Score (Expert - Random) vs. Smoothing-Factor"
    )
  p <- ggplotly(tooltip = c("factor", "label1", "label2", "label3"))
  config(p, displayModeBar = FALSE)
}

## (currently not used in app)

get_bounded <- function(bound = 12, lst) {
  mat <- lst$ranked
  bounded <- rep(TRUE, times = nrow(mat))
  for (j in 1:ncol(mat)) {
    bounded <- bounded & (mat[, j] <= bound)
  }
  list(
    ranked = mat[bounded, , drop = FALSE],
    rating = lst$rating[bounded]
  )
}

get_bounded_all <- function(bound = 12, lst) {
  results <- vector(mode = "list", length = length(lst))
  names(results) <- names(lst)
  for (i in 1:length(lst)) {
    results[[i]] <- get_bounded(bound = bound, lst[[i]])
  }
  results
}
