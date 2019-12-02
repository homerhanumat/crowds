library(gtools)
library(tidyverse)

make_heuristics <- function(k = 3, l = 12) {
  gtools::permutations(n = l, r = k)
}

heuristics <- make_heuristics()

make_landscape <- function(n = 2000, smoothing_factor = 6, 
                           max_value = 100, seed = NULL) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  if (smoothing_factor < 1) {
    stop("smoothing_factor must be an integer >= 1")
  }
  if (smoothing_factor == 1) {
    vals <- sample(1:max_value, size = n, replace = TRUE)
    return(vals)
  }
  vals <- numeric(n)
  current <- 1
  vals[current] <- sample(1:max_value, size = 1)
  while (current < n) {
    run <- sample(seq(1, 2*smoothing_factor, by = 1), size = 1)
    new_pick <- min(n, current + run)
    vals[new_pick] <- sample(1:max_value, size = 1)
    places_between_exist <- current + 1 < new_pick
    if (places_between_exist) {
      places_between <- (current+1):(new_pick - 1)
      slope <- (vals[new_pick] - vals[current]) / (new_pick - current)
      vals[places_between] <- round(
        vals[current] + slope * (places_between - current),
        0
      )
    }
  current <- new_pick
  }
  vals
}

df <-
  data.frame(
    n = 1:200,
    v = make_landscape(n = 200, smoothing_factor = 5)
  )
df %>% 
  ggplot(aes(x = n, y = v)) +
  geom_line()

solve <- function(heuristic, landscape, start = 1) {
  val <- landscape
  current <- start
  best <- val[current]
  alternative <- 0
  strikes <- 0
  steps <- 0
  while (TRUE) {
    try <- steps %% 3 + 1
    possibility <- ifelse(
      current + heuristic[try] %% length(landscape) == 0,
      200,
      (current + heuristic[try]) %% length(landscape)
    )
    if (val[possibility] > best) {
      current <- possibility
      best <- val[possibility]
      strikes <- 0
    } else {
      strikes <- strikes + 1
      if (strikes == 3) break
    }
    steps <- steps + 1
  }
  return(list(solution = current, steps = steps + 1, val = val[current]))
}

solve(c(1, 5, 25), df$v, 190)

rank_heuristics <- function(sims = 1000, seed = NULL,
                            heuristics,
                            n = 200,
                            smoothing_factor = 6,
                            all_starts = FALSE,
                            trace = FALSE) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  sums <- numeric(nrow(heuristics))
  m <- ifelse(all_starts, n, 1)
  for (rep in 1:sims) {
    val = make_landscape(n = n, smoothing_factor = smoothing_factor)
    for (i in 1:nrow(heuristics)) {
      heuristic <- heuristics[i, ]
      for (j in 1:m) {
        res <- solve(
          heuristic = heuristic,
          landscape = val,
          start = j
        )
        sums[i] <- sums[i] + val[res$solution]
      }
    }
    if (trace & (rep %% trace == 0)) {
      cat("Finished simulation ", rep, "...\n", sep = "")
    }
  }
  mean_performance <- sums / (sims * m)
  ranked <- heuristics[order(mean_performance, decreasing = TRUE), ]
  return(list(
    ranked = ranked,
    rating = sort(mean_performance, decreasing = TRUE)
  ))
}

# res <- rank_heuristics(heuristics = heuristics, trace = 50, smoothing_factor = 4)
# res$ranked[1:10, ]
# res$rating[1:10]

all_ranks <- function(k = 3, l = 12, n = 200, factors = 1:10,
                      sims = 1000, all_starts = FALSE,
                      seed = NULL, trace = FALSE) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  heuristics <- make_heuristics(k = k, l = l)
  results <- vector(mode = "list", length = length(factors))
  names(results) <- as.character(factors)
  for (factor in factors) {
    cat("Working on smoothing factor", factor, "... \n")
    results[[as.character(factor)]] <- rank_heuristics(
      sims = sims,
      heuristics = heuristics,
      n = n,
      smoothing_factor = factor,
      trace = trace
    )
  }
  results
}

results <-
  all_ranks(
    k = 3, l = 12, n = 200,
    factors = 1:2,
    all_starts = FALSE,
    seed = 3030,
    trace = 100
  )

hist(results[[2]]$rating)

results_3_12_200_1to10_3030 <-
  all_ranks(
    k = 3, l = 12, n = 200,
    factors = 1:10,
    all_starts = FALSE,
    seed = 3030,
    trace = 500
  )

hist(results_3_12_200_1to10_3030[[3]]$rating)

all_try <- function(team, landscape, start) {
  val <- landscape
  solutions <- numeric(nrow(team))
  steps <- 0
  for(i in 1:nrow(team)) {
    member <- team[i, ]
    res <- solve(member, landscape, start)
    solutions[i] <- res$solution
    steps <- steps + res$steps
  }
  highest <- max(val[solutions])
  best_solution <- solutions[val[solutions] == highest][1]
  list(
    solution = best_solution,
    steps = steps,
    proposals = solutions
  )
}

solve_tournament <- function(team, landscape) {
  previous_proposals <- vector(mode = "list", length = nrow(team))
  for (i in 1:length(previous_proposals)) {
    previous_proposals[[i]] <- numeric()
  }
  current <- 1
  steps <- 0
  solving <- TRUE
  debug_limit <- Inf
  counter <- 0
  while (solving  & counter <= debug_limit) {
    members_to_try <- numeric()
    for (i in 1:nrow(team)) {
      if (!(current %in% previous_proposals[[i]])) {
        members_to_try <- c(members_to_try, i)
      }
    }
    res <- all_try(team[members_to_try, ], landscape, current)
    for (i in 1:length(members_to_try)) {
      member <- members_to_try[i]
      previous_proposals[[member]] <- res$proposals[i]
    }
    steps <- steps + res$steps
    if (res$solution == current) {
      solving <- FALSE
    }
    current <- res$solution
    counter <- counter + 1
  }
  list(
    solution = current,
    steps = steps
  )
}

solve_tournament(
  team = res$ranked[1:9, ],
  landscape = make_landscape(
    n = 2000, smoothing_factor = 4, 
    max_value = 100, seed = NULL
    )
)
