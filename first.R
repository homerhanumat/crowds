library(gtools)
library(tidyverse)

## Making a set of heuristics ----

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
  xvals <- c(1, sort(sample(1:n, size = to_pick)), n + 1)
  start_y <- runif(1, 0, max_value)
  yvals <- c(start_y, runif(to_pick, 0, max_value), start_y)
  vals <- approx(
    x = xvals, 
    y = yvals, 
    xout = 1:(n+1)
  )
  vals <- vals$y[-(n+1)]
  move <- sample(2:(n-1), size = 1)
  vals <- c(vals[move:n], vals[1:(move - 1)])
  ## now scale
  vals <- vals / max(yvals) * max_value
  vals
}

## Solving a Landscape ----

solve <- function(heuristic, landscape, start = 1) {
  h_length <- length(heuristic)
  val <- landscape
  current <- start
  best <- val[current]
  alternative <- 0
  strikes <- 0
  steps <- 0
  while (TRUE) {
    try <- steps %% h_length + 1
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

## Testing Individual Heuristics ----

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

## Team Solving Effort ----

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
    if (length(members_to_try) > 0) {
      res <- all_try(team[members_to_try, , drop = FALSE], landscape, current)
      for (i in 1:length(members_to_try)) {
        member <- members_to_try[i]
        previous_proposals[[member]] <- res$proposals[i]
      }
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

comparison <- function(
  sims = 100,
  seed = NULL,
  expert_size = 9,
  expert_limit = 9,
  random_size = 9,
  set
) {
  mean_diffs <- numeric(length(set))
  sd_diffs <- numeric(length(set))
  factors <- as.numeric(names(set))
  for (i in 1:length(set)) {
    solvers = set[[i]]
    print(length(solvers$rating))
    expert_pool <- solvers$ranked[1:expert_limit, , drop = FALSE]
    differences <- numeric(sims)
    for (m in 1:sims) {
      land <- make_landscape(
        n = 200, smoothing_factor = factors[i], 
        max_value = 100, seed = NULL
      )
      rows_to_pick <- sample(1:nrow(expert_pool), expert_size)
      experts <- expert_pool[rows_to_pick, , drop = FALSE]
      expert_sol <- solve_tournament(
        team = experts,
        landscape = land
      )
      rows_to_pick <- sample(1:nrow(solvers$ranked), random_size)
      randoms <- solvers$ranked[rows_to_pick, , drop = FALSE]
      random_sol <- solve_tournament(
        team = randoms,
        landscape = land
      )
      differences[m] <- land[random_sol$solution] - land[expert_sol$solution]
    }
    mean_diffs[i] <- mean(differences)
    sd_diffs[i] <- sd(differences)
  }
  data.frame(
    factor = factors,
    mean_diff = mean_diffs,
    sd_diff = sd_diffs
  )
}


## Make and Save Heuristic Ranks ----

results_3_36_200_1to20_3030 <-
  all_ranks(
    k = 3, l = 36, n = 200,
    factors = 1:20,
    all_starts = FALSE,
    seed = 3030,
    trace = 50
  )

save(results_3_36_200_1to20_3030, file = "data/results_3_36_200_1to20_3030.rda")


## Quick Graphs ----

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

make_graph <- function(sims = 2500,
                       set,
                       expert_size = 9,
                       expert_limit = 9,
                       random_size = 9,
                       #k = 3,
                       l = 12,
                       seed = NULL) {
  lst <- get_bounded_all(bound = l, lst = set)
  comparison(
    sims = sims,
    seed = seed,
    expert_size = expert_size,
    expert_limit = expert_limit,
    random_size = random_size,
    set = lst
  ) %>% 
    ggplot(aes(x = factor, y = mean_diff)) +
    geom_ribbon(aes(ymin = mean_diff - 2 * sd_diff/sqrt(sims), 
                    ymax = mean_diff + 2 * sd_diff/sqrt(sims)),
                fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = mean_diff)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = 1:20) +
    labs(x = "mean length of a run",
         y = "mean of differences (expert - other)",
         title = paste0("Heuristic bound is ", l, "."))
}

## example of use:
make_graph(sims = 500, set = results_3_36_200_1to20_3030,
           expert_size = 9, expert_limit = 9, l = 12)

## Stars? ----

results_6_12_200_1to20_3030 <-
  all_ranks(
    k = 6, l = 12, n = 200,
    factors = 6,
    sims = 100,
    all_starts = FALSE,
    seed = 3030,
    trace = 2
  )
results_6_12_200_6_3030 <- results_6_12_200_1to20_3030
save(results_6_12_200_6_3030, file = "data/results_6_12_200_6_3030.rda")

hist(get_bounded(bound = 12, results_3_36_200_1to20_3030[["6"]])$rating)

comparison2 <- function(
  sims = 100,
  seed = NULL,
  expert_size = 9,
  expert_limit = 9,
  random_size = 9,
  expert_set,
  random_set
) {
  mean_diffs <- numeric(length(expert_set))
  sd_diffs <- numeric(length(expert_set))
  factors <- as.numeric(names(expert_set))
  for (i in 1:length(expert_set)) {
    expert_solvers = expert_set[[i]]
    random_solvers <- random_set[[i]]
    expert_pool <- expert_solvers$ranked[1:expert_limit, , drop = FALSE]
    differences <- numeric(sims)
    for (m in 1:sims) {
      land <- make_landscape(
        n = 200, smoothing_factor = factors[i], 
        max_value = 100, seed = NULL
      )
      rows_to_pick <- sample(1:nrow(expert_pool), expert_size)
      experts <- expert_pool[rows_to_pick, , drop = FALSE]
      expert_sol <- solve_tournament(
        team = experts,
        landscape = land
      )
      rows_to_pick <- sample(1:nrow(random_solvers$ranked), random_size)
      randoms <- random_solvers$ranked[rows_to_pick, , drop = FALSE]
      random_sol <- solve_tournament(
        team = randoms,
        landscape = land
      )
      differences[m] <- land[random_sol$solution] - land[expert_sol$solution]
    }
    mean_diffs[i] <- mean(differences)
    sd_diffs[i] <- sd(differences)
  }
  data.frame(
    factor = factors,
    mean_diff = mean_diffs,
    sd_diff = sd_diffs
  )
}

comparison2(
  sims = 2500,
  seed = NULL,
  expert_size = 2,
  expert_limit = 665280,
  random_size = 2,
  expert_set = results_6_12_200_6_3030,
  random_set = list(get_bounded(bound = 12, results_3_36_200_1to20_3030[[6]]))
)

## Pool as Sample ----

## What if the actual pool in only a random subset of possible heurustics?
get_sample <- function(set, prop) {
  samp <- vector(mode = "list", length = length(set))
  names(samp) <- names(set)
  for (i in 1:length(set)) {
    pop <- set[[i]]
    m <- length(pop$rating)
    to_pick <- sample(1:m, size = floor(prop * m))
    samp[[i]] <- list(
      ranked = pop$ranked[to_pick, ],
      rating = pop$rating[to_pick]
    )
  }
  samp
}

samps <- get_sample(results_3_36_200_1to20_3030, 0.3)

make_graph(sims = 2500, set = samps,
           expert_size = 9, expert_limit = 9, l = 10)

all_ranks_samp <- function(k = 3, l = 12, n = 200, factors = 1:10,
                      sims = 1000, all_starts = FALSE,
                      seed = NULL, trace = FALSE,
                      sample_size) {
  if (!is.null(seed)) {
    set.seed(seed)
  }
  heuristics <- make_heuristics(k = k, l = l)
  to_pick <- sample(1:nrow(heuristics), size = sample_size)
  heuristics <- heuristics[to_pick, ]
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

res <- all_ranks_samp(
  k = 6, l = 12, n = 200, factors = 1:20,
  sims = 1000, seed = 3030, trace = 100,
  sample_size = 100
)

make_graph_2 <- function(sims = 2500,
                       expert_set,
                       random_set,
                       expert_size = 9,
                       expert_limit = 9,
                       random_size = 9,
                       #k = 3,
                       l = 12,
                       seed = NULL) {
  expert_lst <- get_bounded_all(bound = l, lst = expert_set)
  random_lst <- get_bounded_all(bound = l, lst = random_set)
  comparison2(
    sims = sims,
    seed = seed,
    expert_size = expert_size,
    expert_limit = expert_limit,
    random_size = random_size,
    expert_set = expert_lst,
    random_set = random_lst
  ) %>% 
    ggplot(aes(x = factor, y = mean_diff)) +
    geom_ribbon(aes(ymin = mean_diff - 2 * sd_diff/sqrt(sims), 
                    ymax = mean_diff + 2 * sd_diff/sqrt(sims)),
                fill = "grey70", alpha = 0.5) +
    geom_line(aes(y = mean_diff)) +
    geom_hline(yintercept = 0) +
    scale_x_continuous(breaks = 1:20) +
    labs(x = "mean length of a run",
         y = "mean of differences (expert - other)",
         title = paste0("Heuristic bound is ", l, "."))
}

make_graph_2(
  sims = 2500,
  expert_set = res,
  random_set = results_3_36_200_1to20_3030,
  expert_size = 5,
  expert_limit = 5,
  random_size = 10,
  l = 12
)

by_row_max <- function(mat) {
  lst <- vector(mode = "list", length = ncol(mat))
  for (j in 1:ncol(mat)) {
    lst[[j]] <- mat[, j]
  }
  maxes <- do.call("pmax", args = lst)
  print("call done")
  ord <- order(maxes)
  print("done!")
  list(
    maxes <- maxes[ord],
    mat =  mat[ord, ]
  )
}

by_row_max(permutations(n = 5, r = 3))

sample_from_heuristics <- function(heuristics, prop = 0.01) {
  results <-  by_row_max(heuristics)
  print("sorted")
  heuristics <- results$mat
  maxes <- results$maxes
  min_max <- maxes[1]
  max_max <- maxes[length(maxes)]
  cols <- ncol(heuristics)
  sampled <- matrix(0, nrow = 0, ncol = cols)
  for (m in min_max:max_max) {
    print(m)
    h <- heuristics[maxes == m, ]
    n <- nrow(h)
    pick <- ifelse(prop * n <= 500, 1:n, sample(1:n, floor(prop * n)))
    sampled <- rbind(sampled, heuristics[pick, ])
  }
  sampled
}

res <- sample_from_heuristics(permutations(36, 6), prop = 0.001)
