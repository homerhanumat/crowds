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
  vals
}

df <-
  data.frame(
    n = 1:200,
    v = make_landscape(n = 200, smoothing_factor = 10)
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
  
results_3_12_200_1to10_3030 <-
  all_ranks(
    k = 3, l = 12, n = 200,
    factors = 1:10,
    all_starts = FALSE,
    seed = 3030,
    trace = 500
  )

results_3_12_200_11to20_3030 <-
  all_ranks(
    k = 3, l = 12, n = 200,
    factors = 11:20,
    all_starts = FALSE,
    seed = 3030,
    trace = 500
  )

results_3_12_200_1to20 <-
  c(
    results_3_12_200_1to10_3030,
    results_3_12_200_11to20_3030
  )

sims <- 2500
comparison(set = results_3_12_200_1to20, sims = sims, 
           expert_size = 9, expert_limit = 30) %>% 
  ggplot(aes(x = factor, y = mean_diff)) +
  geom_ribbon(aes(ymin = mean_diff - 2 * sd_diff/sqrt(sims), 
                  ymax = mean_diff + 2 * sd_diff/sqrt(sims)),
              fill = "grey70", alpha = 0.5) +
  geom_line(aes(y = mean_diff)) +
  geom_hline(yintercept = 0) +
  scale_x_continuous(breaks = 1:20) +
  labs(x = "mean length of a run",
       y = "mean of differences (expert - other)")


results_3_36_200_1to20_3030 <-
  all_ranks(
    k = 3, l = 36, n = 200,
    factors = 1:20,
    all_starts = FALSE,
    seed = 3030,
    trace = 50
  )
save(results_3_36_200_1to20_3030, file = "data/results_3_36_200_1to20_3030.rda")

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

stuff <- get_bounded_all(bound = 24, lst = results_3_36_200_1to20_3030)

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

make_graph(sims = 500, set = results_3_36_200_1to20_3030,
           expert_size = 9, expert_limit = 30)
