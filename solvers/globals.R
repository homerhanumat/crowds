library(gtools)
library(tidyverse)

## Settings

settings <- list(
  options = 200,
  max_smoothness = 100,
  max_value = 100
)

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
  xvals <- c(1, sort(sample(2:n, size = to_pick)), n + 1)
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

solve_landscape <- function(heuristic, landscape, start = 1) {
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


