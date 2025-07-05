calc_fpr_diagonal_lower_bound <- function(partial_fpr, partial_tpr) {
  sum(diff(partial_fpr^2)) / 2
}

calc_fpr_square_lower_bound <- function(partial_fpr, partial_tpr) {
  sum(diff(partial_fpr)) * min(partial_tpr)
}

calc_fpr_proper_lower_bound <- function(partial_fpr, partial_tpr) {
  max(
    calc_fpr_diagonal_lower_bound(partial_fpr, partial_tpr),
    calc_fpr_square_lower_bound(partial_fpr, partial_tpr)
  )
}

calc_fpr_plr_lower_bound <- function(partial_fpr, partial_tpr) {
  sum(diff(partial_fpr)) * mean(c(min(partial_tpr), max(partial_tpr)))
}

calc_partial_plr <- function(partial_fpr, partial_tpr) {
  plr <- (partial_tpr - partial_tpr[1]) / (partial_fpr - partial_fpr[1])
  plr <- plr[is.finite(plr)]
}

is_concave_plr <- function(partial_fpr, partial_tpr) {
  plr <- calc_partial_plr(partial_fpr, partial_tpr)
  all(plr >= plr[length(plr)])
}

is_over_chance_line <- function(partial_fpr, partial_tpr) {
  all(partial_tpr >= partial_fpr)
}

calc_fpr_curve_shape <- function(partial_fpr, partial_tpr) {
  if (is_concave_plr(partial_fpr, partial_tpr)) {
    curve_shape <- "Concave"
  } else if (is_over_chance_line(partial_fpr, partial_tpr)) {
    curve_shape <- "Partially Proper"
  } else {
    curve_shape <- "Hook under chance"
  }
  return(curve_shape)
}

calc_fpr_lower_bound <- function(partial_fpr, partial_tpr) {
  curve_shape <- calc_fpr_curve_shape(
    partial_fpr,
    partial_tpr
  )
  lower_square_bound <- calc_fpr_square_lower_bound(partial_fpr, partial_tpr)
  proper_bound <- calc_fpr_proper_lower_bound(partial_fpr, partial_tpr)
  plr_bound <- calc_fpr_plr_lower_bound(partial_fpr, partial_tpr)

  if (curve_shape == "Concave") {
    lower_bound <- plr_bound
  } else if (curve_shape == "Partially Proper") {
    lower_bound <- proper_bound
  } else if (curve_shape == "Hook under chance") {
    lower_bound <- lower_square_bound
  }
  lower_bound
}

calc_fpr_upper_bound <- function(partial_fpr, partial_tpr) {
  if (min(partial_tpr) == max(partial_tpr)) {
    warn_constant_roc()
    upper_bound <- sum(diff(partial_tpr))
  } else {
    upper_bound <- sum(diff(partial_fpr)) * max(partial_tpr)
  }
  return(upper_bound)
}

calc_fpr_bounds <- function(partial_fpr, partial_tpr) {
  list(
    upper_bound = calc_fpr_upper_bound(
      partial_fpr,
      partial_tpr
    ),
    lower_bound = calc_fpr_lower_bound(
      partial_fpr,
      partial_tpr
    )
  )
}

calc_tpr_upper_bound <- function(partial_fpr, partial_tpr) {
  return(
    (1 - partial_fpr[1]) * (1 - partial_tpr[1])
  )
}

calc_tpr_concave_lower_bound <- function(partial_fpr, partial_tpr) {
  return(
    (1 / 2) * (1 - partial_fpr[1]) * (1 - partial_tpr[1])
  )
}

calc_tpr_partial_concave_lower_bound <- function(partial_fpr, partial_tpr) {
  return(
    (1 / 2) * (1 - partial_tpr[1])^2
  )
}

calc_tpr_upper_hook_lower_bound <- function(partial_fpr, partial_tpr) {
  return(0)
}

calc_partial_nlr <- function(partial_fpr, partial_tpr) {
  partial_fpr <- partial_fpr[-length(partial_fpr)]
  partial_tpr <- partial_tpr[-length(partial_tpr)]
  (1 - partial_tpr) / (1 - partial_fpr)
}

is_concave_nlr <- function(partial_fpr, partial_tpr) {
  partial_nlr <- calc_partial_nlr(partial_fpr, partial_tpr)
  partial_nlr_0 <- partial_nlr[1]
  all(partial_nlr <= partial_nlr_0) & (is.finite(partial_nlr_0))
}

is_over_chance_line_nlr <- function(partial_nlr) {
  all(partial_nlr <= 1)
}

calc_tpr_curve_shape <- function(partial_fpr, partial_tpr) {
  partial_nlr <- calc_partial_nlr(partial_fpr, partial_tpr)
  if (is_concave_nlr(partial_fpr, partial_tpr)) {
    curve_shape <- "Concave"
  } else if (is_over_chance_line_nlr(partial_nlr)) {
    curve_shape <- "Partially proper"
  } else {
    curve_shape <- "Hook under chance"
  }
  return(curve_shape)
}

calc_tpr_lower_bound <- function(partial_fpr, partial_tpr) {
  curve_shape <- calc_tpr_curve_shape(
    partial_fpr = partial_fpr,
    partial_tpr = partial_tpr
  )
  if (curve_shape == "Concave") {
    lower_bound <- calc_tpr_concave_lower_bound(
      partial_fpr = partial_fpr,
      partial_tpr = partial_tpr
    )
  } else if (curve_shape == "Partially proper") {
    lower_bound <- calc_tpr_partial_concave_lower_bound(
      partial_fpr = partial_fpr,
      partial_tpr = partial_tpr
    )
  } else if (curve_shape == "Hook under chance") {
    lower_bound <- calc_tpr_upper_hook_lower_bound(
      partial_tpr = partial_tpr,
      partial_fpr = partial_fpr
    )
  }
  return(lower_bound)
}

calc_tpr_bounds <- function(partial_fpr, partial_tpr) {
  list(
    upper_bound = calc_tpr_upper_bound(
      partial_fpr,
      partial_tpr
    ),
    lower_bound = calc_tpr_lower_bound(
      partial_fpr,
      partial_tpr
    )
  )
}

#' @title Calculate curve shape over an specific region
#' @description
#' `calc_curve_shape()` calculates ROC curve shape over a specified region.
#' @returns
#' A string indicating ROC curve shape in the specified region. Result
#' can take any of the following values:
#' * `"Concave"`. ROC curve is concave over the entire specified region.
#' * `"Partially proper"`. ROC curve loses concavity at some point of the
#' specified region.
#' * `"Hook under chance"`. ROC curve loses concavity at some point of the
#' region and it lies below chance line.
#' @inheritParams calc_partial_roc_points
#' @examples
#' # Calc ROC curve shape of Sepal.Width as a classifier of setosa species
#' # in TPR = (0.9, 1)
#' calc_curve_shape(iris, Species, Sepal.Width, 0.9, 1, "tpr")
#' # Change class to virginica
#' calc_curve_shape(iris, Species, Sepal.Width, 0.9, 1, "tpr", .condition = "virginica")
#' @export
calc_curve_shape <- function(
    data = NULL,
    response = NULL,
    predictor = NULL,
    lower_threshold,
    upper_threshold,
    ratio,
    .condition = NULL) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  tpr_fpr <- roc_points(NULL, response, predictor, .condition) %>%
    arrange(.data[["fpr"]], .data[["tpr"]])
  ptpr_pfpr <- calc_partial_roc_points(
    data = tpr_fpr,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio
  )
  if (ratio == "tpr") {
    curve_shape <- calc_tpr_curve_shape(
      ptpr_pfpr$fpr,
      ptpr_pfpr$tpr
    )
  } else if (ratio == "fpr") {
    curve_shape <- calc_fpr_curve_shape(
      ptpr_pfpr$fpr,
      ptpr_pfpr$tpr
    )
  }
  curve_shape
}
