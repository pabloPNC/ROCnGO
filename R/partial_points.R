calc_indexes <- function(ratio, lower_threshold, upper_threshold) {
  lower_index <- min(which(ratio >= lower_threshold))
  upper_index <- max(which(ratio <= upper_threshold))
  c(
    "lower" = lower_index,
    "upper" = upper_index
  )
}

interp_lower_threshold <- function(ratio,
                                   interp_ratio,
                                   lower_threshold,
                                   lower_index) {
  previous_index <- max(lower_index - 1, 1)

  if (ratio[lower_index] > lower_threshold) {
    lower_scale <- (lower_threshold - ratio[previous_index]) /
      (ratio[lower_index] - ratio[previous_index])

    interp_point <- interp_ratio[previous_index] +
      (interp_ratio[lower_index] - interp_ratio[previous_index]) *
        lower_scale

    result <- list(
      threshold = lower_threshold,
      interp_point = interp_point
    )
  } else if (ratio[lower_index] == lower_threshold) {
    inform_lower_threshold(lower_threshold)
    result <- NULL
  }
  result
}

interp_upper_threshold <- function(ratio,
                                   interp_ratio,
                                   upper_threshold,
                                   upper_index) {
  next_index <- min(upper_index + 1, length(ratio))

  if (ratio[upper_index] < upper_threshold) {
    upper_scale <- (ratio[next_index] - upper_threshold) /
      (ratio[next_index] - ratio[upper_index])

    interp_point <- interp_ratio[next_index] -
      (interp_ratio[next_index] - interp_ratio[upper_index]) *
        upper_scale

    result <- list(
      threshold = upper_threshold,
      interp_point = interp_point
    )
  } else if (ratio[upper_index] == upper_threshold) {
    inform_upper_threshold(upper_threshold)
    result <- NULL
  }
  result
}

interp_thresholds <- function(ratio,
                              interp_ratio,
                              lower_threshold,
                              upper_threshold,
                              lower_index,
                              upper_index) {
  lower_interp <- interp_lower_threshold(
    ratio,
    interp_ratio,
    lower_threshold,
    lower_index
  )
  upper_interp <- interp_upper_threshold(
    ratio,
    interp_ratio,
    upper_threshold,
    upper_index
  )
  list(
    lower = lower_interp,
    upper = upper_interp
  )
}

add_thresholds <- function(ratio,
                           interp_ratio,
                           lower_interp,
                           upper_interp) {
  result <- list(
    ratio = ratio,
    interp_ratio = interp_ratio
  )
  if (is.null(lower_interp) == FALSE) {
    result[["ratio"]] <- c(
      lower_interp[["threshold"]],
      result[["ratio"]]
    )
    result[["interp_ratio"]] <- c(
      lower_interp[["interp_point"]],
      result[["interp_ratio"]]
    )
  }
  if (is.null(upper_interp) == FALSE) {
    result[["ratio"]] <- c(
      result[["ratio"]],
      upper_interp[["threshold"]]
    )
    result[["interp_ratio"]] <- c(
      result[["interp_ratio"]],
      upper_interp[["interp_point"]]
    )
  }
  result
}

#' @title Calculate ROC curve partial points
#' @description
#' Calculates a series pairs of (FPR, TPR) which correspond to ROC curve points
#' in a specified region.
#' @inheritParams roc_points
#' @param lower_threshold,upper_threshold Two numbers between 0 and 1,
#' inclusive.
#' These numbers represent lower and upper bounds of the region where to
#' apply calculations.
#' @param ratio Ratio or axis where to apply calculations.
#'
#' * If `"tpr"`, only points within the specified region of TPR, y axis, will be
#' considered for calculations.
#' * If `"fpr"`, only points within the specified region of FPR, x axis, will be
#' considered for calculations.
#'
#' @returns
#' A tibble with two columns:
#'
#' * "partial_tpr". Containing "true positive ratio", or y, values of points
#' within the specified region.
#' * "partial_fpr". Containing "false positive ratio", or x, values of points
#' within the specified region.
#' @examples
#' # Calc ROC points of Sepal.Width as a classifier of setosa species
#' # in TPR = (0.9, 1)
#' calc_partial_roc_points(
#'  iris,
#'  response = Species,
#'  predictor = Sepal.Width,
#'  lower_threshold = 0.9,
#'  upper_threshold = 1,
#'  ratio = "tpr"
#' )
#'
#' # Change class to virginica
#' calc_partial_roc_points(
#'  iris,
#'  response = Species,
#'  predictor = Sepal.Width,
#'  lower_threshold = 0.9,
#'  upper_threshold = 1,
#'  ratio = "tpr",
#'  .condition = "virginica"
#' )
#' @export
calc_partial_roc_points <- function(data = NULL,
                                    response = NULL,
                                    predictor = NULL,
                                    lower_threshold,
                                    upper_threshold,
                                    ratio,
                                    .condition = NULL) {
  UseMethod("calc_partial_roc_points", data)
}

#' @export
calc_partial_roc_points.ratio_df <- function(data = NULL,
                                             response = NULL,
                                             predictor = NULL,
                                             lower_threshold,
                                             upper_threshold,
                                             ratio,
                                             .condition = NULL) {
  ratios <- c("tpr", "fpr")
  int_ratio <- ratios[ratio != ratios]
  indexes <- calc_indexes(
    data[[ratio]], lower_threshold, upper_threshold
  )
  resignal_thresholds({
    interp <- interp_thresholds(
      ratio = data[[ratio]],
      interp_ratio = data[[int_ratio]],
      lower_threshold = lower_threshold,
      upper_threshold = upper_threshold,
      lower_index = indexes[["lower"]],
      upper_index = indexes[["upper"]]
    )
  })
  pratios <- add_thresholds(
    ratio = data[[ratio]][indexes[["lower"]]:indexes[["upper"]]],
    interp_ratio = data[[int_ratio]][indexes[["lower"]]:indexes[["upper"]]],
    lower_interp = interp[["lower"]],
    upper_interp = interp[["upper"]]
  )
  if (ratio == "fpr") {
    new_ratio_df(
      fpr = pratios[["ratio"]],
      tpr = pratios[["interp_ratio"]]
    )
  } else if (ratio == "tpr") {
    new_ratio_df(
      tpr = pratios[["ratio"]],
      fpr = pratios[["interp_ratio"]]
    )
  }
}

#' @export
calc_partial_roc_points.NULL <- function(data = NULL,
                                         response = NULL,
                                         predictor = NULL,
                                         lower_threshold,
                                         upper_threshold,
                                         ratio,
                                         .condition = NULL) {
  ratios <- roc_points(
    data = data,
    response = response,
    predictor = predictor,
    .condition = .condition
  ) %>%
    arrange(.data[["tpr"]], .data[["fpr"]])
  calc_partial_roc_points.ratio_df(
    data = ratios,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio
  )
}

#' @export
calc_partial_roc_points.data.frame <- function(data = NULL,
                                               response = NULL,
                                               predictor = NULL,
                                               lower_threshold,
                                               upper_threshold,
                                               ratio,
                                               .condition = NULL) {
  response <- pull(data, {{ response }})
  predictor <- pull(data, {{ predictor }})
  calc_partial_roc_points.NULL(
    data = NULL,
    response = response,
    predictor = predictor,
    lower_threshold = lower_threshold,
    upper_threshold = upper_threshold,
    ratio = ratio,
    .condition = .condition
  )
}
