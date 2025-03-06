get_thresholds <- function(data = NULL, predictor) {
  if (!is.null(data)) {
    sorted_pred <- data %>%
      arrange({{ predictor }}) %>%
      pull({{ predictor }})
  } else {
    sorted_pred <- sort(predictor)
  }
  thresholds <- sorted_pred[-length(sorted_pred)] + diff(sorted_pred) / 2
  thresholds <- c(min(sorted_pred) - 1, thresholds, max(sorted_pred) + 1)
}

calc_tpr <- function(data = NULL,
                     thresholds,
                     response,
                     predictor,
                     .condition = NULL) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  response <- as_response(response, .condition)
  purrr::map_dbl(
    thresholds,
    \(t) sum(((predictor > t) == 1) * (response == 1)) / sum(response == 1)
  )
}

calc_fpr <- function(data = NULL,
                     thresholds,
                     response,
                     predictor,
                     .condition = NULL) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  response <- as_response(response, .condition)
  purrr::map_dbl(
    thresholds,
    \(t) sum(((predictor > t) == 1) * (response == 0)) / sum(response == 0)
  )
}

calc_ratios <- function(data = NULL,
                        thresholds,
                        response,
                        predictor,
                        .condition = NULL) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  response <- as_response(response, .condition)
  result <- map(
    thresholds,
    \(t) list(
      tpr = sum(((predictor > t) == 1) * (response == 1)) /
        sum(response == 1),
      fpr = sum(((predictor > t) == 1) * (response == 0)) /
        sum(response == 0)
    )
  )
  purrr::list_transpose(result)
}

#' @title Calculate ROC curve points
#' @description
#' Calculates a series pairs of (FPR, TPR) which correspond to
#' points displayed by ROC curve. "false positive ratio" will be represented on
#' x axis, while "true positive ratio" on y one.
#' @param data A data.frame or extension (e.g. a tibble) containing values for
#' predictors and response variables.
#' @param response A data variable which must be a factor, integer or character
#' vector representing the prediction outcome on each observation
#' (*Gold Standard*).
#'
#' If the variable presents more than two possible outcomes, classes or
#' categories:
#'
#' * The outcome of interest (the one to be predicted) will remain distinct.
#' * All other categories will be combined into a single category.
#'
#' New combined category represents the "absence" of the condition to predict.
#' See `.condition` for more information.
#' @param predictor A data variable which must be numeric, representing values
#' of a classifier or predictor for each observation.
#' @param .condition A value from response that represents class, category or
#' condition of interest which wants to be predicted.
#'
#' If `NULL`, condition of interest will be selected automatically depending on
#' `response` type.
#'
#' Once the class of interest is selected, rest of them will be collapsed in a
#' common category, representing the "absence" of the condition to be predicted.
#'
#' See `vignette("selecting-condition")` for further information on how
#' automatic selection is performed and details on selecting the condition of
#' interest.
#' @returns
#' A tibble with two columns:
#'
#' * "tpr". Containing values for "true positive ratio", or y axis.
#' * "fpr". Containing values for "false positive ratio", or x axis.

#' @examples
#' # Calc ROC points of Sepal.Width as a classifier of setosa species
#' roc_points(iris, Species, Sepal.Width)
#' # Change class to predict to virginica
#' roc_points(iris, Species, Sepal.Width, .condition = "virginica")
#' @export
roc_points <- function(data = NULL,
                       response,
                       predictor,
                       .condition = NULL) {
  if (!is.null(data)) {
    thresholds <- data %>% get_thresholds({{ predictor }})
    tpr <- data %>%
      calc_tpr(thresholds, {{ response }}, {{ predictor }}, .condition)
    fpr <- data %>%
      calc_fpr(thresholds, {{ response }}, {{ predictor }}, .condition)
    result <- tibble::tibble(
      tpr = tpr,
      fpr = fpr
    )
  } else {
    thresholds <- get_thresholds(predictor = predictor)
    ratios <- calc_ratios(
      thresholds = thresholds,
      response = response,
      predictor = predictor,
      .condition = .condition
    )
    result <- tibble::tibble(
      tpr = ratios[["tpr"]],
      fpr = ratios[["fpr"]]
    )
  }
  return(result)
}
