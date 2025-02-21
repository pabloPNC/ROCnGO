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

calc_tpr <- function(data = NULL, thresholds, response, predictor) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  if (!all(levels(response) == c(0, 1))) {
    response <- transform_response(response)
  }
  purrr::map_dbl(
    thresholds,
    \(t) sum(((predictor > t) == 1) * (response == 1)) / sum(response == 1)
  )
}

calc_fpr <- function(data = NULL, thresholds, response, predictor) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  if (!all(levels(response) == c(0, 1))) {
    response <- transform_response(response)
  }
  purrr::map_dbl(
    thresholds,
    \(t) sum(((predictor > t) == 1) * (response == 0)) / sum(response == 0)
  )
}

calc_ratios <- function(data = NULL, thresholds, response, predictor) {
  if (!is.null(data)) {
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})
  }
  if (!all(levels(response) == c(0, 1))) {
    response <- transform_response(response)
  }
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

#' @title Calculate curve ROC points
#' @description
#' Calculates a series of pairs of (FPR, TPR) points which correspond to ROC
#' curve displayed points. X axis will represent "false positive ratio", while Y
#' axis the "true positive ratio".
#' @param data A data.frame or extension (e.g. a tibble) containing values for
#' predictors and response variables.
#' @param response A data variable which must be a factor, integer or character
#' vector representing the class to predict on each observation (Gold Standard),
#' (see details).
#' @param predictor A data variable wich must be numeric, representing values of
#' a classifier or predictor for each observation.
#' @returns
#' A tibble, with two columns: "tpr", which contains values for "true positive
#' ratio" or y axis, and "fpr", which contains values for "false positive ratio"
#' or x axis.
#' @details
#' When performing calculations response will be transformed internally to a
#' factor with levels (0,1), which represent absence or presence of the class we
#' want to predict. In order to select which class is the one to predict among
#' availables in response, function follows some criteria according to var type:
#' 
#' * integer. When working with an integer vector, function will consider the
#' smallest as the class to predict.
#' * character. When working with a character vector, function will consider the
#' first value after using [sort()] over all posible options.
#' * factor. When working with a factor var, function will select first class in
#' [levels()].
#' 
#' Al other classes which are not identified as the class to predict will be
#' collapsed into 0. For this reason, try to provide convenient names when
#' working or alternatively provide a factor where levels are equal to 0 and 1,
#' being 1 cases with the condition of interest.
#' @section Data Masking variables:
#' Both response and predictor support `data-masking` variables. For more
#' information please check [rlang::args_data_masking].
#'
#' Nevertheless, function may use environment variables as well. In order to use
#' this type of variables simply set `data = NULL` and provide response and
#' predictor as normal.
#' @examples
#' NULL
#' @export
roc_points <- function(data = NULL, response, predictor) {
  if (!is.null(data)) {
    thresholds <- data %>% get_thresholds({{ predictor }})
    tpr <- data %>% calc_tpr(thresholds, {{ response }}, {{ predictor }})
    fpr <- data %>% calc_fpr(thresholds, {{ response }}, {{ predictor }})
    result <- tibble::tibble(
      tpr = tpr,
      fpr = fpr
    )
  } else {
    thresholds <- get_thresholds(predictor = predictor)
    ratios <- calc_ratios(
      thresholds = thresholds,
      response = response,
      predictor = predictor
    )
    result <- tibble::tibble(
      tpr = ratios[["tpr"]],
      fpr = ratios[["fpr"]]
    )
  }
  return(result)
}
