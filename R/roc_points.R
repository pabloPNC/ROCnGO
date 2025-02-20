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
