#' @importFrom magrittr %>%
get_thresholds <- function(data = NULL, predictor) {
    if (!is.null(data)) {
        sorted_pred <- data %>%
            dplyr::arrange({{ predictor }}) %>%
            dplyr::pull({{ predictor }})
    } else {
        sorted_pred <- sort(predictor)
    }
    thresholds <- sorted_pred[-length(sorted_pred)] + diff(sorted_pred) / 2
    thresholds <- c(min(sorted_pred) - 1, thresholds, max(sorted_pred) + 1)
}

#' @importFrom magrittr %>%
calc_tpr <- function(data = NULL, thresholds, response, predictor) {
    if (!is.null(data)) {
        response <- data %>% dplyr::pull({{ response }})
        predictor <- data %>% dplyr::pull({{ predictor }})
    }
    purrr::map_dbl(
        thresholds,
        \(t) sum(((predictor > t) == 1) * (response == 1)) / sum(response == 1)
    )
}

#' @importFrom magrittr %>%
calc_fpr <- function(data = NULL, thresholds, response, predictor) {
    if (!is.null(data)) {
        response <- data %>% dplyr::pull({{ response }})
        predictor <- data %>% dplyr::pull({{ predictor }})
    }
    purrr::map_dbl(
        thresholds,
        \(t) sum(((predictor > t) == 1) * (response == 0)) / sum(response == 0)
    )
}

#' @importFrom magrittr %>%
calc_ratios <- function(data = NULL, thresholds, response, predictor) {
    if (!is.null(data)) {
        response <- data %>% dplyr::pull({{ response }})
        predictor <- data %>% dplyr::pull({{ predictor }})
    }
    result <- purrr::map(
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

#' @importFrom magrittr %>%
roc_points <- function(data = NULL, response, predictor) {
    # TODO: use cal_ratios
    if (!is.null(data)) {
        thresholds <- data %>% get_thresholds({{ predictor }})
        tpr <- data %>% calc_tpr(thresholds, {{ response }}, {{ predictor }})
        fpr <- data %>% calc_fpr(thresholds, {{ response }}, {{ predictor }})
    } else {
        thresholds <- get_thresholds(predictor = predictor)
        ratios <- calc_ratios(
            thresholds = thresholds,
            response = response,
            predictor = predictor
        )
    }
    tibble::tibble(
        tpr = ratios[["tpr"]],
        fpr = ratios[["fpr"]]
    )
}
