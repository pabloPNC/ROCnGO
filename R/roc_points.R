get_thresholds <- function(data, predictor) {
    sorted_pred <- data %>%
        dplyr::arrange(dplyr::desc({{ predictor }})) %>%
        dplyr::pull({{ predictor }})
    tibble::tibble(
        thresholds = sorted_pred[-length(sorted_pred)] +
            diff(sorted_pred) / 2,
        ) %>%
        tibble::add_row(
            thresholds = max(sorted_pred) + 1,
            .before = 1
        ) %>%
        tibble::add_row(
            thresholds = min(sorted_pred) - 1
        ) %>%
        dplyr::pull(var = 1)
}

calc_tpr <- function(data, thresholds, response, predictor) {
    response <- data %>% dplyr::pull({{ response }})
    predictor <- data %>% dplyr::pull({{ predictor }})
    purrr::map_dbl(
        thresholds,
        \(t) sum(((predictor > t) == 1) * (response == 1)) / sum(response == 1)
    )
}

calc_fpr <- function(data, thresholds, response, predictor) {
    response <- data %>% dplyr::pull({{ response }})
    predictor <- data %>% dplyr::pull({{ predictor }})
    purrr::map_dbl(
        thresholds,
        \(t) sum(((predictor > t) == 1) * (response == 0)) / sum(response == 0)
    )
}

roc_points <- function(data, response, predictor) {
    thresholds <- data %>% get_thresholds({{ predictor }})
    tpr <- data %>% calc_tpr(thresholds, {{ response }}, {{ predictor }})
    fpr <- data %>% calc_fpr(thresholds, {{ response }}, {{ predictor }})
    tibble::tibble(
        tpr = tpr,
        fpr = fpr
    )
}
