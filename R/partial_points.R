calc_indexes <- function(ratio, lower_threshold, upper_threshold) {
    lower_index <- min(which(ratio >= lower_threshold))
    upper_index <- max(which(ratio <= upper_threshold))
    c(
        "lower" = lower_index,
        "upper" = upper_index
    )
}

# Generic function - check interpolation of upper threshold
#interpolate_threshold <- function(threshold, lower_x, upper_x,
    #lower_y, upper_y) {
    #slope <- (threshold - lower_x) / (upper_x - lower_x)
    #lower_y + (upper_y - lower_y) * slope
#}

# TODO: fix returning ratios with whole
interp_lower_threshold <- function(
        ratio,
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
        warning("[*] Lower threshold already included in points ...")
        result <- NULL
    }
    result
}

interp_upper_threshold <- function(
        ratio,
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
        warning("[*] Upper threshold already included in points ...")
        result <- NULL
    }
    result
}

interp_thresholds <- function(
        ratio,
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

add_thresholds <- function(
        ratio,
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

#' @importFrom tibble tibble
#' @importFrom dplyr rename arrange
calc_partial_roc_points_from_ratios <- function(
        data = NULL,
        fpr,
        tpr,
        lower_threshold,
        upper_threshold,
        ratio) {
    if (is.null(data)) {
        roc_points <- tibble(tpr = tpr, fpr = fpr)
    } else {
        roc_points <- data %>%
            rename(
                tpr = {{ tpr }},
                fpr = {{ fpr }}
            )
    }

    roc_points <- roc_points %>% arrange(.data[["tpr"]], .data[["fpr"]])

    if (ratio == "tpr") {
        indexes <- calc_indexes(
            roc_points[["tpr"]],
            lower_threshold,
            upper_threshold
        )
        interp <- interp_thresholds(
            roc_points[["tpr"]],
            roc_points[["fpr"]],
            lower_threshold,
            upper_threshold,
            indexes[["lower"]],
            indexes[["upper"]]
        )
        partial_ratios <- add_thresholds(
            roc_points[["tpr"]][indexes[["lower"]]:indexes[["upper"]]],
            roc_points[["fpr"]][indexes[["lower"]]:indexes[["upper"]]],
            interp[["lower"]],
            interp[["upper"]]
        )
        tibble(
            partial_tpr = partial_ratios[["ratio"]],
            partial_fpr = partial_ratios[["interp_ratio"]]
        )
    } else if (ratio == "fpr") {
        indexes <- calc_indexes(
            roc_points[["fpr"]],
            lower_threshold,
            upper_threshold
        )
        interp <- interp_thresholds(
            roc_points[["fpr"]],
            roc_points[["tpr"]],
            lower_threshold,
            upper_threshold,
            indexes[["lower"]],
            indexes[["upper"]]
        )
        partial_ratios <- add_thresholds(
            roc_points[["fpr"]][indexes[["lower"]]:indexes[["upper"]]],
            roc_points[["tpr"]][indexes[["lower"]]:indexes[["upper"]]],
            interp[["lower"]],
            interp[["upper"]]
        )
        tibble(
            partial_fpr = partial_ratios[["ratio"]],
            partial_tpr = partial_ratios[["interp_ratio"]]
        )
    }
}

#' @importFrom dplyr arrange
#' @importFrom tibble tibble
calc_partial_roc_points_from_predictor <- function(
        data = NULL,
        predictor,
        response,
        lower_threshold,
        upper_threshold,
        ratio) {

    roc_points <- data %>%
        roc_points({{ response }}, {{ predictor }}) %>%
        arrange(.data[["tpr"]], .data[["fpr"]])

    if (ratio == "tpr") {
        indexes <- calc_indexes(
            roc_points[["tpr"]],
            lower_threshold,
            upper_threshold
        )
        interp <- interp_thresholds(
            roc_points[["tpr"]],
            roc_points[["fpr"]],
            lower_threshold,
            upper_threshold,
            indexes[["lower"]],
            indexes[["upper"]]
        )
        partial_ratios <- add_thresholds(
            roc_points[["tpr"]][indexes[["lower"]]:indexes[["upper"]]],
            roc_points[["fpr"]][indexes[["lower"]]:indexes[["upper"]]],
            interp[["lower"]],
            interp[["upper"]]
        )
        tibble(
            partial_tpr = partial_ratios[["ratio"]],
            partial_fpr = partial_ratios[["interp_ratio"]]
        )
    } else if (ratio == "fpr") {
        indexes <- calc_indexes(
            roc_points[["fpr"]],
            lower_threshold,
            upper_threshold
        )
        interp <- interp_thresholds(
            roc_points[["fpr"]],
            roc_points[["tpr"]],
            lower_threshold,
            upper_threshold,
            indexes[["lower"]],
            indexes[["upper"]]
        )
        partial_ratios <- add_thresholds(
            roc_points[["fpr"]][indexes[["lower"]]:indexes[["upper"]]],
            roc_points[["tpr"]][indexes[["lower"]]:indexes[["upper"]]],
            interp[["lower"]],
            interp[["upper"]]
        )
        tibble(
            partial_fpr = partial_ratios[["ratio"]],
            partial_tpr = partial_ratios[["interp_ratio"]]
        )
    }

}

#' @importFrom rlang enquo quo_is_null
#' @export
calc_partial_roc_points <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        lower_threshold,
        upper_threshold,
        ratio) {
    predic_exp <- enquo(predictor)
    resp_exp <- enquo(response)

    if (!quo_is_null(predic_exp) & !quo_is_null(resp_exp)) {
        result <- calc_partial_roc_points_from_predictor(
            data,
            {{ predictor }},
            {{ response }},
            lower_threshold,
            upper_threshold,
            ratio
        )
    } else {
        result <- calc_partial_roc_points_from_ratios(
            data,
            {{ fpr }},
            {{ tpr }},
            lower_threshold,
            upper_threshold,
            ratio
        )
    }
    return(result)
}
