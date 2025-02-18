#' @importFrom ggplot2 ggplot labs
plot_roc <- function(data) {
    ggplot(data) +
        labs(x = "FPR", y = "TPR", color = "Predictor", fill = "Bound")
}

#' @importFrom ggplot2 geom_point aes
#' @importFrom rlang as_name enquo quo_is_null
#' @export
plot_roc_points <- function(
        data,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL) {
    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)
    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        predictor_name <- as_name(enquo(predictor))
        plot_roc(data) +
            geom_point(
                data = roc_points(data, {{ response }}, {{ predictor }}),
                mapping = aes(
                    x = .data[["fpr"]],
                    y = .data[["tpr"]],
                    color = predictor_name
                ),
                size = 0.8
            )
    } else {
        plot_roc(data) +
            geom_point(
                mapping = aes(
                    x = {{ fpr }},
                    y = {{ tpr }}
                ),
                size = 0.8
            )
    }

}

#' @importFrom ggplot2 geom_path aes
#' @importFrom rlang as_name enquo quo_is_null quo_is_symbol quo_is_call
#' @export
plot_roc_curve <- function(
        data,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL) {
    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)
    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        if (quo_is_symbol(predictor_expr)) {
            color <- as_name({{ predictor_expr }})
        } else if (quo_is_call(predictor_expr)) {
            expr_args <- rlang::call_args(
                rlang::get_expr(predictor_expr)
            )
            if (expr_args[[1]] == ".data" | expr_args[[1]] == ".env") {
                color <- expr_args[[2]]
            } else {
                color <- NULL
            }
        }
        plot_roc(data) +
            geom_path(
                data = roc_points(data, {{ response }}, {{ predictor }}),
                mapping = aes(
                    x = .data[["fpr"]],
                    y = .data[["tpr"]],
                    color = color
                ),
                size = 0.8
            )
    } else {
        plot_roc(data) +
            geom_path(
                mapping = aes(
                    x = {{ fpr }},
                    y = {{ tpr }}
                ),
                size = 0.8
            )
    }
}

#' @importFrom ggplot2 geom_abline
#' @export
add_chance_line <- function() {
    geom_abline(slope = 1, linetype = "dashed", alpha = 1/5)
}

#' @importFrom ggplot2 aes
#' @importFrom rlang enquo as_name
add_roc_from_predictor <- function(
        data,
        response,
        predictor,
        geom = NULL) {
    predictor_expr <- enquo(predictor)
    predictor_name <- mask_name(predictor_expr)
    if (is.null(data)) {
        geom(
            data = . %>% roc_points({{ response }}, {{ predictor }}),
            mapping = aes(
                x = .data[["fpr"]],
                y = .data[["tpr"]],
                color = predictor_name
            ),
            size = 0.8
        )
    } else {
        geom(
            data = roc_points(data, {{ response }}, {{ predictor }}),
            mapping = aes(
                x = .data[["fpr"]],
                y = .data[["tpr"]],
                color = predictor_name
            ),
            size = 0.8
        )
    }
}

#' @importFrom ggplot2 aes
add_roc_from_ratios <- function(
        data,
        fpr = fpr,
        tpr = tpr,
        geom = NULL) {
    geom(
        data = data,
        mapping = aes(
            x = {{ fpr }},
            y = {{ tpr }}
        ),
        size = 0.8
    )
}

#' @importFrom ggplot2 geom_path
add_roc_curve_from_predictor <- function(
        data,
        response,
        predictor) {
    add_roc_from_predictor(data, {{ response }}, {{ predictor }}, geom_path)
}

#' @importFrom ggplot2 geom_path
add_roc_curve_from_ratios <- function(
        data,
        fpr,
        tpr) {
    add_roc_from_ratios(data, {{ fpr }}, {{ tpr }}, geom_path)
}

#' @importFrom ggplot2 geom_point
add_roc_points_from_predictor <- function(
        data,
        response,
        predictor) {
    add_roc_from_predictor(data, {{ response }}, {{ predictor }}, geom_point)
}

#' @importFrom ggplot2 geom_point
add_roc_points_from_ratios <- function(
        data,
        fpr,
        tpr) {
    add_roc_from_ratios(data, {{ fpr }}, {{ tpr }}, geom_point)
}

#' @importFrom rlang enquo quo_is_null
#' @export
add_roc_curve <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL) {
    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)
    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        add_roc_curve_from_predictor(data, {{ response }}, {{ predictor }})
    } else {
        add_roc_curve_from_ratios(data, {{ fpr }}, {{ tpr }})
    }
}

#' @importFrom rlang as_name enquo quo_is_null
#' @export
add_roc_points <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL) {
    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)
    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        add_roc_points_from_predictor(data, {{ response }}, {{ predictor }})
    } else {
        add_roc_points_from_ratios(data, {{ fpr }}, {{ tpr }})
    }
}

#' @importFrom  ggplot2 geom_vline
#' @export
add_fpr_threshold_line <- function(
        threshold) {
    geom_vline(xintercept = threshold, linetype = "dashed")
}

#' @importFrom  ggplot2 geom_hline
#' @export
add_tpr_threshold_line <- function(
        threshold) {
    geom_hline(yintercept = threshold, linetype = "dashed")
}

#' @export
add_threshold_line <- function(
        threshold,
        ratio = NULL) {
    if (ratio == "fpr") {
        add_fpr_threshold_line(threshold)
    } else if (ratio == "tpr") {
        add_tpr_threshold_line(threshold)
    }
}

#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
add_partial_roc_from_ratios_fpr <- function(
        data,
        fpr,
        tpr,
        threshold,
        geom) {
    geom(
        data = data %>% filter( {{fpr}} <= threshold),
        mapping = aes(
            x = {{ fpr }},
            y = {{ tpr }}
        ),
        size = 0.8
    )
}

add_partial_roc_from_ratios_tpr <- function(
        data,
        fpr,
        tpr,
        threshold,
        geom) {
    geom(
        data = data %>% filter( {{tpr}} >= threshold),
        mapping = aes(
            x = {{ fpr }},
            y = {{ tpr }}
        ),
        size = 0.8
    )
}

add_partial_roc_from_ratios <- function(
        data,
        fpr,
        tpr,
        ratio,
        threshold,
        geom) {
    if (ratio == "tpr") {
        add_partial_roc_from_ratios_tpr(
            data,
            {{ fpr }},
            {{ tpr }},
            threshold,
            geom
        )
    } else if (ratio == "fpr") {
        add_partial_roc_from_ratios_fpr(
            data,
            {{ fpr }},
            {{ tpr }},
            threshold,
            geom
        )
    }
}

#' @importFrom rlang as_name enquo
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
add_partial_roc_from_predictor_tpr <- function(
        data,
        response,
        predictor,
        threshold,
        geom) {
    predictor_name <- as_name(enquo(predictor))
    if (is.null(data)) {
        geom(
            data = . %>%
                roc_points({{ response }}, {{ predictor }}) %>%
                filter(.data[["tpr"]] >= threshold),
            mapping = aes(
                x = .data[["fpr"]],
                y = .data[["tpr"]],
                color = predictor_name
            ),
            size = 0.8
        )
    } else {
        geom(
            data = roc_points(data, {{ response }}, {{ predictor }}) %>%
                filter(.data[["tpr"]] >= threshold),
            mapping = aes(
                x = .data[["fpr"]],
                y = .data[["tpr"]],
                color = predictor_name
            ),
            size = 0.8
        )
    }
}

#' @importFrom rlang as_name enquo
#' @importFrom dplyr filter
#' @importFrom ggplot2 aes
add_partial_roc_from_predictor_fpr <- function(
        data,
        response,
        predictor,
        threshold,
        geom) {
    predictor_name <- as_name(enquo(predictor))
    if (is.null(data)) {
        geom(
            data = . %>%
                roc_points({{ response }}, {{ predictor }}) %>%
                filter(.data[["fpr"]] <= threshold),
            mapping = aes(
                x = .data[["fpr"]],
                y = .data[["tpr"]],
                color = predictor_name
            ),
            size = 0.8
        )
    } else {
        geom(
            data = roc_points(data, {{ response }}, {{ predictor }}) %>%
                filter(.data[["fpr"]] <= threshold),
            mapping = aes(
                x = .data[["fpr"]],
                y = .data[["tpr"]],
                color = predictor_name
            ),
            size = 0.8
        )
    }
}

add_partial_roc_from_predictor <- function(
        data,
        response,
        predictor,
        ratio,
        threshold,
        geom) {
    if (ratio == "tpr") {
        add_partial_roc_from_predictor_tpr(
            data,
            {{ response }},
            {{ predictor }},
            threshold,
            geom
        )
    } else if (ratio == "fpr") {
        add_partial_roc_from_predictor_fpr(
            data,
            {{ response }},
            {{ predictor }},
            threshold,
            geom
        )
    }

}

#' @importFrom rlang quo_is_null enquo
add_partial_roc <- function(
        data,
        fpr,
        tpr,
        response,
        predictor,
        ratio,
        threshold,
        geom) {
    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)
    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        add_partial_roc_from_predictor(
            data,
            {{ response }},
            {{ predictor }},
            ratio,
            threshold,
            geom
        )
    } else {
        add_partial_roc_from_ratios(
            data,
            {{ fpr }},
            {{ tpr }},
            ratio,
            threshold,
            geom
        )
    }
}

#' @importFrom ggplot2 geom_path
#' @export
add_partial_roc_curve <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        ratio,
        threshold) {
    add_partial_roc(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        ratio,
        threshold,
        geom_path
    )
}

#' @importFrom ggplot2 geom_point
#' @export
add_partial_roc_points <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        ratio,
        threshold) {
    add_partial_roc(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        ratio,
        threshold,
        geom_point
    )
}

#' @export
plot_partial_roc_curve <- function(
        data,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        ratio,
        threshold) {
    plot_roc(data) +
        add_partial_roc_curve(
            data,
            {{ fpr }},
            {{ tpr }},
            {{ response }},
            {{ predictor }},
            ratio,
            threshold
        )
}

#' @export
plot_partial_roc_points <- function(
        data,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        ratio,
        threshold) {
    plot_roc(data) +
        add_partial_roc_points(
            data,
            {{ fpr }},
            {{ tpr }},
            {{ response }},
            {{ predictor }},
            ratio,
            threshold
        )
}

#' @importFrom ggplot2 geom_polygon
#' @importFrom tibble tibble
#' @importFrom rlang enquo quo_is_null
#' @export
add_fpauc_partially_proper_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        threshold) {

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)

    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        geom_polygon(
            data = tibble(
                x = c(threshold, 1, 1),
                y = c(threshold, 1, threshold)
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " FpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(threshold, 1, 1),
                y = c(threshold, 1, threshold)
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }
}

#' @importFrom ggplot2 aes geom_polygon
#' @importFrom tibble tibble
#' @importFrom rlang enquo quo_is_null
#' @export
add_fpauc_concave_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        threshold) {

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)

    partial_points <- calc_partial_roc_points(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        threshold,
        1,
        "tpr"
    )
    threshold_fpr <- partial_points[["partial_fpr"]][1]

    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        geom_polygon(
            data = tibble(
                x = c(threshold_fpr, 1, 1),
                y = c(threshold, threshold, 1)
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " FpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(threshold_fpr, 1, 1),
                y = c(threshold, threshold, 1)
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }
}

#' @export
add_fpauc_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        threshold) {
    curve_shape <- calc_curve_shape(
        data,
        fpr,
        tpr,
        response = {{ response }},
        predictor = {{ predictor }},
        lower_threshold = threshold,
        upper_threshold = 1,
        ratio = "tpr"
    )
    if (curve_shape == "Concave") {
        bound <- add_fpauc_concave_lower_bound(
            data,
            fpr,
            tpr,
            response = {{ response }},
            predictor = {{ predictor }},
            threshold
        )
    } else if (curve_shape == "Partially proper") {
        bound <- add_fpauc_partially_proper_lower_bound(
            data,
            fpr,
            tpr,
            response = {{ response }},
            predictor = {{ predictor }},
            threshold
        )
    } else if (curve_shape == "Hook under chance") {
        bound <- NULL
    }
    bound
}

#' @importFrom dplyr slice_max slice_min filter pull
#' @importFrom ggplot2 geom_polygon
#' @export
add_tpauc_concave_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        lower_threshold,
        upper_threshold) {
    partial_points <- calc_partial_roc_points(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        lower_threshold,
        upper_threshold,
        "fpr"
    )
    lower_threshold_tpr <- partial_points %>%
        filter(partial_fpr == lower_threshold) %>%
        slice_min(partial_tpr) %>%
        pull(partial_tpr)
    upper_threshold_tpr <- partial_points %>%
        filter(partial_fpr == upper_threshold) %>%
        slice_max(partial_tpr) %>%
        pull(partial_tpr)

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)

    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        geom_polygon(
            data = tibble(
                x = c(
                    lower_threshold,
                    upper_threshold,
                    upper_threshold,
                    lower_threshold
                ),
                y = c(0, 0, upper_threshold_tpr, lower_threshold_tpr)
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " TpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(
                    lower_threshold,
                    upper_threshold,
                    upper_threshold,
                    lower_threshold
                ),
                y = c(0, 0, upper_threshold_tpr, lower_threshold_tpr)
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }
}

#' @importFrom dplyr slice_min filter pull
#' @importFrom ggplot2 geom_polygon
#' @export
add_tpauc_partially_proper_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        lower_threshold,
        upper_threshold) {

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)

    partial_points <- calc_partial_roc_points(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        lower_threshold,
        upper_threshold,
        "fpr"
    )
    lower_threshold_tpr <- partial_points %>%
        filter(partial_fpr == lower_threshold) %>%
        slice_min(partial_tpr) %>%
        pull(partial_tpr)
    diagonal_area <- calc_fpr_diagonal_lower_bound(
        partial_points[["partial_fpr"]],
        partial_points[["partial_tpr"]]
    )
    square_area <- calc_fpr_square_lower_bound(
        partial_points[["partial_fpr"]],
        partial_points[["partial_tpr"]]
    )

    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        if (diagonal_area > square_area ) {
            geom_polygon(
                data = tibble(
                    x = c(
                        lower_threshold,
                        upper_threshold,
                        upper_threshold,
                        lower_threshold
                    ),
                    y = c(
                        0, 0, upper_threshold, lower_threshold
                    )
                ),
                mapping = aes(
                    x,
                    y,
                    color = mask_name(predictor_expr),
                    fill = str_c(
                        mask_name(predictor_expr),
                        " TpAUC lower bound"
                    )
                ),
                alpha = 1/5,
                linetype = "solid"
            )
        } else if (diagonal_area < square_area) {
            geom_polygon(
                data = tibble(
                    x = c(
                        lower_threshold,
                        upper_threshold,
                        upper_threshold,
                        lower_threshold
                    ),
                    y = c(0, 0, lower_threshold_tpr, lower_threshold_tpr)
                ),
                mapping = aes(
                    x,
                    y,
                    color = mask_name(predictor_expr),
                    fill = str_c(
                        mask_name(predictor_expr),
                        " TpAUC lower bound"
                    )
                ),
                alpha = 1/5,
                linetype = "solid"
            )
        }
    } else {
        if (diagonal_area > square_area ) {
            geom_polygon(
                data = tibble(
                    x = c(
                        lower_threshold,
                        upper_threshold,
                        upper_threshold,
                        lower_threshold
                    ),
                    y = c(
                        0, 0, upper_threshold, lower_threshold
                    )
                ),
                mapping = aes(
                    x,
                    y
                ),
                color = "black",
                alpha = 1/5,
                linetype = "solid"
            )
        } else if (diagonal_area < square_area) {
            geom_polygon(
                data = tibble(
                    x = c(
                        lower_threshold,
                        upper_threshold,
                        upper_threshold,
                        lower_threshold
                    ),
                    y = c(0, 0, lower_threshold_tpr, lower_threshold_tpr)
                ),
                mapping = aes(
                    x,
                    y
                ),
                color = "black",
                alpha = 1/5,
                linetype = "solid"
            )
        }
    }
}

#' @importFrom dplyr slice_min filter pull
#' @importFrom ggplot2 geom_polygon
#' @export
add_tpauc_under_chance_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        lower_threshold,
        upper_threshold) {

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)


    partial_points <- calc_partial_roc_points(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        lower_threshold,
        upper_threshold,
        "fpr"
    )
    lower_threshold_tpr <- partial_points %>%
        filter(partial_fpr == lower_threshold) %>%
        slice_min(partial_tpr) %>%
        pull(partial_tpr)

    if (!quo_is_null(response_expr) & !quo_is_null(predictor_expr)) {
        geom_polygon(
            data = tibble(
                x = c(
                    lower_threshold,
                    upper_threshold,
                    upper_threshold,
                    lower_threshold
                ),
                y = c(0, 0, lower_threshold_tpr, lower_threshold_tpr)
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " TpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(
                    lower_threshold,
                    upper_threshold,
                    upper_threshold,
                    lower_threshold
                ),
                y = c(0, 0, lower_threshold_tpr, lower_threshold_tpr)
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }
}

#' @export
add_tpauc_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        lower_threshold,
        upper_threshold) {
    curve_shape <- calc_curve_shape(
        data,
        fpr,
        tpr,
        response = {{ response }},
        predictor = {{ predictor }},
        lower_threshold = lower_threshold,
        upper_threshold = upper_threshold,
        ratio = "fpr"
    )
    if (curve_shape == "Concave") {
        bound <- add_tpauc_concave_lower_bound(
            data,
            fpr,
            tpr,
            response = {{ response }},
            predictor = {{ predictor }},
            upper_threshold = upper_threshold,
            lower_threshold = lower_threshold
        )
    } else if (curve_shape == "Partially Proper") {
        bound <- add_tpauc_partially_proper_lower_bound(
            data,
            fpr,
            tpr,
            response = {{ response }},
            predictor = {{ predictor }},
            upper_threshold = upper_threshold,
            lower_threshold = lower_threshold
        )
    } else if (curve_shape == "Hook under chance") {
        bound <- add_tpauc_under_chance_lower_bound(
            data,
            fpr,
            tpr,
            response = {{ response }},
            predictor = {{ predictor }},
            upper_threshold = upper_threshold,
            lower_threshold = lower_threshold
        )
    }
    bound
}

#' @importFrom ggplot2 geom_polygon
#' @export
add_npauc_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        threshold) {

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)
    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        geom_polygon(
            data = tibble(
                x = c(threshold, 1, 1),
                y = c(threshold, 1, threshold)
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " NpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(threshold, 1, 1),
                y = c(threshold, 1, threshold)
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }

}

#' @importFrom ggplot2 geom_polygon
#' @importFrom stringr str_c
#' @export
add_npauc_normalized_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        threshold) {
    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)

    if (!quo_is_null(predictor_expr) & !quo_is_null(response_expr)) {
        geom_polygon(
            data = tibble(
                x = c(0, 1, 1),
                y = c(threshold, 1, threshold)
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " NpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(0, 1, 1),
                y = c(threshold, 1, threshold)
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }

}

#' @importFrom ggplot2 geom_polygon aes
#' @importFrom dplyr filter slice_min pull
#' @export
add_spauc_lower_bound <- function(
        data = NULL,
        fpr = NULL,
        tpr = NULL,
        response = NULL,
        predictor = NULL,
        lower_threshold,
        upper_threshold) {

    predictor_expr <- enquo(predictor)
    response_expr <- enquo(response)

    partial_points <- calc_partial_roc_points(
        data,
        {{ fpr }},
        {{ tpr }},
        {{ response }},
        {{ predictor }},
        lower_threshold,
        upper_threshold,
        "fpr"
    )

    if (!quo_is_null(response_expr) & !quo_is_null(predictor_expr)) {
        geom_polygon(
            data = tibble(
                x = c(
                    lower_threshold,
                    upper_threshold,
                    upper_threshold,
                    lower_threshold
                ),
                y = c(
                    0, 0, upper_threshold, lower_threshold
                )
            ),
            mapping = aes(
                x,
                y,
                color = mask_name(predictor_expr),
                fill = str_c(
                    mask_name(predictor_expr),
                    " SpAUC lower bound"
                )
            ),
            alpha = 1/5,
            linetype = "solid"
        )
    } else {
        geom_polygon(
            data = tibble(
                x = c(
                    lower_threshold,
                    upper_threshold,
                    upper_threshold,
                    lower_threshold
                ),
                y = c(
                    0, 0, upper_threshold, lower_threshold
                )
            ),
            mapping = aes(
                x,
                y
            ),
            color = "black",
            alpha = 1/5,
            linetype = "solid"
        )
    }
}

#' @importFrom ggplot2 theme
#' @export
hide_legend <- function() {
    theme(legend.position = "none")
}



