#' @importFrom ggplot2 ggplot labs
plot_roc <- function(data) {
    ggplot(data) +
        labs(x = "FPR", y = "TPR", color = "Predictor")
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
#' @importFrom rlang as_name enquo quo_is_null
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
        predictor_name <- as_name(enquo(predictor))
        plot_roc(data) +
            geom_path(
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
    predictor_name <- as_name(enquo(predictor))
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
    if (ratio == "tpr") {
        add_fpr_threshold_line(threshold)
    } else if (ratio == "fpr") {
        add_tpr_threshold_line(threshold)
    }
}



# FIX here ----------------------------------------------------------------


add_partial_roc <- function(
        data,
        fpr,
        tpr,
        response,
        predictor,
        ratio = NULL,
        threshold) {

}

#' @importFrom ggplot2 geom_path
#' @importFrom dplyr filter
#' @export
plot_partial_fpr_curve <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL) {
    plot_roc(data) +
        geom_path(
            data = . %>% filter( {{ fpr }} < threshold),
            size = 0.8
        )
}

#' @importFrom ggplot2 geom_path
#' @importFrom dplyr filter
#' @export
plot_partial_tpr_curve <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL) {
    plot_roc(data, {{ fpr }}, {{ tpr }}, {{ response }}, {{ predictor }}) +
        geom_path(
            data = . %>% filter( {{ tpr }} > threshold),
            size = 0.8
        )
}

#' @export
plot_partial_roc_curve <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL,
        ratio = NULL) {
    if (ratio == "tpr") {
        plot_partial_tpr_curve(data, fpr, tpr, threshold)
    } else if (ratio == "fpr") {
        plot_partial_fpr_curve(data, fpr, tpr, threshold)
    }
}

#' @importFrom ggplot2 geom_point
#' @importFrom dplyr filter
#' @export
plot_partial_fpr_points <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL) {
    plot_roc(data, {{ fpr }}, {{ tpr }}, {{ response }}, {{ predictor }}) +
        geom_point(
            data = . %>% filter( {{ fpr }} > threshold)
        )
}

#' @importFrom ggplot2 geom_point
#' @importFrom dplyr filter
#' @export
plot_partial_tpr_points <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL) {
    plot_roc(data, {{ fpr }}, {{ tpr }}, {{ response }}, {{ predictor }}) +
        geom_point(
            data = . %>% filter( {{ tpr }} > threshold)
        )
}

#' @export
plot_partial_roc_points <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL,
        ratio = NULL) {
    if (ratio == "sens") {
        plot_partial_tpr_points(data, fpr, tpr, threshold)
    } else if (ratio == "spec") {
        plot_partial_fpr_points(data, fpr, tpr, threshold)
    }
}
