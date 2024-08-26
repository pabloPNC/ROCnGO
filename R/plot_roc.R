#' @importFrom ggplot2 ggplot aes labs
plot_roc <- function(
        data = NULL,
        fpr,
        tpr) {
    ggplot(data, mapping = aes(x = {{ fpr }}, y = {{ tpr }})) +
        labs(x = "FPR", y = "TPR")
}

#' @importFrom ggplot2 geom_point
#' @export
plot_roc_points <- function(
        data = NULL,
        fpr,
        tpr) {
    plot_roc(data, fpr, tpr) +
        geom_point()
}

#' @importFrom ggplot2 geom_abline
#' @export
add_chance_line <- function() {
    geom_abline(slope = 1, linetype = "dashed", alpha = 1/5)
}

#' @importFrom ggplot2 geom_path
#' @export
plot_roc_curve <- function(
        data = NULL,
        fpr,
        tpr) {
    plot_roc(data, fpr, tpr) +
        geom_path(size = 0.8)
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
    if (ratio == "sens") {
        add_fpr_threshold_line(threshold)
    } else if (ratio == "spec") {
        add_tpr_threshold_line(threshold)
    }
}

#' @importFrom ggplot2 geom_path
#' @importFrom dplyr filter
#' @export
plot_partial_fpr_curve <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL) {
    plot_roc(data, fpr, tpr) +
        geom_path(
            data = . %>% filter( {{ fpr }} > threshold),
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
    plot_roc(data, fpr, tpr) +
        geom_path(
            data = . %>% filter( {{ tpr }} > threshold),
            size = 0.8
        )
}

#' @importFrom ggplot2 geom_path
#' @importFrom dplyr filter
#' @export
plot_partial_fpr_curve <- function(
        data = NULL,
        fpr,
        tpr,
        threshold = NULL) {
    plot_roc(data, fpr, tpr) +
        geom_path(
            data = . %>% filter( {{ fpr }} > threshold),
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
    plot_roc(data, fpr, tpr) +
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
    if (ratio == "sens") {
        plot_partial_tpr_curve(data, fpr, tpr, threshold)
    } else if (ratio == "spec") {
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
    plot_roc(data, fpr, tpr) +
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
    plot_roc(data, fpr, tpr) +
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
