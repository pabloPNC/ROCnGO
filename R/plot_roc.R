#' @importFrom ggplot2 ggplot aes labs
plot_roc <- function(
        data = NULL,
        fpr,
        tpr) {
    ggplot(data, mapping = aes(x = {{ fpr }}, y = {{ tpr }})) +
        labs(x = "FPR", y = "TPR")
}

#' @importFrom ggplot2 geom_point
plot_roc_points <- function(
        data = NULL,
        fpr,
        tpr) {
    plot_roc(data, fpr, tpr) +
        geom_point()
}

#' @importFrom ggplot2 geom_abline
add_chance_line <- function() {
    geom_abline(slope = 1, linetype = "dashed", alpha = 1/5)
}

#' @importFrom ggplot2 geom_path
plot_roc_line <- function(
        data = NULL,
        fpr,
        tpr) {
    plot_roc(data, fpr, tpr) +
        geom_path(size = 0.8)
}

#' @importFrom  ggplot2 geom_vline
add_fpr_threshold_line <- function(
        threshold) {
    geom_vline(xintercept = threshold, linetype = "dashed")
}

#' @importFrom  ggplot2 geom_hline
add_tpr_threshold_line <- function(
        threshold) {
    geom_hline(yintercept = threshold, linetype = "dashed")
}

add_threshold_line <- function(
        threshold,
        ratio = NULL) {
    if (ratio == "sens") {
        add_fpr_threshold_line(threshold)
    } else if (ratio == "spec") {
        add_tpr_threshold_line(threshold)
    }
}
