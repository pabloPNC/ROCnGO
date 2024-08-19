#' @importFrom dplyr pull
#' @export
tp_auc <- function(
            data = NULL,
            response,
            predictor,
            lower_fpr,
            upper_fpr) {
    if (!is.null(data)) {
        tpr_fpr <- data %>% roc_points({{ response }}, {{ predictor }})
        tpr <- tpr_fpr %>% pull(tpr)
        fpr <- tpr_fpr %>% pull(fpr)
    } else {
        tpr_fpr <- roc_points(NULL, response, predictor)
        tpr <- tpr_fpr[["tpr"]]
        fpr <- tpr_fpr[["fpr"]]
    }

    if (lower_fpr >= upper_fpr) {
        stop("Error in prefixed FPR range")
    }

    partial_tpr_fpr <- calc_partial_roc_points(
        tpr = tpr,
        fpr = fpr,
        lower_threshold = lower_fpr,
        upper_threshold = upper_fpr,
        ratio = "fpr",
        sort = TRUE,
        include_thresholds = TRUE
    )

    partial_tpr <- partial_tpr_fpr[["partial_tpr"]]
    partial_fpr <- partial_tpr_fpr[["partial_fpr"]]

    # TODO: use better way to compute pauc
    pauc <- sum(
        diff(partial_fpr) *
        apply(
            cbind(
                partial_tpr[-1],
                partial_tpr[-length(partial_tpr)]
            ),
            1,
            mean
        )
    )
    bounds <- calc_fpr_bounds(partial_fpr, partial_tpr)
    lower_bound <- bounds[["lower_bound"]]
    upper_bound <- bounds[["upper_bound"]]

    tpauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
    return(tpauc)
}