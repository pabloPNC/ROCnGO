calculate_lower_bound <- function(fpr, tpr) {
    # TODO: Try to avoid min/max calculation if fixed value (sorted)
    diagonal_pauc <- sum(diff(fpr^2)) / 2
    lower_square_pauc <- sum(diff(fpr)) * min(tpr)
    proper_roc_lower_bound <- max(diagonal_pauc, lower_square_pauc)
    upper_diagonal <- sum(diff(fpr)) * mean(c(min(tpr), max(tpr)))

    partial_plr <- (tpr - tpr[1]) / (fpr - fpr[1])
    partial_plr <- partial_plr[is.finite(partial_plr)]

    if (all(partial_plr >= partial_plr[length(partial_plr)])) {
        lower_bound <- upper_diagonal
    } else {
        if (all(tpr >= fpr)) {
            lower_bound <- proper_roc_lower_bound
        } else {
            lower_bound <- lower_square_pauc
        }
    }
    return(lower_bound)
}

calculate_upper_bound <- function(fpr, tpr) {
    if (min(tpr) == max(tpr)) {
        warning("Constant ROC curve over the prefixed FPR range")
        upper_bound <- sum(diff(tpr))
    } else {
        upper_bound <- sum(diff(fpr)) * max(tpr)
    }
}