calc_fpr_diagonal_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    sum(diff(partial_fpr^2)) / 2
}

calc_fpr_square_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    sum(diff(partial_fpr)) * min(partial_tpr)
}

calc_fpr_proper_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    max(
        calc_fpr_diagonal_lower_bound(partial_fpr, partial_tpr),
        calc_fpr_square_lower_bound(partial_fpr, partial_tpr)
    )
}

calc_fpr_plr_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    sum(diff(partial_fpr)) * mean(c(min(partial_tpr), max(partial_tpr)))
}

calc_plr <- function(
        partial_fpr,
        partial_tpr) {
    plr <- (partial_tpr - partial_tpr[1]) / (partial_fpr - partial_fpr[1])
    plr <- plr[is.finite(plr)]
}

is_concave <- function(partial_fpr, partial_tpr) {
    plr <- calc_plr(partial_fpr, partial_tpr)
    all(plr >= plr[length(plr)])
}

has_hook <- function(partial_fpr, partial_tpr) {
    all(partial_tpr >= partial_fpr)
}

calc_fpr_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    lower_square_bound <- calc_fpr_square_lower_bound(partial_fpr, partial_tpr)
    proper_bound <- calc_fpr_proper_lower_bound(partial_fpr, partial_tpr)
    plr_bound <- calc_fpr_plr_lower_bound(partial_fpr, partial_tpr)

    if (is_concave(partial_fpr, partial_tpr)) {
        lower_bound <- plr_bound
    } else if (has_hook(partial_fpr, partial_tpr)) {
        lower_bound <- proper_bound
    } else {
        lower_bound <- lower_square_bound
    }
    lower_bound
}

calc_fpr_upper_bound <- function(
        partial_fpr,
        partial_tpr) {
    if (min(partial_tpr) == max(partial_tpr)) {
        warning("Constant ROC curve over the prefixed FPR range")
        upper_bound <- sum(diff(partial_tpr))
    } else {
        upper_bound <- sum(diff(partial_fpr)) * max(partial_tpr)
    }
    return(upper_bound)
}

calc_fpr_bounds <- function(
        partial_fpr,
        partial_tpr) {
    list(
        upper_bound = calc_fpr_upper_bound(
            partial_fpr,
            partial_tpr
        ),
        lower_bound = calc_fpr_lower_bound(
            partial_fpr,
            partial_tpr
        )
    )
}