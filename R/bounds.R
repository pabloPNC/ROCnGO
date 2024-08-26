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

calc_partial_plr <- function(
        partial_fpr,
        partial_tpr) {
    plr <- (partial_tpr - partial_tpr[1]) / (partial_fpr - partial_fpr[1])
    plr <- plr[is.finite(plr)]
}

is_concave <- function(partial_fpr, partial_tpr) {
    plr <- calc_partial_plr(partial_fpr, partial_tpr)
    all(plr >= plr[length(plr)])
}

is_over_chance_line <- function(partial_fpr, partial_tpr) {
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
    } else if (is_over_chance_line(partial_fpr, partial_tpr)) {
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

calc_tpr_upper_bound <- function(
        partial_fpr,
        partial_tpr) {
    return(
        (1 - partial_fpr[1])*(1 - partial_tpr[1])
    )
}

calc_tpr_concave_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    return(
        (1/2)*(1 - partial_fpr[1])*(1 - partial_tpr[1])
    )
}

calc_tpr_partial_concave_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    return(
        (1/2)*(1 - partial_tpr[1])^2
    )
}

calc_tpr_upper_hook_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    return(0)
}

calc_nlr <- function(
        partial_fpr,
        partial_tpr) {
    partial_fpr <- partial_fpr[-length(partial_fpr)]
    partial_tpr <- partial_tpr[-length(partial_tpr)]
    (1 - partial_tpr)/(1 - partial_fpr)
}

is_concave_tpr <- function(
        partial_nlr) {
    partial_nlr_0 <- partial_nlr[1]
    all(partial_nlr <= partial_nlr_0) & (is.finite(partial_nlr_0))
}

is_partially_concave_tpr <- function(
        partial_nlr) {
    all(partial_nlr <= 1)
}

calc_tpr_curve_shape <- function(
        partial_fpr,
        partial_tpr) {
    partial_nlr <- calc_nlr(partial_fpr, partial_tpr)
    if (is_concave_tpr(partial_nlr)) {
        curve_shape <- "Proper"
    } else if (is_partially_concave_tpr(partial_nlr)) {
        curve_shape <- "Partially proper"
    } else {
        curve_shape <- "Improper"
    }
    return(curve_shape)
}

calc_tpr_lower_bound <- function(
        partial_fpr,
        partial_tpr) {
    curve_shape <- calc_tpr_curve_shape(
        partial_fpr = partial_fpr,
        partial_tpr = partial_tpr
    )
    if (curve_shape == "Proper") {
        lower_bound <- calc_tpr_concave_lower_bound(
            partial_fpr = partial_fpr,
            partial_tpr = partial_tpr
        )
    } else if (curve_shape == "Partially proper") {
        lower_bound <- calc_tpr_partial_concave_lower_bound(
            partial_fpr = partial_fpr,
            partial_tpr = partial_tpr
        )
    } else if (curve_shape == "Improper") {
        lower_bound <- calc_tpr_upper_hook_lower_bound(
            partial_tpr = partial_tpr,
            partial_fpr = partial_fpr
        )
    }
    return(lower_bound)
}

calc_tpr_bounds <- function(
        partial_fpr,
        partial_tpr) {
    list(
        upper_bound = calc_tpr_upper_bound(
            partial_fpr,
            partial_tpr
        ),
        lower_bound = calc_tpr_lower_bound(
            partial_fpr,
            partial_tpr
        )
    )
}
