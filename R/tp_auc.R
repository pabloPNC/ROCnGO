interpolate_threshold <- function(threshold, lower_x, upper_x,
    lower_y, upper_y) {
    slope <- (threshold - lower_x) / (upper_x - lower_x)
    lower_y + (upper_y - lower_y) * slope
}

get_fpr_indexes <- function(fpr, lower_fpr, upper_fpr) {
    lower_index <- min(which(fpr >= lower_fpr))
    upper_index <- max(which(fpr <= upper_fpr))
    c(lower_index, upper_index)
}

tp_auc <- function(data, response, predictor, lower_fpr, upper_fpr, pauc) {
    tpr_fpr <- data %>% roc_points({{ response }}, {{ predictor }})
    tpr <- tpr_fpr %>% pull(tpr)
    fpr <- tpr_fpr %>% pull(fpr)
    response <- data %>% pull({{ response }})
    predictor <- data %>% pull({{ predictor }})

    if (lower_fpr >= upper_fpr) {
        stop("Error in prefixed FPR range")
    }

    partial_fpr_indexes <- get_fpr_indexes(fpr, lower_fpr, upper_fpr)
    lower_index <- partial_fpr_indexes[1]
    upper_index <- partial_fpr_indexes[2]
    partial_tpr <- tpr[lower_index:upper_index]
    partial_fpr <- fpr[lower_index:upper_index]

    previous_point_index <- max(lower_index - 1, 1)
    next_point_index <- min(1 + upper_index, length(fpr))

    if (tpr[lower_index] > lower_fpr) {
        partial_fpr <- c(lower_fpr, partial_fpr)
        interpolated <- interpolate_threshold(
            lower_fpr, fpr[previous_point_index], fpr[lower_index],
            tpr[previous_point_index], tpr[lower_index]
        )
        partial_tpr <- c(interpolated, partial_tpr)
    }

    if (fpr[upper_index] < upper_fpr) {
        partial_fpr <- c(partial_fpr, upper_fpr)
        interpolated <- interpolate_threshold(
            upper_fpr, fpr[upper_index], fpr[next_point_index],
            tpr[upper_index], tpr[next_point_index]
        )
        partial_tpr <- c(interpolated, partial_tpr)
    }
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
    lower_bound <- calculate_lower_bound(partial_fpr, partial_tpr)
    upper_bound <- calculate_upper_bound(partial_fpr, partial_tpr)

    tpauc <- (1 + ((pauc - lower_bound) / (upper_bound - lower_bound))) / 2
    return(tpauc)
}