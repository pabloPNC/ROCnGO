#' @importFrom ROCnGO np_auc fp_auc
#' @importFrom tibble tibble
#' @importFrom pROC auc
summarize_tpr_predictor <- function(
        data = NULL,
        predictor,
        response,
        threshold) {
    if (!is.null(data)) {
        predictor <- data %>% pull( {{ predictor }} )
        response <- data %>% pull( {{ response }} )
    }
    tpr_fpr <- roc_points(NULL, response, predictor)
    ptpr_pfpr <- calc_partial_roc_points(
        tpr = tpr_fpr$tpr,
        fpr = tpr_fpr$fpr,
        lower_threshold = threshold,
        upper_threshold = 1,
        ratio = "tpr"
    )
    tibble(
        auc = as.double(
            auc(
                response = response,
                predictor = predictor,
                direction = "<",
                quiet = TRUE
            )
        ),
        pauc = as.double(
            auc(
                response = response,
                predictor = predictor,
                direction = "<",
                quiet = TRUE,
                partial.auc = c(threshold, 1),
                partial.auc.focus = "sens"
            )
        ),
        np_auc = np_auc(NULL, response, predictor, threshold),
        fp_auc = fp_auc(NULL, response, predictor, threshold),
        curve_shape = calc_tpr_curve_shape(
            ptpr_pfpr$partial_fpr,
            ptpr_pfpr$partial_tpr
        )
    )
}

#' @importFrom dplyr pull
#' @importFrom tibble tibble
#' @importFrom pROC auc
#' @importFrom ROCnGO tp_auc
summarize_fpr_predictor <- function(
        data = NULL,
        predictor,
        response,
        threshold) {
    if (!is.null(data)) {
        predictor <- data %>% pull( {{ predictor }})
        response <- data %>% pull( {{ response }})
    }
    tpr_fpr <- roc_points(NULL, response, predictor)
    ptpr_pfpr <- calc_partial_roc_points(
        tpr = tpr_fpr$tpr,
        fpr = tpr_fpr$fpr,
        lower_threshold = 0,
        upper_threshold = threshold,
        ratio = "fpr"
    )
    tibble(
        auc = as.double(
            auc(
                response = response,
                predictor = predictor,
                direction = "<",
                quiet = TRUE
            )
        ),
        pauc = as.double(
            auc(
                response = response,
                predictor = predictor,
                direction = "<",
                quiet = TRUE,
                partial.auc = c(1 - threshold, 1),
                partial.auc.focus = "spec"
            )
        ),
        sp_auc = as.double(
            auc(
                response = response,
                predictor = predictor,
                direction = "<",
                quiet = TRUE,
                partial.auc = c(1 - threshold, 1),
                partial.auc.focus = "spec",
                partial.auc.correct = TRUE,
                allow.invalid.partial.auc.correct = FALSE
            )
        ),
        tp_auc = tp_auc(NULL, response, predictor, 0, threshold),
        curve_shape = calc_fpr_curve_shape(
            ptpr_pfpr$partial_fpr,
            ptpr_pfpr$partial_tpr
        )
    )
}

#' @export
#' @importFrom tibble tibble
summarize_predictor <- function(
        data = NULL,
        predictor,
        response,
        ratio,
        threshold) {
    if (ratio == "tpr") {
        summarize_tpr_predictor(
            data,
            {{ predictor }},
            {{ response }},
            threshold
        )
    } else if (ratio == "fpr") {
        summarize_fpr_predictor(
            data,
            {{ predictor }},
            {{ response }},
            threshold
        )
    }

}

#' @export
#' @importFrom dplyr select bind_rows group_by summarize n
#' @importFrom stringr str_glue
#' @importFrom rlang enquo quo_is_null
summarize_dataset <- function(
        data,
        predictors = NULL,
        response,
        ratio,
        threshold,
        .progress = FALSE) {

    results <- list()
    predictors_expr <- enquo(predictors)

    if (!quo_is_null(predictors_expr)) {
        predictors_dataset <- data %>%
            select({{ predictors }})
        response <- data %>% pull( {{ response }} )
    } else {
        predictors_dataset <- data
    }

    for (i in 1:length(predictors_dataset)) {
        if (ratio == "tpr") {
            result <- summarize_tpr_predictor(
                NULL,
                predictors_dataset[[i]],
                response,
                threshold
            )
        } else if (ratio == "fpr") {
            result <- summarize_fpr_predictor(
                NULL,
                predictors_dataset[[i]],
                response,
                threshold
            )
        }

        id <- names(predictors_dataset[i])
        results[[id]] <- result

        if (.progress == TRUE) {
            print(stringr::str_glue("[*] {length(results)}/{length(predictors_dataset)}"))
        }

    }

    metrics <- list(
        data = bind_rows(results, .id = "identifier")
    )
    metrics[["curve_shape"]] <- metrics$data %>%
        group_by(curve_shape) %>%
        summarize(count = n())

    metrics[["auc"]] <- metrics$data %>%
        group_by(
            auc > 0.5,
            auc > 0.8
        ) %>%
        summarize(count = n())

    return(metrics)

}
