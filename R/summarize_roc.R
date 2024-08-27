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
    tibble(
        auc = auc(
            response = response,
            predictor = predictor,
            direction = "<",
            quiet = TRUE
        ),
        pauc = auc(
            response = response,
            predictor = predictor,
            direction = "<",
            quiet = TRUE,
            partial.auc = c(threshold, 1),
            partial.auc.focus = "sens"
        ),
        np_auc = np_auc(NULL, response, predictor, threshold),
        fp_auc = fp_auc(NULL, response, predictor, threshold)
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
    tibble(
        auc = auc(
            response = response,
            predictor = predictor,
            direction = "<",
            quiet = TRUE
        ),
        pauc = auc(
            response = response,
            predictor = predictor,
            direction = "<",
            quiet = TRUE,
            partial.auc = c(1 - threshold, 1),
            partial.auc.focus = "spec"
        ),
        sp_auc = auc(
            response = response,
            predictor = predictor,
            direction = "<",
            quiet = TRUE,
            partial.auc = c(1 - threshold, 1),
            partial.auc.focus = "spec",
            partial.auc.correct = TRUE,
            allow.invalid.partial.auc.correct = FALSE
        ),
        tp_auc = tp_auc(NULL, response, predictor, 0, threshold)
    )
}

#' @importFrom tibble tibble
summarize_predictor <- function(
        data = NULL,
        predictor,
        response,
        ratio,
        threshold) {
    if (ratio == "tpr") {
        summarize_tpr_predictor(
            data, {{ predictor }}, {{ response }}, threshold
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



