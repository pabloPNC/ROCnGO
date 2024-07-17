data <- tibble::tibble(readRDS(test_path("fixtures", "roc_data.rds")))
response <- "disease"
predictor <- "ENSG00000000003.15"
lower_fpr <- 0
upper_fpr <- 0.1

test_that("tp_auc == TpAUC.function", {
    skip()
    print(
        tp_auc(
            response = data[[response]],
            predictor = data[[predictor]],
            lower_fpr = lower_fpr,
            upper_fpr = upper_fpr
        )
    )
})
