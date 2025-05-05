auc <- function(.data = NULL,
                response,
                predictor,
                .condition = NULL) {
  ratios <- roc_points(
    data = .data,
    response = {{ response }},
    predictor = {{ predictor }},
    .condition = {{ .condition }}
  ) %>%
    arrange(.data[["fpr"]], .data[["tpr"]])

  height <- diff(ratios[["fpr"]])
  bases <- apply(
    cbind(
      ratios[["tpr"]][-1],
      ratios[["tpr"]][-length(ratios[["tpr"]])]
    ),
    MARGIN = 1,
    FUN = mean
  )
  sum(height * bases)
}
