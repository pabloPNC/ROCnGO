temp_auc <- function(.data,
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

temp_auc_tpauc <- function(.data,
                           response,
                           predictor,
                           .condition = NULL) {
  df <- .data %>% dplyr::relocate({{ response }})
  sumexp <- ROCpAI::mcpAUC(df, low.value = 0, up.value = 1, plot = FALSE)
  SummarizedExperiment::assay(sumexp)
}

temp_auc_proc <- function(.data,
                          response,
                          predictor,
                          .condition = NULL) {
  response <- pull(.data, {{ response }})
  response <- transform_response(response, .condition = .condition)
  predictor <- pull(.data, {{ predictor }})
  as.double(
    auc(
      response = response,
      predictor = predictor,
      direction = "<",
      quiet = TRUE
    )
  )
}

# TODO: prepare function to compare != options
temp_comparison <- function(.data,
                            response,
                            predictor,
                            .condition) {
  
}
