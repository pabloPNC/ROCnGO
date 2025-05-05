#' @title Calculate area under ROC curve
#' @description
#' Calculates area under curve (AUC) of a predictor's ROC curve.
#' @inheritParams roc_points
#' @returns
#' A numerical value representing the area under ROC curve.
#' @examples
#' # Calc AUC of Sepal.Width as a classifier of setosa species
#' auc(iris, Species, Sepal.Width)
#' # Change class to predict to virginica
#' auc(iris, Species, Sepal.Width, .condition = "virginica")
#' @export
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
