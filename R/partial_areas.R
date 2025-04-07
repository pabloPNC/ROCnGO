pauc_fpr <- function(partial_fpr, partial_tpr) {
  pauc <- sum(
    diff(partial_fpr) *
      apply(
        cbind(
          partial_tpr[-1],
          partial_tpr[-length(partial_tpr)]
        ),
        MARGIN = 1,
        FUN = mean
      )
  )
  pauc
}

pauc_tpr <- function(partial_fpr, partial_tpr) {
  pauc <- sum(
    diff(partial_tpr) *
      apply(
        cbind(
          1 - partial_fpr[-1],
          1 - partial_fpr[-length(partial_tpr)]
        ),
        MARGIN = 1,
        FUN = mean
      )
  )
  pauc
}
