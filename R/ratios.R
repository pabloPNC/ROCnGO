new_ratio <- function(x) {
  stopifnot(is.numeric(x))
  structure(x, class = "ratio")
}

validate_ratio <- function(ratio) {
  values <- unclass(ratio)
  if (max(values) > 1) {
    stop("Values in ratio must be under `1`")
  }
  if (min(values) < 0) {
    stop("Values in ratio must be over `0`")
  }
  ratio
}

new_ratio_tibble <- function(tpr, fpr) {
  data <- tibble(tpr = tpr, fpr = fpr)
  structure(data, class = c("ratio_tibble", class(data)))
}
