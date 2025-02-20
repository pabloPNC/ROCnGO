#' @title Transforms a response variable into a valid factor that can be
#' processed downstream.
#' @description
#' `transform_response` transforms response so that it can be processed in
#' further steps. Function transforms input into a `factor` of values 1 and 0
#' corresponding to the condition of interest and absence of it respectively.
#' @returns `factor` of levels `(0,1)`, where 1 represents the condition of
#' interest and 0 absence of it.
transform_response <- function(response) {
  UseMethod("transform_response")
}

#' @export
transform_response.factor <- function(response) {
  condition <- levels(response)[1]
  absent <- levels(response)[-1]
  # TODO: extract fct_collapse + fct_relevel to another function
  fct_collapse(response, "1" = condition, "0" = absent) %>%
    fct_relevel("0", "1")
}

#' @export
transform_response.integer <- function(response) {
  categories <- unique(response)
  condition <- min(categories)
  absent <- categories[categories != condition]
  categories <- as.character(categories)
  condition <- as.character(condition)
  absent <- as.character(absent)
  response <- as.character(response)
  fct(response, levels = categories) %>%
    fct_collapse(response, "1" = condition, "0" = absent) %>%
    fct_relevel("0", "1")
}

#' @export
transform_response.character <- function(response) {
  categories <- sort(unique(response))
  condition <- categories[1]
  absent <- categories[categories != condition]
  fct(response, levels = categories) %>%
    fct_collapse(response, "1" = condition, "0" = absent) %>%
    fct_relevel("0", "1")
}
