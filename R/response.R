#' @title Transforms a response variable into a valid factor that can be
#' processed downstream.
#' @description
#' `transform_response` transforms response so that it can be processed in
#' further steps. Function transforms input into a `factor` of values 1 and 0
#' corresponding to the condition of interest and absence of it respectively.
#' @returns `factor` of levels `(0,1)`, where 1 represents the condition of
#' interest and 0 absence of it.
#' @param response A factor, integer or character vector of categories.
#' @details
#' By default function takes some assumption on how to make transformation,
#' depending on the class of `response`:
#' * factor. Function considers the condition of interest first level in factor.
#' * integer. Function considers the condition of interest the `min` value of\
#' response.
#' * character. Function considers the condition of interest the first value in
#' `unique(response)` after using `sort`.
#' @keywords internal
transform_response <- function(response) {
  UseMethod("transform_response")
}

#' @export
transform_response.factor <- function(response) {
  condition <- levels(response)[1]
  absent <- levels(response)[-1]
  reorder_response_factor(response, condition, absent)
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
    reorder_response_factor(condition, absent)
}

#' @export
transform_response.character <- function(response) {
  categories <- sort(unique(response))
  condition <- categories[1]
  absent <- categories[categories != condition]
  fct(response, levels = categories) %>%
    reorder_response_factor(condition, absent)
}

#' @title Establish condition of interest as 1 and absence as 0.
#' @description
#' Transforms levels in a `factor` to 1 if they match condition of interest (
#' `condition`) or 0 otherwise (`absent`) or 0 otherwise (`absent`).
#' @param response_fct A factor with different categories (`levels`).
#' @param condition Name of category being the condition of interest.
#' @param absent Character vector of categories not corresponding to the
#' condition of interest.
#' @returns `factor`with values (0, 1) where 1 matches condition of interest.
#' @keywords internal
reorder_response_factor <- function(response_fct, condition, absent) {
  response_fct %>%
    fct_collapse("1" = condition, "0" = absent) %>%
    fct_relevel("0", "1")
}