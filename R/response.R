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
transform_response <- function(response, .condition = NULL) {
  # delete methods
  UseMethod("transform_response")
}

#' @export
transform_response.factor <- function(response, .condition = NULL) {
  condition <- select_condition(response, .condition)
  absents <- select_absents(response, condition)
  reorder_response_factor(response, condition, absents)
}

#' @export
transform_response.integer <- function(response, .condition = NULL) {
  categories <- calc_categories(response)
  condition <- select_condition(response, .condition)
  absents <- select_absents(response, condition)
  fct(as.character(response), levels = categories) %>%
    reorder_response_factor(condition, absents)
}

#' @export
transform_response.character <- function(response, .condition = NULL) {
  categories <- calc_categories(response)
  condition <- select_condition(response, .condition)
  absents <- select_absents(response, condition)
  fct(response, levels = categories) %>%
    reorder_response_factor(condition, absents)
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

as_response <- function(x) {
  inv_fct_cond <- is.factor(x) && !all(levels(x) %in% c(0, 1))
  if (is.character(x) || is.integer(x) || inv_fct_cond) {
    x <- transform_response(x)
  }
  x
}

calc_categories <- function(response) {
  UseMethod("calc_categories")
}

#' @export
calc_categories.factor <- function(response) {
  levels(response)
}

#' @export
calc_categories.integer <- function(response) {
  as.character(unique(response))
}

#' @export
calc_categories.character <- function(response) {
  unique(response)
}

#' @keywords internal
select_condition <- function(response, .condition = NULL) {
  categories <- calc_categories(response)
  if (!is.null(.condition) && !.condition %in% categories) {
    rlang::abort("`.condition` should be among `resonse` values")
  }
  UseMethod("select_condition")
}

#' @export
select_condition.factor <- function(response, .condition = NULL) {
  categories <- calc_categories(response)
  if (is.null(.condition)) {
    condition <- categories[1]
  } else {
    condition <- .condition
  }
  condition
}

#' @export
select_condition.integer <- function(response, .condition = NULL) {
  categories <- calc_categories(response)
  if (is.null(.condition)) {
    condition <- min(categories)
  } else {
    condition <- .condition
  }
  condition
}

#' @export
select_condition.character <- function(response, .condition = NULL) {
  categories <- calc_categories(response)
  if (is.null(.condition)) {
    condition <- sort(categories)[1]
  } else {
    condition <- .condition
  }
  condition
}

select_absents <- function(response, condition) {
  categories <- calc_categories(response)
  categories[categories != condition]
}