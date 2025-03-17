extract_row_data <- function(se) {
  as_tibble(rowData(se), rownames = ".id")
}

extract_col_data <- function(se) {
  as_tibble(colData(se), rownames = ".id")
}

extract_assays <- function(se, .n = NULL) {
  if (!is.null(.n)) {
    return(as_tibble(assay(se, .n), rownames = ".var_names"))
  }

  assay_names <- names(assays(se))
  if (!is.null(assay_names)) {
    assays <- map(
      assay_names,
      \(name) {
        as_tibble(assay(se, name), rownames = ".var_names") %>%
          mutate(.var_names = str_c(.data[[".var_names"]], "_", name))
      }
    )
  } else {
    assays <- map(
      seq_along(assays(se)),
      \(index) {
        as_tibble(assay(se, index), rownames = ".var_names") %>%
          mutate(.var_names = str_c(.data[[".var_names"]], "_", index))
      }
    )
  }
  reduce(assays, bind_rows)
}

#' @title
#' Transform data in a SummarizedExperiment to a data.frame
#' @description
#' Transforms a SummarizedExperiment into a data.frame which can be used as
#' input for other functions.
#' @param se A SummarizedExperiment object.
#' @param .n An integer or string, representing the index or name of the assay
#' to use. Same as `i` in [SummarizedExperiment::assay()] function.
#'
#' By default, function combines every assay in `se` argument.
#' @returns
#' A data.frame created from combining assays and colData in a
#' SummarizedExperiment.
sumexp_to_df <- function(se, .n = NULL) {
  col_data <- extract_col_data(se)
  ass <- extract_assays(se, .n = .n)
  pivot_longer(
    data = ass,
    names_to = ".obs_name",
    values_to = ".values",
    cols = !all_of(".var_names")
  ) %>%
    pivot_wider(
      names_from = ".var_names",
      values_from = ".values"
    ) %>%
    bind_cols(
      col_data %>% select(-all_of(".id"))
    )
}
