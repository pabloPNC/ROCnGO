create_iris_df <- function() {
  # Transform in tibble
  iris_df <- tibble::as_tibble(iris)
  # Add test variables
  iris_df <- iris_df %>% dplyr::mutate(
    Species_chr = as.character(.data$Species),
    Species_int = as.integer(.data$Species),
    Species_bin_fct = forcats::fct_recode(
      .data$Species,
      "0" = "virginica",
      "0" = "versicolor",
      "1" = "setosa"
    ) %>% forcats::fct_relevel("0")
  ) %>%
    dplyr::mutate(
      Species_bin_fct_inv = forcats::fct_recode(
        .data$Species_bin_fct,
        "0" = "1",
        "1" = "0"
      ) %>% forcats::fct_relevel("0")
    ) %>%
    dplyr::mutate(
      Species_bin_fct_virg = .data$Species %>%
        fct_collapse("0" = c("setosa", "versicolor"), "1" = "virginica") %>%
        fct_relevel("0", "1")
    )
}
