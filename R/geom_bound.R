geom_bound <- function(mapping = NULL,
                       data = NULL,
                       stat = "roc",
                       position = "identity",
                       ...,
                       predictor_name = NULL,
                       fpr_threshold = NULL,
                       tpr_threshold = NULL,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE) {
  layer(
    geom = GeomBoundSpauc,
    stat = stat,
    data = data,
    mapping = mapping,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      fpr_threshold = fpr_threshold,
      tpr_threshold = tpr_threshold,
      ...
    )
  )
}
