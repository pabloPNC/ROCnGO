recompute_groups <- function(data, exclude_response = TRUE) {

  discrete <- vapply(
    data,
    \(x) is.factor(x) || is.character(x) || is.logical(x),
    logical(1)
  )

  excluded <- c("label", "PANEL", "group")

  if (exclude_response) {
    excluded <- c(excluded, "response")
  }

  discrete[names(discrete) %in% excluded] <- FALSE

  group_vars <- data[discrete]

  if (length(group_vars) == 0L) {
    return(rep(1L, nrow(data)))
  }

  interaction(
    group_vars,
    drop = TRUE,
    lex.order = TRUE
  )
}

StatRoc <- ggproto("StatRoc", Stat,
  required_aes = c("response", "predictor"),
  dropped_aes = c("response", "predictor"),
  default_aes = aes(
    x = after_stat(fpr),
    y = after_stat(tpr)
  ),
  setup_data = function(data, params) {

    automatic_group <- recompute_groups(
      data,
      exclude_response = FALSE
    )

    desired_group <- recompute_groups(
      data,
      exclude_response = TRUE
    )

    if (identical(
      as.integer(interaction(data$group, drop = TRUE)),
      as.integer(interaction(automatic_group, drop = TRUE))
    )) {
      data$group <- as.integer(desired_group)
    }

    data
  },
  compute_group = function(data, scales, .condition = NULL) {
    x <- roc_points(
      data = data,
      response = response,
      predictor = predictor,
      .condition = .condition
    )
    x
  }
)

stat_roc <- function(mapping = NULL,
                     data = NULL,
                     geom = "path",
                     position = "identity",
                     ...,
                     .condition = NULL,
                     na.rm = FALSE,
                     show.legend = NA,
                     inherit.aes = TRUE) {

  # mapping <- aes(group = 1) + mapping
  layer(
    stat = StatRoc,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      .condition = .condition,
      na.rm = na.rm,
      ...
    )
  )
}