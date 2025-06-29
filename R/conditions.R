inform_upper_threshold <- function(threshold) {
  cli_inform(
    message = c(
      "i" = "Upper threshold {.val {threshold}} already included in points."
    ),
    class = "inform_upper_threshold",
    threshold = threshold
  )
  cli_ul(
    "Skipping upper threshold interpolation",
    class = "skip_upper_inter_msg"
  )
}

inform_lower_threshold <- function(threshold) {
  cli_inform(
    message = c(
      "i" = "Lower threshold {.val {threshold}} already included in points."
    ),
    class = "inform_lower_threshold",
    threshold = threshold
  )
  cli_ul(
    "Skipping lower threshold interpolation",
    class = "skip_lower_inter_msg"
  )
}

inform_both_thresholds <- function(thresholds) {
  cli_inform(
    message = c(
      "i" = "Lower {.val {thresholds[[1]]}} and upper {.val {thresholds[[2]]}}
      thresholds already included in points"
    ),
    class = "inform_both_thresholds",
    thresholds = thresholds
  )
  cli_ul(
    "Skipping lower and upper threshold interpolation",
    class = "skip_both_inter_msg"
  )
}

capture_threshold_messages <- function(expr) {
  threshold_messages <- list(lower = list(), upper = list(), both = list())
  result <- withCallingHandlers(
    expr,
    inform_both_thresholds = function(cnd) {
      threshold_messages$both <<- c(threshold_messages$both, list(cnd))
      invokeRestart("muffleMessage")
    },
    inform_lower_threshold = function(cnd) {
      threshold_messages$lower <<- c(threshold_messages$lower, list(cnd))
      invokeRestart("muffleMessage")
    },
    inform_upper_threshold = function(cnd) {
      threshold_messages$upper <<- c(threshold_messages$upper, list(cnd))
      invokeRestart("muffleMessage")
    },
    cliMessage = function(cnd) {
      invokeRestart("muffleMessage")
    }
  )
  list(value = result, messages = threshold_messages)
}

resignal_thresholds <- function(expr) {
  results <- capture_threshold_messages(expr)
  msgs <- results$messages
  n_lower <- length(msgs$lower)
  n_upper <- length(msgs$upper)
  n_both <- length(msgs$both)
  if (n_both > 0) {
    inform_both_thresholds(msgs$both[[1]]$thresholds)
  } else if ((n_lower > 0) && (n_upper > 0)) {
    inform_both_thresholds(
      c(msgs$lower[[1]]$threshold, msgs$upper[[1]]$threshold)
    )
  } else if (n_lower > 0) {
    inform_lower_threshold(msgs$lower[[1]]$threshold)
  } else if (n_upper > 0) {
    inform_upper_threshold(msgs$upper[[1]]$threshold)
  }
  results$value
}
