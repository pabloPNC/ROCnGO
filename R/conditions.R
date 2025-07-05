inform_upper_threshold <- function(threshold) {
  cli_inform(
    message = c(
      "i" = "Upper threshold {.val {threshold}} already included in points."
    ),
    class = "inform_upper_threshold",
    threshold = threshold
  )
  cli_inform(
    message = c(
      "*" = "Skipping upper threshold interpolation"
    ),
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
  cli_inform(
    message = c(
      "*" = "Skipping lower threshold interpolation"
    ),
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
  cli_inform(
    message = c(
      "*" = "Skipping lower and upper threshold interpolation"
    ),
    class = "skip_both_inter_msg"
  )
}

warn_under_chance <- function() {
  cli_warn(
    message = c(
      "!" = "ROC curve lies under chance line in the region.
      {.fn sp_auc} may not be applied and return {.val {NA}} values."
    ),
    class = "spauc_under_chance_warn"
  )
}

warn_multiple_under_chance <- function() {
  cli_warn(
    message = c(
      "!" = "Some predictors present ROC curves which lie under chance line
      in the region. {.fn sp_auc} may not be applied and return {.val {NA}}
      values."
    ),
    class = "multiple_spauc_under_chance_warn"
  )
}

warn_constant_roc <- function() {
  cli_warn(
    message = c(
      "!" = "Constant ROC curve over the prefixed FPR range."
    ),
    class = "constant_roc_curve_warn"
  )
}

warn_multiple_constant_roc <- function() {
  cli_warn(
    message = c(
      "!" = "Multiple constant ROC curves over the prefixed FPR range."
    ),
    class = "multiple_constant_roc_curve_warn"
  )
}

capture_threshold_messages <- function(expr) {
  threshold_messages <- list(lower = list(), upper = list(), both = list())
  warnings <- list(constant = list(), under_chance = list())
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
    skip_both_inter_msg = function(cnd) {
      invokeRestart("muffleMessage")
    },
    skip_lower_inter_msg = function(cnd) {
      invokeRestart("muffleMessage")
    },
    skip_upper_inter_msg = function(cnd) {
      invokeRestart("muffleMessage")
    },
    constant_roc_curve_warn = function(cnd) {
      warnings$constant <<- c(warnings$constant, list(cnd))
      invokeRestart("muffleWarning")
    },
    spauc_under_chance_warn = function(cnd) {
      warnings$under_chance <<- c(warnings$under_chance, list(cnd))
      invokeRestart("muffleWarning")
    }
  )
  list(value = result, messages = threshold_messages, warnings = warnings)
}

resignal_thresholds <- function(expr, .print = FALSE) {
  results <- capture_threshold_messages(expr)
  msgs <- results$messages
  warnings <- results$warnings
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

  if (length(warnings$constant) > 1) {
    warn_multiple_constant_roc()
  } else if (length(warnings$constant) == 1) {
    warn_constant_roc()
  }

  if (length(warnings$under_chance) > 1) {
    warn_multiple_under_chance()
  } else if (length(warnings$under_chance) == 1) {
    warn_under_chance()
  }

  if (.print == TRUE) {
    print(results)
  }
  results$value
}
