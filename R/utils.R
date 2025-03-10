pronoun_var_name <- function(expr) {
  expr_args <- call_args(expr)
  expr_args[[2]]
}


mask_name <- function(expr) {
  if (quo_is_call(expr)) {
    pronoun_var_name(expr)
  } else {
    as_name(expr)
  }
}

capture_messages <- function(expr) {
  messages <- list()

  result <- withCallingHandlers(
    expr,
    message = function(cnd) {
      messages <<- c(messages, list(cnd))
      invokeRestart("muffleMessage")
    }
  )

  list(
    result = result,
    messages = messages
  )
}