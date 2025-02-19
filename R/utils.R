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
