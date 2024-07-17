expect_faster <- function(expression_1, expression_2) {
    actual <- quasi_label(
        rlang::enquo(expression_1), arg = "actual"
    )
    expected <- quasi_label(
        rlang::enquo(expression_2), arg = "expected"
    )

    start <- Sys.time()
    expression_1
    end <- Sys.time()
    expr_1_time <- end - start

    start <- Sys.time()
    expression_2
    end <- Sys.time()
    expr_2_time <- end - start

    expect(
        expr_1_time < expr_2_time,
        stringr::str_glue(
            "`{actual$lab}` takes longer than `{expected$lab}`. 
            Times: {expr_1_time} - {expr_2_time}"
        )
    )

    invisible(
        stringr::str_glue(
            "`{actual$lab} {expected$lab}
            {expr_1_time} {expr_2_time}"
        )
    )
}
