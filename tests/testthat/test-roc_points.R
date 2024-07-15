test_that("multiplication works", {
    expect_equal(2 * 2, 4)
})

test_that("roc_points is faster than paper func", {
    data <- readRDS(test_path("fixtures", "roc_data.rds"))
    data <- tibble::tibble(data)
    predictor <- "ENSG00000000003.15"

    expect_lt(
        system.time(roc_points(data, disease, ENSG00000000003.15)),
        system.time(points.curve(data[["disease"]], data[[predictor]]))
    )

})
