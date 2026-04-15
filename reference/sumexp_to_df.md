# Transform data in a SummarizedExperiment to a data.frame

Transforms a SummarizedExperiment into a data.frame which can be used as
input for other functions.

## Usage

``` r
sumexp_to_df(se, .n = NULL)
```

## Arguments

- se:

  A SummarizedExperiment object.

- .n:

  An integer or string, representing the index or name of the assay to
  use. Same as `i` in
  [`SummarizedExperiment::assay()`](https://rdrr.io/pkg/SummarizedExperiment/man/SummarizedExperiment-class.html)
  function.

  By default, function combines every assay in `se` argument.

## Value

A data.frame created from combining assays and colData in a
SummarizedExperiment.
