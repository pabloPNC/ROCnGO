# Summarize classifiers performance in a dataset

Calculate a series of metrics describing global and local performance
for selected classifiers in a dataset.

## Usage

``` r
summarize_dataset(
  data,
  predictors = NULL,
  response,
  ratio,
  threshold,
  .condition = NULL,
  .progress = FALSE
)
```

## Arguments

- data:

  A data.frame or extension (e.g. a tibble) containing values for
  predictors and response variables.

- predictors:

  A vector of numeric data variables which represents the different
  classifiers or predictors in data to be summarized.

  If `NULL`and by default, `predictors` will match all numeric variables
  in `data` with the exception of `response`, given that it has a
  numeric type.

- response:

  A data variable which must be a factor, integer or character vector
  representing the prediction outcome on each observation (*Gold
  Standard*).

  If the variable presents more than two possible outcomes, classes or
  categories:

  - The outcome of interest (the one to be predicted) will remain
    distinct.

  - All other categories will be combined into a single category.

  New combined category represents the "absence" of the condition to
  predict. See `.condition` for more information.

- ratio:

  Ratio or axis where to apply calculations.

  - If `"tpr"`, only points within the specified region of TPR, y axis,
    will be considered for calculations.

  - If `"fpr"`, only points within the specified region of FPR, x axis,
    will be considered for calculations.

- threshold:

  A number between 0 and 1, both inclusive, which represents the region
  bound where to calculate partial area under curve.

  If `ratio = "tpr"`, it represents lower bound of the TPR region, being
  its upper limit equal to 1.

  If `ratio = "fpr"`, it represents the upper bound of the FPR region,
  being its lower limit equal to 0.

- .condition:

  A value from response that represents class, category or condition of
  interest which wants to be predicted.

  If `NULL`, condition of interest will be selected automatically
  depending on `response` type.

  Once the class of interest is selected, rest of them will be collapsed
  in a common category, representing the "absence" of the condition to
  be predicted.

  See
  [`vignette("selecting-condition")`](https://pablopnc.github.io/ROCnGO/articles/selecting-condition.md)
  for further information on how automatic selection is performed and
  details on selecting the condition of interest.

- .progress:

  If `TRUE`, show progress of calculations.

## Value

A list with different elements:

- Performance metrics for each of evaluated classifiers.

- Overall description of performance metrics in the dataset.

## Examples

``` r
summarize_dataset(iris, response = Species, ratio = "tpr", threshold = 0.9)
#> ℹ Lower 0.9 and upper 1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> $data
#> # A tibble: 4 × 7
#>   identifier      auc   pauc np_auc  fp_auc ncp_auc curve_shape      
#>   <chr>         <dbl>  <dbl>  <dbl>   <dbl>   <dbl> <chr>            
#> 1 Sepal.Length 0.0414 0       0     NaN       0     Hook under chance
#> 2 Sepal.Width  0.880  0.0472  0.472   0.852   0.906 Partially proper 
#> 3 Petal.Length 0      0       0     NaN       0     Hook under chance
#> 4 Petal.Width  0      0       0     NaN       0     Hook under chance
#> 
#> $curve_shape
#> # A tibble: 2 × 2
#>   curve_shape       count
#>   <chr>             <int>
#> 1 Hook under chance     3
#> 2 Partially proper      1
#> 
#> $auc
#> # A tibble: 2 × 3
#> # Groups:   auc > 0.5 [2]
#>   `auc > 0.5` `auc > 0.8` count
#>   <lgl>       <lgl>       <int>
#> 1 FALSE       FALSE           3
#> 2 TRUE        TRUE            1
#> 
```
