# Calculate area under ROC curve

Calculates area under curve (AUC) of a predictor's ROC curve.

## Usage

``` r
auc(data = NULL, response, predictor, .condition = NULL)
```

## Arguments

- data:

  A data.frame or extension (e.g. a tibble) containing values for
  predictors and response variables.

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

- predictor:

  A data variable which must be numeric, representing values of a
  classifier or predictor for each observation.

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

## Value

A numerical value representing the area under ROC curve.

## Examples

``` r
# Calc AUC of Sepal.Width as a classifier of setosa species
auc(iris, Species, Sepal.Width)
#> [1] 0.8796
# Change class to predict to virginica
auc(iris, Species, Sepal.Width, .condition = "virginica")
#> [1] 0.4146
```
