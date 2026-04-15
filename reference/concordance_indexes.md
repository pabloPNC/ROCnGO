# Concordance indexes

Concordance derived indexes allow calculation and explanation of area
under ROC curve in a specific region. They use a dual perspective since
they consider both TPR and FPR ranges which enclose the region of
interest.

`cp_auc()` applies *concordan partial area under curve* (CpAUC), while
`ncp_auc()` applies its normalized version by dividing by the total
area.

## Usage

``` r
cp_auc(
  data = NULL,
  response,
  predictor,
  lower_threshold,
  upper_threshold,
  ratio,
  .condition = NULL
)

ncp_auc(
  data = NULL,
  response,
  predictor,
  lower_threshold,
  upper_threshold,
  ratio,
  .condition = NULL
)
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

- lower_threshold, upper_threshold:

  Two numbers between 0 and 1, inclusive. These numbers represent lower
  and upper bounds of the region where to apply calculations.

- ratio:

  Ratio or axis where to apply calculations.

  - If `"tpr"`, only points within the specified region of TPR, y axis,
    will be considered for calculations.

  - If `"fpr"`, only points within the specified region of FPR, x axis,
    will be considered for calculations.

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

A numeric value representing index score for the partial area under ROC
curve.

## References

Carrington, André M., et al. A new concordant partial AUC and partial c
statistic for imbalanced data in the evaluation of machine learning
algorithms. *BMC medical informatics and decision making* 20 (2020):
1-12.

## Examples

``` r
# Calculate cp_auc of Sepal.Width as a classifier of setosa especies in
# FPR = (0, 0.1)
cp_auc(
  iris,
  response = Species,
  predictor = Sepal.Width,
  lower_threshold = 0,
  upper_threshold = 0.1,
  ratio = "fpr"
)
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> [1] 0.3446
# Calculate ncp_auc of Sepal.Width as a classifier of setosa especies in
# FPR = (0, 0.1)
ncp_auc(
  iris,
  response = Species,
  predictor = Sepal.Width,
  lower_threshold = 0,
  upper_threshold = 0.1,
  ratio = "fpr"
)
#> ℹ Lower 0 and upper 0.1 thresholds already included in points
#> • Skipping lower and upper threshold interpolation
#> [1] 0.9068421
```
