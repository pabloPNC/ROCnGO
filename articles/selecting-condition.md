# Selecting condition of interest

ROC based analyses aim to evaluate **binary classification performance**
of a classifier. In other words, this type of analyses evaluate a
classifier performance on differentiating two different outcomes,
classes or categories.

In real world scenarios, classification processes usually present more
than two possible outcomes. Thus, these scenarios can be dichotomized by
selecting one outcome as the **condition of interest**, or the one to be
predicted, and others as not being it.

Following vignette aims to show:

- How ROCnGO select a condition of interest or its absence.
- How this condition be selected through `.condition` argument.
- How an outcome can be manually selected as the condition of interest.

We’ll start by loading ROCnGO and some other libraries which will help
in the analysis.

``` r

library(ROCnGO)
library(dplyr)
```

## Selection of the condition of interest

As mentioned before, the outcomes of these analyses can be dichotomized
in being a condition of interest $`(D=1)`$ or not $`(D=0)`$. In this
way, ROCnGO internally transform the variable with each case outcome
(`response`) to a factor of values 1 and 0, representing presence or
absence of the condition.

Taking the following example with three different outcomes, if we
considered *setosa* as the condition of interest, the following factor
would be generated.

| Case |  Response  | Factor |
|------|:----------:|-------:|
| 1    |   Setosa   |      1 |
| 2    | Versicolor |      0 |
| 3    | Virginica  |      0 |

`response` may be of different types, so in order to select by default
which class will correspond to the condition of interest among its
values, library functions follow some criteria based on the variable
type:

- **integer**. When working with an integer vector, functions will
  consider the smallest one as the class to predict.
- **character**. When working with a character vector, functions will
  consider the first value after using
  [`sort()`](https://rdrr.io/r/base/sort.html) over all posible options.
- **factor**. When working with a factor variable, functions will select
  first class in [`levels()`](https://rdrr.io/r/base/levels.html).

All other classes not identified as the class to predict will be
combined into a common category, labelled as 0.

## `.condition` argument

Sometimes, default criteria used by functions may not be desirable.
Thus, if we want to change the category identified as the condition of
interest we can use `.condition` argument.

This argument takes as an input one of the values of `response`, setting
it as the condition of interest of the classifier.

### Examples

These behaviours can be tested with the following examples. In the first
place we will create an small dataset by using a small subset of `iris`
dataset.

``` r

# Create a small subset of iris with 5 random flowers of each species
iris_subset <- as_tibble(iris) %>%
  group_by(Species) %>%
  slice_sample(n = 5) %>%
  ungroup()
iris_subset
#> # A tibble: 15 × 5
#>    Sepal.Length Sepal.Width Petal.Length Petal.Width Species   
#>           <dbl>       <dbl>        <dbl>       <dbl> <fct>     
#>  1          5.1         3.8          1.9         0.4 setosa    
#>  2          4.6         3.6          1           0.2 setosa    
#>  3          4.8         3.4          1.6         0.2 setosa    
#>  4          5.5         3.5          1.3         0.2 setosa    
#>  5          4.9         3.6          1.4         0.1 setosa    
#>  6          5.7         2.9          4.2         1.3 versicolor
#>  7          5.7         2.8          4.1         1.3 versicolor
#>  8          5.5         2.4          3.8         1.1 versicolor
#>  9          5.5         2.3          4           1.3 versicolor
#> 10          6.3         2.3          4.4         1.3 versicolor
#> 11          6.7         2.5          5.8         1.8 virginica 
#> 12          6.5         3            5.8         2.2 virginica 
#> 13          6.2         3.4          5.4         2.3 virginica 
#> 14          6.3         2.7          4.9         1.8 virginica 
#> 15          5.8         2.8          5.1         2.4 virginica
```

Once we have created our dataset, we can check the performance of the
different variables as predictors for the species, for this task we may
use
[`summarize_dataset()`](https://pablopnc.github.io/ROCnGO/reference/summarize_dataset.md)
function.

``` r

# Check levels in Species
levels(iris_subset$Species)
#> [1] "setosa"     "versicolor" "virginica"

# Summarize dataset classifiers
iris_results <- summarize_dataset(
  iris_subset,
  response = Species,
  ratio = "tpr",
  threshold = 0.9
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
iris_results$data
#> # A tibble: 4 × 7
#>   identifier     auc   pauc np_auc  fp_auc ncp_auc curve_shape      
#>   <chr>        <dbl>  <dbl>  <dbl>   <dbl>   <dbl> <chr>            
#> 1 Sepal.Length  0.02 0       0     NaN       0     Hook under chance
#> 2 Sepal.Width   0.99 0.0925  0.925   0.974   0.990 Concave          
#> 3 Petal.Length  0    0       0     NaN       0     Hook under chance
#> 4 Petal.Width   0    0       0     NaN       0     Hook under chance
```

As we may see Sepal.Width scores the best performance in the dataset, at
least for *setosa* species. As we have mentioned before, this class has
been selected as the condition of interest since it is the first element
in species levels. Furthermore, the performance of Sepal.Width as a
*setosa* classifier may be addressed since it presents slightly higher
scores.

Now, if we want to repeat the analysis but considering *virginica* as
the species of interest, we can consider `.condition` argument.

``` r

# Summarize dataset classifiers with virginica species as D=1
virginica_results <- summarize_dataset(
  iris_subset,
  response = Species,
  ratio = "tpr",
  threshold = 0.9,
  .condition = "virginica"
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
virginica_results$data
#> # A tibble: 4 × 7
#>   identifier     auc  pauc np_auc fp_auc ncp_auc curve_shape
#>   <chr>        <dbl> <dbl>  <dbl>  <dbl>   <dbl> <chr>      
#> 1 Sepal.Length  0.95  0.09    0.9      1   0.99  Concave    
#> 2 Sepal.Width   0.4   0.03    0.3      1   0.825 Concave    
#> 3 Petal.Length  1     0.1     1        1   1     Concave    
#> 4 Petal.Width   1     0.1     1        1   1     Concave
```

As we may see, new results highly differ from previous ones. Now
Sepal.Length, Petal.Length and Petal.Width behave as better classifiers
instead of Sepal.Width. In the same way, these results can be
qualitatively matched with values in dataset, where variables score
higher for this species.

## Manual selection of the condition of interest

Sometimes, it may be more useful to select manually the condition of
interest. This may be the case, e.g. when working with a variable type
than cannot be easily treated.

In order to manually select this condition, we could simply transform
`response` to another type that can be recognized by the library, even
`.condition` may be used to specify which class to use.

Alternatively, we can transform `response` to a factor of 0 and 1
values, where its first item in
[`levels()`](https://rdrr.io/r/base/levels.html) will be 0. Library
recognizes this variable as not needing any treatment, so it can be used
to easily define this new responses.

### Examples

We can check this manual selection with the following example. In this
scenario, we will be supposing that we cannot make directly calculations
over Species and we will need to define new variables to do it.

``` r

# Create new variables to evaluate "virginica" species classifiers
iris_subset <- iris_subset %>%
  mutate(
    Species_int = ifelse(Species == "virginica", 2L, 1L),
    Species_fct = factor(
      ifelse(Species == "virginica", 1, 0),
      levels = c(0, 1)
    )
  )
# Check new variables
iris_subset[, c("Species", "Species_int", "Species_fct")]
#> # A tibble: 15 × 3
#>    Species    Species_int Species_fct
#>    <fct>            <int> <fct>      
#>  1 setosa               1 0          
#>  2 setosa               1 0          
#>  3 setosa               1 0          
#>  4 setosa               1 0          
#>  5 setosa               1 0          
#>  6 versicolor           1 0          
#>  7 versicolor           1 0          
#>  8 versicolor           1 0          
#>  9 versicolor           1 0          
#> 10 versicolor           1 0          
#> 11 virginica            2 1          
#> 12 virginica            2 1          
#> 13 virginica            2 1          
#> 14 virginica            2 1          
#> 15 virginica            2 1
```

Now we can evaluate the classifier performance.

``` r

# Select predictors
predictors <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")

# Check performance of virginica classifiers with .condition = 2
int_results <- summarize_dataset(
  iris_subset,
  predictors = predictors,
  response = Species_int,
  ratio = "tpr",
  threshold = 0.9,
  .condition = 2
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
int_results$data
#> # A tibble: 4 × 7
#>   identifier     auc  pauc np_auc fp_auc ncp_auc curve_shape
#>   <chr>        <dbl> <dbl>  <dbl>  <dbl>   <dbl> <chr>      
#> 1 Sepal.Length  0.95  0.09    0.9      1   0.99  Concave    
#> 2 Sepal.Width   0.4   0.03    0.3      1   0.825 Concave    
#> 3 Petal.Length  1     0.1     1        1   1     Concave    
#> 4 Petal.Width   1     0.1     1        1   1     Concave

# Check performance of virginica classifiers with factor
fct_results <- summarize_dataset(
  iris_subset,
  predictors = predictors,
  response = Species_fct,
  ratio = "tpr",
  threshold = 0.9
)
#> ℹ Upper threshold 1 already included in points.
#> • Skipping upper threshold interpolation
fct_results$data
#> # A tibble: 4 × 7
#>   identifier     auc  pauc np_auc fp_auc ncp_auc curve_shape
#>   <chr>        <dbl> <dbl>  <dbl>  <dbl>   <dbl> <chr>      
#> 1 Sepal.Length  0.95  0.09    0.9      1   0.99  Concave    
#> 2 Sepal.Width   0.4   0.03    0.3      1   0.825 Concave    
#> 3 Petal.Length  1     0.1     1        1   1     Concave    
#> 4 Petal.Width   1     0.1     1        1   1     Concave
```

As we may see results for each scenario correspond to ones obtained in
the previous section, where we evaluated Species variable using
`.condition = "virginica"` directly.
