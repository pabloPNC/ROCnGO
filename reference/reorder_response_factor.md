# Establish condition of interest as 1 and absence as 0.

Transforms levels in a `factor` to 1 if they match condition of interest
( `condition`) or 0 otherwise (`absent`) or 0 otherwise (`absent`).

## Usage

``` r
reorder_response_factor(response_fct, condition, absent)
```

## Arguments

- response_fct:

  A factor with different categories (`levels`).

- condition:

  Name of category being the condition of interest.

- absent:

  Character vector of categories not corresponding to the condition of
  interest.

## Value

`factor`with values (0, 1) where 1 matches condition of interest.
