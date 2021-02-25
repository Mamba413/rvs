
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rvs

The goal of rvs is to carry out robust variable selection through
exponential sqared loss. Specifically, it solves the following
optimization problem:

\[
argmin_{\beta} \sum_{i=1}^n(1-exp{-(y_i-x_i^T\beta)^2/\gamma_n})+n\sum_{i=1}^d p_{\lambda_{nj}(|\beta_j|)}.
\]

We use the adaptive LASSO penalty. Regularization parameters are chosen
adaptively by default, while they can be supplied by the user. Block
coordinate gradient descent algorithm is used to efficiently solve the
optimiztion problem.

<!-- ## Installation -->

<!-- You can install the released version of rvs from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("rvs") -->

<!-- ``` -->

## Example

This is a basic example which shows you how to use this package:

``` r
library(MASS)
#> Warning: package 'MASS' was built under R version 3.6.3
library(rvs)
## basic example code

#### generate data
N <- 100
p <- 8
rho <- 0.2
mu <- rep(0, p)
Sigma <- rho * outer(rep(1, p), rep(1, p)) + (1 - rho) * diag(p)
ind <- 1:p
beta <- (-1)^ind * exp(-2 * (ind - 1) / 20)
lambda_seq <- seq(0.05, 5, length.out = 100)
X <- mvrnorm(N, mu, Sigma)
Z <- rnorm(N, 0, 1)
k <- sqrt(var(X %*% beta) / (3 * var(Z)))
Y <- X %*% beta + drop(k) * Z

#### apply rvs
rvs(X, Y)
#> [1] -0.60154492  0.00000000 -0.35921692  0.05213527  0.00000000  0.00000000
#> [7]  0.00000000  0.00000000
```
