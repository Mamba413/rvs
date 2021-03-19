
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rvs

The goal of rvs is to carry out robust variable selection through
exponential squared loss. Specifically, it solves the following
optimization problem:

\[
\arg\min_{\beta} \sum_{i=1}^n(1-\exp\{-(y_i-x_i^T\beta)^2/\gamma_n\})+n\sum_{i=1}^d \lambda_{n j}|\beta_{j}|.
\]

We use the adaptive LASSO penalty. Regularization parameters are chosen
adaptively by default, while they can be supplied by the user. Block
coordinate gradient descent algorithm is used to efficiently solve the
optimization problem.

<!-- ## Installation -->

<!-- You can install the released version of rvs from [CRAN](https://CRAN.R-project.org) with: -->

<!-- ``` r -->

<!-- install.packages("rvs") -->

<!-- ``` -->

## Example

This is a basic example which shows you how to use this package. First, we generate data which contain influential points in the response: 

```
set.seed(1)
library(MASS)
N <- 1000
p <- 8
rho <- 0.5
beta_true <- c(1, 1.5, 2, 1, 0, 0, 0, 0)
H <- abs(outer(1:p, 1:p, "-"))
V <- rho^H
X <- mvrnorm(N, rep(0, p), V)

# generate error term from a mixture normal distribution
components <- sample(1:2, prob=c(0.8, 0.2), size=N, replace=TRUE)
mus <- c(0, 10)
sds <- c(1, 6)
err <- rnorm(n=N,mean=mus[components],sd=sds[components])

Y <- X %*% beta_true + err
```

We apply *rvs* function to select important variables:
```
library(rvs)
rvs1 <- rvs(X, Y)
rvs1
#> $beta
#> [1] 0.9411568 1.5839011 2.0716890 0.9489619 0.0000000 0.0000000 0.0000000
#> [8] 0.0000000
#> 
#> $alpha
#> [1] 0
#> 
#> $gamma
#> [1] 8.3
#> 
#> $weight
#> [1]   87.140346    7.033846    4.340160    3.343782    6.833033  703.863830
#> [7]  193.860493  858.412613 2183.876884
#> 
#> $loss
#> [1] 250.3821
```
The estimated regression coefficients $(0.94, 1.58, 2.07, 0.95, 0.00, 0.00, 0.00, 0.00)$ are close to the true values$(1, 1.5, 2, 1, 0, 0, 0, 0)$. There is no mistaken selection or discard. 