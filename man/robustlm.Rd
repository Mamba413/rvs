% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/robustlm.R
\name{robustlm}
\alias{robustlm}
\title{Robust variable selection with exponential squared loss}
\usage{
robustlm(x, y, gamma = NULL, weight = NULL, intercept = TRUE)
}
\arguments{
\item{x}{Input matrix, of dimension nobs * nvars; each row is an observation vector. Should be in matrix format.}

\item{y}{Response variable. Should be a numerical vector or matrix with a single column.}

\item{gamma}{Tuning parameter in the loss function, which controls the degree of robustness and efficiency of the regression estimators.
The loss function is defined as \deqn{1-exp(-t^2/\gamma).}
When \code{gamma} is large, the estimators are similar to the least squares estimators
in the extreme case. A smaller \code{gamma} would limit the influence of an outlier on the estimators, 
although it could also reduce the sensitivity of the estimators. If \code{gamma=NULL}, it is selected by a
data-driven procedure that yields both high robustness and high efficiency.}

\item{weight}{Weight in the penalty. The penalty is given by \deqn{n\sum_{j=1}^d\lambda_{n j}|\beta_{j}|.} \code{weight} is a vector consisting of \eqn{\lambda_{n j}}s. If \code{weight=NULL} (by default),
it is set to be \eqn{(log(n))/(n|\tilde{\beta}_j|),}
where \eqn{\tilde{\beta}} is a numeric vector, which is an
initial estimator of regression coefficients obtained by an MM procedure. The default value meets a BIC-type criterion (See Details).}

\item{intercept}{Should intercepts be fitted (TRUE) or set to zero (FALSE)}
}
\value{
An object with S3 class "robustlm", which is a \code{list} with the following components:
\item{beta}{The regression coefficients.}
\item{alpha}{The intercept.}
\item{gamma}{The tuning parameter used in the loss.}
\item{weight}{The regularization parameters.}
\item{loss}{Value of the loss function calculated on the training set.}
}
\description{
\code{robustlm} carries out robust variable selection with exponential squared loss. 
A block coordinate gradient descent algorithm is used to minimize the loss function.
}
\details{
\code{robustlm} solves the following optimization problem to obtain robust estimators of regression coefficients:
\deqn{argmin_{\beta} \sum_{i=1}^n(1-exp{-(y_i-x_i^T\beta)^2/\gamma_n})+n\sum_{i=1}^d p_{\lambda_{nj}}(|\beta_j|),}
where \eqn{p_{\lambda_{n j}}(|\beta_{j}|)=\lambda_{n j}|\beta_{j}|} is the adaptive LASSO penalty. Block coordinate gradient descent algorithm is used to efficiently solve the optimization problem. 
The tuning parameter \code{gamma} and regularization parameter \code{weight} are chosen adaptively by default, while they can be supplied by the user.
Specifically, the default \code{weight} meets the following BIC-type criterion: 
\deqn{min_{\tau_n} \sum_{i=1}^{n}[1-exp {-(Y_i-x_i^T} {\beta})^{2} / \gamma_{n}]+n \sum_{j=1}^{d} \tau_{n j}|\beta_j| /|\tilde{\beta}_{n j}|-\sum_{j=1}^{d} \log (0.5 n \tau_{n j}) \log (n).}
}
\examples{
library(MASS)
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
k <- sqrt(var(X \%*\% beta) / (3 * var(Z)))
Y <- X \%*\% beta + drop(k) * Z
robustlm(X, Y)
}
\references{
Xueqin Wang, Yunlu Jiang, Mian Huang & Heping Zhang (2013) Robust Variable Selection With Exponential Squared Loss, Journal of the American Statistical Association, 108:502, 632-643, DOI: 10.1080/01621459.2013.766613

Tseng, P., Yun, S. A coordinate gradient descent method for nonsmooth separable minimization. Math. Program. 117, 387-423 (2009). https://doi.org/10.1007/s10107-007-0170-0
}
\author{
Borui Tang, Jin Zhu, Xueqin Wang
}
