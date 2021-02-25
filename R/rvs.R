#' @title Robust variable selection with exponential squared loss
#'
#' @description \code{rvs} carries out robust variable selection with exponential squared loss. 
#' A block coordinate gradient descent is implemented to minimize the loss function.
#'
#' @param x input matrix; each row is an observation vector.
#' @param y response variable.
#' @param gamma tuning parameter in the loss function.
#' The loss function defined as \deqn{1-exp(-t^2/\gamma)}
#' @param lambda regularization parameters in the penalty.
#' The penalty is defined as \deqn{\lambda\sum_{j=1}^p weight_{j}|\beta_j|.}
#' @param weight If \code{weight=NULL},
#' it is set to be \eqn{(log(n))/(n|\tilde{\beta}_j|),}
#' where \eqn{\tilde{\beta}} is an initial estimator.
#' If also \code{lambda=1}(by default),
#' the parameters meet a BIC-type criterion.
#' @param intercept should intercepts be fitted(TRUE) or set to zero(FALSE)
#'
#'
#' @return A numerical vector \code{beta}, which is the estimated regression coefficients.
#'
#' @details \code{rvs} solves the following optimization problem to obtain robust estimators of regression coefficients:
#' \deqn{argmin_{\beta} \sum_{i=1}^n(1-exp{-(y_i-x_i^T\beta)^2/\gamma_n})+n\sum_{i=1}^d p_{\lambda_{nj}(|\beta_j|)}.}
#' We use the adaptive LASSO penalty. Regularization parameters are chosen adaptively by default, while they can be supplied by the user.
#' Block coordinate gradient descent algorithm is used to efficiently solve the optimiztion problem.
#'
#' @importFrom stats model.frame
#' @importFrom matrixStats rowMedians
#' @importFrom MASS rlm
#' @importFrom MASS mvrnorm
#' @importFrom stats cov
#' @importFrom stats median
#' @importFrom stats rnorm
#' @import stats
#'
#'
#' @references Xueqin Wang, Yunlu Jiang, Mian Huang & Heping Zhang (2013) Robust Variable Selection With Exponential Squared Loss, Journal of the American Statistical Association, 108:502, 632-643, DOI: 10.1080/01621459.2013.766613
#' @references Tseng, P., Yun, S. A coordinate gradient descent method for nonsmooth separable minimization. Math. Program. 117, 387-423 (2009). https://doi.org/10.1007/s10107-007-0170-0
#' 
#' @author Borui Tang, Jin Zhu, Xueqin Wang
#'
#' @examples
#' library(MASS)
#' N <- 100
#' p <- 8
#' rho <- 0.2
#' mu <- rep(0, p)
#' Sigma <- rho * outer(rep(1, p), rep(1, p)) + (1 - rho) * diag(p)
#' ind <- 1:p
#' beta <- (-1)^ind * exp(-2 * (ind - 1) / 20)
#' lambda_seq <- seq(0.05, 5, length.out = 100)
#' X <- mvrnorm(N, mu, Sigma)
#' Z <- rnorm(N, 0, 1)
#' k <- sqrt(var(X %*% beta) / (3 * var(Z)))
#' Y <- X %*% beta + drop(k) * Z
#' rvs(X, Y)
#' @export
#'
rvs <- function(x, y, gamma = NULL,
                lambda = 1, weight = NULL,
                intercept = FALSE) {
  obs <- nrow(x)
  if (isTRUE(intercept)) {
    x <- cbind(rep(1, obs), x)
  }
  data <- x
  res <- y

  # compute beta0 (initial estimate)
  data_all <- data.frame(res, data)
  rlmb <- MASS::rlm(res ~ . - 1, data = data_all, method = "MM")
  beta0 <- rlmb$coefficients

  if (is.null(gamma)) {
    # choose gamma
    c1 <- data.frame(rlmb[1])$coefficients
    y_1 <- res
    x <- data
    Z <- y_1 - (x %*% (as.matrix(c1)))
    S <- 1.4826 * stats::median(abs(Z - stats::median(Z)))
    Q <- abs(Z / S)
    index <- which(Q >= 2.5)
    zeta <- function(g) {
      m <- length(index)
      n <- length(y_1)
      rho <- 1 - exp(-Z^2 / (g))
      obj <- 2 * m / n + sum(rho[-c(index)]) * 2 / n
      return(obj)
    }
    gamma_1 <- seq(0.1, 4.5, by = 0.2)
    a_gamma <- sapply(gamma_1, function(x) zeta(x))
    index_1 <- which(a_gamma <= 1)
    gamma_2 <- gamma_1[index_1]
    eff <- sapply(gamma_2, function(t) TT(x, y_1, t, c1))
    gamma <- gamma_2[which(eff == min(eff))]
  }

  # choose tau
  lambda <- 1
  if (is.null(weight)) {
    weight <- obs * log(obs) / (obs * abs(beta0))
  }

  # update beta
  x <- beta0

  ############# cgdsq

  alpha <- NULL
  p <- ncol(data)
  tol <- 1e-04 # Termination tolerance (for diag. scaled residual maxRr)
  g <- grad(data, res, x, gamma) # f-gradient at x
  g <- as.vector(g)
  objf <- fnc(data, res, x, gamma)
  objl1 <- adanorm(x, weight) # l1-norm
  objl1 <- as.vector(objl1)

  k <- 0 # iteration count
  cgdk <- 0 # iteration count for cgd
  lbfgsk <- 0 # iteration count for L-BFGS
  flagl <- 0

  step <- 1 # initial stepsize for coord. gradient iterations.
  stepl <- 1
  ups <- 0.5 # initial threshold for choosing J.

  ml <- 5 # L-BFGS memory size.
  S <- matrix(0, p, ml) # S stores the last ml changes in x
  Y <- matrix(0, p, ml) # Y stores the last ml changes in gradient of f.
  rho <- matrix(0, 1, ml) # rho stores the last ml   s'*y.
  kl <- 0 # number of nonempty columns in S & Y.

  # Parameters for Armijo stepsize rule
  sigma <- 0.1
  beta2 <- 0.5
  gamma1 <- 0 # parameter for dirderiv. in cgd iterations.
  # gamma2 <- 0 # parameter for dirderiv. in rank-1 Hessian accel.
  stop <- 0 # flag for termination

  while ((stop == 0) & (k < 1000)) {
    # run cgd for first 10 iterations
    if (k < 10) {
      flagl <- 1
    }

    h <- hessian(data, res, x, gamma)
    h <- sapply(h, max, 0.01)
    h <- sapply(h, min, 1e+09)

    if (flagl == 1) {
      r_dirq <- dirq(lambda, x, g, h, ups, weight) # compute cgd direction
      maxRr <- r_dirq$maxRr
      d <- r_dirq$d
      d <- as.vector(d)
      x <- as.vector(x)

      # Check for termination
      if (maxRr <= tol) {
        stop <- 1
        break
      }

      # Armijo stepsize rule ################################################
      step <- min(step / beta2, 1)
      newobjf <- fnc(data, res, x + step * d, gamma)
      dirderiv <- t(g) %*% d + gamma1 * t(h) %*% (d^2) + lambda *
        (adanorm(x + d, weight) - objl1)
      nf <- 1
      while ((newobjf - objf) / step + lambda * (adanorm(x + step *
        d, weight) - adanorm(x, weight)) / step > sigma * dirderiv) {
        step <- step * beta2
        newobjf <- fnc(data, res, x + step * d, gamma)
        nf <- nf + 1
        if (step < 1e-30) {
          print("CGD stepsize small! Check roundoff error in newobj-obj")
          stop <- 1
          break
        }
      }

      ######################################################################
      s <- step * d # save change in x for L-BFGS.
      x <- x + s
      objf <- newobjf
      objl1 <- adanorm(x, weight)

      # Update the threshold for choosing J, based on the current stepsize.
      if (step > 0.001) {
        ups <- max(1e-04, ups / 10)
      } else {
        if (step < 1e-06) {
          ups <- min(0.9, 50 * ups)
          step <- 0.001
        }
      }
      if (((k %% 100) < 50) & (k >= 10)) {
        flagl <- 0 # run BFGS acceleration
      } else {
        flagl <- 1 # run CGD iteration
      }
      cgdk <- cgdk + 1
    } else {
      # L-BFGS acceleration iteration

      # compute sigx using the Hessian diagonals
      r_signx <- signx(lambda, x, g, h, weight)
      sigx <- r_signx$s
      t <- r_signx$t
      maxRr <- r_signx$maxRr

      if (maxRr <= tol) {
        stop <- 1
        break
      }

      asigx <- abs(sigx)
      ################################### q=asigx*g+c*sigx; #q is the obj gradient w.r.t. x_i 'far from 0'.
      q <- asigx * g + lambda * weight * sigx
      if (norm_vec(q) > tol) {
        if (kl == 0) {
          d <- -q
        } else {
          ql <- q
          for (i in 1:kl) {
            alpha[i] <- t(S[, i]) %*% ql / rho[i]
            ql <- ql - alpha[i] * Y[, i]
          }
          r <- ql * max(1e-06, rho[1] / (t(Y[, 1]) %*% Y[, 1]))
          for (i in kl:1) {
            betal <- t(Y[, i]) %*% r / rho[i]
            r <- r + S[, i] * as.vector(alpha[i] - betal)
          }
          d <- -r * asigx #|x_i|>t whenever d_i not=0

          # steepest descent safeguard to ensure convergence
          if (t(q) %*% d > -1e-20 * t(q) %*% q | norm_vec(d) >
            1e+06 * norm_vec(q)) {
            d <- -q
          }
        }

        # Armijo stepsize rule
        stepl <- 1
        newobjf <- fnc(data, res, x + stepl * d, gamma)
        # nsx = sign(x+stepl*d); psx = sign(x);
        dirderiv <- t(q) %*% d
        nfl <- 1
        while ((newobjf - objf) / stepl + lambda * (adanorm(x + stepl *
          d, weight) - adanorm(x, weight)) / stepl > sigma * dirderiv) {
          stepl <- stepl * beta2
          newobjf <- fnc(data, res, x + stepl * d, gamma)

          if (stepl < 1e-30) {
            cat("L-BFGS stepsize small! Check roundoff error in newobj-obj.\n")
            stop <- 1
            break
          }
          nfl <- nfl + 1
        }
        ####################################################################
        s <- stepl * d
        x <- x + s
        objf <- newobjf
        objl1 <- adanorm(x, weight)

        if ((k %% 100) < 50) {
          # | stepl> 0.1
          flagl <- 0 # run L-BFGS acceleration
        } else {
          flagl <- 1 # run CGD iteration
        }
        lbfgsk <- lbfgsk + 1
      } else {
        flagl <- 1 # run CGD iteration
      }
    }

    gold <- g
    g <- grad(data, res, x, gamma)
    y <- g - gold # save change in g for L-BFGS.

    # Check to save s and y for L-BFGS.
    if (t(y) %*% y > 1e-40) {
      sy <- t(s) %*% y
      gammal <- sy / (t(y) %*% y)
      if (gammal * max(h) > 1e-10) {
        kl <- min(ml, kl + 1)
        S <- cbind(s, S)
        Y <- cbind(y, Y)
        rho <- cbind(sy, rho)
        S <- S[, 1:ml]
        Y <- Y[, 1:ml]
        rho <- rho[, 1:ml]
        rho <- matrix(rho, nrow = 1)
      }
    }

    # # Acceleration step
    # # ##########################################################
    # if ((flagl == 1) & (k%%10 == 1) & (kl > 0)) {
    #   # Use a rank-1 approx. of Hessian of f in quadratic model and minimize
    #   # with respect to all coordinates: min g'd + |b'd|^2/2 + c||x+d||_1 The
    #   # quadratic Hessian bb' is only psd, so it may have no minimum.
    #
    #   # Choose b to satisfy (bb')s = y
    #   b <- Y[, 1]/sqrt(rho[1])
    #
    #   # If there exists i with b_i=0 and |g_i|>c, then subproblem has no
    #   # optimal soln
    #
    #   sgc <- which(lambda >= abs(g))
    #   absb <- abs(b)
    #   maxb <- max(absb)
    #   absb[sgc] <- maxb
    #   if (min(absb) > maxb * 1e-06) {
    #     # d = -x + db_i*e_i, where e_i is ith unit coordinate vector, db_i
    #     # solves qb_i = min (g_i - (b'x)b_i)db_i + b_i^2 db_i^2/2 + c|db_i|,
    #     # and i is chosen to minimize qb_i .
    #
    #     b[which(abs(b) <= 0)] = 1e-20  #perturb the zero entries of b to be nonzero
    #     bt <- t(b)
    #     gb <- t(g) - as.vector(b %*% x) * bt
    #     ############################### ???
    #
    #     db <- -matrixStats::rowMedians(cbind(t(t(rep(0, p))), t(gb + lambda * t(weight)/(bt^2)),
    #                             t(gb - lambda * t(weight)/(bt^2))))
    #     qb <- gb * db + (bt * db)^2/2 + lambda * t(weight) * abs(db)
    #     minqb <- min(qb)
    #     index <- which.min(qb)
    #     d <- -x
    #     d[index] <- d[index] + db[index]
    #
    #     dirderiv <- t(g) %*% d + gamma2 * (bt %*% d)^2 + lambda * (adanorm(x +
    #                                                                          d, weight) - objl1)
    #     if (dirderiv < (1e-08) * norm_vec(d)) {
    #       # if the direction is descent, increase the iteration and check the
    #       # termination
    #       k <- k + 1
    #       # h=min(max(hessian(data, res, x, gamma),1e-12),1e12); R=-stats::median([ x' ; (g'+c)./h ;
    #       # (g'-c)./h ]); absR=abs(R); maxRr=max(h.*absR);
    #       R <- -matrixStats::rowMedians(cbind(t(x), (t(g) + lambda * t(weight)), (t(g) -
    #                                                                    lambda * t(weight))))
    #       absR <- abs(R)
    #       maxRr <- max(absR)
    #
    #       if (maxRr <= tol) {
    #         stop <- 1
    #         break
    #       }
    #
    #       # Armijo rule for acceleration step #################################
    #
    #       sp <- 1
    #       newobjf <- fnc(data, res, x + sp * d, gamma)
    #       # nsx = sign(x+sp*d); psx = sign(x);
    #       nf <- 1
    #       while ((newobjf - objf)/sp + lambda * (adanorm(x + sp * d, weight) -
    #                                              adanorm(x, weight))/sp > sigma * dirderiv) {
    #         sp <- sp * beta2
    #         newobjf <- fnc(data, res, x + sp * d, gamma)
    #         # nsx = sign(x+sp*d);
    #         nf <- nf + 1
    #
    #         if (sp < 1e-30) {
    #           cat("rank-1 accel. stepsize small!  Check roundoff error in newobj-obj.\n")
    #           stop <- 1
    #           break
    #         }
    #       }
    #     }
    #   }
    # }
    k <- k + 1
  }
  x <- as.vector(x)
  return(x)
}

TT <- function(x, y, g, c1) {
  x0 <- x
  theta <- c1
  Ahat <- matrix(0, nrow = ncol(x0), ncol = ncol(x0))
  result <- matrix(0, nrow = length(y), ncol = ncol(x0))
  a <- 0
  
  for (i in 1:length(y)) {
    a[i] <- exp(-(y[i] - crossprod(x0[i, ], theta))^2 / (g)) *
      (1 - 2 * (y[i] - crossprod(x0[i, ], theta))^2 / (g)) *
      2 / g
    
    a1 <- matrix(rep(a[i], ncol(x0)^2), nrow = ncol(x0), ncol = ncol(x0))
    Ahat <- Ahat + tcrossprod(x0[i, ], x0[i, ]) * a1
    
    result[i, ] <- as.numeric(exp(-(y[i] - crossprod(x0[i, ], theta))^2 / (g)) *
                                (2 * (y[i] - crossprod(x0[i, ], theta)) / g)) * x0[i, ]
  }
  Ahat <- Ahat / length(y)
  Sigma <- stats::cov(result)
  Vhat1 <- solve(Ahat) %*% (Sigma) %*% solve(Ahat)
  det(Vhat1)
}

norm_vec <- function(x) sqrt(sum(x^2))

fnc <- function(data, res, beta, gamma) {
  rss <- res - data %*% beta
  y <- sum(1 - exp(-rss^2 / gamma))
  return(y)
}

grad <- function(data, res, beta, gamma) {
  rss <- res - data %*% beta
  y <- t(data) %*% (exp(-rss^2 / gamma) * rss) * (-2 / gamma)
  return(y)
}

hessian <- function(data, res, beta, gamma) {
  rss <- res - data %*% beta
  h1 <- -4 * t(data)^2 %*% (exp(-rss^2 / gamma) * rss^2) / gamma^2
  h2 <- 2 * t(data)^2 %*% exp(-rss^2 / gamma) / gamma
  y <- t(h1 + h2)
  return(y)
}

adanorm <- function(x, weight) {
  y <- t(weight) %*% abs(x)
  return(y)
}

dirq <- function(c, x, g, h, ups, weight) {
  R <- -matrixStats::rowMedians(cbind(x, (g + c * weight) / h, (g - c * weight) / h)) # R=d_{H}(x)
  hR <- h * R
  Q <- -g * R - 0.5 * R * hR - c * weight * abs(x + R) + c * weight *
    abs(x)
  maxQ <- max(Q) # ||q_H||_infty
  
  maxRr <- max(abs(hR)) # maxRr=||Hd_{H}(x)||_{\infty}
  
  # indx=which(Q'<ups*sumQ)
  indx <- which(t(Q) < ups * maxQ)
  d <- t(R)
  d[indx] <- 0 # set d(i)=0 if Q(i)<ups*maxQ
  return(list(maxRr = maxRr, d = d))
}

signx <- function(c, x, g, h, weight) {
  pena <- c * weight
  R <- -matrixStats::rowMedians(cbind(x, (g + pena) / h, (g - pena) / h)) # R=d_{H}(x)
  absR <- abs(R)
  maxR <- max(absR)
  maxRr <- max(h %*% absR) # ||H*d_H(x)||_{\infty}
  if (maxRr == 0) {
    t <- 0
  } else {
    t <- -1e-04 / log(min(0.1, 0.01 * maxR))
  }
  s <- sign(x)
  s[which(t > abs(x))] <- 0
  return(list(s = s, t = t, maxRr = maxRr))
}
