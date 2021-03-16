#' Provides estimated coefficients from a fitted "rvs" object.
#'
#' This function provides estimated
#' coefficients from a fitted "\code{rvs}" object.
#' @param object An "\code{rvs}" project.
#' @param ... Other arguments.
#' 
#' @return A list consisting of the intercept and regression coefficients of the fitted model.
#' @export
#'
coef.rvs <- function(object, ...){
  coef <- list(
    alpha = object[["alpha"]],
    beta = object[["beta"]]
  )
  return(coef)
}
