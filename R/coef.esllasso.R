#' Provides estimated coefficients from a fitted "esllasso" object.
#'
#' This function provides estimated
#' coefficients from a fitted "\code{esllasso}" object.
#' @param object An "\code{esllasso}" project.
#' @param ... Other arguments.
#' 
#' @return A list consisting of the intercept and regression coefficients of the fitted model.
#' @export
#'
coef.esllasso <- function(object, ...){
  coef <- list(
    alpha = object[["alpha"]],
    beta = object[["beta"]]
  )
  return(coef)
}
