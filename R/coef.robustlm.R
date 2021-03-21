#' Provides estimated coefficients from a fitted "robustlm" object.
#'
#' This function provides estimated
#' coefficients from a fitted "\code{robustlm}" object.
#' @param object An "\code{robustlm}" project.
#' @param ... Other arguments.
#' 
#' @return A list consisting of the intercept and regression coefficients of the fitted model.
#' @export
#'
coef.robustlm <- function(object, ...){
  coef <- list(
    alpha = object[["alpha"]],
    beta = object[["beta"]]
  )
  return(coef)
}
