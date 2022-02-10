#' myrsq
#'
#' @param xk
#' @param data
#'
#' @return
#' @export
#'
#' @examples
#'
myrsq = function(xk,data){ # data=spruce.df
  df=within(data, X<-(BHDiameter-xk)*(BHDiameter>xk))
  lmp=lm(Height ~ BHDiameter + X, data=df)
  tmp = summary(lmp)
  tmp$r.squared
}
