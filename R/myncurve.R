#' my distribution curve
#'
#' @param x variable containing x value
#' @param mu variable indicating mean
#' @param sigma variable indicating sd
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm

#' @return plot
#' @export
#'
#' @examples
#' \dontrun{mycurve(mu=10,sigma=5,x=6)}
#'
myncurve=function(x, mu, sigma){
  curve(dnorm(x,mu,sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(-1000,x,length=1000)
  ycurve=dnorm(xcurve,mean=mu,sd=sigma)
  polygon(c(-1000,xcurve,x),c(0,ycurve,0),col="Red")
  prob=pnorm(x,mean=mu,sd=sigma)
  area=round(prob,4)
  text(mu,0.5*dnorm(x,mu,sigma), paste("Area = ", area, sep=""))
}
