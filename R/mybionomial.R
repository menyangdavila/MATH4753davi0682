#' my binomial experiment
#'
#' @param iter variable containing iteration times
#' @param n variable indicating sample size
#' @param p variable indicating possibility
#' @importFrom grDevices rainbow
#'
#' @return plot
#' @export
#'
#' @examples
#'\dontrun{mybin(10000,10,.7)}

mybin=function(iter,n, p){
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(c(1,0),n,replace=TRUE, prob=c(p,1-p))
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="Binomial simulation", xlab="Number of successes")
  succ.tab/iter
}
