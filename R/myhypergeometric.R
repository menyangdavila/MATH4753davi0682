#' my hypergeometric simulation
#'
#' @param iter variable containing iteration times
#' @param N variable indicating population size
#' @param r variable indicating number of success in population
#' @param n variable indicating sample size
#'
#' @return plot
#' @export
#'
#' @examples
#'\dontrun{myhyper(iter=100,N=20,n=5,r=12)}

myhyper=function(iter,N,r,n){
  sam.mat=matrix(NA,nrow=n,ncol=iter, byrow=TRUE)
  succ=c()
  for( i in 1:iter){
    sam.mat[,i]=sample(rep(c(1,0),c(r,N-r)),n,replace=FALSE)
    succ[i]=sum(sam.mat[,i])
  }
  succ.tab=table(factor(succ,levels=0:n))
  barplot(succ.tab/(iter), col=rainbow(n+1), main="HYPERGEOMETRIC simulation", xlab="Number of successes")
  succ.tab/iter
}
