#' Distribution plots
#'
#' Creates the ggplot variables and puts them into a shiny dashboard in a plot that is created
#' by calling multiplot
#'
#' @param n sample size
#' @param iter number of iterations
#' @param mu population mean
#' @param sigma population variance-covariance matrix
#' @param ci confidence
#' @param con coefficients for linear combination
#'
#' @return
#' @export
#'
#' @examples
xbarthetadist = function(n,iter,mu,sigma,ci,con){
  library(mvtnorm)
  library(ggplot2)
  mat = matrix(NA, nr= iter, nc=3)
  colnames(mat)= c("xbar1","xbar2","theta")
  for(i in 1:iter){
    x = rmvnorm(n,mu,sigma)
    mat[i,c(1,2)] <- colMeans(x)
    s=cov(x)
    eig=eigen(s)
    theta =  acos(eig$vectors[,1][1])
    mat[i,3]<-theta
  }

  df=as.data.frame(mat)
  g=ggplot(df, aes(x=xbar1,y=xbar2))  + coord_equal()
  a = ggplot(df, aes(x=theta))

  gp = g + geom_point()
  #print(gp)
  gd = g + stat_density2d(aes(fill = ..density..), contour = F, geom = 'tile') +  theme(legend.position="none")
  #print(gd)

  ah = a + geom_histogram()
  ad = a + geom_density(fill="cyan")

  cx = con[1] * x[,1] + con[2] * x[,2]
  cx = data.frame(cx)
  names(cx) = c("Linear_combination")

  lc = ggplot(data.frame(cx), aes(x=Linear_combination)) + geom_histogram(aes(y=..density..))


  multiplot(gp, gd, ah, ad,lc, cols=2)
  #print(ah)
  #print(ad)
  #head(mat)
}
