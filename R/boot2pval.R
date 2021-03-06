#' My function of bootstraped p values
#'
#' A function to conduct t tests using bootstrap sampling methods as well as return useful graphs.
#'
#' @param x1 a sample vector
#' @param x2 a sample vector to be subtracted from x1
#' @param conf.level the confidence level
#' @param iter the number of iterations for bootstrap sampling
#' @param mudiff the null value
#' @param test the type of t test to perform
#'
#' @return graphical explanations of the t test as well as key parameters
#' @export
#'
#' @examples
#' boot2pval(x1 = y, x2 = x, mudiff = 0)
boot2pval<-function(x1,x2,conf.level=0.95,iter=3000,mudiff=0, test="two"){
  n1=length(x1)
  n2=length(x2)
  y1=x1-mean(x1)+mean(c(x1,x2))  # transform the data so that it is centered at the NULL
  y2=x2-mean(x2)+mean(c(x1,x2))
  y1rs.mat<-c()    #rs.mat will be come a resample matrix -- now it is an empty vector
  x1rs.mat<-c()
  y2rs.mat<-c()
  x2rs.mat<-c()
  for(i in 1:iter){ # for loop - the loop will go around iter times
    y1rs.mat<-cbind(y1rs.mat,sample(y1,n1,replace=TRUE)) #sampling from y cbind -- column bind -- binds the vectors together by columns
    y2rs.mat<-cbind(y2rs.mat,sample(y2,n2,replace=TRUE))

  }
  x1rs.mat<-y1rs.mat+mean(x1)-mean(c(x1,x2))
  x2rs.mat<-y2rs.mat+mean(x2)-mean(c(x1,x2))

  xbar1=mean(x1)
  xbar2=mean(x2)
  sx1sq=var(x1)
  sx2sq=var(x2)

  tcalc=(xbar1-xbar2-mudiff)/sqrt(sx1sq/n1+sx2sq/n2)

  sy1sq=apply(y1rs.mat,2,var)
  sy2sq=apply(y2rs.mat,2,var)
  y1bar=apply(y1rs.mat,2,mean)
  y2bar=apply(y2rs.mat,2,mean)

  tstat=(y1bar-y2bar-mudiff)/sqrt(sy1sq/n1+sy2sq/n2)


  alpha=1-conf.level # calculating alpha
  #ci=quantile(xstat,c(alpha/2,1-alpha/2))# Nice way to form a confidence interval
  pvalue=ifelse(test=="two",length(tstat[tstat>abs(tcalc) | tstat < -abs(tcalc)])/iter,
                ifelse(test=="upper",length(tstat[tstat>tcalc])/iter,
                       length(ytstat[tstat<tcalc])/iter))

  h=hist(tstat,plot=FALSE)
  mid=h$mid
  if(test=="two"){
    ncoll=length(mid[mid<= -abs(tcalc)])
    ncolr=length(mid[mid>=  abs(tcalc)])
    col=c(rep("Green",ncoll),rep("Gray",length(mid)-ncoll-ncolr),rep("Green",ncolr))
  }
  hist(tstat,col=col,freq=FALSE)
  #segments(ci[1],0,ci[2],0,lwd=2)

  return(list(pvalue=pvalue))
  #return(list(pvalue=pvalue,tcalc=tcalc,n=n,x=x,test=test,ci=ci))
}
