percentciboot<-function(x,b,alpha){
  # x is a vector containing the original sample.
  # b is the desired number of bootstraps.
  # alpha: (1 - alpha) is the confidence coefficient.
  #
  # theta is the point estimate.
  # lower is the lower end of the percentile confidence interval.
  # upper is the upper end of the percentile confidence interval.
  # thetastar is the vector of bootstrapped theta^*s.
  #
  theta<-mean(x)
  thetastar<-rep(0,b)
  n<-length(x)
  for(i in 1:b){xstar<-sample(x,n,replace=T)
  thetastar[i]<-mean(xstar)
  }
  thetastar<-sort(thetastar)
  pick<-round((alpha/2)*(b+1))
  lower<-thetastar[pick]
  upper<-thetastar[b-pick+1]
  list(theta=theta,lower=lower,upper=upper,thetastar=thetastar)
  #list(theta=theta,lower=lower,upper=upper)
}