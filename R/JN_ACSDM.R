JN_ACSDM <-
function(ts,lgmx){
l_ts<-length(ts)
ahat<-vector('list',2)
names(ahat) = c('acf','pacf')
acf_y<-matrix(NA,nrow=l_ts,ncol=lgmx)
pacf_y<-matrix(NA,nrow=l_ts,ncol=lgmx)
for(t in 1:l_ts){
ts.j<-ts[-t]
acf_y[t,]<-acf(ts.j,lag.max=lgmx,plot=F,
na.action = na.pass)$acf[2:(lgmx+1)]
pacf_y[t,]<-acf(ts.j,lag.max=lgmx,plot=F,
na.action = na.pass,type = 'partial')$acf[1:lgmx]
}
ahat$acf = apply(acf_y,2,get.ahat)
ahat$pacf = apply(pacf_y,2,get.ahat)
return(ahat)
}
