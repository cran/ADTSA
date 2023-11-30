Sug_dm <-
function(ahat,ts,a1,a2,boot,lgmx){
acf_y<-matrix(NA,nrow=boot,ncol=lgmx)
pacf_y<-matrix(NA,nrow=boot,ncol=lgmx)
l_ts<-length(ts)
for(i in 1:boot){
for(j in 1:50){
ts.b<-ts[sample(1:l_ts,l_ts,replace=FALSE)]
tmp.acf<-acf(ts.b,lag.max=lgmx,plot=F,
na.action = na.pass)$acf[2:(lgmx+1)]
tmp.pacf<-acf(ts.b,lag.max=lgmx,plot=F,
na.action = na.pass,type = 'partial')$acf[1:lgmx]
if(sum(abs(tmp.pacf>1) + abs(tmp.pacf < (-1)),na.rm = TRUE)==0){break}
}
acf_y[i,]<- tmp.acf
pacf_y[i,]<- tmp.pacf
}
acf.l <- list(se = apply(acf_y,2,sd,na.rm = TRUE),
CI = list(per = P_CI(acf_y,a1,a2),
BCa = B_CI(acf_y,rep(0,lgmx),boot,ahat$acf,a1,a2)))
pacf.l <- list(se = apply(pacf_y,2,sd,na.rm = TRUE),
CI = list(per = P_CI(pacf_y,a1,a2),
BCa = B_CI(pacf_y,rep(0,lgmx),boot,ahat$pacf,a1,a2)))
res = list(acf= acf.l,pacf = pacf.l)
return(res)
}

