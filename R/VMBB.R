VMBB <-
function(acf.est,pacf.est,ahat,ts,bs,a1,a2,boot,lgmx){
acf_y<-matrix(NA,nrow=boot,ncol=lgmx)
pacf_y<-matrix(NA,nrow=boot,ncol=lgmx)
l_ts<-length(ts)
k <- floor(l_ts/bs)
num_bl<-l_ts-bs+1 #number of blocks
seq_bl<-seq(1,num_bl,by=1)
odr<- seq_bl %x% t(rep(1,bs))
add<- rep(1,num_bl) %x% t(0:(bs-1))
odr <- odr + add
for(i in 1:boot){
for(j in 1:50){
temp<- as.vector(sample(seq_bl,k+1,replace=TRUE))
odri<- t(odr[temp,])
pair_mat <- pairwise_MBL(odri,lgmx,l_ts)
tmp.acf<- MB_Ac(pair_mat,ts)
tmp.pacf<- Der_Lev_Pac(tmp.acf)
if(sum(abs(tmp.pacf>1) + abs(tmp.pacf < (-1)),na.rm = TRUE)==0){break}
}
acf_y[i,]<- tmp.acf
pacf_y[i,]<- tmp.pacf
}
acf_l <- list(se = apply(acf_y,2,sd,na.rm = TRUE),
CI = list(per = P_CI(acf_y,a1,a2),
BCa = B_CI(acf_y,acf.est,boot,ahat$acf,a1,a2)))
pacf_l <- list(se = apply(pacf_y,2,sd,na.rm = TRUE),
CI = list(per = P_CI(pacf_y,a1,a2),
BCa = B_CI(pacf_y,pacf.est,boot,ahat$pacf,a1,a2)))
return(list(acf = acf_l,pacf = pacf_l))
}
