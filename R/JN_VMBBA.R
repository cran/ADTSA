JN_VMBBA <-
function(ts,lgmx,bs){
l_ts<- length(ts)   
ahat<- vector('list',2); names(ahat) = c('acf','pacf')                 
acf_y<- vector('list',lgmx)
pacf_y<- vector('list',lgmx)
for(i in 1:lgmx){
pair.id = rbind(1:(l_ts-i),(1+i):(l_ts))
upper = l_ts-i-bs+1
tmp = rep(NA,upper)
for(j in 1:upper){
sel = j:(j+bs-1)
x.j<-ts[pair.id[1,-sel]]
y.j<-ts[pair.id[2,-sel]]
tmp1 = try(cor(x.j,y.j,use = 'complete.obs'),silent = TRUE)
if(inherits(tmp1,'try-error')==0 && is.na(tmp1)==0){
if((round(tmp1,3) > -1) && (round(tmp1,3) < 1)){
tmp[j]<-tmp1
}
} 
}
acf_y[[i]] = tmp
pacf_y[[i]] = rep(NA,upper)
}
for(t in 1:(l_ts-bs)){
v.acf = unlist(lapply(acf_y,function(x,i) x[i], i = t))
tmp = Der_Lev_Pac(v.acf)
if(length(na.omit(tmp))>0){
for(i in 1:length(tmp)){
pacf_y[[i]][t] = tmp[i]
}
}
}
ahat$acf = unlist(lapply(acf_y,get.ahat))
ahat$pacf = unlist(lapply(pacf_y,get.ahat))
return(ahat)
}
