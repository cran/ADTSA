MB_Ac <-
function(pair_mat,ts){
lgmx = length(pair_mat)
lcorr= rep(NA,lgmx)
l_ts = length(ts)
for(i in 1:lgmx){
x = ts[pair_mat[[i]][1,]]
y = ts[pair_mat[[i]][2,]]
tmp = try(cor(x,y,use = 'complete.obs'),silent=TRUE)
if(inherits(tmp,'try-error')==0 && is.na(tmp)==0){
if((round(tmp,3) > -1) && (round(tmp,3) < 1)){
lcorr[i] <- tmp
}
} 
}
return(lcorr)
}
