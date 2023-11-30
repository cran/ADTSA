pairwise_MBL <-
function(mat,lgmx,l_ts){
l = nrow(mat)
pair.mat = vector('list',lgmx)
for(i in 1:lgmx){
tmp = mat+i
tmp2 = rbind(c(mat),c(tmp))
sel = which(tmp2[2,]>l_ts)
if(length(sel)>0){tmp2 = tmp2[,-sel]}
if(ncol(tmp2)>l_ts){tmp2 = tmp2[,1:l_ts]}
pair.mat[[i]] = tmp2
}
return(pair.mat)
}
