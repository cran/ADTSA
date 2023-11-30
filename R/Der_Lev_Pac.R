Der_Lev_Pac <-
function(x){
lgmx <- length(x)
phi_value <- rep(NA,lgmx)
phi_value[1] <- x[1]
phi_value.lk <- vector("list",lgmx+1)
phi_value.lk[[1]] <- 0
phi_value.lk[[2]] <- as.vector(x[1])
for(l in 2:lgmx){
phi_value.lk[[l+1]] <- rep(NA,l)
if(l>2){
for(k in 1:(l-2)){
phi_value.lk[[l]][k] <- phi_value.lk[[l-1]][k] - phi_value.lk[[l]][l-1]*phi_value.lk[[l-1]][l-1-k]
}
}
numer <- x[l] - phi_value.lk[[l]][1:(l-1)]%*%x[(l-1):1]
denom <- 1-phi_value.lk[[l]][1:(l-1)]%*%x[1:(l-1)]
phi_value.lk[[l+1]][l] <- numer / denom
phi_value[l] <- numer/denom
}
return(phi_value)
}
