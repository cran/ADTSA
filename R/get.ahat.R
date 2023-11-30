get.ahat <- function(x){
	x = na.omit(x)
	x = as.vector(x)
	x.c = mean(x,na.rm = TRUE)- x
	x3 =t(x.c)%*%(x.c^2)
	sd.x = sqrt(t(x.c)%*%x.c)
	a = x3/6/(sd.x^3)
	return(a)
}
