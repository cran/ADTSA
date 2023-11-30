P_CI	<- function(e.b,a1,a2){
	lagmax = ncol(e.b)
	CI.l		<- as.vector(apply(e.b,2,quantile,a1,na.rm = TRUE))
	CI.u		<- as.vector(apply(e.b,2,quantile,a2,na.rm = TRUE))
	CI.per = cbind(CI.l,CI.u)
	rownames(CI.per) = paste('lag',1:lagmax,sep='')
	colnames(CI.per) = c('low','up')
	return(CI.per)
}

######
B_CI	<- function(e.b,e,B,ahat,a1,a2){
	lagmax = ncol(e.b)
	num	<- apply(rbind(e,e.b),2,function(x) sum(x[2:(B+1)] < x[1],na.rm=TRUE))
	B.na = apply(e.b,2,function(x) length(na.omit(x)))
	z0	<- qnorm(num/B.na)
	if(sum(num == B.na)>0){z0[num==B.na]=1000}
	if(min(num) == 0){z0[num==0]=-1000}

	zlow	<- qnorm(a1)
	qlow	<- z0 + (z0+zlow)/(1-ahat*(z0+zlow))
	plow	<- matrix(pnorm(qlow),nrow = 1)
	zup	<- qnorm(a2)	
	qup	<- z0 + (z0+zup)/(1-ahat*(z0+zup))
	pup	<- matrix(pnorm(qup),nrow = 1)
	
	BCa.l	<- as.vector(apply(rbind(plow,e.b),2,
		function(x) quantile(x[2:(B+1)],prob=x[1],na.rm = TRUE)))
	BCa.u	<- as.vector(apply(rbind(pup,e.b),2,
		function(x) quantile(x[2:(B+1)],prob=x[1],na.rm = TRUE)))
	
	CI.BCa	<-cbind(BCa.l,BCa.u)
	rownames(CI.BCa) = paste('lag',1:lagmax,sep='')
	colnames(CI.BCa) = c('low','up')
	return(CI.BCa)
}
