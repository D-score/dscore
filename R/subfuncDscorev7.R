
# Longitudinal data, but estimation does not use posterior of the previous occassion
# 29/12/2009 SvB
# With adaptions in March-June 2010  ED
# V6 veranderingen: 1. Extraitems vector laden 2. VWOdata op NA zetten op de meetmomenten die er in staan. 3. hotvector: 38 in, 37 uit.
# 10/3/2015: V7: item 1440 (= item 54 version 2005) is not used for D-score computation
#for SMOCK data: item 1449  is used for D-score computation at occasion 3 and 4
#for later versions: item54(version 1996) and item 55 (version 2005) are used for D-score computation ONLY at occasion  4 
#function Dscore: rounding is at 7 decimals (was 5 in version 6)

Dscore <- function(VWOdata=VWOdata,year=version,itembank=itembank, ref, OnlyTypicalOccassions=FALSE){
  
  #bereken prior:
  .prior.per.day <-approx(y=ref$mu,x=ref$day,xout=1:1015,yleft=ref$mu[1])$y
  
  #bereken D
  d <- post.seq(VWOdata,adm=1:3,hotvec,itembank,priordef=.prior.per.day)
  #bewaar d-score (dout) op ieder meetmoment:
  dout<-d[d$last,c(1:3,6)]
  names(dout)[4]<-"Dscore"
  dout<-round(dout,digits=7)
  return(dout)
  
}


# functions.r : EAP algorithm
# 
# 29/12/2009 SvB
# 30/03/2010 ED




post.all <- function(smocks, persons, adm, hot, itembank, mem.within=1, mem.between=0, theta=seq(-10,80,1)){
  # calls post.seq for multiple persons and stores eap estimate per occasion
  eap <- vector("list",length=length(persons))
  for (i in 1:length(persons)) {
    if(i%%10==0) cat(paste(i,"\n"))
    person <- persons[i]
    infant <- smocks[[person]]
    z <- post.seq(infant,adm,hot,itembank,mem.within,mem.between)
    eap[[i]] <- z$eap[z$last]
  }
  return(unlist(eap))
}


plot.infant.eap <- function(infant,add=FALSE,col="red"){
  if (!add) {
    plot(infant$eap,ylim=c(0,80),col=col,xlab="Measurement",ylab="D score",type="l",lwd=2)
    at <- pmatch(unique(infant$dag),infant$dag)
    labels <- unique(infant$dag)
    axis(3,at=at,labels=labels,lty=2,col="grey",tck=1)
  } else {
    lines(infant$eap,col=col,lwd=2)
  }
}

plot.infant.eap.byday <- function(infant,add=FALSE,col="red"){
  if (!add) {
    plot(x=infant$dag[infant$last],y=infant$eap[infant$last],xlim=c(0,930),ylim=c(0,80),col=col,xlab="Day",ylab="D-score",type="l",lwd=2,axes=FALSE)
    axis(1,at=seq(0,930,60),tck=1,lty=2,col="grey")
    axis(2,at=seq(0,80,10),tck=1,lty=2,col="grey")
    box()
  } else {
    lines(x=infant$dag[infant$last],y=infant$eap[infant$last],col=col,lwd=2)
    points(x=infant$dag[infant$last],y=infant$eap[infant$last],pch=20,col=col,lwd=2)
  }
}

# old transform
#transform <- function(x){
#  return(49.273 + 1.1981*x)
#}

transformD <- function(x, ib){
  leftx <- ib[ib$item=="v1445","difficulty"]
  lefty <- 20 
  rightx <- ib[ib$item=="v1463","difficulty"]
  righty <- 40
  
  d <- data.frame(y=c(lefty,righty),x=c(leftx,rightx))
  fit <- lm(y~x, data=d)
  yhat <- predict(fit,newdata=data.frame(x=x),type="response")
  # return(yhat)
  return(list(yhat=yhat,fit=fit))
}







