#' D-score calculation
#' 
#' This function estimates the D-score, a numerical score that measures
#' generic development in children, from PASS/FAIL observations 
#' on developental
#' milestones. Variable names are matched by name against the built-in item 
#' bank in \code{itembank}. 
#' Variables that do not match, or for which the score is 
#' \code{NA}, are ignored. 
#' 
#' @details By default, the algorithm produces the expected a 
#' posteriori (EAP) estimate from \code{scores} using item 
#' difficulty as obtained from the item bank, and - 
#' if age is given - an age-dependent
#' prior of about twice the normal variation at the given age.
#' The method uses Bayes rule. The function can also return the 
#' full posterior instead of the EAP.
#' 
#' @aliases dscore 
#' @param scores A vector containing PASS/FAIL observations 
#' of a child on one or more developmental milestones. Scores are coded 
#' as numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used.
#' @param names A character vector with item names in the chosen \code{lexicon}. 
#' The default is \code{names(responses)}.
#' @param lexicon A character string indicating the set of variable names against 
#' which \code{items} are matched. It may be either \code{ID.VWO1996}, 
#' \code{ID.VWO2005}, \code{ID.VWO1983} or \code{ID.smock} (the default). 
#' @param age Numeric, decimal age of the child in years. This information 
#' is used to specify the age-dependent prior of about twice the normal 
#' variation. 
#' @param EAP A logical indicating to return the mean of the posterior (\code{EAP = TRUE}, the default) or the full posterior (\code{EAP = FALSE}).
#' @param \dots Parameters passed down to \code{posterior()}, for example, 
#' to set quadrature points, the prior, or the calculation method.
#' @return Either a single value (if \code{EAP == TRUE}), or a vector 
#' with the density estimate at each quadature point (if \code{EAP == FALSE}).
#' @export
dscore <- function(scores, names = names(scores), 
                   lexicon = "ID.smock", 
                   age = NULL, EAP = TRUE, 
                   ...
                   ) {
}

# Longitudinal data, but estimation does not use posterior of the previous occassion
# 29/12/2009 SvB
# With adaptions in March-June 2010  ED
# V6 veranderingen: 1. Extraitems vector laden 2. VWOdata op NA zetten op de meetmomenten die er in staan. 3. hotvector: 38 in, 37 uit.
# 10/3/2015: V7: item 1440 (= item 54 version 2005) is not used for D-score computation
#for SMOCK data: item 1449  is used for D-score computation at occasion 3 and 4
#for later versions: item54(version 1996) and item 55 (version 2005) are used for D-score computation ONLY at occasion  4 
#function Dscore: rounding is at 7 decimals (was 5 in version 6)

Dscore <- function(VWOdata=VWOdata,year=version,itembank=itembank, ref, OnlyTypicalOccassions=FALSE){
  
  #select the right column of the itembank
  if(year == "1996"|year == "2005"|year == "1983"){
    colnam <- paste("ID.VWO",year,sep="")}
  if(year == "smock"){  colnam <- paste("ID.",year,sep="")}
  
  if (OnlyTypicalOccassions==TRUE){
    singleocc<- VWOdata
    singleocc[,1:3] <- FALSE
    singleocc[,4:dim(VWOdata)[2]] <- TRUE
    for (i in 4:dim(VWOdata)[2]){singleocc[itembank[which(itembank[,colnam]==names(VWOdata)[i]),"occ"],names(VWOdata)[i]] <-FALSE}
    VWOdata[as.matrix(singleocc)] <- NA
  }
  
  itemnames <-  names(VWOdata)[-c(1:3)]
  numitems <-   length(itemnames)
  
  VWOdata<-as.data.frame(VWOdata)
  
  # Special VWO items:
  # item54 if 1996 or item 55 of 2005 must not be scored on any occassion but 4, because tau is estimated on occassion 4
  # item53 is removed from the hotvector
  # item51+52 are varying items over time, but the difficulty is based on occassion 1.
  # Therefore they won't influence the estimation much if used at later occassions. 
  
  #LET OP onderstaande code aangepast[ED jan 2012]: V54/v55 mag alleen meedoen op occ=4
 
  if(year=="1996") VWOdata[VWOdata$occ!=4, "v54" ] <- NA
  if(year=="2005") VWOdata[VWOdata$occ!=4, "v55" ] <- NA
  
  #NB item 68 van VWO 2005 meerdere keren meenemen: 
  if(year=="2005"){
    VWOdata<-cbind(VWOdata,v68b=NA)
    #verplaats score op occasion 9 voor item 68 naar item 68b
    VWOdata[VWOdata$occ==9 , "v68b"]<-VWOdata[VWOdata$occ==9,"v68"]
    #v68 alleen op occasion 8 meenemen
    VWOdata[VWOdata$occ!=8, "v68" ]<- NA
  }
  
  # make hotvec and itembank, depending on year. hotvec is specified in the itembank.
  
  hotnames <- as.character(itembank[itembank[,"hot"]==1,colnam])
  hot <- match(hotnames,names(VWOdata))   #hot geeft juiste kolomnummers van VWOdata
  hotvec <- hot[!is.na(hot)]
  itembank <- itembank[,c("occ",colnam,"labelEN","m","tau") ]
  names(itembank) <- c("occ","item",  "label" ,"m"    , "tau"  )
  
  #check waarde van dag voor matrix: 
  if (any(VWOdata[,"dag"]<=0,na.rm=T)){
    warning("Waarde voor leeftijd van kind ", VWOdata$pnr[1]," in dagen is gelijk aan: ", VWOdata[which(VWOdata[,"dag"]<=0),"dag"], ". Deze waarde is onwaarschijnlijk en het betreffende meetmoment werd verwijderd.",immediate. = TRUE)
    which(VWOdata[,"dag"]<=0)
    VWOdata<-VWOdata[-which(VWOdata[,"dag"]<=0),] 
  }
  #verwijder rijen met "dag" missing
  if(sum(is.na(VWOdata[,"dag"]))!=0) {
    VWOdata<-VWOdata[-which(is.na(VWOdata[,"dag"])),]}
  
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


          
posterior <- function(score=0, tau, prior = dunif(theta,min(theta),max(theta)), theta=seq(-10,80,1))
{
	# calculates the posterior ability distribution 
	# inputs: 
	#      score scalar between 0 and m
    #      tau   vector of length m with uncentralized thresholds from the Rasch model
    #      prior vector of prior values on quadrature points 
    #      theta vector of equally spaced quadrature points
	# Stef van Buuren 18 Feb 2009

    m <- length(tau)
    score <- floor(score)
    if (score < 0 | score > m) stop("score out-of-range.")

    # compute category response probability under the 1PL (Rasch) model
	# for a vector of uncentralized threshold parameters tau
	cpc <- t(exp(outer(0:m,theta)+c(0,-cumsum(tau))))
	cpc <- cpc[,score+1] / rowSums(cpc)
	cpc <- cpc / sum(cpc)

	# clean out any missing entries
	prior[is.na(prior)] <- 0

	# calculate the posterior per category
	postcat <- cpc * prior
	postcat <- postcat/sum(postcat)
	postcat <- postcat / (theta[2]-theta[1])

    # 
	return(postcat)
}

getkey <- function(item,itembank){
	# gets the appropriate thresholds
	p <- match(item, itembank[,"item"])
	if (!is.na(p)) {
	   m <- itembank[p,"m"]
	   r <- itembank[p,5:ncol(itembank)]
    } else {
       r <- NA
    }
	return(r)
}                                 

setprior <- function(dag, theta,.prior.theta.per.day){
    # returns the unscaled age-dependent prior N(theta.day,6)
    # return(dnorm(theta,mean=.prior.theta.per.day[dag],sd=4))
    return(dnorm(theta,mean=.prior.theta.per.day[dag],sd=5))
}

post.seq <- function(data, adm=1:3, hot, itembank, mem.within=1, mem.between=0, theta=seq(-10,80,1),priordef){
    # for one infant
    # calculate the posterior for a sequence of measurements in hot columns
    # mem.within  = fraction of posterior that forms new prior within occasion (normally 1)
    # mem.between = fraction of posterior of previous occasion (0=no smoothing at all, 1=maximal smoothing)
    # SvB 22 nov 2009, adapted for smock
    nrow <- sum(!is.na(data[,hot]))   # nrow=number of valid measurements
    hot.names <- dimnames(data)[[2]][hot]
    
    # create output arrays
    admi <- data.frame(matrix(NA, nrow = nrow,
                                  ncol = length(adm),
                                  dimnames=list(NULL,dimnames(data)[[2]][adm])))
    name <- vector("character", length = nrow)
    scor <- vector("integer", length = nrow)
    eap  <- vector("double",length = nrow)
    idx <- vector("integer",length = nrow(data))
    # post <- matrix(NA,nr=nrow,nc=length(theta),dimnames=list(NULL,as.character(theta)))

    # iterate
    k <- 0                            # counts valid scores
    for (i in 1:nrow(data)) {         # occasions
        nextocc <- TRUE
        for (j in 1:length(hot)) {    # variables
            score <- data[i, hot[j]]  # observed score?
            if (!is.na(score)) {
                k <- k + 1            # yes
                idx[i] <- k           # store index of last observation of occ i          
                tau <- getkey(hot.names[j], itembank)

                # CASE A: k==1: start with age-dependent prior for first data point
                if (k==1) {
                    prior <- setprior(data[i,"dag"], theta, priordef)
                    nextocc <- FALSE
                }
                # CASE B: nextocc: weight starting prior with 'previous occasion posterior' by mem.between
                else if (nextocc) {
                    prior <- mem.between*pst + (1-mem.between)*setprior(data[i,"dag"], theta,priordef)
                    nextocc <- FALSE
                }
                # CASE C: weight 'previous score posterior' by mem.within
                else prior <- mem.within*pst + (1-mem.within)*setprior(data[i,"dag"], theta,priordef)
                # scale priors
                prior <- prior/sum(prior)
                prior <- prior/(theta[2]-theta[1])
                
                admi[k,] <- data[i,adm]
                name[k]  <- hot.names[j]
                scor[k]  <- score
                # post[k,] <- pst <- posterior(score, tau, prior, theta) # delete the assignment post[k,] for speed
                pst <- posterior(score, tau, prior, theta)
                eap[k]   <- weighted.mean(theta,w=pst)    
            }
        }             
    }
    # return(data.frame(admi,name,scor,eap,post))
    last <- rep(FALSE,length=nrow)
    last[idx] <- TRUE
    return(data.frame(admi,name,scor,eap,last=last))
}

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







