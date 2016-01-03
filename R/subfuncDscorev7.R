#' D-score estimation
#' 
#' This function estimates the D-score, a numerical score that measures
#' generic development in children, from PASS/FAIL observations 
#' on developental milestones (items). 
#' Item names are matched by name against the built-in item 
#' bank in \code{itembank}. 
#' Items that do not match, or for which the score is 
#' \code{NA}, are ignored. 
#' 
#' @details By default, the algorithm produces the expected a 
#' posteriori (EAP) estimate from \code{scores} using item 
#' difficulty as obtained from the item bank, and - 
#' if age is given - an age-dependent
#' prior of about twice the normal variation at the given age.
#' The method uses Bayes rule. The function can also return the 
#' full posterior instead of the EAP. See Bock and Mislevy (1982).
#' 
#' The mixing coefficients \code{mem.between} and \code{mem.within} are 
#' 0-1 numbers (fractions) that regulate the influence 
#' of the prior distribution on the posterior. 
#' There are three cases, depending on age and whether its the 
#' first age and first item:
#' \describe{
#' \item{First age, first item:}{For the first item at the first age
#' \code{t}, the prior is set to \eqn{N(\mu(t), \sigmaˆ2)}, 
#' where \eqn{\mu(t)} is the 
#' median D-score at age \code{t} as calculated from \code{Dreference[,"mu"]} and 
#' where \eqn{\sigmaˆ2 = 25} equals about twice the normal variation in 
#' \code{Dreference}.}
#' \item{Later age, first item:}{For the first item at the second or later age \code{t}, the 
#' prior is set to \code{mem.between * post + (1 - mem.between) * adp(t)}, 
#' where \code{post} is the D-score posterior as calculated from the 
#' previous age, and where \code{adp(t)} is the age-dependent prior at
#' age \code{t}. The mixing coefficient \code{mem.between = 0} by default, which
#' implies that D-scores of the same child at different ages are calculated 
#' independently.}
#' \item{Any age, later item:}{For the second or further items at any 
#' given age \code{t}, the prior
#' is set to \code{mem.within * post + (1 - mem.within) * adp(t)}, where 
#' \code{post} is the D-score posterior as calculated from the 
#' previous item within age \code{t}, and 
#' where \code{adp(t)} is the age-dependent prior at age \code{t}. 
#' By default the mixing coefficient \code{mem.within = 1}, which
#' mimmicks repeated application of Bayesian theorem to the current data 
#' using 'old' data as prior.}
#' }
#' 
#' @aliases dscore 
#' @param scores A vector containing PASS/FAIL observations 
#' of a child on one or more developmental milestones. Scores are coded 
#' as numerically as \code{pass = 1} and \code{fail = 0}. 
#' Alternatively, \code{pass = TRUE} and \code{fail = FALSE} may be used.
#' @param age Numeric vector with \code{length(scores)} elements, 
#' specifying decimal age in years. This information 
#' is used 1) to break up calculations into separate D-scores per age, 
#' and 2) to specify age-dependent priors. 
#' @param items A character vector with item names in the chosen \code{lexicon}. 
#' The default is \code{names(scores)}.
#' @param theta A number vector of equally spaced quadrature points.
#' This vector should span the range of all D-score values, and 
#' have at least 80 elements.
#' The default \code{theta = seq(-10, 80, 1)} is suitable for children 
#' aged 0-2 years.
#' @param mem.between Fraction of posterior from previous occasion 
#' relative to age-dependent prior. The 
#' value \code{mem.between = 0} (the default) means that 
#' no smoothing over age is performed, while a \code{mem.between = 1} 
#' corresponds to maximal smoothing over age. See details.
#' @param mem.within Fraction of posterior from previous item within
#' the same age  relative to age-dependent prior. The 
#' value \code{mem.within = 1} (the default) means that all items 
#' count equally in the posterior, while a \code{mem.within = 0} 
#' corresponds to counting only the last item. See details.
#' @param full A logical indicating to return the mean of the posterior (\code{full = FALSE}, the default) or the full posterior 
#' (\code{full = TRUE}).
#' @param \dots Additional parameters passed down to \code{gettau()}, e.g., 
#' \code{lexicon} or \code{itembank}.
#' @return If \code{full} is \code{FALSE}): 
#' A vector of EAP estimates per age 
#' with \code{length(unique(age))} elements. If \code{full} is \code{TRUE}: 
#' A \code{list} of \code{length(unique(age))} elements with the density estimate at each quadature point.
#' @references
#' Bock DD, Mislevy RJ (1982).  
#' Adaptive EAP Estimation of Ability in a Microcomputer Environment.
#' Applied Psychological Measurement, 6(4), 431-444.
#' 
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' 
#' @author Stef van Buuren 2016
#' @export
# @param prior A function that produces a numerical vector with
# \code{length(theta)} elements that sum to unity. The default function
# \code{adp(age, theta)} produces a an age-dependent normal prior
# centered around the median D-score and a standard deviation equal
# to 5.
dscore <- function(scores, 
                   age,
                   items = names(scores), 
                   theta = seq(-10, 80, 1),
                   mem.between = 0,
                   mem.within = 1,
                   full = FALSE, 
                   ...) {
  # check inputs
  # split into age groups
  # create output arrays
  # iterate
  # unsplit groups
  
  # check input lengths
  if (length(scores) != length(age)) stop("Arguments `scores` and 
                                            `age` of different length")
  if (length(scores) != length(items)) stop("Arguments `scores` and 
                                            `items` of different length")
  if (length(theta) != length(prior)) stop("Arguments `theta` and 
                                            `prior` of different length")
  
  # find the difficulty levels  
  tau <- gettau(items = items, ...)
  if (all(is.na(tau))) warning("No difficulty parameters found for ", 
                               head(items), 
                               ". Consider changing the `lexicon` argument.")
  
  # split the data by age
  dg <- split(data.frame(scores, age, items, tau), f = age)
  
  # iterate
  k <- 0                            # counts valid scores
  for (i in 1:length(dg)) {         # loop over unique ages
    d <- dg[[i]]
    # tau <- gettau(items = d$items, ...)
    nextocc <- TRUE
    for (ij in 1:nrow(d)) {    # variables
      score <- d[ij, "scores"]  # observed score?
      if (!is.na(score)) {
        k <- k + 1            # yes, increase k
        tau <- d[ij, "tau"]
        
        # CASE A: k == 1: start with age-dependent prior for first valid score
        if (k == 1) {
          prior <- adp(d[ij, "age"], theta)
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
        prior <- normalize(prior, theta)

        admi[k,] <- data[i,adm]
        name[k]  <- hot.names[j]
        scor[k]  <- score
        # post[k,] <- pst <- posterior(score, tau, prior, theta) # delete the assignment post[k,] for speed
        pst <- posterior(score, tau, prior, theta)
        eap[k]   <- weighted.mean(theta,w=pst)
      }
    }
    
    
    # create output arrays
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
          prior <- normalize(prior, theta)
          
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

#' Obtain difficulty parameters from item bank
#' 
#' Searches the item bank for matching items, and returns the 
#' difficulty estimates. Matching is done by item name. Comparisons
#' are done in lower case.
#' @aliases gettau
#' @param items A character vector with item names in the 
#' chosen lexicon.
#' @param itembank A \code{data.frame} that contains the item names 
#' (in various lexicons), the item label, the item difficulty parameter
#' \code{tau}. Lexicon column names start with \code{"lex."}. 
#' The default \code{dscore::itembank} contains the 
#' difficulty levels as published in Van Buuren (2014).
#' @param lexicon A character string indicating the column in 
#' \code{itembank} used to match item names. It must be one of 
#' the lexicon columns, e.g., \code{lex.dutch1983}, 
#' \code{lex.dutch1996}, \code{lex.dutch2005} or \code{lex.smocc}.
#' The default is \code{lex.smocc}. 
#' @param check Logical that indicates whether the lexicon name
#' should be checked. The default is \code{TRUE}.
#' @param \dots Additional arguments (ignored).
#' @return A named vector with the difficulty estimate per item with
#' \code{length(items)} elements.
#' @author Stef van Buuren 2016
#' @export
gettau <- function(items, 
                   itembank = dscore::itembank, 
                   lexicon = "lex.smocc", 
                   check = TRUE, 
                   ...) {
  # check whether lexicon is a column in item bank
  if (check) {
    q <- pmatch(tolower(lexicon), names(itembank))
    if (is.na(q)) stop ("Lexicon name `", lexicon, "` not found in item bank.")
  }
  
  # find exact matching items rows
  p <- match(tolower(items), itembank[, lexicon])
  if (all(is.na(p))) return(rep(NA, length = length(items)))
  r <- itembank[p, "tau"]
  names(r) <- items
  return(r)
}

#' Age-dependent prior
#'
#' Returns the age-dependent prior N(mu, 5) at the 
#' specified quadrature points.
#' @aliases adp
#' @param age Numeric, single value
#' @param theta A number vector of equally spaced quadrature points
#' @param mu The mean of the prior. If \code{is.null(mu)} (the default) the prior is taken from the age-dependent reference. 
#' @param sd Standard deviation of the prior. The default is 5.
#' @param reference the LMS reference values. The default uses the 
#' built-in reference \code{dscore::Dreference} for Dutch children
#' published in Van Buuren (2014).
#' @param \dots Additional parameters (ignored)
#' @return  A \code{vector} of \code{length(theta)} elements with 
#' the prior density estimate at each quadature point \code{theta}.
#' @references
#' Van Buuren S (2014). Growth charts of human development.
#' Stat Methods Med Res, 23(4), 346-368.
#' @examples 
#' # define quadrature points for D-score
#' theta <- seq(-10, 80, 1)
#' 
#' # calculate and plot three priors
#' plot(x = theta, y= adp(1/12, theta), type = "l", 
#'   main = "Priors at ages of 1, 12 and 24 months", 
#'   ylab = "Density", xlab = "D-score")
#' lines(x = theta, adp(1, theta), lty = 2)
#' lines(x = theta, adp(2, theta), lty = 3)
#' @export
adp <- function(age, theta, mu = NULL, sd = 5, 
                reference = dscore::Dreference, ...) {
  if (is.null(mu)) mu <- approx(y = reference$mu, x = reference$year,
               xout = round(age, 4), yleft = reference$mu[1])$y
  p <- dnorm(theta, mean = mu, sd = sd)
  return(normalize(p, theta))
}

#' Normalize distribution
#'
#' Normalizes the distribution so that the total mass equals 1.
#' @aliases normalize
#' @param d A vector with \code{length(theta)} elements representing
#' the unscaled density at each quadrature point.
#' @param theta Vector of equally spaced quadrature points.
#' @return A \code{vector} of \code{length(d)} elements with 
#' the prior density estimate at each quadature point.
#' @export
normalize <- function(d, theta) {
  d <- d / sum(d)
  return(d / (theta[2] - theta[1]))
}



setprior <- function(dag, theta,.prior.theta.per.day){
  # returns the unscaled age-dependent prior N(theta.day,6)
  # return(dnorm(theta,mean=.prior.theta.per.day[dag],sd=4))
  
  return(dnorm(theta,mean=.prior.theta.per.day[dag],sd=5))
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







