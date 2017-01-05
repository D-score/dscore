#' Pairwise Estimation of the Rasch Model under Item Equating
#'
#' This function uses pairwise conditional likelihood estimation for
#' estimating item parameters in the Rasch model.
#' @aliases rasch
#' @param data A matrix or a data frame. Item responses should be coded
#' as `1` (pass) or `0` (fail). Missing responses are allowed and must
#' be coded as \code{NA}.
#' @param equate Optional list with elements corresponding to a character
#' vector of names of variable to be equated. The default (\code{NULL}) indicates
#' that no equating takes place.
#' @param itemcluster Optional integer vector of itemcluster. Different
#' integers correspond to different item clusters. The default (\code{NULL})
#' indicated that no item cluster is set.
#' @param b_fixed Numeric, named vector used for fixing item parameters,
#' whose value are the parameters to be fixed. \code{names(b_fixed)} indicates
#' the column in the data to which the fixed value applies.
#' @param b_init Numeric, named vector of initial item difficulty estimates.
#' Under the default (\code{NULL}) values initial values are calculated internally.
#' @param count A table of counts \code{t(data == 0)} times \code{data == 1}. 
#' Items 
#' not present in \code{count} are silently discarded from the estimation 
#' process. The default (\code{NULL}) calculates the count table from the 
#' data. This will increase execution time substantially if \code{ncol(data)} 
#' is large, e.g. several hundreds of items.
#' @param conv Convergence criterion in maximal absolute parameter change
#' @param maxiter Maximal number of iterations
#' @param progress A logical which displays the iterative process.
#' Default is \code{FALSE}.
#' @param zerosum Optional logical indicating whether item difficulties
#' should be centered in each iteration. The default is that no centering
#' is conducted.
#' @details
#'
#' This function is based on \code{\link[sirt]{rasch.pairwise.itemcluster}},
#' additional detail and examples can be found.
#'
#' The \code{rasch()} function extend the original by incorporating the
#' \code{equate} argument. This facility allows the users to specify that
#' Rasch parameters corresponding to different columns should receive
#' identical values. In addition, some of the arguments have been changed
#' to a more robust and friendly format (e.g. named vectors).
#' @seealso \code{\link[sirt]{rasch.pairwise.itemcluster}},
#' \code{\link[eRm]{RM}}
#' @author Alexander Robitzch (\code{sirt} author), Stef van Buuren
#' @return Fitted object that extends class \code{c("dRm","Rm","eRm")}
#' that is defined and understood by the \code{eRm} package.
#' See \code{\link[eRm]{RM}} for a description. The \code{rasch()} function
#' adds several additional elements (most for compatibility with \code{sirt}):
#' \describe{
#' \item{equate}{The \code{equate} argument}
#' \item{b_fixed}{The \code{b_fixed} argument}
#' \item{itemcluster}{The \code{itemcluster} argument}
#' \item{b}{Vector of item difficulties}
#' \item{item}{Data frame of item parameters (\eqn{N}, \eqn{p} and item
#' difficulty)}
#' \item{count}{The count table that is used by the algorithm. May differ 
#' from the count argument.}
#' \item{dropped}{Items that were dropped because they were not present in the 
#' count table (if specified)}
#' }
#' @references van der Linden, W. J., & Eggen, T. J. H. M. (1986).
#' \emph{An empirical Bayes approach to item banking}. Research Report 86-6,
#' University of Twente.
#'
#' Zwinderman, A. H. (1995). Pairwise parameter estimation in Rasch models.
#' \emph{Applied Psychological Measurement}, \bold{19}, 369-375.
#'
#' @note
#' No standard errors are provided by this function. Use resampling methods
#' for conducting statistical inference.
#'
#' Formulas for asymptotic standard errors of this pairwise estimation method are
#' described in Zwinderman (1995).
#' @examples
#' library("sirt")
#' # Simulate data from the Rasch model
#' base::set.seed(9765)
#' N <- 50000    # number of persons
#' I <- 11       # number of items
#' b <- base::seq(-2, 2, length = I)
#' dat <- sirt::sim.raschtype(stats::rnorm(N), b)
#' base::colnames(dat) <- base::paste0("I", 1:I)
#'
#' # Do the conventional analysis
#' fit1 <- rasch(dat)
#' fit1$item
#'
#' # split items I6 and I9 into two parts
#' dat2 <- dat[, -c(6, 9)]
#' dat2 <- cbind(dat2,
#'            c(dat[1:(N/2),"I6"], rep(NA, N/2)),
#'            c(rep(NA, N/2), dat[(N/2+1):N,"I6"]),
#'            c(dat[1:(N/4),"I9"], rep(NA, 3*N/4)),
#'            c(rep(NA, N/4), dat[(N/4+1):N,"I9"]))
#' colnames(dat2)[10:13] <- c("I6a", "I6b","I9a","I9b")
#'
#' # no equating, parameters close but not identical
#' fit2 <- rasch(dat2)
#' fit2$item
#'
#' # equate I6a and I6b, and I9a and I9b
#' # parameters identical AND close to conventional analysis
#' fit3 <- rasch(dat2,
#'               equate = list(cube = c("I6a", "I6b"), wave = c("I9a", "I9b")))
#' fit3$item
#' @export
rasch <- function(data, equate = NULL, itemcluster = NULL,
                  b_fixed = NULL, b_init = NULL, zerosum = FALSE,
                  count = NULL,
                  conv = .00001, maxiter = 3000, progress = FALSE) {
  call <- match.call()
  
  # discard items from data that are not present in count
  # and set Aij
  count_items <- colnames(count)
  data_items <- colnames(data)
  itemset <- count_items[count_items %in% data_items]
  Aij <- count[itemset, itemset]
  
  X01 <- data
  # silently drop items not present in count (if specified)
  if (!is.null(itemset)) data <- data[, itemset]
  dropped <- setdiff(names(X01), names(data))
  
  data <- as.matrix(data)
  p <- colMeans(data, na.rm = TRUE)
  N <- colSums(1 - is.na(data))
  I <- ncol(data)
  if (is.null(b_init)) b_init <- -stats::qlogis(p)
  
  # initialize b
  b <- b_init
  if (!is.null(b_fixed)) {
    b[names(b_fixed)] <- b_fixed
    exp_b_fixed <- exp(b_fixed)
    zerosum <- FALSE
  }
  
  # create count tables
  if (is.null(count)) {
    data[is.na(data)] <- 9
    Aij <- t(data == 0) %*% (data == 1)
  }
  
  # identify orphans (zero counts) and stop
  orphans <- NULL
  flags <- rowSums(Aij) == 0
  if (any(flags)) {
    orphans <- dimnames(Aij)[[1]][flags]
    cat("Orphans found: ", orphans, "\n")
  }
  
  # set some entries to zero for itemclusters
  clusters <- unique(itemcluster[itemcluster != 0])
  for (cc in clusters) {
    icc <- which(itemcluster == cc)
    Aij[icc, icc] <- 0
  }
  
  # prepare for loop
  nij <- Aij + t(Aij)
  eps0 <- eps <- exp(b)
  max.change <- 10
  iter <- 1
  
  if (is.null(orphans)) {
    while (max.change > conv & iter <= maxiter) {
      b0 <- b
      eps0 <- eps
      m1 <- matrix(eps0, I, I, byrow = TRUE) + matrix(eps0, I, I)
      g1 <- rowSums(nij / m1)
      eps <- rowSums(Aij) / g1
      b <-  log(eps)
      
      # put item parameter constraints
      if (!is.null(b_fixed)) {
        eps[names(exp_b_fixed)] <- exp_b_fixed
      }
      
      # equate estimates
      if (!is.null(equate)) {
        for (i in 1:length(equate)) {
          pos <- match(equate[[i]], names(eps))
          eps[pos] <- weighted.mean(x = eps[pos], w = N[pos])
        }
      }
      
      if (zerosum) {
        b1 <- -log(eps)
        b2 <- b1 - mean(b1)
        eps <- exp(-b2)
      }
      max.change <- max(abs(b - b0))
      if (progress) {
        cat("PL Iter.", iter, ": max. parm. change = ",
            round( max.change , 6 ), "\n")
        flush.console()
      }
      iter <- iter + 1
    }
  }
  item <- data.frame("N" = N,
                     "p" = p ,
                     "b" =  log(eps))
  if (is.null(itemcluster)) { itemcluster <- rep(0, I) }
  item$itemcluster <- itemcluster
  
  # return fitted object that can be understood by eRm package
  res <- list(X = X01, X01 = X01, count = Aij,
              dropped = dropped, orphans = orphans,
              model = "RM", equate = equate, itemcluster = itemcluster,
              b_fixed = b_fixed, zerosum = zerosum,
              loglik = 0, npar = I,
              iter = iter, convergence = conv,
              item = item, b = log(eps),
              etapar = -log(eps), se.eta = NULL, hessian = NULL,
              betapar = -log(eps), se.beta = NULL,
              W = diag(I), call = call)
  class(res) <- c("dRm", "Rm", "eRm")
  return(res)
}

#' Extract difficulty estimates
#'
#' This function extracts the difficulty estimates from a fitted Rasch model.
#' @param object An object that inherits class \code{eRm}
#' @return A named vector with the Rasch item difficulty estimates.
#' @export
get_diff <- function(object) {
  if (inherits(object, "eRm")) return(-object$betapar)
  return(NULL)
}
