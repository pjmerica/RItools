alignedrank.test <- function(formula, data, tx.effects=c('0'=0), na.last=NA, ties.method="average", effect.remover=function(tx.effect,tx.var,response.var){response.var - tx.effect*tx.var}, stratum.weights=harmonic,normalize.weights=TRUE)
{
### use rank(), not tfrm().

  stopifnot(is.function(effect.remover),
            !is.null(nfmls <- names(formals(effect.remover))))

  if (is.list(stratum.weights)) stop("stratum.weights argument should be a weight-generating function or a vector of weights.")
  if (nfmls[1]!='tx.effect') stop("First arg of function effect.remover should be 'tx.effect'.")
  nfmls <- nfmls[-1]
  if (!all(nfmls%in%c('tx.var','response.var', names(data))))
    stop("Args of function effect.remover must appear in
c('tx.effect','tx.var',names(data))")

  
  tes <- unpackHyps(tx.effects)
  parsed <- parseFormula(formula)

  xb.fmla <- paste(names(tes), collapse="+")
  xb.fmla <- as.formula(paste(parsed[['z']], xb.fmla,sep='~'))

  pf <- parent.frame()
  zz <- evalq(parsed[['z']],data,pf)
  yy <- evalq(parsed[['y']],data,pf)
  ss <- evalq(parsed[['s']],data,pf)
    
  if (length(tes)-1)
    effect.remover <- function(txeffects,...) {
    sapply(txeffects, effect.remover, ...)}

  adjymat <-do.call(effect.remover, list(txeffects=tes,"..."=data.frame(tx.var=zz,response.var=yy, data)[nfmls]), envir=pf) # Have to test the ... part

gs.df <- xBalance.find.goodstrats(data.frame('a'=ss), zz, adjymat)
swt <- xBalance.make.stratwts(stratum.weights,data.frame('a'=ss),gs.df,zz,data,normalize.weights=normalize.weights)
??? <-  xBalanceEngine(ss=ss, zz=zz, mm=adjymat,
               report=c('z.scores','p.values'), swt, s.p=1, normalize.weights=normalize.weights,zzname=NULL)
### PROBABLY DON'T NEED THIS AFTER ALL:                                      
  structure.fmla <- if (!is.null(parsed[['s']])) {
    as.formula(paste(parsed[['z']],parsed[['s']],sep="~")) } else {
      as.formula(paste(parsed[['z']],"~1",sep="~")) }

                        
                      }

unpackHyps <- function(x, ...) UseMethod("unpackHyps", x)
unpackHyps.default <- function(x)
  {
 nms <- paste('te', if (is.null(names(x))) as.character(x) else names(x), sep="")
ans <- x
 names(ans) <- nms
 ans
  }

parseFormula <- function(fmla)
  {
outcome <- fmla[[2]]
  rhs <- fmla[[3]]
  if (length(rhs) == 3 && rhs[[1]] == as.name("|")) {
    treatment <- rhs[[2]]
    group <- rhs[[3]]
  } else {
  treatment <- rhs
  group <- NULL
}  
  return(c('y'=outcome, 'z'=treatment, 's'=group))
}

Alignedranktest <- function(fmla, trtvar, data, level=.05, length=12)
  {
    stopifnot(length(fmla)==3, is.logical(trtvar),
              all(substr(names(data),1,2)!="T="))
m0 <- MASS::rlm(update.formula(fmla, .~.+ZzZz), data.frame(data, ZzZz=trtvar))
rv <- summary(m0)$coeff['ZzZz',1:2]
  est <- rv[1] ; se <- rv[2]
    ct <- qnorm(1-level/2)*1.5
    ndf <- m0$model
    dgt <- floor(log10(signif(ct*se/length, 2) - ct*se/length))
    ndf[['T=-Inf']] <-  m0$y + trtvar*(max(m0$y[!trtvar]) - min(m0$y[trtvar]))

    for (t.o in seq(est - ct*se, est + ct*se, len=(length-2)) )
      {
        ndf[[paste('T', signif(t.o, dgt), sep="=")]] <-
          m0$y - trtvar*t.o
      }
    ndf[['T=+Inf']] <-  m0$y - trtvar*(max(m0$y[trtvar]) - min(m0$y[!trtvar]))

    newfmla <- paste("ZzZz~",
                     paste(names(ndf)[substr(names(ndf),1,2)=="T="],
                           collapse=" + ")
                     )
    newfmla <- as.formula(newfmla)

    xb <- xBalance(newfmla, fmla[-2],
                    data=data.frame(ndf, ZzZz=trtvar),
                   chisquare=FALSE, covariate.scaling=1)
    for (tv in names(ndf)[substr(names(ndf),1,2)=="T="]) {
      ndf[[tv]] <- rep(NA_real_, dim(ndf)[1])
      ndf[[tv]][complete.cases(ndf)] <-
        rank(resid(lm(update.formula(fmla, paste(tv,"~.")),
                      data=ndf[complete.cases(ndf),])
                   )
             )
      NULL
    }
    
   xb1 <- xBalance(newfmla, fmla[-2], data=data.frame(ndf, ZzZz=trtvar),
                   chisquare=FALSE, covariate.scaling=1)
    xb[4] <- xb1[4]
    xb
  }
