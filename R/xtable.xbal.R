
xtable.xbal <- function(x,caption = NULL, label = NULL, align =c("l",rep("r",ncol(xvardf))),
                          digits = 2, display = NULL, col.labels=NULL, ...)
  {##By default use decimal alignment, which will require the dcolumn package in latex and an appropriate column definition like:
    ##\newcolumntype{.}{D{.}{.}{2.2}}
    ##Here is an example which works
    ##xb1<-xBalance(pr~ date + t1 + t2 + cap + ne + ct + bw + cum.n,
    ##         strata=data.frame(unstrat=factor(character(32)),
    ##           pt=factor(nuclearplants$pt)),
    ##         data=nuclearplants,
    ##         report=c("adj.means","adj.mean.diffs",'std.diffs', 'z.scores', 'chisquare.test','p.values'))
    ##
    ##junk<-xtable(xb1)
    ##print(junk,add.to.row=attr(junk,"latex.add.to.row"),hline.after=c(0,nrow(junk)),sanitize.text.function=function(x){x},floating=TRUE,floating.environment="sidewaystable")

    stopifnot(require(xtable))
    xprint <- flatten.xbalresult(x)
    numstrata<-dim(x$results)[3]
    latex.annotation <- attr(xprint, "latex.annotation")
    xvardf<-xprint$vartable

    byrow.rounding <- c(grep("=0$", names(xvardf)),
                        grep("=1$", names(xvardf)),
                        grep("adj.diff", names(xvardf))
                        )
    byrow.rounding <- sort(unique(byrow.rounding))
    if (length(byrow.rounding))
      {
        byrow.rounding.digits <- if (length(digits)>1)
          c(digits[1],rep_len(digits[-1], ncol(xvardf)))[byrow.rounding[1]] else digits
        roundfn <- function(x)
            {
              thedigits <- floor(-log10(min(abs(x))))+byrow.rounding.digits
              format(round(x, digits=thedigits))
          }

        xvardf[byrow.rounding] <- if (length(byrow.rounding)>1) {
          t(apply(xvardf[byrow.rounding],1,roundfn))
        } else signif(xvardf[byrow.rounding], byrow.rounding.digits)
                                                
    }
    if (is.null(display)) {
        display <- rep("fg", ncol(xvardf))
        display[sapply(xvardf, is.character) | sapply(xvardf, is.factor) ] <- "s"
        display[byrow.rounding] <- "s"
        display <- c("s", display)
    }
        clabs <- names(xvardf)

    if (!is.null(col.labels))
      {
        stopifnot(length(col.labels)<=length(clabs), #replace w/ more informative version
                  is.character(col.labels))
        
        if (!is.null(names(col.labels)))
          {
            names(clabs) <- clabs
            col.labels <- col.labels[names(col.labels)%in%names(clabs)]
            for (clnm in names(col.labels))
              clabs[names(clabs)==clnm] <- col.labels[clnm]
      } else if (length(col.labels)) {
        clabs[1L:length(col.labels)] <- col.labels
      }
      } 
            names(xvardf) <- clabs
    ##call xtable on the resulting data.frame
    vartab <- xtable(xvardf,caption=caption, label=label, digits=digits,align=align,display=display,col.labels=col.labels,...) ##NextMethod("xtable",xvardf)
    structure(vartab,
              latex.add.to.row=list(pos=list(-1),command=latex.annotation),
              hline.after=c(0,nrow(xvardf)))

  }


