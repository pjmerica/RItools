require("RItools")

data(nuclearplants)
### should be same as wilcox.test result
(ar1 <- alignedrank.test(cost~pr, nuclearplants))
### wilcox test stuff...

alignedrank.test(cost~pr|pt, nuclearplants)

alignedrank.test(cost~pr, nuclearplants, effect.generator=function(tx.effect,tx.var,response.var,...){response.var/(1+tx.var*tx.effect)})

### some internal stuff
myfun0 <- function(x) {x+a}
myfun1 <- function(x,...) {x+a}

do.call(myfun0, list(x=3, a=2))
do.call(myfun1, list(x=3, a=2)) # Didn't do what I was hoping it would...
