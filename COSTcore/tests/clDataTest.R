#=====================================================================
#
# Date: 13/04/2007
# Version: 0.1-0
# Authors: Ernesto Jardim
#
# Short description: tests for FLlst
#
# ToDo:
#
# References (bibtex):
#
#!Notes:
#
#=====================================================================

library(COSTcore)

# start test
setCon()
zz <- startTest("clDataTest.txt")
tagTest("clData testing ...")

data(soleData)
checkRun(sole.cl <- clData(cl[,-1]))
checkTrue(is(sole.cl, "clData"))

#! Checks
checkTrue(checkCLnms(cl))

#! Constructors
checkRun(clobj <- new("clData"))
checkTrue(is(clobj, "clData"))
checkRun(clobj <- clData())
checkTrue(is(clobj, "clData"))
checkEqual(clobj@cl, cl(clobj))
checkRun(clobj <- clData(cl[,-1]))
checkTrue(is(clobj, "clData"))

# Acclssors
checkEqual(clobj@cl, cl(clobj))

#! check object is well formed
checkRun(o0 <- cl(clobj))
o0 <- c(o0)
names(o0) <- NULL
o <- c(cl)
names(o) <- NULL
checkEqual(o[-1], o0)

#! Replacement
#checkFail(sole.cl[cl(sole.cl)$quarter==1,"quarter"]<-5)
#checkRun(sole.cl[,"landValue"]<-10)
#checkTrue(is(sole.cl,"clData"))
#checkFail(sole.cl[,"quarter"]<-10)

#! Selection

#checkRun(sole.cl[cl(sole.cl)$quarter==1,])
#checkTrue(is(sole.cl[cl(sole.cl)$quarter==1,], "clData"))
#checkRun(sole.cl[cl(sole.cl)$quarter==1,2])
#checkTrue(is(sole.cl[cl(sole.cl)$quarter==1,2], "factor"))
#checkRun(sole.cl[,2])
#checkTrue(is(sole.cl[,2], "factor"))
#checkRun(sole.cl[cl(sole.cl)$quarter==1,"quarter"])
#checkTrue(is(sole.cl[cl(sole.cl)$quarter==1,"quarter"],"factor"))
#checkRun(sole.cl[,"quarter"])
#checkTrue(is(sole.cl[,"quarter"],"factor"))
#checkRun(sole.cl[cl(sole.cl)$quarter==1,c(1:4)])
#checkTrue(is(sole.cl[cl(sole.cl)$quarter==1,c(1:4)],"data.frame"))
#checkRun(sole.cl[cl(sole.cl)$quarter==1,c("year","quarter")])
#checkTrue(is(sole.cl[cl(sole.cl)$quarter==1,c("year","quarter")],"data.frame"))
checkRun(subset(sole.cl, quarter==1))
checkTrue(is(subset(sole.cl, quarter==1), "clData"))
checkRun(subset(sole.cl, quarter==1 & area=="27.7.e"))
checkTrue(is(subset(sole.cl, quarter==1 & area=="27.7.e"), "clData"))
checkRun(subset(sole.cl, 1))
checkTrue(is(subset(sole.cl, 1), "clData"))

#! utils methods
checkRun(head(sole.cl))
checkRun(tail(sole.cl))
checkRun(summary(sole.cl))
checkRun(dim(sole.cl))

#! rbind2
cl1 <- subset(sole.cl, quarter==1)
cl2 <- subset(sole.cl, quarter==2)
checkRun(cl3 <- rbind2(cl1,cl2))
checkTrue(is(cl3,"clData"))

finishTest()
