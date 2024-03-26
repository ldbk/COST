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
zz <- startTest("ceDataTest.txt")
tagTest("ceData testing ...")

data(soleData)
checkRun(sole.ce <- ceData(ce[,-1]))
checkTrue(is(sole.ce, "ceData"))

#! Checks
checkTrue(checkCEnms(ce))

#! Constructors
checkRun(ceobj <- new("ceData"))
checkTrue(is(ceobj, "ceData"))
checkRun(ceobj <- ceData())
checkTrue(is(ceobj, "ceData"))
checkEqual(ceobj@ce, ce(ceobj))
checkRun(ceobj <- ceData(ce[,-1]))
checkTrue(is(ceobj, "ceData"))

# Accessors
checkEqual(ceobj@ce, ce(ceobj))

#! check object is well formed
#! need more work !!
checkRun(o0 <- ce(ceobj))
o0 <- c(o0)
names(o0) <- NULL
o0 <- lapply(o0, "as.character")
o <- c(ce)
names(o) <- NULL
o <- lapply(o, "as.character")
checkEqual(o[-1], o0)

#! Selection
checkRun(subset(sole.ce, quarter==1))
checkTrue(is(subset(sole.ce, quarter==1), "ceData"))
checkRun(subset(sole.ce, quarter==1 & area=="27.7.e"))
checkTrue(is(subset(sole.ce, quarter==1 & area=="27.7.e"), "ceData"))
checkRun(subset(sole.ce, 1))
checkTrue(is(subset(sole.ce, 1), "ceData"))

#! utils methods
checkRun(head(sole.ce))
checkRun(tail(sole.ce))
checkRun(summary(sole.ce))
checkRun(dim(sole.ce))

#! rbind2
ce1 <- subset(sole.ce, quarter==1)
ce2 <- subset(sole.ce, quarter==2)
checkRun(ce3 <- rbind2(ce1,ce2))
checkTrue(is(ce3,"ceData"))

finishTest()
