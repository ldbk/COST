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
zz <- startTest("csDataTest.txt")
tagTest("csData testing ...")

data(soleData)           
checkRun(sole.cs <- csData(tr=tr[,-1], hh=hh[,-1], sl=sl[,-1], hl=hl[,-1], ca=ca[,-1]))
checkTrue(is(sole.cs, "csData"))

#! Checks

checkTrue(checkTRnms(tr))
checkTrue(checkHHnms(hh))
checkTrue(checkSLnms(sl))
checkTrue(checkHLnms(hl))
checkTrue(checkCAnms(ca))

# Constructors
checkRun(csobj <- new("csData"))
checkTrue(is(csobj, "csData"))
checkRun(csobj <- csData())
checkTrue(is(csobj, "csData"))
checkRun(csobj <- csData(tr[,-1], hh[,-1], sl[,-1], hl[,-1]))
checkTrue(is(csobj, "csData"))
checkRun(csobj <- csData(tr[,-1], hh[,-1], sl[,-1], hl[,-1], ca[,-1]))
checkTrue(is(csobj, "csData"))

# Accessors
checkEqual(csobj@tr, tr(csobj))
checkEqual(csobj@hh, hh(csobj))
checkEqual(csobj@sl, sl(csobj))
checkEqual(csobj@hl, hl(csobj))
checkEqual(csobj@ca, ca(csobj))

#! check object is well formed
# tr
checkRun(o0 <- tr(csobj))
o0 <- c(o0)
names(o0) <- NULL
o <- c(tr)
names(o) <- NULL
checkEqual(o[-1], o0)

# hh
checkRun(o0 <- hh(csobj))
o0 <- c(o0)
names(o0) <- NULL
o <- c(hh)
names(o) <- NULL
checkEqual(o[-1], o0)

# sl
checkRun(o0 <- sl(csobj))
o0 <- c(o0)
names(o0) <- NULL
o <- c(sl)
names(o) <- NULL
checkEqual(o[-1], o0)

# hl
checkRun(o0 <- hl(csobj))
o0 <- c(o0)
names(o0) <- NULL
o <- c(hl)
names(o) <- NULL
checkEqual(o[-1], o0)

# ca
checkRun(o0 <- ca(csobj))
o0 <- c(o0)
names(o0) <- NULL
o <- c(ca)
names(o) <- NULL
checkEqual(o[-1], o0)

#! Replacement

#! Selection
checkRun(cs1 <- subset(sole.cs, trpCode=="LIM1"))
checkTrue(is(cs1, "csData"))
checkRun(cs1 <- subset(sole.cs, 1))
checkTrue(is(cs1, "csData"))

#! utils methods
checkRun(head(sole.cs))
checkRun(tail(sole.cs))
checkRun(summary(sole.cs))
checkRun(dim(sole.cs))

#! rbind2
cs1 <- subset(sole.cs, trpCode=="LIM1")
cs2 <- subset(sole.cs, 1)
checkRun(cs3 <- rbind2(cs1,cs2))
checkTrue(is(cs3, "csData"))

finishTest()

