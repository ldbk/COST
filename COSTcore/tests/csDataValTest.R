#=====================================================================
#
# Date: 17/10/2007
# Version: 0.1-0
# Authors: Ernesto Jardim
#
# Short description: tests for ceDataVal
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
zz <- startTest("csDataValTest.txt")
tagTest("csDataVal testing ...")

data(sole)

# constructor
checkRun(csv <- csDataVal(sole.cs))            
checkTrue(is(csv, "csDataVal"))            

# SUid creator
#checkRun(lst <- createSUid(csv))
#checkIdentical(nrow(lst$tr), nrow(tr(csv)))
#checkIdentical(nrow(lst$hh), nrow(hh(csv)))
#checkIdentical(nrow(lst$sl), nrow(sl(csv)))
#checkIdentical(nrow(lst$hl), nrow(hl(csv)))
#checkIdentical(nrow(lst$ca), nrow(ca(csv)))
#
finishTest()

