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
zz <- startTest("clDataValTest.txt")
tagTest("clDataVal testing ...")

data(sole)

# constructor
checkRun(clv <- clDataVal(sole.cl))
checkTrue(is(clv, "clDataVal"))

finishTest()

