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
zz <- startTest("ceDataValTest.txt")
tagTest("ceDataVal testing ...")

data(sole)

# constructor
checkRun(cev <- ceDataVal(sole.ce))
checkTrue(is(cev, "ceDataVal"))

finishTest()

