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
zz <- startTest("ceDataConsTest.txt")
tagTest("ceDataCons testing ...")

data(sole)

# constructor
checkRun(cec <- ceDataCons())
checkTrue(is(cec, "ceDataCons"))
checkRun(cec <- ceDataCons(ceDataVal(sole.ce)))
checkTrue(is(cec, "ceDataCons"))

finishTest()

