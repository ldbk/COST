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
zz <- startTest("clDataConsTest.txt")
tagTest("clDataCons testing ...")

data(sole)

# constructor
checkRun(clc <- clDataCons())
checkTrue(is(clc, "clDataCons"))
checkRun(clc <- clDataCons(clDataVal(sole.cl)))
checkTrue(is(clc, "clDataCons"))

finishTest()

