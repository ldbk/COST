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
zz <- startTest("csDataConsTest.txt")
tagTest("csDataCons testing ...")
                                  
data(sole)

# constructor
checkRun(csc <- csDataCons(csDataVal(sole.cs)))
checkTrue(is(csc, "csDataCons"))
#test on dimensions
checkTrue(nrow(sl(sole.cs))==nrow(sl(csc)))
checkTrue(nrow(hl(sole.cs))==nrow(hl(csc)))
checkTrue(nrow(ca(sole.cs))==nrow(ca(csc)))

finishTest()                          

