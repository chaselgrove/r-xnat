library(RUnit)
source('xnat.R')

test.suite <- defineTestSuite("xnat", 
                              dirs="test.d", 
                              testFileRegexp="^test\\..+\\.R")

test.result <- runTestSuite(test.suite)
printTextProtocol(test.result)

# eof
