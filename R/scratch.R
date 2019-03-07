source('R/audiofile.R')
source('R/cwt.R')
testfile1 <- "/home/ybot/code/R/Bees/data/20190210_CLAY001/22-44.870-22-51.554.wav"
testfile2 <- "/home/ybot/code/R/Bees/data/20190226_CLAY001/18-21.074-18-38.698.wav"
testsound1 <- Audiofile$new(testfile1)
testsound2 <- Audiofile$new(testfile2)


