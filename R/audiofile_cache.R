# Handling of large audio files for analysis
# Functions to trade-off memory and cpu
# NOTE - THIS CODE ASSUMES AUDIO FILENAMES ARE UNIQUE
if(!exists("AUDIOFILE_CACHE.RR"))
{ AUDIOFILE_CACHE.R <- TRUE

#library(data.table)
#source("./audiofile.R")

# How much memory (max) do we want R to use for open audio files
AUDIOFILE_CACHE.MAX_MEMORY <- 2^34

# Maintain a database of open audio files
# audioDB <- data.table(filename=character(),
#                       frames=integer(),
#                       channels=integer(),
#                       samplerate=integer(),
#                       audiofile=vector(mode="list",length=0),
#                       stringsAsFactors = FALSE)
audiofileCache <- list()

# Check if file is cached
.audiofile_loaded <- function(fname)
{
  # check cache
  if (sum(grep(fname,as.vector(audioDB$filename),fixed=TRUE)) > 0)
    return(TRUE)
  else 
    return(FALSE)
}


.load_audiofile_into_cache <- function(fnm)
{
  obj <- Audiofile$new(fname=fnm)
  audiofileCache <<- list(obj,audiofileCache)
    
  # obj <- readWave(fname,toWaveMC=TRUE)
  # newRow <- list(filename=fname,
  #                      frames=dim(obj@.Data)[[1]],
  #                      channels=dim(obj@.Data)[[2]],
  #                      samplerate=obj@samp.rate,
  #                      audioData=obj@.Data
  #                      )
  # audioDB <<- list(newRow,audioDB)
}


# Read from audio file
# copyAudioData <- function(filename,from=0,to=-1)
# {
#     
#   
#   
# }


#3testfile1 <- "/home/ybot/code/R/ybot.R/data/audio/catfish/grunts cleared/0_02_150.wav"  
#testfile2 <- "/home/ybot/code/R/ybot.R/data/audio/catfish/grunts cleared/0_10_830.wav"  

  
}