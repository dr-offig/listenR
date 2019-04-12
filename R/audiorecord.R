########## Quasi Continuous Recording ##########
## made up of a sequence of audio files
## abstracts away sample rate etc.

# Maximum amount of memory
R_MEMORY_MAX <- 2^32
mem_required <- function(frames,channels) { frames * channels * 8 * 2}
mem_available <- function() { R_MEMORY_MAX - pryr::mem_used() }

# Checking whether a file can be completely loaded
determineLoadFrames <- function(fname) {

  # read in the header
  waveHeader <- tuneR::readWave(fname, header=TRUE, toWaveMC=TRUE)
  frames <- waveHeader$samples
  channels <- waveHeader$channels
  #samplerate <- waveHeader$samplerate

  # see if there is enough memory to load the full audio data
  ma <- mem_available();
  mr <- mem_required(frames=frames, channels=channels)
  N <- ceiling(mr / ma)
  L <- frames %/% N

  # partition file into N+1 'subfiles'
  startFrames <- seq(from=0, by=L, length.out = N)
  endFrames <- seq(from=L, by=L, length.out = N)
  fnames <- rep(fname,N)
  #return(mapply(function(a,b) { list(filename=fname, from=a, to=b) }, startFrames, endFrames))
  data.frame(filename=fnames, from=startFrames, to=endFrames, stringsAsFactors = FALSE)
}


Audiorecord <- R6::R6Class(
  "Audiorecord",
   public = list(
     audiofiles = NULL,
     start_times = NULL,
     initialize = function(filenames=NULL, ...) {
       arguments <- list(...)
       if (is.null(filenames) && length(arguments) == 0) {
         # default constructor
         # nothing to do
       } else if (!is.null(filenames)) {
         fnames <- filenames
         loadList <- do.call(rbind,lapply(fnames, function(fname) { determineLoadFrames(fname) }))
         #str(loadList)

        self$audiofiles <- mapply(
          function(fnm, a, b) {
            Audiofile$new(fnm, from=a, to=b)
          },
          loadList$filename, loadList$from, loadList$to)

        # assume continuous unless otherwise told
         elapsed <- 0
         self$start_times <- vector("numeric",length(self$audiofiles))
         for (i in seq_along(self$audiofiles)) {
            self$start_times[[i]] <- elapsed
            elapsed <- elapsed + self$audiofiles[[i]]$duration()
         }

       #} else if (is.list(arg) && length(arg) > 0 && "Audiofile" %in% class(arg[[1]])) {
        # self$audiofiles <- arg
       #} else if ("Audiorecord" %in% class(arg)) { # actually copies the data
        # self$audiofiles <- lapply(arg$audiofiles,Audiofile$new)
       } else {
         cat(paste0("Attempted to construct audiofile from ", str(arguments)))
       }
     },
     duration = function() {
       if (length(self$audiofiles) > 0) {
         return(last(self$start_times) + last(self$audiofiles)$duration())
       }
     },
     candidateFileIndexForTime = function(t) {
       min(which(self$start_times > t)) - 1
     },
     hasObservationAtTime = function(t) {
       ind <- candidateFileIndexForTime(t)
       if (is.infinite(ind))
         return(FALSE)
       else {
         st <- self$start_times[[ind]]
         cand <- self$audiofiles[[ind]]
         if (t < st + cand$duration()) return(TRUE)
         else return(FALSE)
       }
     },
     observedFrameCount = function() {
       sum(sapply(self$audiofiles,function(af){ af$duration() }))
     },
     observedFrameCountBetween = function(from,to) {
      ind1 <- candidateFileIndexForTime(from)
      if (is.infinite(ind1)) {
        return(0)
      } else {
        #if (!hasObservationAtTime(from)) ind1 <- ind1 + 1
        ind2 <- candidateFileIndexForTime(to)
        if (is.infinite(ind2)) {
          return( max(0,(self$audiofiles[[ind1]]$duration() - (from - self$start_times[[ind]]))) +
                  sum(sapply(self$audiofiles[which(self$start_times > from)],function(af){ af$duration() })) )
        } else if (ind2 - ind1 > 1) {
          return( max(0,(self$audiofiles[[ind1]]$duration() - (from - self$start_times[[ind1]]))) +
                  min(self$audiofiles[[ind2]]$duration(), to - self$start_times[[ind2]]) +
                    sum(sapply(self$audiofiles[(ind1+1):(ind2-1)],function(af){ af$duration() })) )
        } else {
          return( max(0,(self$audiofiles[[ind1]]$duration() - (from - self$start_times[[ind1]]))) +
                    min(self$audiofiles[[ind2]]$duration(), to - self$start_times[[ind2]]))
        }
      }
     }
   )
)


Audiorecord$set("public","spectrogramFrame",
function(from=0,fftSize,fftHop,frameWidth,frameHeight)
{
  numWindows <- frameWidth  # one (vertical line of) pixel(s) for each fft window
  numAudioFrames <- fftSize + fftHop * (numWindows - 1)
  to <- from + numAudioFrames

  # TODO: If period is missing segments, just paste together spectrogram bits
  # N <- self$observedFrameCountBetween(from=1, to=numAudioFrames)

  output <- NULL

  # Read in as many files as needed to fill the data.table
  tmp <- from
  candInd <- self$candidateFileIndexForTime(tmp)
  remainingWindows <- numWindows
  while(tmp < to && is.finite(candInd)) {
    af <- self$audiofiles[[candInd]]
    af$calculateSpectrogram(n=fftSize,h=fftHop) # will also loadAudio if needed

    # append this spectrogram to the output
    if (is.null(output)) { output <- copy(af$spectrogram[1:remainingWindows,]) }
    else { output <- rbind(output,af$spectrogram[1:remainingWindows,]) }

    tmp <- tmp + af$duration(); # TODO: think about this for gapped files
    candInd <- max(candInd+1,self$candidateFileIndexForTime(tmp))
    remainingWindows <- numWindows - dim(output)[[1]]
  }

  return(output)

})

