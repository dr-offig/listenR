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

        # set parts
        numParts <- length(self$audiofiles)
        for (i in 1:numParts) {
          af <- self$audiofiles[[i]]
          af$part <- i
          af$parts <- numParts
        }

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
         return(self$start_times[[length(self$start_times)]] + (self$audiofiles[[length(self$audiofiles)]])$duration())
       }
     },
     candidateFileIndexForTime = function(t) {
       lastAfter <- min(which(self$start_times > t))
       if (lastAfter == 1) {
         return(1)
       } else if (is.infinite(lastAfter)) {
         return(length(self$start_times))
       } else {
         return(lastAfter-1)
       }
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
     },
     filenames = function() {
       lapply(self$audiofiles, function(af) { af$filename })
     },
     physicalFiles = function() {
       unique(unlist(self$filenames()))
     },
     copyToFolder = function(folderURL, overwrite=FALSE) {
       file.copy(from=self$physicalFiles(), to=folderURL, overwrite=overwrite)
     }

   )
)


Audiorecord$set("public","spectrogramFrame",
function(from=0,fftSize,fftHop,frameWidth,frameHeight, channel=1)
{
  numWindows <- frameWidth  # one (vertical line of) pixel(s) for each fft window
  #numAudioFrames <- fftSize + fftHop * (numWindows - 1)
  #to <- from + numAudioFrames

  # TODO: If period is missing segments, just paste together spectrogram bits
  # N <- self$observedFrameCountBetween(from=1, to=numAudioFrames)

  output <- NULL

  # Read in as many files as needed to fill the data.table
  tmp <- from
  candInd <- self$candidateFileIndexForTime(tmp)
  remainingWindows <- numWindows
  while(remainingWindows > 0 && is.finite(candInd)) {
    af <- self$audiofiles[[candInd]]
    af$calculateSpectrogram(n=fftSize, h=fftHop, ch=channel) # will also loadAudio if needed
    cat(sprintf("Calculating spectrogram for part %d of %d\n", candInd, length(self$audiofiles)))

    # append this spectrogram to the output
    if (is.null(output)) { output <- copy(af$spectrogram[1:remainingWindows,]) }
    else { output <- rbind(output,af$spectrogram[1:remainingWindows,]) }

    tmp <- tmp + af$duration(); # TODO: think about this for gapped files
    candInd <- max(candInd+1,self$candidateFileIndexForTime(tmp))
    remainingWindows <- numWindows - dim(output)[[1]]
    af$unloadAudio()
  }

  return(list("start_time"=from,"end_time"=tmp, "power"=output))

})


spectroFrame2tiff <- function(frm, filename, contrast=1) {
  A <- t(as.matrix(frm$power[,-c("spectroBlock")]))
  AX <- max(A)
  AA <- A
  if (abs(AX) > 0.0)
    AA <- A / AX

  AA <- AA^contrast
  n <- dim(A)[[1]]
  m <- dim(A)[[2]]
  AAA <- AA[n:1,]
  rasta <- raster(nrows=n, ncols=m)
  rasta <- setValues(rasta, AAA)
  bricka <- brick(rasta)

  tokens <- unlist(strsplit(filename,"[.]"))
  ext <- last(tokens)
  if (ext != "tif" && ext != "tiff")
    filename <- paste0(filename,".tif")

  writeRaster(bricka, filename, overwrite=TRUE)

}


Audiorecord$set("public","spectrogramMovie",
function(filepath, from=0, to=self$duration(), fftSize, fftHop, frameWidth, frameHeight, channel=1, contrast=1)
{
  # create a folder for all the tiff images
  mainIdentifier <- function(path) {
    tokens <- unlist(strsplit(path,"[/]"))
    bits <- unlist(strsplit(tokens[[length(tokens)]],"[.]"))
    paste0(bits[1:(length(bits)-1)],collapse = ".")
  }

  fname <- mainIdentifier(filepath)
  tokens <- unlist(strsplit(filepath,"[.]"))
  dirname <- paste0(tokens[1:(length(tokens)-1)], collapse=".")
  dir.create(dirname)

  self$copyToFolder(dirname)

  frameNumber <- 0
  tifs <- list()
  while (from < to) {
    frm <- self$spectrogramFrame(from=from, fftSize=fftSize, fftHop=fftHop, frameWidth=frameWidth, frameHeight=frameHeight, channel=channel)
    imgFilename <- sprintf("%s_%06d", fname, frameNumber)
    tifPath <- paste0(dirname, "/", imgFilename, ".tif")
    # if (stringr::str_sub(imgPath,stringr::str_length(imgPath)-4) == ".tiff")
    #   imgPath <- stringr::str_sub(imgPath,end=-2)
    #
    #tifPath <- stringr::str_replace(imgPath,".tif{1,2}$",".tif")
    spectroFrame2tiff(frm,tifPath,contrast)
    tifImg <- image_read(tifPath)
    tifs <- c(tifs,tifImg)
    gifPath <- stringr::str_replace(tifPath,".tif{1,2}$",".gif")
    image_write(tifImg, gifPath, format='gif')
    frameNumber <- frameNumber + 1
    from <- frm$end_time + 0.001
    cat(sprintf("Exported frame covering %d secs to %d secs, of a total %d secs\n", floor(frm$start_time), floor(frm$end_time), floor(to)))
  }

  img1filename <- sprintf("%s_%06d", fname, 0)
  img1path <- paste0(dirname, "/", img1filename, ".gif")
  blankImgFilename <- sprintf("%s_blank", fname, 0)
  blankImgPath <- paste0(dirname, "/", blankImgFilename, ".gif")
  imgdev <- image_draw(image_read(img1path))
  rect(0,0,frameWidth,frameHeight,col='black')
  blankImg <- image_capture()
  dev.off()
  image_write(blankImg,blankImgPath,format='gif')

  fullImage <- image_append(do.call(c,tifs))
  fullImgPath <- paste0(dirname, "/", fname, ".gif")
  image_write(fullImage, fullImgPath, format='gif')

  return(TRUE)
})
