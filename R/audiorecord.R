########## Quasi Continuous Recording ##########
## made up of a sequence of audio files
## abstracts away sample rate etc.

## Memory constraint aware parellelisation from package snpEnrichment ##
maxCores <- function (mc.cores = 1) {
  if (Sys.info()[["sysname"]] == "Linux") {
    nbCores <- parallel::detectCores()
    mc.cores.old <- mc.cores
    if (file.exists("/proc/meminfo")) {
      memInfo <- readLines("/proc/meminfo")
      sysMemFree <- memInfo[grep('^MemFree:', memInfo)]
      sysMemCached <- memInfo[grep('^Cached:', memInfo)]
      sysMemAvailable <- 0.95*(as.numeric(gsub("[^0-9]*([0-9]*)", "\\1", sysMemFree)) + as.numeric(gsub("[^0-9]*([0-9]*)", "\\1", sysMemCached)))
      sysProc <- as.numeric(unlist(strsplit(system(paste("ps v", Sys.getpid()), intern = TRUE)[2], " +"), use.names = FALSE)[8])
      mc.cores <- max(min(as.numeric(mc.cores), floor(sysMemAvailable/sysProc)), 1)
      if (mc.cores > nbCores) {
        mc.cores <- nbCores
      } else {}
      if (mc.cores != mc.cores.old) {
        warning(paste0('To avoid memory overload "mc.cores" was decreased to "', mc.cores, '".'), call. = FALSE)
      } else {}
    } else {
      mc.cores <- ifelse(mc.cores.old>nbCores, nbCores, mc.cores.old)
    }
  } else {
    mc.cores <- 1
  }
  return(mc.cores)
}


mclapply2 <- function (X, FUN, ..., mc.preschedule = TRUE, mc.set.seed = TRUE, mc.silent = FALSE, mc.cores = getOption("mc.cores", 2L), mc.cleanup = TRUE, mc.allow.recursive = FALSE) {
  if (Sys.info()[["sysname"]] != "Linux") {
    mc.cores <- 1
  } else {
    mc.cores <- min(parallel::detectCores(), mc.cores)
  }
  return(parallel::mclapply(X = X, FUN = FUN, ...,
                  mc.preschedule = mc.preschedule, mc.set.seed = mc.set.seed, mc.silent = mc.silent,
                  mc.cores = maxCores(mc.cores), mc.cleanup = mc.cleanup, mc.allow.recursive = mc.allow.recursive))
}


# utility functions
as.row.list <- function(tbl) {
  unlist(apply(tbl, 1, list), recursive = FALSE)
}

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
  ma <- as.numeric(mem_available());
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


## Utilities ##
mainIdentifier <- function(path) {
  tokens <- unlist(strsplit(path,"[/]"))
  bits <- unlist(strsplit(tokens[[length(tokens)]],"[.]"))
  paste0(bits[1:(length(bits)-1)],collapse = ".")
}

directoryOfFile <- function(path) {
  tokens <- unlist(strsplit(path,"[/]"))
  paste0(tokens[1:(length(tokens)-1)],collapse="/")
}

correspondingWavFilename <- function(path) {
  paste0(directoryOfFile(path), "/" , mainIdentifier(path), ".wav")
}


## Main class ##
Audiorecord <- R6::R6Class(
  "Audiorecord",
   public = list(
     audiofiles = NULL,
     start_times = NULL,
     initialize = function(filenames=NULL, samplerate=48000, ...) {
       arguments <- list(...)
       if (is.null(filenames) && length(arguments) == 0) {
         # default constructor
         # nothing to do
       } else if (!is.null(filenames)) {

         fnames <- unique(filenames)
         fnames <- lapply(fnames, function(fname) { convertIntoWav(fname,samplerate) })
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
       firstAfter <- min(which(self$start_times > t))
       if (firstAfter == 1) {
         return(1)
       } else if (is.infinite(firstAfter)) {
         return(length(self$start_times))
       } else {
         return(firstAfter-1)
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
     regionIsFullyContainedInPart = function(timeA, timeB, part) {
       return(self$start_times[[part]] <= timeA && timeB <= self$start_times[[part]] + self$audiofiles[[part]]$duration())
     },
     filenames = function() {
       lapply(self$audiofiles, function(af) { af$filename })
     },
     physicalFiles = function() {
       unique(unlist(self$filenames()))
     },
     copyToFolder = function(folderURL, overwrite=FALSE) {
       file.copy(from=self$physicalFiles(), to=folderURL, overwrite=overwrite)
     },
     unloadAudio = function() {
       lapply(self$audiofiles, function(af) { af$unloadAudio() })
     }

   )
)


Audiorecord$set("public","spectrogramFrame",
function(from=0,fftSize,fftHop,frameWidth,frameHeight, channel=1)
{
  # 'from' may be passed in as an hms time string
  if (is.character(from))
    from <- as.numeric(as.hms(from))

  cat(sprintf("\n\n----------------------\nRendering frame from time: %f seconds\n",from))
  numWindows <- frameWidth  # one (vertical line of) pixel(s) for each fft window
  #numAudioFrames <- fftSize + fftHop * (numWindows - 1)
  #to <- from + numAudioFrames

  # TODO: If period is missing segments, just paste together spectrogram bits
  # N <- self$observedFrameCountBetween(from=1, to=numAudioFrames)

  output <- NULL

  # Read in as many files as needed to fill the data.table
  tmp <- from
  candInd <- self$candidateFileIndexForTime(tmp)
  lastCandInd <- 0
  remainingWindows <- numWindows
  while(remainingWindows > 0 && is.finite(candInd)) {
    cat(sprintf("%d out of %d windows remaining to be processed\n", remainingWindows, numWindows))
    if (candInd != lastCandInd) {
      if (lastCandInd > 0) {
        oldAf <- self$audiofiles[[lastCandInd]]
        oldAf$unloadAudio()
      }
      af <- self$audiofiles[[candInd]]
      af$calculateSpectrogram(n=fftSize, h=fftHop, ch=channel) # will also loadAudio if needed
      lastCandInd <- candInd
    }
    #cat(sprintf("Calculating spectrogram for part %d of %d\n", candInd, length(self$audiofiles)))

    # Now that we have a particular audio file, calculate
    # various time-sample conversions
    # see if the whole frame sits inside this audio part
    #remainingAudioFrames <- fftSize + fftHop * (remainingWindows - 1)
    #fftWindowDuration <- fftSize / af$samplerate

    # all of these 'remaining' measures are relative to the audiofile starttime
    remainingStartTimeOffset <- tmp - self$start_times[[candInd]]
    possibleStartFFTWindows <- af$spect_windows_containing_offset_time(remainingStartTimeOffset)
    remainingStartFFTWindow <- possibleStartFFTWindows[[length(possibleStartFFTWindows)]]
    remainingEndFFTWindow <- remainingStartFFTWindow + (remainingWindows - 1)
    LAST_WINDOW_IN_FILE <- NROW(af$spectrogram)

    #remainingEndTimeOffset <- remainingStartTimeOffset + (remainingAudioFrames / af$samplerate)
    #remainingStartFFTWindow <- af$convert(remainingStartTimeOffset,from="seconds", to="spectral_blocks")
    #remainingEndFFTWindow <- af$convert(remainingEndTimeOffset,from="seconds", to="spectral_blocks") - 1

    cat(sprintf("Absolute start time is %f. Relative start %f (fft window %d). Requested relative end %f (fft window %d)\n",
                tmp, remainingStartTimeOffset, remainingStartFFTWindow, af$spect_window_end(remainingEndFFTWindow), remainingEndFFTWindow))

    if (remainingStartFFTWindow > LAST_WINDOW_IN_FILE) {
      # TODO: think about this for gapped files
      cat(sprintf("Whoops! Requested relative first fft window %d beyond the available %d for this audio part %d\n", remainingEndFFTWindow, LAST_WINDOW_IN_FILE, af$part))
      tmp <- self$start_times[[candInd]] + af$duration() # + 0.001
      if (candInd >= length(self$audiofiles)) { break }
      candInd <- candInd + 1
    } else if (remainingEndFFTWindow > LAST_WINDOW_IN_FILE) {
      cat(sprintf("Requested relative fft window %d beyond the available %d for this audio part %d\n", remainingEndFFTWindow, LAST_WINDOW_IN_FILE, af$part))
      if (is.null(output)) { output <- copy(af$spectrogram[remainingStartFFTWindow:LAST_WINDOW_IN_FILE,]) }
      else { output <- rbind(output, af$spectrogram[remainingStartFFTWindow:LAST_WINDOW_IN_FILE,]) }
      cat(sprintf("added %d fft windows from %d to %d of audio part %d\n", 1 + LAST_WINDOW_IN_FILE - remainingStartFFTWindow,remainingStartFFTWindow, LAST_WINDOW_IN_FILE, af$part))
      tmp <- self$start_times[[candInd]] + af$spect_window_end(LAST_WINDOW_IN_FILE) # + 0.001
      #tmp + (af$duration() - remainingStartTimeOffset); # TODO: think about this for gapped files
      if (candInd >= length(self$audiofiles)) { break }
      candInd <- candInd + 1
    } else {
      if (is.null(output)) { output <- copy(af$spectrogram[remainingStartFFTWindow:remainingEndFFTWindow,]) }
      else { output <- rbind(output, af$spectrogram[remainingStartFFTWindow:remainingEndFFTWindow,]) }
      cat(sprintf("Added %d fft windows from %d to %d of audio part %d\n", 1 + (remainingEndFFTWindow - remainingStartFFTWindow), remainingStartFFTWindow, remainingEndFFTWindow, af$part))
      #tmp <- tmp + remainingEndTimeOffset - remainingStartTimeOffset
      tmp <- self$start_times[[candInd]] + af$spect_window_end(remainingEndFFTWindow) # + 0.001
      candInd <- self$candidateFileIndexForTime(tmp)
    }

    #candInd <- max(candInd+1,self$candidateFileIndexForTime(tmp))

    if (is.null(output)) { remainingWindows <- 0 }
    else { remainingWindows <- numWindows - dim(output)[[1]] }
    cat(sprintf("Reduced the number of remaining windows to %d out of %d\n", remainingWindows, numWindows))
  }

  return(list("start_time"=from,"end_time"=tmp, "power"=output))

})


spectroFrame2tiff <- function(frm, filename, contrast=0.5, normalisation) {
  A <- t(as.matrix(frm$power[,-c("spectroBlock")]))
  AA <- A

  # scaling factor
  if (missing(normalisation)) {
    AX <- max(A)
    if (abs(AX) > 0.0)
      AA <- A / AX
  } else {
    AA <- A / normalisation
  }

  # contrast
  AA <- AA^contrast

  # rasterise
  n <- dim(A)[[1]]
  m <- dim(A)[[2]]
  AAA <- AA[n:1,]
  rasta <- raster::raster(nrows=n, ncols=m)
  rasta <- raster::setValues(rasta, AAA)
  bricka <- raster::brick(rasta)

  # save to file
  tokens <- unlist(strsplit(filename,"[.]"))
  ext <- last(tokens)
  if (ext != "tif" && ext != "tiff")
    filename <- paste0(filename,".tif")

  raster::writeRaster(bricka, filename, overwrite=TRUE)

}


Audiorecord$set("public","spectrogramMovie",
function(filepath, from=0, to=self$duration(),
         fftSize, fftHop,
         frameWidth, frameHeight,
         channel=1, contrast=1,
         normalisation)
{
  # create a folder for all the tiff images
  # mainIdentifier <- function(path) {
  #   tokens <- unlist(strsplit(path,"[/]"))
  #   bits <- unlist(strsplit(tokens[[length(tokens)]],"[.]"))
  #   paste0(bits[1:(length(bits)-1)],collapse = ".")
  # }

  fname <- mainIdentifier(filepath)
  #tokens <- unlist(strsplit(filepath,"[.]"))
  #dirname <- paste0(tokens[1:(length(tokens)-1)], collapse=".")
  enclosingDir <- directoryOfFile(filepath)
  dirname <- paste(sep="/", enclosingDir, fname)
  dir.create(dirname, recursive=TRUE)

  ### no need for this anymore because audiorecord already creates a wav file as needed
  # self$copyToFolder(dirname)

  frameNumber <- 0
  frameIDs <- integer(128)
  imgFiles <- character(128)
  frameStarts <- numeric(128)
  frameEnds <- numeric(128)
  #storyboard <- data.frame(frame=integer(),imgFile=character(),start=numeric(),end=numeric(),stringsAsFactors = FALSE)

  if (missing(normalisation)) {
    # use average power from the first file for normalisation
    ff <- self$audiofiles[[1]]
    ff$calculateSpectrogram(n=fftSize, h=fftHop, ch=channel)
    normalisation <- max(as.matrix(ff$spectrogram[,-c("spectroBlock")]))
    if (normalisation == 0.0) { normalisation <- 1.0 }
  }

  #tifs <- list()
  while (from < to) {
    frm <- self$spectrogramFrame(from=from,
                                 fftSize=fftSize, fftHop=fftHop,
                                 frameWidth=frameWidth, frameHeight=frameHeight,
                                 channel=channel)
    imgFilename <- sprintf("%s_%06d", fname, frameNumber)
    tifPath <- paste0(dirname, "/", imgFilename, ".tif")
    # if (stringr::str_sub(imgPath,stringr::str_length(imgPath)-4) == ".tiff")
    #   imgPath <- stringr::str_sub(imgPath,end=-2)
    #
    #tifPath <- stringr::str_replace(imgPath,".tif{1,2}$",".tif")
    if (is.null(frm$power)) { break }
    spectroFrame2tiff(frm, tifPath, contrast, normalisation)
    tifImg <- magick::image_read(tifPath)
    #tifs <- c(tifs,tifImg)
    pngPath <- stringr::str_replace(tifPath,".tif{1,2}$",".png")
    magick::image_write(tifImg, pngPath, format='png')
    frameNumber <- frameNumber + 1
    from <- frm$end_time # + 0.001

    # add an entry for the storyboard
    frameIDs[[frameNumber]] <- frameNumber
    imgFiles[[frameNumber]] <- paste0(imgFilename,".png")
    frameStarts[[frameNumber]] <- frm$start_time
    frameEnds[[frameNumber]] <- frm$end_time
    cat(sprintf("Exported frame covering %f secs to %f secs, of a total %f secs\n", frm$start_time, frm$end_time, to))
  }

  img1filename <- sprintf("%s_%06d", fname, 0)
  img1path <- paste0(dirname, "/", img1filename, ".png")
  blankImgFilename <- sprintf("%s_blank", fname, 0)
  blankImgPath <- paste0(dirname, "/", blankImgFilename, ".png")
  imgdev <- magick::image_draw(magick::image_read(img1path))
  rect(0,0,frameWidth,frameHeight,col='black')
  blankImg <- magick::image_capture()
  dev.off()
  magick::image_write(blankImg,blankImgPath,format='png')

  # fullImage <- image_append(do.call(c,tifs))
  # fullImgPath <- paste0(dirname, "/", fname, ".gif")
  # image_write(fullImage, fullImgPath, format='gif')

  # write the storyboard to file
  storyboard <- data.frame(ID=frameIDs[1:frameNumber], image=imgFiles[1:frameNumber], start=frameStarts[1:frameNumber], end=frameEnds[1:frameNumber], stringsAsFactors = FALSE)
  storyboardFilename <- sprintf("%s_storyboard", fname, 0)
  storyboardPath <- paste0(dirname, "/", storyboardFilename, ".csv")
  write.csv(storyboard, storyboardPath, quote=FALSE, row.names = FALSE)

  # Now turn the image sequence into an mp4 with ffmpeg (if available)
  fnd <- Sys.which('ffmpeg')
  if (fnd['ffmpeg'][1] != "") {
    frameDur <- as.numeric(frameEnds[[1]]) - as.numeric(frameStarts[[1]])
    frameRate <- 1 / frameDur
    # ffmpeg -f image2 -framerate 0.0448 -i '20190210_CLAY001_SPECT_%06d.png' -y -r 30 test.mp4
    cmdArgs <- c('-hide_banner', '-v', 'quiet', '-f','image2', '-framerate', frameRate, '-i', paste0(dirname, "/", fname, '_%06d.png'), '-y', '-pix_fmt', 'yuv420p', '-r', 30.0, filepath)
    system2('ffmpeg', cmdArgs)
  }

  return(TRUE)
})


Audiorecord$set("public","renderAudioSnippet",function(filepath, from, to=from+1) {
  ind1 <- self$candidateFileIndexForTime(from)
  ind2 <- self$candidateFileIndexForTime(to)

  # FIXME: check if requested times are out of record

  af <- self$audiofiles[[ind1]]
  if (!af$audioLoaded) {
    print(sprintf("RenderAudioSnippet is loading audio for %s part %d\n",filepath,ind1))
    af$loadAudio()
  }
  startRelative <- from - self$start_times[[ind1]]
  stopRelative <- min(to-self$start_times[[ind1]], af$duration())
  stopAbsolute <- stopRelative + self$start_times[[ind1]]
  outputWave <- af$wave(units = "seconds", from = startRelative, to = stopRelative)

  cc <- ind1+1
  tmp = stopAbsolute
  while(cc <= ind2) {
    print(sprintf("Snippet from %f to %f crosses boundary between parts %d and %d\n", from, to, ind1, cc))
    af2 <- self$audiofiles[[cc]]
    if (!af2$audioLoaded) { af2$loadAudio() }
    startRelative <- tmp - self$start_times[[cc]]
    stopRelative <- min(to-self$start_times[[cc]], af2$duration())
    stopAbsolute <- stopRelative + self$start_times[[cc]]
    nextWave <- af2$wave(units = "seconds", from = startRelative, to = stopRelative)
    outputWave <- tuneR::bind(outputWave, nextWave)
    rm(nextWave)
    tmp <- stopAbsolute
    cc <- cc+1
  }

  tuneR::writeWave(tuneR::normalize(outputWave, rescale=FALSE), filepath)
  rm(outputWave); gc()
})


Audiorecord$set("public","renderAudioSnippets",function(baseName, targetDir, tbl) {

  if (!dir.exists(targetDir))
    dir.create(targetDir,recursive = TRUE)

  renderSnippet <- function(bounds) {
    from <- bounds["timeA"]
    to <- bounds["timeB"]
    startStr <- gsub(":","-",format(hms::as.hms(from)),fixed=TRUE)
    endStr <- gsub(":","-",format(hms::as.hms(to)),fixed=TRUE)
    snippetFilePath <- paste0(targetDir, '/', baseName, '_', startStr, '_', endStr, '.wav')
    self$renderAudioSnippet(snippetFilePath,from,to)
  }

  # Note about parallelisation: because the Audiorecord class is designed
  # to load as much of the underlying audiofile into memory as possible,
  # parallelisation should only be applied to snippets within a single audio part
  parts <- 1:length(self$audiofiles)
  rowsFullyInPart <- function(part) { mapply(function(timeA,timeB) { self$regionIsFullyContainedInPart(timeA,timeB,part) }, tbl$timeA, tbl$timeB) }
  lapply(parts, # within each part we can parallelise
    function(part) {
      partTable <- tbl[rowsFullyInPart(part),]
      snippetList <- as.row.list(partTable)
      if (NROW(partTable) > 0) {
        if (!self$audiofiles[[part]]$audioLoaded) { self$audiofiles[[part]]$loadAudio() }
        mclapply2(snippetList, renderSnippet, mc.cores = parallel::detectCores())
        self$audiofiles[[part]]$unloadAudio()
      }
    })

  #FIXME: deal with snippets that cross the border between two (or more) parts

})


Audiorecord$set("public","spectrogramRegions",
                function(filepath, tbl,
                         fftSize, fftHop,
                         frameWidth, frameHeight,
                         channel=1, contrast=1,
                         normalisation, startRow, offset=0)
                {
                  if (missing(normalisation)) {
                    # use average power from the first file for normalisation
                    ff <- self$audiofiles[[1]]
                    ff$calculateSpectrogram(n=fftSize, h=fftHop, ch=channel)
                    normalisation <- max(as.matrix(ff$spectrogram[,-c("spectroBlock")]))
                    if (normalisation == 0.0) { normalisation <- 1.0 }
                  }

                  # create a top level folder
                  # mainIdentifier <- function(path) {
                  #   tokens <- unlist(strsplit(path,"[/]"))
                  #   bits <- unlist(strsplit(tokens[[length(tokens)]],"[.]"))
                  #   paste0(bits[1:(length(bits)-1)],collapse = ".")
                  # }

                  fname <- mainIdentifier(filepath)
                  tokens <- unlist(strsplit(filepath,"[.]"))
                  dirname <- paste0(tokens[1:(length(tokens)-1)], collapse=".")
                  dir.create(dirname)
                  self$copyToFolder(dirname)

                  cc <- startRow
                  N <- NROW(tbl)

                  while(cc <= N) {

                      # create a folder for this region
                      regionDirname <- paste0(dirname, "/", "REGION", as.character(cc), "_", tbl[cc,comment])
                      dir.create(regionDirname)
                      frameNumber <- 0
                      frameIDs <- integer(128)
                      imgFiles <- character(128)
                      frameStarts <- numeric(128)
                      frameEnds <- numeric(128)

                      from <- tbl[cc,timeA]
                      to <- tbl[cc,timeB]
                      while (from < to) {
                        frm <- self$spectrogramFrame(from=from,
                                                     fftSize=fftSize, fftHop=fftHop,
                                                     frameWidth=frameWidth, frameHeight=frameHeight,
                                                     channel=channel)
                        imgFilename <- sprintf("%s_%06d", fname, frameNumber)
                        tifPath <- paste0(regionDirname, "/", imgFilename, ".tif")
                        if (is.null(frm$power)) { break }
                        spectroFrame2tiff(frm, tifPath, contrast, normalisation)
                        tifImg <- magick::image_read(tifPath)

                        pngPath <- stringr::str_replace(tifPath,".tif{1,2}$",".png")
                        magick::image_write(tifImg, pngPath, format='png')
                        frameNumber <- frameNumber + 1
                        from <- frm$end_time # + 0.001

                        # copy the audio snippet for this frame
                        snippetPath <- paste0(regionDirname, "/", imgFilename, ".wav")
                        self$renderAudioSnippet(snippetPath, frm$start_time, frm$end_time)

                        # add an entry for the storyboard
                        frameIDs[[frameNumber]] <- frameNumber
                        imgFiles[[frameNumber]] <- paste0(imgFilename,".png")
                        frameStarts[[frameNumber]] <- frm$start_time
                        frameEnds[[frameNumber]] <- frm$end_time
                        cat(sprintf("Exported frame covering %f secs to %f secs, of a total %f secs\n", frm$start_time, frm$end_time, to))
                      }

                      # img1filename <- sprintf("%s_%06d", fname, 0)
                      # img1path <- paste0(dirname, "/", img1filename, ".gif")
                      # blankImgFilename <- sprintf("%s_blank", fname, 0)
                      # blankImgPath <- paste0(dirname, "/", blankImgFilename, ".gif")
                      # imgdev <- image_draw(image_read(img1path))
                      # rect(0,0,frameWidth,frameHeight,col='black')
                      # blankImg <- image_capture()
                      # dev.off()
                      # image_write(blankImg,blankImgPath,format='gif')

                      # write the storyboard to file
                      storyboard <- data.frame(ID=frameIDs, image=imgFiles, start=frameStarts, end=frameEnds, stringsAsFactors = FALSE)
                      storyboardFilename <- sprintf("%s_storyboard", fname, 0)
                      storyboardPath <- paste0(regionDirname, "/", storyboardFilename, ".csv")
                      write.csv(storyboard, storyboardPath, quote=FALSE, row.names = FALSE)

                      cc <- cc + 1
                    }

                  return(TRUE)
                })


####### conversion into wav files #######
convertIntoWav <- function(fname, targetSR=48000) {

  fndprobe <- Sys.which('ffprobe')
  fndmpeg <- Sys.which('ffmpeg')
  if (fndprobe['ffprobe'][1] == "" || fndmpeg['ffmpeg'][1] == "")
    return(NA)

  ## helper functions
  ffInfoFieldValue <- function(info, fieldName) {
    N <- length(info)
    for (i in 1:N) {
      line <- info[[i]]
      #print(line)
      tokens <- unlist(strsplit(line,"="))
      if (length(tokens) > 1)
        if (tokens[[1]] == fieldName)
          return(tokens[[2]])
    }
    return(NA)
  }

  ## does this contain any sort of audio file?
  cmdArgs <- c('-hide_banner', '-show_streams', '-select_streams', 'a', fname)
  info <- system2('ffprobe',cmdArgs,stdout=TRUE)
  # info <- av::av_video_info(fname)
  # if (is.null(info$audio))
  #   return(NA)
  if (length(info) == 0)
    return(NA)

  if (ffInfoFieldValue(info, 'codec_name') == 'pcm_s16le' && ffInfoFieldValue(info, 'sample_rate') == targetSR)
    return(fname)

  ## we will create a .wav file of the same base filename
  outName <- correspondingWavFilename(fname)

  ## also check if there is already a corresponding wav
  ## file at the requested sample rate
  if (file.exists(outName)) {
    #info <- av::av_video_info(outName)
    cmdArgs <- c('-hide_banner', '-show_streams', '-select_streams', 'a', outName)
    info <- system2('ffprobe',cmdArgs,stdout=TRUE)

    if (length(info) != 0) {
      if (ffInfoFieldValue(info, 'codec_name') == 'pcm_s16le' && ffInfoFieldValue(info, 'sample_rate') == targetSR) {
        return(outName)
      } else {
        # make a copy of the original file
        strSR <- paste0(format(as.numeric(ffInfoFieldValue(info, 'sample_rate')) / 1000),"k")
        bname <- mainIdentifier(outName)
        bdir <- directoryOfFile(outName)
        bpath <- paste0(bdir, "/", bname, ".wav")
        file.copy(from=outName,to=bpath)
      }
    }
  }

  # otherwise we will reencode using ffmpeg (if available)
  cmdArgs <- c('-hide_banner','-y', '-i', fname, '-acodec pcm_s16le', '-ar', as.character(targetSR), outName)
  system2('ffmpeg', cmdArgs)
  return(outName)

}



