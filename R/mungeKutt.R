#### Data augmentation for audio records ####

### Workflow ###
# Input is an 'annotations' file, read in as a data.table with format
#  _____________________________________________________
# |  timeA       timeB     type    colour    comment    |
# |-----------------------------------------------------|
# |  7.330357    92.12963  REGION  darkgreen No Buzz    |
# |  95.721000   97.05031  REGION  indianred Honey Bee  |
# |  107.943287  108.69797 REGION  indianred Honey Bee  |
# |     .           .         .       .         .       |
# |     .           .         .       .         .       |
# |     .           .         .       .         .       |
#
# where the fields are
# type: either REGION or POINT.
# timeA: start of REGION
# timeB: end of REGION
# (if type is POINT then timeA = timeB)
# colour: only used for visual rendering in videoR or spectR
# comment: the annotation for the region
#
# There are two different Protocols with different workflows
#
# ---- FIXED LENGTH SNIPPETS PROTOCOL ----
# The following workflow assumes a particular annotation protocol and
# assumes a fixed length of audio snippet to be fed to AI/ML classifiers.
# For example, 1 second might be a typical snippet duration.
# The idea is that it is long enough to contain a typical short example
# of the target sound, or at least to contain enough of the target sound
# to be recognisable.
#
# Some Terminology:
# A SNIPPET is a contiguous time-slice from the audio recording of a fixed
# (typically short ~1s) duration called SNIPPET_DURATION
# A SNIPPET is called POSITIVE if it contains SOME part of the target sound(s)
# A SNIPPET is called NEGATIVE if it contains NO part of the target sound(s)
#
# A REGION is a contiguous time-slice from the audio recording, which may
# be of any length: it may be much longer than SNIPPET_DURATION -
# for example a REGION of "No Buzz" might last for 10 minutes, however it may
# also be shorter - for example a 200 millisecond short buzz.
# Each REGION should be annotated so that it is either
#   (a) NEGATIVE - entirely DEVOID of any part of the target sound(s), OR
#   (b) POSITIVE - entirely FILLED with the target sound, except perhaps with
#                  gaps less then SNIPPET_DURATION
# Note that this mean a REGION of length SNIPPET_DURATION that contains SOME
# part of the target sound counts as POSITIVE
#
# The purpose of using this protocol is to allow for the algorithmic
# generation of large numbers of 'augmented' audio snippets which
# mix together background sounds of different environments with positively
# identified target sounds. This protocol implies a simple algebra of
# snippet combination: a POSITIVE snippet combined with any other snippet(s)
# remains POSITIVE. NEGATIVE snippets combined with each other remain NEGATIVE.
#
# --- Step 1 ---
# The first step in the workflow is to (randomly) sample snippets of SNIPPET_DURATION
# from the REGIONS. The function 'sampleFromRegions' when applied to POSITIVE REGIONS
# returns a collection of POSITIVE snippets. The function 'sampleInsideRegions' when
# applied to NEGATIVE REGIONS returns a collection of NEGATIVE snippets. These snippets
# should then be written to directories containing all POSITIVE snippets from a
# particular location, and directories containing all NEGATIVE snippets from a particular
# location. Just to be clear: this will result in a collection of directories, in fact two
# directories for every recording location. The function 'Audiorecord$renderAudioSnippets'
# can be used for this purpose
#
# --- Step 2 ---
# The second step is to 'mix and match' the POSITIVE and NEGATIVE snippets across
# locations, so that identification of a particular species is not simply reacting
# to the environmental sounds of that location. The function 'mix-n-match' does this.
# It takes as input the collections of POSITIVE and NEGATIVE directories produced in
# Step 1, and performs between-site pairwise audio mixing of POSITIVE with NEGATIVE
# snippets. This will result in a much expanded set of POSITIVE snippets, where the
# ambient sounds of the site are controlled for (if multi-species classification is
# the goal). The function works factorially in the sense that every POSITIVE snippet
# is combined with at least one (and at most N) NEGATIVE snippets from EACH recording
# site. The input argument N allows for limiting the number of combinations used, which
# could otherwise be very large. If a particular site has more than N NEGATIVE snippets
# then for every POSITIVE snippet the function will randomly sample N NEGATIVE snippets
# from that site to combine with. Got it?
#
# We also apply a mix and match NEGATIVE snippets with themselves. This is to avoid
# any unintended 'hint' to the system arising from all the POSITIVE snippets being
# combinations of two environments.
#
# --- Step 3 ---
# Audio filtering. As a final step, in order to further augment the sounds we apply
# some subtle audio filtering, in particular frequency equalisation (or more precisely
# frequency manipulation - we are not trying to make anything equal) and addition of
# noise.
#
# ---- WHOLE TARGET SOUND PROTOCOL ----
# In contrast to the 'fixed snippet length' protocol, the 'whole target sound' protocol
# requires snippets to either be DEVOID of the target sound (NEGATIVE) or to contain
# a target sound in it's entirety (POSITIVE). Whether this is meaningful will depend
# on the application context. For an elephant trumpeting it probably makes sense. For a
# bee buzzing it may make sense - for example the takeoff and landing phases of bee flight
# have a particular sound and it may be desirable to train an AI/ML to expect these at the
# beginning/end of a flight buzz sound respectively.
#
# FIXME: develop this protocol and implement appropriate functions
#


##--- if using this file in isolation, load these libraries ---##
#library(hms)
#library(data.table)
#library(listenR)
#library(parallel)
#--------------------------------------------------------------##


##### These functions sample random snippets using an annotation table #####

# Randomly sample snippets entirely contained in each region
sampleInsideRegions <- function(tbl, numSamples, minDur, maxDur) {

  output <- data.frame("timeA"=numeric(0),"timeB"=numeric(0), stringsAsFactors=FALSE)
  sampleStrictlyBetween <- function(timeA, timeB) {
    if (timeB - timeA >=  minDur) {
      for (n in 1:numSamples) {
        from <- runif(1,timeA, timeB-minDur)
        to <- runif(1,from + minDur, min(from + maxDur, timeB))
        output <<- rbind(output, data.frame("timeA"=from,"timeB"=to, stringsAsFactors=FALSE))
      }
    }
  }

  mapply(sampleStrictlyBetween, as.numeric(hms::as.hms(tbl$timeA)), as.numeric(hms::as.hms(tbl$timeB)))
  return(output)
}


# Randomly sample snippets overlapping the regions
sampleFromRegions <- function(tbl, numSamples, minDur, maxDur) {

  output <- data.frame("timeA"=numeric(0),"timeB"=numeric(0), stringsAsFactors=FALSE)
  sampleBetween <- function(timeA, timeB) {
    for (n in 1:numSamples) {
      from <- runif(1,timeA-minDur, timeB)
      to <- runif(1,from + minDur, from + maxDur)
      output <<- rbind(output, data.frame("timeA"=from,"timeB"=to, stringsAsFactors=FALSE))
    }
  }

  mapply(sampleBetween, as.numeric(hms::as.hms(tbl$timeA)), as.numeric(hms::as.hms(tbl$timeB)))
  return(output)
}


############### These operations work on already rendered audio snippets ###############
## the snippets should be organised into directories of all POSITIVE
## or all NEGATIVE, and by recording session

# mix_n_match <- function(positive_dirs, negative_dirs, N, outputDir) {
#   suppressWarnings(dir.create(outputDir, recursive=TRUE))
#   mix_pos_dir <- function(positive_dir) {
#     mix_pos_file <- function(positive_file) {
#       mix_with_neg_dir <- function(negative_dir) {
#         mix_with_neg_file <- function(negative_file) {
#           outName <- paste0(outputDir, "/POSITIVE_MIXED_", stringi::stri_rand_strings(1,8, pattern="[A-Z]"),".wav")
#           cmdArgs <- c('-y', '-i', shQuote(positive_file), '-i', shQuote(negative_file), '-filter_complex', 'amix=inputs=2:duration=first', outName)
#           system2('ffmpeg', cmdArgs)
#         }
#         neg_files_in_dir <- list.files(negative_dir,pattern=".*\\.wav$", full.names=TRUE)
#         m <- min(length(neg_files_in_dir),N)
#         chosen_neg_files <- sample(neg_files_in_dir,size=m)
#         lapply(chosen_neg_files, mix_with_neg_file)
#       }
#       lapply(negative_dirs, mix_with_neg_dir)
#     }
#     lapply(list.files(positive_dir,pattern=".*\\.wav$",full.names=TRUE), mix_pos_file)
#   }
#   lapply(positive_dirs, mix_pos_dir)
#   return(TRUE)
# }


mix_n_match <- function(A_dirs, B_dirs, N, outputDir) {
  suppressWarnings(dir.create(outputDir, recursive=TRUE))
  mix_A_dir <- function(A_dir) {
    mix_A_file <- function(A_file) {
      mix_with_B_dir <- function(B_dir) {
        mix_with_B_file <- function(B_file) {
          outName <- paste0(outputDir, "/", mainIdentifier(A_file), "_MIX_", mainIdentifier(B_file), ".wav")
          #cmdArgs <- c('-hide_banner', '-v', 'quiet', '-y', '-i', shQuote(A_file), '-i', shQuote(B_file), '-filter_complex', 'amix=inputs=2:duration=first', outName)
          #system2('ffmpeg', cmdArgs)
          cmdArgs <- c(shQuote(A_file), shQuote(B_file), shQuote(outName))
          system2('mix', cmdArgs)
        }
        B_files_in_dir <- list.files(B_dir,pattern=".*\\.wav$", full.names=TRUE)
        chosen_B_files <- sample(B_files_in_dir, size=min(length(B_files_in_dir),N))
        lapply(chosen_B_files, mix_with_B_file)
      }
      lapply(B_dirs, mix_with_B_dir)
    }
    mclapply2(list.files(A_dir,pattern=".*\\.wav$",full.names=TRUE), mix_A_file, mc.cores = parallel::detectCores())
  }
  lapply(A_dirs, mix_A_dir)
  return(TRUE)
}


## helper functions
ffInfoFieldValue <- function(info, fieldName) {
  for (i in seq_along(info)) {
    tokens <- unlist(strsplit(info[[i]],"="))
    if (length(tokens) > 1)
      if (tokens[[1]] == fieldName)
        return(tokens[[2]])
  }
  return(NA)
}


rand_noise_and_eq <- function(filepath, outputDir, eqFactor=1.0, noiseFactor=0.001) {
  fname <- mainIdentifier(filepath)
  suppressWarnings(dir.create(outputDir,recursive=TRUE))
  outName <- paste0(outputDir, "/", fname, "_FILT_", stringi::stri_rand_strings(1,8, pattern="[A-Z]"),".wav")

  ## how many channels does this audio file have?
  cmdArgs <- c('-hide_banner', '-v', 'quiet', '-show_streams', '-select_streams', 'a', filepath)
  info <- system2('ffprobe',cmdArgs,stdout=TRUE)
  chans <- ffInfoFieldValue(info, 'channels')
  if (is.na(chans)) return(NA)

  r <- runif(8)
  s <- runif(8)
  u <- runif(8)

  f <- 40 * 2^(r + seq_along(r) - 1)
  w <- 40 * 2^(s + seq_along(s) - 1)
  g <- 10.0 * eqFactor * atanh(u-1.0)

  eqBandStr <- function(freq,width,gain) {
    eqBandChanStr <- function (i) { sprintf("c%d f=%f w=%f g=%f t=0", i, freq, width, gain) }
    do.call(function(...) { paste(sep="|", ...) }, as.list(lapply(1:chans,eqBandChanStr)))
  }
  bands <- mapply(eqBandStr,f,w,g)
  allBands <- do.call(function(...) { paste(sep="|", ...) }, as.list(bands))
  equalizerStr <- paste0("anequalizer=", allBands)


  noiseChanStr <- function(i) { paste0("anoisesrc=c=pink:a=", noiseFactor,"[nz", as.character(i), "]") }
  noiseChanStrs <- lapply(1:chans, noiseChanStr)
  allNoises <- do.call(function(...) { paste(sep=";", ...) }, as.list(noiseChanStrs))

  noiseLabels <- lapply(1:chans,function(i) { paste0("[nz",i,"]") })
  noiseLabelStr <- do.call(paste0, noiseLabels)
  mergeStr <- paste0(noiseLabelStr, "amerge=inputs=2[nz]")

  mixStr <- paste0("[0:a][nz]amix=inputs=2:duration=shortest,", equalizerStr)

  filterStr <- paste(sep=";", allNoises, mergeStr, mixStr)

  # Stereo example
  # ffmpeg -i INPUT.wav -filter_complex \
  #       "anoisesrc=c=pink:a=0.01[nz0]; anoisesrc=c=pink:a=0.01[nz1]; [nz0][nz1]amerge[nz]; \
  #       [0:a][nz]amix=inputs=2:duration=shortest,anequalizer=c0 f=200 w=100 g=-10 t=1|c1 f=200 w=100 g=-10 t=1" \
  #       -y OUTPUT.wav
  cmdArgs <- c('-hide_banner', '-v', 'quiet', '-i', shQuote(filepath), '-filter_complex', shQuote(filterStr), '-y', shQuote(outName))
  system2('ffmpeg', cmdArgs)

}


## this puts mix-n-match and rand_noise_and_eq together
augmentSnippets <- function(positive_dirs, negative_dirs, N, M, eqFactor, noiseFactor, posCollectedOutputDir, negCollectedOutputDir) {

  # mixing positive and negative samples yields all positive samples
  mix_n_match(A_dirs=positive_dirs, B_dirs=negative_dirs, N=N, outputDir=posCollectedOutputDir)

  # mixing negative samples together yields all negative samples
  mix_n_match(A_dirs=negative_dirs, B_dirs=negative_dirs, N=N, outputDir=negCollectedOutputDir)

  posFiles <- list.files(posCollectedOutputDir, pattern="\\.wav$", full.names = TRUE, recursive = TRUE)
  negFiles <- list.files(negCollectedOutputDir, pattern="\\.wav$", full.names = TRUE, recursive = TRUE)

  mclapply2(posFiles, function(fpath) { rand_noise_and_eq(fpath, posCollectedOutputDir, eqFactor, noiseFactor) }, mc.cores = parallel::detectCores())
  mclapply2(negFiles, function(fpath) { rand_noise_and_eq(fpath, negCollectedOutputDir, eqFactor, noiseFactor) }, mc.cores = parallel::detectCores())
  return(TRUE)
}
##############################################################


#### Putting it all together ####
fixedLengthSnippetsProtocol <- function(audioFilePaths, annotationFilePaths,
                                        positiveLabels, negativeLabels,
                                        snippetDur, N1, N2, N3,
                                        eqFactor, noiseFactor,
                                        outputDir)
{
  # Prepare output directories
  scratchDir <- paste0(outputDir,"/TMP")
  posScratchDir <- paste0(scratchDir,"/POSITIVE")
  negScratchDir <- paste0(scratchDir,"/NEGATIVE")
  posOutputDir <- paste0(outputDir,"/POSITIVE")
  negOutputDir <- paste0(outputDir,"/NEGATIVE")

  suppressWarnings(dir.create(outputDir,recursive=TRUE))
  suppressWarnings(dir.create(scratchDir,recursive=TRUE))
  suppressWarnings(dir.create(posScratchDir,recursive=TRUE))
  suppressWarnings(dir.create(negScratchDir,recursive=TRUE))
  suppressWarnings(dir.create(posOutputDir,recursive=TRUE))
  suppressWarnings(dir.create(negOutputDir,recursive=TRUE))

  sanitiseLabel <- function(lbl) { gsub(pattern="[ /\'\":]",replacement = "", lbl) }


  ## -- Stage 1 -- ##
  prepareSnippetsForFile <- function(audioFilePath, annotationFilePath,
                                     positiveLabel, negativeLabel)
  {
    ar <- Audiorecord$new(filenames=list(audioFilePath))
    annotations <- fread(annotationFilePath)
    regions <- annotations[type=="REGION",duration:=as.seconds(timeB)-as.seconds(timeA)]

    positiveRegions <- regions[comment==positiveLabel]
    negativeRegions <- regions[comment==negativeLabel]

    posLabel <- sanitiseLabel(positiveLabel)
    negLabel <- sanitiseLabel(negativeLabel)

    posSnippetTable <- sampleFromRegions(tbl=positiveRegions, numSamples=N1, minDur=snippetDur, maxDur=snippetDur)
    negSnippetTable <- sampleInsideRegions(tbl=negativeRegions, numSamples=N1, minDur=snippetDur, maxDur=snippetDur)

    record <- mainIdentifier(audioFilePath)
    # FIXME: Rendering out the Positive and Negative snippets separately is inefficient
    # because it involves loading/unloading each audiofile twice. However, integrating
    # the two calls together would involve some refactoring
    ar$renderAudioSnippets(baseName=paste0(posLabel, "_", record),
                           targetDir=paste0(posScratchDir, "/", posLabel, "_", record),
                           posSnippetTable)
    ar$renderAudioSnippets(baseName=paste0(negLabel, "_", record),
                           targetDir=paste0(negScratchDir, "/", negLabel, "_", record),
                           posSnippetTable)
    ar$unloadAudio()
  }

  # NOTE: do not parallelise this call. It already contains parallelised subroutines.
  mapply(prepareSnippetsForFile, audioFilePaths, annotationFilePaths, positiveLabels, negativeLabels)


  ## -- Stages 2 and 3 -- ##
  posDirs <- list.dirs(posScratchDir, recursive=TRUE, full.names = TRUE)
  negDirs <- list.dirs(negScratchDir, recursive=TRUE, full.names = TRUE)

  augmentSnippets(posDirs, negDirs, N2, N3, eqFactor, noiseFactor, posOutputDir, negOutputDir)
  return(TRUE)
}


#
# ar <- Audiorecord$new(filenames=list(audiofile))
# dt <- fread(commentsFile)
# zones <- dt[type=="REGION",duration:=as.seconds(timeB)-as.seconds(timeA) ]
### End Testing Only ###


#ar$spectrogramRegions(filepath=paste0(dir,"/",rec,"_EXPORT"),
#                       tbl=zones,fftSize=1024, fftHop=512, frameWidth=128, frameHeight=512, channel=1, contrast = 0.25, startRow=1)
#


