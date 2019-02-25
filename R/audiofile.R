# Reference class abstraction for caching large audio files

# library(tuneR)
# library(data.table)
# library(stats)
# library(signal)
# library(R6)
# library(plotly)
# library(ggthemr)
# library(shiny)
# library(hms)
# 
# library(e1071)

#source('./cqt.R')
#source('./DTDWT.R')  


#testfile1 <- "/home/ybot/code/R/ybot.R/data/audio/catfish/grunts cleared/0_02_150.wav"  
#testfile2 <- "/home/ybot/code/R/ybot.R/data/audio/catfish/grunts cleared/0_10_830.wav"  
#testfile3 <- "/home/ybot/code/R/ybot.R/data/audio/catfish/BlueCatfish_LonePine_25052018_Stereo.wav"
#testfile4 <- "/home/ybot/code/R/ybot.R/data/audio/Talaroo/test_sound.wav"

#.datatable.aware = TRUE

# Utilities
rms <- function(x) { sqrt(mean(x^2)) }

dt <- function(clockTime) {
  cc <- 0; ss <- 0; mm <- 0; hh <- 0; dd <- 0
  tokens1 <- unlist(strsplit(clockTime,".",fixed=TRUE))
  if (length(tokens1) > 2) { return(NA) }
  if (length(tokens1) > 1) { cc <- as.numeric(paste0(".",tokens1[[2]])) }
  ww <- tokens1[[1]]
  if (ww != "") {
    tokens2 <- rev(unlist(strsplit(ww,":",fixed=TRUE)))
    ss <- as.numeric(tokens2[[1]])
    if (length(tokens2) > 1) { mm <- as.numeric(tokens2[[2]]) }
    if (length(tokens2) > 2) { hh <- as.numeric(tokens2[[3]]) }
    if (length(tokens2) > 3) { dd <- as.numeric(tokens2[[4]]) }
  }

  elapsed <- cc + ss + 60 * (mm + 60 * (hh + 24 * dd))
  return(elapsed)    
}
  
# Main class
Audiofile <- R6::R6Class("Audiofile",
                  public = list(
                    filename = NULL,
                    frames = NULL,
                    channels = NULL,
                    samplerate = NULL,
                    audioData = NULL,
                    spectrogram = NULL,
                    envelope = NULL,
                    constQ = NULL,
                    DTDWT = NULL,
                    envelopeCalculated = FALSE,
                    envelopeWindowSize = 32,
                    spectrogramCalculated = FALSE,
                    spectrogramWindowSize = 512,
                    spectrogramHop = 256,
                    spectrogramWindow = NULL,
                    constQCalculated = FALSE,
                    constQWindowSize = 2048,
                    constQHop = 1024,
                    constQbins = NULL,
                    DTDWTCalculated = FALSE,
                    DTDWTLevels = 4,
                    initialize = function(fname) {
                      self$filename <- fname
                      waveObject <- tuneR::normalize(tuneR::readWave(fname, toWaveMC = TRUE),rescale=FALSE)
                      self$frames <- dim(waveObject@.Data)[[1]]
                      self$channels <- dim(waveObject@.Data)[[2]]
                      self$samplerate <- waveObject@samp.rate
                      self$audioData <- as.data.table(waveObject@.Data)
                      names(self$audioData) <- paste0("channel",as.character(seq(from=1,to=self$channels,by=1)))
                      self$audioData[, frame:=.I]
                      self$spectrogramCalculated <- FALSE
                      #self$spectrogramWindowSize <- 512
                      self$spectrogramWindow <- signal::hanning
                      self$calculateEnvelope()
                      self$loadMsg()
                    },
                    loadMsg = function() {
                      cat(paste0("Loaded audio file ", basename(self$filename), ".\n"))
                    },
                    calculateEnvelope = function(h=self$envelopeWindowSize) {
                      self$envelopeWindowSize <- h
                      self$audioData[, envBlock := ((frame %/% h) + 1)]
                      self$envelope <- self$audioData[,lapply(.SD,rms),by=envBlock,.SDcols=-c("frame")]
                      self$envelopeCalculated = TRUE
                    },
                    calculateSpectrogram = function(n=512,h=256,wt=signal::hanning,ch=1) {
                      m <- floor((self$frames - n) / h)
                      self$audioData[, spectroBlock := (((frame + (h/2) - (n/2)) %/% h)+1)]
                      gt <- function(i) {
                        x <- unlist(self$audioData[(i*h + 1):(i*h + n),ch:ch],use.names=FALSE)
                        wx <- x * wt(n)
                        return(fft(wx) / length(wx))
                      }
                      fullTransform <- lapply(0:m,gt)
                      powerSpectrum <- sapply(fullTransform,abs)
                      self$spectrogram <- as.data.table(t(powerSpectrum[1:floor(dim(powerSpectrum)[1] / 2),]))
                      self$spectrogram[, spectroBlock := .I]
                      self$spectrogramWindowSize <- n
                      self$spectrogramHop <- h
                      self$spectrogramWindow <- signal::hanning
                      self$spectrogramCalculated <- TRUE
                    },
                    calculateConstQ = function(n=2048,h=1024,ch=1,minFreq=60,maxFreq=10000,bins=4) {
                      SK <- sparseKernel(minFreq,maxFreq,bins,self$samplerate)
                      self$constQbins <- constQfreqbins(minFreq,maxFreq,bins)
                      m <- floor((self$frames - n) / h)
                      self$audioData[, constQBlock := (((frame + (h/2) - (n/2)) %/% h)+1)]
                      gt <- function(i) {
                        x <- unlist(self$audioData[(i*h + 1):(i*h + n),ch:ch],use.names=FALSE)
                        #wx <- x * wt(n)
                        return(cqt(x,SK))
                      }
                      fullTransform <- lapply(0:m,gt)
                      powerSpectrum <- sapply(fullTransform,abs)
                      self$constQ <- as.data.table(t(powerSpectrum))
                      self$constQ[, constQBlock := .I]
                      self$constQWindowSize <- n
                      self$constQHop <- h
                      self$constQCalculated <- TRUE
                    },
                    calculateDualTree = function(J=4,ch=1) {
                      # These are constants
                      Faf <- FSfarras()$af
                      Fsf <- FSfarras()$sf
                      af <- dualfilt1()$af
                      sf <- dualfilt1()$sf
                      
                      r <- nextpow2(self$frames)
                      n <- 2^r
                      xx <- unlist(self$audioData[,ch:ch],use.names=FALSE)
                      x <- rep(xx,length.out=n)
                      x[(self$frames+1):n] <- 0
                                       
                      # This is the wavelet analysis
                      self$DTDWT <- dualtree(x,J,Faf,af)
                      self$DTDWTCalculated <- TRUE
                      self$DTDWTLevels <- J
                      # This does denoising
                      #v1 <- lapply(w1,function(a) lapply(a, function(b) sapply(b, function(c) {if (abs(c) < 0.5) return(0.0) else return(c)})))
                        
                      # This is the wavelet resynthesis
                      #y1 <- idualtree(v1,J,Fsf,sf)

                    },
                    get_channel_names = function() {
                      unlist(lapply(1:self$channels,function(i) { paste0("channel",as.character(i))}))
                    }
                  )
)
  
Audiofile$set("public", "plot", function(units="seconds",from=0,
                                             to=if(units == "seconds") { 1 } else { self$samplerate })
{
  #channel_names <- unlist(lapply(1:self$channels,function(i) { paste0("channel",as.character(i))}))
  channel_names <- self$get_channel_names()
  selectCols <- c("frame",channel_names)
  start_frame <- if (units=="seconds") { from * self$samplerate} else { from }
  end_frame <- if (units=="seconds") { to * self$samplerate} else { to }
  snippet <- self$audioData[start_frame:end_frame,..selectCols]
  ds <- data.table::melt(snippet[,..selectCols], id.vars = "frame")
  names(ds) <- c("frame","channel","signal")
  #ds$channel <- sapply(ds$channel, function(x){as.numeric(substring(toString(x),2,2))})
  ds[,clock:=hms::as.hms(frame / ..self$samplerate)]
  gp <- ggplot2::ggplot(ds,aes(x=frame)) + geom_line(aes(y=signal)) + facet_grid(rows = vars(channel))
  pp <- plotly::ggplotly(gp)
  return(pp)
})
  

Audiofile$set("public", "plot.env", function(units="seconds",from=0,
                                         to=if(units == "seconds") { self$frames / self$samplerate } else { self$frames })
{
  if (!self$envelopeCalculated) 
    self$calculateEnvelope()
  
  d <- self$envelopeWindowSize
  a <- if(units == "seconds") from * self$samplerate else from
  b <- if(units == "seconds") to * self$samplerate else to
  A <- floor(a / d)
  B <- floor(b / d)
  
  cat(paste0("Plotting audio file ", basename(self$filename), " frames [", a,":", b, "]\n"))
  #assign("channel_names", unlist(lapply(1:self$channels,function(i) { paste0("channel",as.character(i))})), .GlobalEnv)
  channel_names <- self$get_channel_names()
  
  # maximum of screen width points should ever be plotted
  screenDim <- dev.size(units="px");
  w <- screenDim[[1]]
  h <- screenDim[[2]]
  # y <- array("numeric",dim=c(w,2))
  x <- 1:w
  C <- B - A + 1
  Q <- max(1,C %/% w)
  
  c <- b - a + 1
  q <- max(1,c %/% w)
  
  downsampledEnv <- self$envelope[envBlock %% Q == 0 & envBlock >= A & envBlock <= B]
  selectCols <- c("frame",channel_names)
  de <- data.table::melt(downsampledEnv, id.vars="envBlock")
  de$envBlock <- (de[,envBlock] - 0.5) * self$envelopeWindowSize
  names(de) <- c("frame","channel","rms")
  de$channel <- sapply(de$channel, function(x){as.numeric(substring(toString(x),2,2))})
  de$frame <- as.integer(de$frame)  
  de[,clock:=hms::as.hms(frame / ..self$samplerate)]
  
  if (c < 100*w) {
  
    downsampledSignal <- self$audioData[frame %% q == 0 & frame >= a & frame <= b]
    ds <- data.table::melt(downsampledSignal[,..selectCols], id.vars = "frame")
    names(ds) <- c("frame","channel","signal")
    ds$channel <- sapply(ds$channel, function(x){as.numeric(substring(toString(x),2,2))})
    ds[,clock:=hms::as.hms(frame / ..self$samplerate)]
    
    gp <- ggplot2::ggplot(ds,aes(x=frame)) + geom_line(aes(y=signal)) + facet_grid(rows = vars(channel))
    #gp <- ggplot(de,aes(x=clock)) + geom_area(aes(y=rms),alpha=0.5) + geom_area(aes(y=-1*rms),alpha=0.5) + geom_line(data=ds,aes(y=signal)) + facet_grid(rows = vars(channel))
    pp <- plotly::ggplotly(gp)
    return(pp)

  } else {
  
    gp <- ggplot2::ggplot(de,aes(x=clock)) + geom_area(aes(y=rms),alpha=0.5) + geom_area(aes(y=-1*rms),alpha=0.5) + facet_grid(rows = vars(channel))
    pp <- plotly::ggplotly(gp)
    return(pp)
      
  }
  
})


Audiofile$set("public", "plot.spectrum", function(units="seconds",from=0,
                                         to=if(units == "seconds") { self$frames / self$samplerate } else { self$frames })
{
  if (!self$spectrogramCalculated) 
    { self$calculateSpectrogram() }  
  
  d <- self$spectrogramHop
  a <- if(units == "seconds") from * self$samplerate else from
  b <- if(units == "seconds") to * self$samplerate else to
  A <- a / d
  B <- b / d
  
  cat(paste0("Plotting audio spectrum for ", basename(self$filename), " frames [", a,":", b, "]\n"))
  
  # maximum of screen width points should ever be plotted
  screenDim <- dev.size(units="px");
  w <- screenDim[[1]]
  h <- screenDim[[2]]
  # y <- array("numeric",dim=c(w,2))
  x <- 1:w
  q <- B - A + 1
  Q <- max(1,(q %/% w))
  
  # y <- sapply(x,function(i) { self$audioData[((i-1) * B)+1,] })
  #downsampledData <- self$audioData[frame %% B == 0 & frame >= a & frame <= b]
  downsampledData <- self$spectrogram[spectroBlock %% Q == 0 & spectroBlock >= A & spectroBlock <= B]
  names(downsampledData) <- c(as.character(1:floor(self$spectrogramWindowSize / 2)),"spectroBlock")
  #dd <- melt(downsampledData, id.vars="spectroBlock")
  
  #dd <- t(as.matrix(downsampledData[,-c("spectroBlock")]))
  #assign("dd",melt(as.matrix(downsampledData[,-c("spectroBlock")]),variable.factor=FALSE),.GlobalEnv)
  dd <- data.table::melt(as.matrix(downsampledData[,-c("spectroBlock")]),variable.factor=FALSE)
  names(dd) <- c("time_bin","freq_bin","power")
  md <- min(dd$power)
  if (md > 0)
    dd$power <- 10*log10(dd$power / md)
  else
    dd$power <- 10*log10((dd$power + 10e-12) / 10e-13)

  
  #gp <- ggplot(dd,aes(xmin=time_bin-1,xmax=time_bin,ymin=freq_bin*self$samplerate/self$spectrogramWindowSize,ymax=(freq_bin+1)*self$samplerate/self$spectrogramWindowSize)) + geom_rect(aes(fill=power)) + scale_x_continuous("Block") + scale_y_continuous("Frequency")
  gp <- ggplot2::ggplot(dd,aes(xmin=time_bin-1,xmax=time_bin,ymin=freq_bin*self$samplerate/self$spectrogramWindowSize,ymax=(freq_bin+1)*self$samplerate/self$spectrogramWindowSize)) + geom_rect(aes(fill=power)) + scale_y_log10("Frequency") + ggplot2::theme(plot.background=ggplot2::element_rect(fill="black"))
  #gp <- ggplot(dd,aes(x=time_bin-1,y=freq_bin*self$samplerate/self$spectrogramWindowSize)) + geom_segment(aes(xend=time_bin,yend=(freq_bin+1)*self$samplerate/self$spectrogramWindowSize,color=power)) + scale_y_log10("Frequency")
  return(gp)
  #pp <- plot_ly(dd,x = ~time_bin, y = ~freq_bin, color = ~power, type="scatter", mode='markers', symbol='square',size=20)
  #return(pp)
})



Audiofile$set("public", "plot.constQ", function(units="seconds",from=0,
                                                  to=if(units == "seconds") { self$frames / self$samplerate } else { self$frames })
{
  if (!self$constQCalculated) 
  { self$calculateConstQ() }  
  
  d <- self$constQHop
  a <- if(units == "seconds") from * self$samplerate else from
  b <- if(units == "seconds") to * self$samplerate else to
  A <- a / d
  B <- b / d
  
  cat(paste0("Plotting audio spectrum for ", basename(self$filename), " frames [", a,":", b, "]\n"))
  
  # maximum of screen width points should ever be plotted
  screenDim <- dev.size(units="px");
  w <- screenDim[[1]]
  h <- screenDim[[2]]
  # y <- array("numeric",dim=c(w,2))
  x <- 1:w
  q <- B - A + 1
  Q <- max(1,(q %/% w))
  
  # y <- sapply(x,function(i) { self$audioData[((i-1) * B)+1,] })
  #downsampledData <- self$audioData[frame %% B == 0 & frame >= a & frame <= b]
  downsampledData <- self$constQ[constQBlock %% Q == 0 & constQBlock >= A & constQBlock <= B]
  names(downsampledData) <- c(as.character(1:(length(names(downsampledData))-1)),"constQBlock")
  #dd <- melt(downsampledData, id.vars="spectroBlock")
  
  #dd <- t(as.matrix(downsampledData[,-c("spectroBlock")]))
  dd <- data.table::melt(as.matrix(downsampledData[,-c("constQBlock")]),variable.factor=FALSE)
  names(dd) <- c("time_bin","freq_bin","power")
  
  md <- min(dd$power)
  if (md > 0)
    dd$power <- 10*log10(dd$power / md)
  else
    dd$power <- 10*log10((dd$power + 10e-12) / 10e-13)
  
  # qfreq <- function(bin) { self$constQbins[[bin]] }
  gp <- ggplot2::ggplot(dd,aes(xmin=time_bin-1,xmax=time_bin,ymin=freq_bin-1,ymax=freq_bin)) + geom_rect(aes(fill=power))
  return(gp)
})


Audiofile$set("public", "apply", function(analysis=rms, wt=signal::hanning, n=512, h=256, ch=1, offset=0, units="seconds",from=0,
                                                to=if(units == "seconds") { self$frames / self$samplerate } else { self$frames }) {
  
  a <- if(units == "seconds") from * self$samplerate else from
  b <- if(units == "seconds") to * self$samplerate else to
  A <- floor(a / h)
  B <- floor((b-(n-1)) / h)
  
    
  cat(paste0("Analysing audio file ", basename(self$filename), " frames [", a,":", b, "]\n"))
  #m <- floor((self$frames - n) / h)
  #self$audioData[, spectroBlock := (((frame + (h/2) - (n/2)) %/% h)+1)]
  gt <- function(i) {
    x <- unlist(self$audioData[(offset + i*h + 1):(offset + i*h + n),ch:ch],use.names=FALSE)
    wx <- x * wt(n)
    return(analysis(wx))
  }
  fullAnalysis <- lapply(A:B,gt)
  
})

  


