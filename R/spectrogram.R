frequency_analyser <- function(k) {
  function(x) {
    N <- length(x)
    z <- seq(from=0,by=1,length.out=N)
    y <- exp(-2i*pi*k*z/N)
    return(abs(sum(x * y)))
  }
}

log_scaler <- function(f0,b) { function(x) { logb(x/f0,b) } }
exp_scaler <- function(f0,b) { function(x) { f0 * (b^x) } }

# Draw a spectrogram for arbitrary frequencies through time
plot.timefreq <- function(af, freqs=af$spectrumBin2Freq(0:(af$spectrogramWindowSize-1)),
                          n=af$spectrogramWindowSize,h=af$spectrogramHop,
                          wt=af$spectrogramWindow,ch=1,
                          units="samples",
                          from=0,
                          to=if(units=="samples") { af$frames }
                          else if (units == "seconds") {af$frames / af$samplerate }
                          else if (units == "hms") { as.hms(af$frames / af$samplerate) }
                          else { ((af$frames - n) %/% h) + 1 })
{
  start_sample <- if(units=="samples") { from }
                  else if (units == "seconds") {from * af$samplerate }
                  else if (units == "hms") { as.numeric(from) * af$samplerate }
                  else { ((h-1) * from) + 1 }

  end_sample <- if(units=="samples") { to }
                else if (units == "seconds") {to * af$samplerate }
                else if (units == "hms") { as.numeric(to) * af$samplerate }
                else { ((h-1) * to) + n }

  bins <- af$freq2spectrumBin(freqs)
  analysers <- sapply(bins,frequency_analyser)
  applicator <- function(x) { function(func) { func(x) } }

  analysis <- function(x) {
    g <- applicator(x)
    return(sapply(analysers,g))
  }

  res <- af$apply(analysis=analysis, wt=wt, n=n, h=h, ch=ch, offset=0, units="samples",
           from=start_sample, to=end_sample)

  dd <- data.frame(res)
  names(dd) <- 1:(dim(dd)[[2]])
  dt <- data.table(dd)
  dt[,freq_bin:=.I]
  dt[,freq:=..freqs]
  md <- data.table::melt(dt,id.vars=c("freq_bin","freq"),variable.factor=FALSE)
  setnames(md,c("variable"),c("time_bin"))

  ggplot(md,aes(time_bin,freq_bin)) + geom_raster(aes(fill=value),interpolate = TRUE)

}



# Wavelet version
plot.cwt <- function(af, noctave=10, nvoice=8,
                          n=af$spectrogramWindowSize,h=af$spectrogramHop,
                          wt=af$spectrogramWindow,ch=1,
                          units="samples",
                          from=0,
                          to=if(units=="samples") { af$frames }
                          else if (units == "seconds") {af$frames / af$samplerate }
                          else if (units == "hms") { as.hms(af$frames / af$samplerate) }
                          else { ((af$frames - n) %/% h) + 1 })
{
  start_sample <- if(units=="samples") { from }
  else if (units == "seconds") {from * af$samplerate }
  else if (units == "hms") { as.numeric(from) * af$samplerate }
  else { ((h-1) * from) + 1 }

  end_sample <- if(units=="samples") { to }
  else if (units == "seconds") {to * af$samplerate }
  else if (units == "hms") { as.numeric(to) * af$samplerate }
  else { ((h-1) * to) + n }

  #bins <- af$freq2spectrumBin(freqs)
  freqs <- cwt_freqs(noctave,nvoice,af$samplerate)
  # analysers <- sapply(bins,frequency_analyser)
  # applicator <- function(x) { function(func) { func(x) } }
  #
  # analysis <- function(x) {
  #   g <- applicator(x)
  #   return(sapply(analysers,g))
  # }
  #
  analysis <- cwt_analyser(noctave=noctave,nvoice=nvoice)

  res <- af$apply(analysis=analysis, wt=wt, n=n, h=h, ch=ch, offset=0, units="samples",
                  from=start_sample, to=end_sample)

  dd <- data.frame(res)
  names(dd) <- 1:(dim(dd)[[2]])
  dt <- data.table(dd)
  dt[,freq_bin:=.I]
  dt[,freq:=..freqs]
  md <- data.table::melt(dt,id.vars=c("freq_bin","freq"),variable.factor=FALSE)
  setnames(md,c("variable"),c("time_bin"))

  ggplot(md,aes(time_bin,freq_bin)) + geom_raster(aes(fill=value),interpolate = TRUE)

}



# binning the frequency data
# logify <- function(tfdata,f0=1,f1=f0,f2=10000*f0,x1=1,x2=max(tfdata$time_bin),b=2,N=800)
# {
#   trans <- log_scaler(f0,b)
#   inv_trans <- exp_scaler(f0,b)
#   ymax <- trans(f2-f1)
#   h <- ymax / N
#   #ylbs <- h * (0:(N-1))
#   #yubs <- h * (1:N)
#   ys <- h * (0:(N-1))
#   fs <- f1 + inv_trans(ys) - f0
#   #flbs <- inv_trans(ylbs)
#   #fubs <- inv_trans(yubs)
#
#   pidgeonhole <- function(f) {
#     candidates <- tfdata[lb_freq <= f & f < ub_freq,c(freq_bin)]
#     return(candidates[[1]])
#   }
#
#   # figure out lookup table
#   fbins <- sapply(fs,pidgeonhole)
#   return(fbins)
# }
#


# more control over spectrograms
# log_scaler <- function(f0) { function(x) { log2(x/f0) } }
# exp_scaler <- function(f0) { function(x) { f0 * (2^x) } }
#
#
# plot.spectrogram <- function(tfdata=NULL,gain=1.0,
#                              f0=2^3,f1=2^6,f2=2^15,
#                              x1=min(pm1$windowStart),
#                              x2=max(pm1$windowStart))
# {
#   sliderule <- scales::trans_new(name="sliderule",
#                                  transform=log_scaler(f0),
#                                  inverse=exp_scaler(f0),
#                                  domain=c(f0,Inf))
#
#   regionOfInterest <- tfdata[(x1 <= windowStart & windowStart <= x2 & f1 <= centre_freq & centre_freq <= f2),]
#   regionOfInterest[,power:=power * ..gain]
#   ggplot2::ggplot(regionOfInterest, aes(xmin=windowStart,xmax=windowEnd,ymin=lb_freq,ymax=ub_freq)) +
#     geom_rect(aes(fill=power)) +
#     scale_y_continuous(name="Octave", trans=sliderule) +
#     ggplot2::theme(plot.background=ggplot2::element_rect(fill="black"))
# }
