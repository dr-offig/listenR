library(Rwave)

scaleFor <- function(octave,voice,num_voices) { 2^(octave + (voice-1)/num_voices) }
scaleAt <- function(index,num_voices) {
  octave <- ((index-1) %/% num_voices) + 1
  voice <- ((index-1) %% num_voices) + 1
  scaleFor(octave,voice,num_voices)
}

scale2freq <- function(a,sr) {
  sr / a
}


freqAt <- function(index,num_voices,sr) {
  scale2freq(scaleAt(index,num_voices),sr)
}


cwt_analyser <- function(noctave,nvoice) {
  function(x) {
    tf <- cwt(x,noctave,nvoice,plot=FALSE)
    q <- colSums(Mod(tf))
  }
}


cwt_freqs <- function(noctave,nvoice,sr) {
  N <- noctave * nvoice
  freqAt(1:N,nvoice,sr)
}


