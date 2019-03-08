#library(Matrix)
#library(pracma)

constQfreqbins <- function(minFreq,maxFreq,bins)
{
  K <- pracma::ceil( bins * log2(maxFreq/minFreq))
  minFreq*2^(((1:K)-1)/bins)
}

sparseKernel <- function(minFreq, maxFreq, bins, fs, thresh=0.0054) {

  # for Hamming window
  Q <- 1/(2^(1/bins)-1)
  K <- pracma::ceil( bins * log2(maxFreq/minFreq))
  fftLen <- 2^pracma::nextpow2(pracma::ceil(Q*fs/minFreq))
  tempKernel <- complex(length.out=fftLen)
  sparKernel <- matrix(data=0i,nrow=fftLen,ncol=0)
  for (k in seq(from=K,to=1,by=-1)) {
    len <- pracma::ceil(Q * fs / (minFreq*2^((k-1)/bins)))
    tempKernel[1:len] <- (signal::hamming(len) / len) * exp(2*pi*1i*Q*(0:(len-1))/len)
    specKernel <- stats::fft(tempKernel)
    #specKernel[Mod(specKernel) <= thresh] <- 0 + 0i
    sparKernel <- cbind(matrix(data=Conj(specKernel),nrow=fftLen,ncol=1),sparKernel)
  }
  sparKernel= sparKernel / fftLen;
}

cqt <- function(x, sparKernel) {# x must be a row vector
  # pad or truncate x as required
  z <- as.complex(x)
  N <- dim(sparKernel)[[1]]
  if (N <= length(x))
    y <- x[1:N]
  else
    y <- c(x,complex(length.out = (N-length(x)), modulus=0))

  output <- matrix(data=stats::fft(y),nrow=1,ncol=N) %*% sparKernel
  return(output)
}
