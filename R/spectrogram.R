if (!exists("SPECTROGRAM.R")) 
{ SPECTROGRAM.R <- TRUE  # include guard

# dependencies  
library(tuneR)
library(signal)

length_in_samples <- function(filename)
{
	t <- 0
	n <- 44100
	snippet <- readWave(filename,from=1,to=n,units="samples",toWaveMC=TRUE)
	audio_data <- snippet@.Data
	x <- audio_data[,1]
	FLAG <- (length(x) == n)
	while (FLAG)
	{
		t <- t + 1
		snippet <- readWave(filename,from=(t*n)+1,to=(t+1)*n,units="samples",toWaveMC=TRUE)
		audio_data <- snippet@.Data
		x <- audio_data[,1]
		FLAG <- (length(x) == n)
	}
	return(t*n+length(x))
}


length_in_blocks <- function(filename,n)
{
	t <- 0
	snippet <- readWave(filename,from=1,to=n,units="samples",toWaveMC=TRUE)
	audio_data <- snippet@.Data
	x <- audio_data[,1]
	FLAG <- (length(x) == n)

	while (FLAG)
	{
		t <- t + 1
		snippet <- readWave(filename,from=t*n,to=(t+1)*n,units="samples",toWaveMC=TRUE)
		audio_data <- snippet@.Data
		x <- audio_data[,1]
		FLAG <- (length(x) == n)
	}
	return(t)
}


spectrogram <- function(filename,channel,n,hop)
{
	# Figure out the length of the audio file
	# t <- 0
	# shape <- hanning.window(n);
	# snippet <- readWave(filename,from=1,to=n,units="samples",toWaveMC=TRUE)
	# audio_data <- snippet@.Data;
	# x <- audio_data(,channel);
	# FLAG <- (length(x) == n)
	# 
	# while (FLAG)
	# {
	# 	t <- t + 1
	# 	snippet <- readWave(filename,from=t*n,to=(t+1)*n,units="samples",toWaveMC=TRUE)
	# 	audio_data <- snippet@.Data;
	# 	x <- audio_data(,channel);
	# 	FLAG <- (length(x) == n)
	# }
	# 
	# return(0)
	# # z <- fft(x * shape);
	# P <- abs(z);

  L <- length_in_blocks(filename,hop)
  
  
  

}


} # end include guard