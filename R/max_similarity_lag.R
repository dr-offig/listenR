max_similarity_lag <- function(audio_wave)
{

	g <- as.vector(seewave::hilbert(audio_wave))
	L <- length(g)
	N <- floor(L/2)
	z <- g[1:N]

	errors <- numeric(N)
	for (k in 1:N)
	{
		y = g[k:(k + N - 1)]
		W = cbind(1,y)
		Wstar = Conj(t(W))
		WSW = Wstar %*% W
		b = Wstar %*% z
		beta = solve(WSW) %*% b
		zhat = W %*% beta
		res = z - zhat
		err = norm(res,"1") / N
		errors[k] <- err

	}

	return(errors)

}
