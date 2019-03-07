downsample <- function(y,d=2,offset=0) {
  L <- length(y)
  N <- L %/% d
  x <- vector(mode="numeric",N)
  cc <- 1
  for (i in 1:L) {
    if (i+offset %% d == 0) {
      x[[cc]] <- y[[i]]
      cc <- cc + 1
    }
  }
}
