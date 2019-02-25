twister <- function(n, freq, phase, sr) {
  exp(2i*pi*(phase + freq*(0:(n-1))/sr))
}

adj <- function(W) {
  return (Conj(t(W)))
}

ols <- function(z, W, constant=TRUE) {
  V <- if (constant) { cbind(rep(1,nrow(W)),W) } else { W }
  solve(adj(V) %*% V) %*% adj(V) %*% z
}

fit <- function(z,W) {
  V <- cbind(rep(1,nrow(W)),W)
  beta <- solve(adj(V) %*% V) %*% adj(V) %*% z
  zhat <- V %*% beta
}
