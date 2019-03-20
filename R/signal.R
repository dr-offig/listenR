# Moving average filter for data.table
DT.MA <- function(tbl,coeff,ch)
{
  N <- length(coeff)
  strChanName <- paste0("channel",ch)
  strAssign <- paste0(strChanName,":=")
  terms <- lapply(1:N,function(i) { paste0(coeff[[i]]," * shift(",strChanName,",",i-1,")")})
  pasteArgs <- append(alist(sep=' + '), terms)
  strTerms <- do.call(paste,pasteArgs)
  strJ <- paste0(strAssign,strTerms)
  e <- parse(text=strJ)
  tbl[,eval(e)]
}


DT.ARMA <- function(tbl,a,b,ch)
{
  A <- length(a)
  B <- length(b)
  N <- max(A,B)
  a <- pad(a,N-A)
  b <- pad(b,N-B)

  strChanName <- paste0("channel",ch)
  strAssign <- paste0(strChanName,":=")

  terms <- lapply(1:N,function(i) { paste0(b[[i]]," * shift(",strChanName,",",i-1,")")})
  pasteArgs <- append(alist(sep=' + '), terms)
  strTerms <- do.call(paste,pasteArgs)
  strJ1 <- paste0("list(",strTerms,")")
  e1 <- parse(text=strJ1)

  strJ2 <- paste0(strAssign,"block[",N,"]")
  e2 <- parse(text=strJ2)

  for (t in (N+2):nrow(tbl)) {
    block <- tbl[(t-(N-1)):t,eval(e1)]
    tbl[t,eval(e2)]
  }

  #fd[5,channel1:=fd[1:4,list(0.1*shift(channel1,1)+0.2*shift(channel1,2))][4]]

}



# downsample <- function(y,d=2,offset=0) {
#   L <- length(y)
#   N <- L %/% d
#   x <- vector(mode="numeric",N)
#   cc <- 1
#   for (i in 1:L) {
#     if (i+offset %% d == 0) {
#       x[[cc]] <- y[[i]]
#       cc <- cc + 1
#     }
#   }
# }
