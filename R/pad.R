pad <- function(x,n,b=0,m=0,a=0)
{
  c(rep(a,m),x,rep(b,n))
}


Dx <- function(x,forwards=TRUE)
{
  dx <- diff(x)
  if(forwards)
    pad(dx,1)
  else
    pad(dx,n=0,m=1)

}