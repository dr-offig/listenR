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


plot.shade <- function(f)
{
  g <- Dx(f)
  h <- g / sd(g)
  ggplot(data.frame(x=seq_along(f),y=f,w=h), aes(x,y,col=abs(w))) + geom_line(size=1)
}

