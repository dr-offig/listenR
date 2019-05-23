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
  ggplot2::ggplot(data.frame(x=seq_along(f),y=f,w=h), ggplot2::aes(x,y,col=abs(w))) + ggplot2::geom_line(size=1)
}

