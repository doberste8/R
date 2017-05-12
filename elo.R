library(dplyr)

x <- seq(-5,5,length=100)
plot(x,type="l")
f <- function(x) {
    return(x^2)
}
plot(f(x), type="l", lwd=2, col="red")
