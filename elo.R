library(dplyr)
library(ggplot2)

x <- seq(-5,5,length=100)
plot(x,type="l")
f <- function(x) {
    return(x^2)
}
qplot(x,f(x))+theme_dark()
