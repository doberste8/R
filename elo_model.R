# create list of teams and give them random ratings from a normal distribution
teams <- LETTERS[1:20]
#set.seed(161)
spread <- 1000
ratings <- rlogis(20,0,(spread/10*sqrt(3))/pi)
ratings <- sort(ratings, decreasing = TRUE)
ratingTable <- data.frame(teams,ratings,spread)
names(ratingTable) <- c("team", "rating", "ratingDeviation")

pMOV <- function(x) {
  mov <- ifelse(x>=.5,55.7*log(x+.5)/(x+.5),-55.7*log(-x+1.5)/(-x+1.5))
  return(mov)
}

expPoint <- function(rA,rB,rdA=spread,rdB=spread) {
  p <- 1/(1+exp((rB-rA)*pi/sqrt(3*(rdA^2+rdB^2))))
  return(p)
}

sim <- function(df) {
  rSample <- df[sample(nrow(df),2),]
  cat("rA: ",rSample[1,"rating"],"rB: ",rSample[2,"rating"],"\n")
  result <- simGame(rSample[1,],rSample[2,])
  return(c(rSample[1,"team"],rSample[2,"team"],result[1],result[2]))
}

simGame <- function(tA,tB) {
  sA <- 0
  sB <- 0
  while((sA<15) & (sB<15)) {
  rA <- rlogis(1,tA[1,"rating"],(tA[1,"ratingDeviation"]*sqrt(3))/pi)
  rB <- rlogis(1,tB[1,"rating"],(tB[1,"ratingDeviation"]*sqrt(3))/pi)
  ifelse(rA>=rB,sA<-sA+1,sB<-sB+1)
  }
  cat("sA: ",sA,"sB: ",sB,"\n")
  return(c(sA,sB))
}

t <- replicate(1000,sim(ratingTable),simplify = T)
games <- data.frame(teamA=LETTERS[t[1,]],teamB=LETTERS[t[2,]],scoreA=t[3,],scoreB=t[4,])
hist(games[,3]-games[,4],probability = TRUE)
lines(density(games[,3]-games[,4]),col="blue",lwd=2)
summary(games[,3]-games[,4])

lines(density(sl_games$"home score"-sl_games$"away score"),col="red",lwd=2)
summary(sl_games$"home score"-sl_games$"away score")
sd(sl_games$"home score"-sl_games$"away score")
sd(games[,3]-games[,4])
