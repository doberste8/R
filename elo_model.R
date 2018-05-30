library(readr)
library(dplyr)
library(ggplot2)
library(Metrics)
sl_games <- read_csv("Elo data - Sheet1.csv")

# create list of teams and give them random ratings from a normal distribution
numTeams <- 12
teams <- LETTERS[1:numTeams]
#set.seed(161)
spread <- 600 # set to 600 means that every 100 point difference in rating
              # roughly corresponds to an additional 5% chance to win a point
rlogis2 <- function(n,mean,sd) { mean+sd*scale(rlogis(n)) }
ratings <- rlogis2(numTeams,0,(spread/10*sqrt(3))/pi)
ratings <- sort(ratings, decreasing = TRUE)
spreads <- rnorm(numTeams,spread,0)
ratingTable <- data.frame(teams,ratings,spreads,0,spread)
names(ratingTable) <- c("team", "rating", "ratingDeviation", "calcRating", "calcRatingDeviation")

# returns predicted margin of victory given x=team's probability of scoring
# an individual point
pMOV <- function(x) {
  mov <- ifelse(x>=.5,55.7*log(x+.5)/(x+.5),-55.7*log(-x+1.5)/(-x+1.5))
  return(mov)
}

# returns the probability of a team winning a game given x=team's probability
# of scoring an individual point
pWIN <- function(x,sA=0,sB=0,gameTo=15) {
    otPoint <- 2*gameTo-sA-sB-2
    i <- ((gameTo-sA):otPoint)
    pWNOT <- ifelse(otPoint>=gameTo-sA,sum(choose(i-1,gameTo-sA-1)*x^(gameTo-sA)*(1-x)^(i-gameTo+sA)),0) #prob of win without overtime
    pOT <- choose(otPoint,gameTo-sA-1)*x^(gameTo-sA-1)*(1-x)^(gameTo-sB-1) #prob of OT
    pWOT <- x^2+2*x^3*(1-x)+4*x^3*(1-x)^2
    return(pWNOT+pOT*pWOT)
}

# returns the probability of a team to score an individual point against
# an opponent given their respective ratings
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

t <- replicate(100,sim(ratingTable),simplify = T)
games <- data.frame(teamA=LETTERS[t[1,]],teamB=LETTERS[t[2,]],scoreA=t[3,],scoreB=t[4,])
hist(games[,3]-games[,4],probability = TRUE)
lines(density(games[,3]-games[,4]),col="blue",lwd=2)
summary(games[,3]-games[,4])

lines(density(sl_games$"home score"-sl_games$"away score"),col="red",lwd=2)
summary(sl_games$"home score"-sl_games$"away score")
sd(sl_games$"home score"-sl_games$"away score")
sd(games[,3]-games[,4])

ratingTable$calcRating <- 0
ratingTable$calcRatingDeviation <- spread
ratingTable$calcRatingStandard <- 0
ratingTable$rIndex <- 1

gameRating <- function(tA, tB, sA, sB) {
    k <- 1500
    rA <- subset(ratingTable, team==tA)[,4]
    rB <- subset(ratingTable, team==tB)[,4]
    rdA <- subset(ratingTable, team==tA)[,5]
    rdB <- subset(ratingTable, team==tB)[,5]
    #k <- (rdA*sqrt(3))/pi*4.5
    rating <- ifelse((sA+1)/(sA+sB+2)>.5, k*(log((sA+1)/(sA+sB+2)+.5)+.5-expPoint(rA,rB,rdA,rdB)), k*(.5-log(-(sA+1)/(sA+sB+2)+1.5)-expPoint(rA,rB,rdA,rdB)))
    return(rating)
}

gameRatingStandard <- function(tA, tB, sA, sB) {
    k <- 30
    rA <- subset(ratingTable, team==tA)[,6]
    rB <- subset(ratingTable, team==tB)[,6]
    rdA <- subset(ratingTable, team==tA)[,5]
    rdB <- subset(ratingTable, team==tB)[,5]
    rIndexA <- subset(ratingTable, team==tA)[,7]
    rIndexB <- subset(ratingTable, team==tB)[,7]
    #k <- (rdA*sqrt(3))/pi*4.5
    ratingA <- ifelse((sA+1)/(sA+sB+2)>.5, k*(log((sA+1)/(sA+sB+2)+.5)+.5-expPoint(rA,rB,rdA,rdB)), k*(.5-log(-(sA+1)/(sA+sB+2)+1.5)-expPoint(rA,rB,rdA,rdB)))
    ratingB <- ifelse((sB+1)/(sB+sA+2)>.5, k*(log((sB+1)/(sB+sA+2)+.5)+.5-expPoint(rB,rA,rdB,rdA)), k*(.5-log(-(sB+1)/(sB+sA+2)+1.5)-expPoint(rB,rA,rdB,rdA)))
    ratingTable[which(ratingTable[,1] == tA),6] <<- rA + ratingA
    ratingTable[which(ratingTable[,1] == tB),6] <<- rB + ratingB
    #ratingTable[which(ratingTable[,1] == tA),5] <<- 600/rIndexA^(1/8)
    #ratingTable[which(ratingTable[,1] == tB),5] <<- 600/rIndexB^(1/8)
    ratingTable[which(ratingTable[,1] == tA),7] <<- rIndexA+1
    ratingTable[which(ratingTable[,1] == tB),7] <<- rIndexB+1
    #cat(tA, " Rating: ",rA+rating," ", tB, " Rating: ",rB-rating,"\n")
    return(ratingA)
}

calcRating <- function(games) {
    t <- games
    t$gameRatingA <- mapply(gameRating, t$teamA, t$teamB, t$scoreA, t$scoreB)
    t$gameRatingB <- mapply(gameRating, t$teamB, t$teamA, t$scoreB, t$scoreA)
    teamScores <- bind_rows(rename(select(t, teamA, gameRatingA),team=teamA, gameRating=gameRatingA),transmute(select(t, teamB, gameRatingB), team=teamB, gameRating=gameRatingB))
    avg <- (teamScores %>% group_by(team) %>% summarise(sd = sd(gameRating), avg = mean(gameRating), number = n()))
    avg$avg <- avg$avg + ratingTable$calcRating
    return(avg)
}

calcRatingStandard <- function(games) {
    t <- games
    t$gameRatingA <- mapply(gameRatingStandard, t$teamA, t$teamB, t$scoreA, t$scoreB)
}

calcRatingStandard(games)
iterations <- 10

ratingHist <- data.frame('0'=rep(0,numTeams))
for(i in 1:iterations) {
    avg <- calcRating(games)
    ratingTable$calcRating <- avg$avg
    #ratingTable$calcRatingDeviation <- avg$sd*pi/sqrt(3)
    ratingHist[,i] <- ratingTable$calcRating
    print(cor(ratingTable$rating, ratingTable$calcRating, method = "pearson"))
    cat("iteration: ",i,"ratings: ",ratingTable$calcRating,"\n")
}

cat("Pearson Cor Iter: ",cor(ratingTable$rating, ratingTable$calcRating, method = "pearson"),"\n")
cat("RMSE Iter: ",rmse(ratingTable$rating, ratingTable$calcRating),"\n")
cat("MAE Iter: ",mae(ratingTable$rating, ratingTable$calcRating),"\n")
cat("Pearson Cor Std: ",cor(ratingTable$rating, ratingTable$calcRatingStandard, method = "pearson"),"\n")
cat("RMSE Standard Std: ",rmse(ratingTable$rating, ratingTable$calcRatingStandard),"\n")
cat("MAE Standard Std: ",mae(ratingTable$rating, ratingTable$calcRatingStandard),"\n")

for(i in 1:numTeams) {
    plot(seq(1:iterations),ratingHist[i,])
}

testRating <- function(A,B) {
    return((count(games, teamA==A, teamB==B, scoreA==15)[8,4]+count(games, teamA==B, teamB==A, scoreB==15)[8,4])/(count(games, teamA==A, teamB==B)[4,3]+count(games, teamA==B, teamB==A)[4,3]))
}