library(readr)
library(parallel)
library(GA)

# Calculate the number of cores
no_cores <- detectCores() - 1

# Initiate cluster
cl <- makeCluster(no_cores)

castle_solutions <- read_csv("castle-solutions.csv")

clusterExport(cl, "castle_solutions")

par(mfrow = c(2, 5))  # 3 rows and 2 columns
for (i in c(1:10)) {
    hist(castle_solutions[[i]], prob='T', breaks=50, main=paste("Castle",i))
}

sfitness <- function(genome) {
    score <- vector("numeric",nrow(castle_solutions))
    for (j in 1:nrow(castle_solutions)) {
        match_score <- 0
        for (i in 1:length(genome)) {
            if (genome[i] > castle_solutions[j,i]) {
                match_score <- match_score + i 
            } else {
                if (genome[i] == castle_solutions[j,i]) {
                    match_score <- match_score + i*.5 
                }
            }
        }
        
        if (match_score>27.5) {
            score[j] <- 1
        } else {
            if (match_score==27.5) {
                score[j] <- .5
            } else {
                score[j] <- 0
            }
        }
    }
    return(Reduce("+",score)/nrow(castle_solutions))
}

weighted_fitness <- function(genome) {
    score <- vector("numeric",nrow(castle_solutions))
    for (j in 1:nrow(castle_solutions)) {
        match_score <- 0
        for (i in 1:length(genome)) {
            if (genome[i] > castle_solutions[j,i]) {
                match_score <- match_score + i 
            } else {
                if (genome[i] == castle_solutions[j,i]) {
                    match_score <- match_score + i*.5 
                }
            }
        }
        
        if (match_score>27.5) {
            score[j] <- castle_solutions[j,12]^2
        } else {
            if (match_score==27.5) {
                score[j] <- .5*castle_solutions[j,12]^2
            } else {
                score[j] <- 0
            }
        }
    }
    return(Reduce("+",score))
}

castle_solutions$wp <- parApply(cl, castle_solutions[,1:10],1,fitness)
clusterExport(cl, "castle_solutions")
castle_solutions$wpw <- parApply(cl, castle_solutions[,1:10],1,weighted_fitness)
stopCluster(cl)
c_s <- castle_solutions
castle_solutions <- subset(c_s, wp>=0)

#g <- c(3,5,8,10,13,1,26,30,2,2)
#print(fitness(g))
#print(weighted_fitness(g))

#genome <- function() {
#    t <- sample(1:10,100,replace = T)
#    return(table(t))
#}
#t <- data.frame(t(replicate(3,genome())))
#t$wp <- apply(t,1,fitness)

#par(mfrow = c(1, 1))  # 3 rows and 2 columns
#hist(castle_solutions$wp, prob='T', breaks=50)
#hist(castle_solutions$wpw, prob='T', breaks=50)
#plot(castle_solutions$wp,castle_solutions$wpw)
#plot(castle_solutions$wpw,castle_solutions$wp)

c1 <- function(x) {
    return(100 - sum(x))
}

f <- function(x) {
    x <- round(x)
    pen <- sqrt(.Machine$double.xmax)
    return(sfitness(x)+min(c1(x),0)*pen)
}

#summary(GA)
#plot(GA)
#g1 <- c(9,8,6,16,21,7,11,7,8,7)
#g2 <- c(8,10,7,17,23,6,5,6,10,8)

initPop <- function(object) {
    return(t(replicate(object@popSize,table(sample(1:10,100,replace = T)),simplify = "matrix")))
}

pop2 <- function(object) {
    t<-c_s[order(-c_s$wp),1:10]
    return(as.matrix(t[1:object@popSize,]))
}

GA <- replicate(3,ga(type = "real-valued",
         fitness = f,
         min = c(0,0,0,0,0,0,0,0,0,0),
         max = c(50,50,50,50,50,50,50,50,50,50),
         popSize = 100, maxiter = 1000, run = 100,
         parallel = T, monitor = gaMonitor,
         pmutation = .2,
         population = pop2))

gather <- function(x) {
    solutions <- data.frame(t(round(x@solution[1,])), f=sfitness(round(x@solution[1,])), wf=weighted_fitness(round(x@solution[1,])))
    return(solutions)
}
solutions <- rbind(solutions,do.call(rbind.data.frame, lapply(GAT,gather)))
