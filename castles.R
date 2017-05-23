library(readr)
library(parallel)

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

fitness <- function(genome) {
    score <- vector("numeric",nrow(castle_solutions))
    for (j in 1:nrow(castle_solutions)) {
        match_score <- 0;
    for (i in 1:length(genome)) {
        if (genome[i] > castle_solutions[j,i]) {
            match_score <- match_score + i 
        } else {
            if (genome[i] == castle_solutions[j,i]) {
                match_score <- match_score + i*.5 
            }
        }
    }
    score[j] <- match_score
    }
    wp <- (length(score[score>27.5])+(length(score[score==27.5])*.5))/length(score)
    return(wp)
}

weighted_fitness <- function(genome) {
    score <- vector("numeric",nrow(castle_solutions))
    for (j in 1:nrow(castle_solutions)) {
        match_score <- 0;
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
            }
        }
    }
    return(Reduce("+",score))
}

castle_solutions$wp <- parApply(cl, castle_solutions[,1:10],1,fitness)
clusterExport(cl, "castle_solutions")
castle_solutions$wpw <- parApply(cl, castle_solutions[,1:10],1,weighted_fitness)
stopCluster(cl)

g <- c(3,5,8,10,13,1,26,30,2,2)
print(fitness(g))
print(weighted_fitness(g))

genome <- function() {
    t <- sample(1:10,100,replace = T)
    return(table(t))
}
t <- data.frame(t(replicate(3,genome())))
t$wp <- apply(t,1,fitness)

par(mfrow = c(1, 1))  # 3 rows and 2 columns
hist(castle_solutions$wp, prob='T', breaks=50)
hist(castle_solutions$wpw, prob='T', breaks=50)
plot(castle_solutions$wp,castle_solutions$wpw)
plot(castle_solutions$wpw,castle_solutions$wp)
