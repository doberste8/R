library(readr)
castle_solutions <- read_csv("B:/R/castle-solutions.csv")

par(mfrow = c(2, 5))  # 3 rows and 2 columns
for (i in c(1:10)) {
    hist(castle_solutions[[i]], prob='T', breaks=50, main=paste("Castle",i))
}

fitness <- function(genome) {
    score <- vector("numeric",nrow(castle_solutions));
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
    wp <- (length(score[score>=28])+(length(score[score==27.5])*.5))/length(score)
    return(wp)
}

g <- c(3,5,8,10,13,1,26,30,2,2)
print(fitness(g))
 