library(readr)
castle_solutions <- read_csv("B:/R/castle-solutions.csv")
par(mfrow = c(2, 5))  # 3 rows and 2 columns
for (i in c(1:10)) {
    hist(castle_solutions[[i]], prob='T', breaks=50, main=paste("Castle",i))
}
