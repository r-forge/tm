## Adjust as necessary:
dir <- "Data"
dir <- file.path("/home/Hornik/src/org/R-project/R-Forge/tm/pkg/Work",
                 "Data")

n <- 30000

texts <- readLines(file.path(dir, "sentences.txt"), n = n)

saveRDS(texts, file = sprintf("texts_sentences_%d.rds", n))
