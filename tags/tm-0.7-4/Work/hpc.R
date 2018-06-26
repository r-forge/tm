## Adjust as necessary:
dir <- "Data"

nd <- 3e4
nc <- 4

require("tm")

s <- readLines(file.path(dir, "sentences.txt"), n = nd)
C <- VCorpus(VectorSource(s))

timings <- list()

tm_parLapply_engine(NULL)
timings <-
    replace(timings,
            "lapply",
            list(system.time(DocumentTermMatrix(C))))

## PSOCKcluster
cl <- parallel::makePSOCKcluster(nc)
tm_parLapply_engine(cl)
timings <-
    replace(timings,
            sprintf("PSOCKcluster(%d)/directly", nc),
            list(system.time(DocumentTermMatrix(C))))
parallel::setDefaultCluster(cl)
tm_parLapply_engine(function(X, FUN, ...)
                        parallel::parLapply(NULL, X, FUN, ...))
timings <-
    replace(timings,
            sprintf("PSOCKcluster(%d)/via_dflt", nc),
            list(system.time(DocumentTermMatrix(C))))
parallel::stopCluster(cl)
parallel::setDefaultCluster(NULL)

## ForkCluster
cl <- parallel::makeForkCluster(nc)
tm_parLapply_engine(cl)
timings <-
    replace(timings,
            sprintf("ForkCluster(%d)", nc),
            list(system.time(DocumentTermMatrix(C))))
parallel::stopCluster(cl)

## mclapply
tm_parLapply_engine(parallel::mclapply)
timings <-
    replace(timings,
            sprintf("mclapply(%d)", getOption("mc.cores", 2L)),
            list(system.time(DocumentTermMatrix(C))))

tm_parLapply_engine(function(X, FUN, ...)
                        parallel::mclapply(X, FUN, ..., mc.cores = nc))
timings <-
    replace(timings,
            sprintf("mclapply(%d)", nc),
            list(system.time(DocumentTermMatrix(C))))

timings <- do.call(rbind, lapply(timings, summary))
timings
