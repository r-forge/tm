dtm_corpus <-
function(text, stem = TRUE, bigrams = FALSE, min_count = 5) {
    ngrams <- if(bigrams) 1:2 else 1
    stemmer <- if(stem) "english" else NULL
    f <- corpus::text_filter(stemmer = stemmer,
                             drop_punct = TRUE,
                             drop_number = TRUE,
                             drop = stop_words)
    stats <- corpus::term_stats(text, f, ngrams = ngrams,
                                min_count = min_count)
    x <- corpus::term_matrix(text, f, select = stats$term)
    x
}

dtm_quanteda <-
function(text, stem = TRUE, bigrams = FALSE, min_count = 5) {
    ngrams <- if(bigrams) 1:2 else 1    
    x <- quanteda::dfm(text,
                       stem = stem,
                       remove_punct = TRUE,
                       remove_numbers = TRUE,
                       remove = stop_words,
                       ngrams = ngrams,
                       verbose = FALSE)
    x <- quanteda::dfm_trim(x, min_termfreq = min_count,
                            verbose = FALSE)
    x
}
    
dtm_tidytext <-
function(text, stem = TRUE, bigrams = FALSE, min_count = 5) {
    ## Note: this filters punctuation but keeps numbers

    data <- tibble::tibble(text_id = seq_along(text), text = text)
    stops <- tibble::tibble(word = stop_words)

    ## unigrams
    freqs <- (data
        %>% tidytext::unnest_tokens(word, text)
        %>% anti_join(stops, by = "word")
        %>% mutate(term = if(stem) {
                              SnowballC::wordStem(word, "english")
                          } else word)
        %>% count(text_id, term)
        %>% ungroup())

    ## bigrams
    if(bigrams) {
        freqs2 <- (data
            %>% tidytext::unnest_tokens(bigram, text, token = "ngrams", n = 2)
            %>% tidyr::separate(bigram, c("type1", "type2"), sep = " ")
            %>% filter(!type1 %in% stop_words,
                       !type2 %in% stop_words))
        if(stem)
            freqs2 <- (freqs2
                %>% mutate(type1 = SnowballC::wordStem(type1,
                                                       "english"),
                           type2 = SnowballC::wordStem(type2,
                                                       "english")))
        
        freqs2 <- (freqs2
            %>% mutate(term = paste(type1, type2))
            %>% count(text_id, term)
            %>% ungroup())

        freqs <- rbind(freqs, freqs2)
    }

    ## form matrix in slam format
    x <- freqs %>% tidytext::cast_dtm(text_id, term, n)
    ## remove rare terms
    x <- x[, slam::col_sums(x) >= min_count, drop = FALSE]
    ## cast to dgCMatrix format
    x <- Matrix::sparseMatrix(i = x$i, j = x$j, x = x$v, dims = dim(x),
                              dimnames = dimnames(x), check = FALSE)
    
    x
}
    
BigramTokenizer <- function(x) {
    unlist(lapply(NLP::ngrams(NLP::words(x), 2), paste, collapse = " "),
           use.names = FALSE)
}

dtm_tm_slow <-
function(text, stem = TRUE, bigrams = FALSE, min_count = 5) {
   corpus <- (tm::VCorpus(tm::VectorSource(text))
       %>% tm::tm_map(tm::content_transformer(tolower))
       %>% tm::tm_map(tm::removeWords, stop_words)
       %>% tm::tm_map(tm::removePunctuation)
       %>% tm::tm_map(tm::removeNumbers))
   if(stem)
       corpus <- (corpus
           %>% tm::tm_map(tm::stemDocument, language = "english"))

    control <- list(wordLengths = c(1, Inf),
                    bounds = list(global = c(min_count, Inf)))

    x <- tm::DocumentTermMatrix(corpus, control = control)

    if (bigrams) {
        control$tokenize <- BigramTokenizer
        x2 <- tm::DocumentTermMatrix(corpus, control = control)

        x <- cbind(x, x2)
    }
    x
}
   
dtm_tm_fast <-
function(text, stem = TRUE, bigrams = FALSE, min_count = 5,
         simple = TRUE) {
    corpus <- if(simple)
                  tm::SimpleCorpus(tm::VectorSource(text))
              else
                  tm::VCorpus(tm::VectorSource(text))

    control <- list(removeNumbers = TRUE,
                    removePunctuation = TRUE,
                    stemming = stem,
                    stopwords = stop_words,
                    wordLengths = c(1, Inf),
                    bounds = list(global = c(min_count, Inf)))
    
    x <- tm::DocumentTermMatrix(corpus, control = control)

    if (bigrams) {
        control$tokenize <- BigramTokenizer
        x2 <- tm::DocumentTermMatrix(corpus, control = control)

        x <- cbind(x, x2)
    }
    x
}

## Oh dear.  Simply using
##   timings <- function(B, expr)
##     system.time(replicate(B, expr, FALSE))
## does not work due to lazy evaluation issues [it evaluates the
## promise when first called].  Hence:
timings <- function(B, expr)
    system.time(sapply(integer(B),
                       eval.parent(substitute(function(...) expr)),
                       simplify = FALSE))

if(FALSE) {
    ## Experiment 1.
    ## Benchmark timings of various dtms, stemming without bigrams.
    
    install.packages(c("corpus", "quanteda", "tidytext"))
    
    text <- readRDS("texts_prideprejudice.rds")
    stop_words <- corpus::stopwords_en

    require("magrittr")
    require("dplyr")

    B <- 42
    times <- list()
    times$corpus <-
        timings(B, dtm_corpus(text, bigrams = FALSE))
    times$quanteda <-
        timings(B, dtm_quanteda(text, bigrams = FALSE))
    times$tidytext <- 
        timings(B, dtm_tidytext(text, bigrams = FALSE))
    ## times$"tm-slow" <-
    ##     timings(B, dtm_tm_slow(text, bigrams = FALSE))
    times$"tm-fast:simple=TRUE" <-
        timings(B, dtm_tm_fast(text, bigrams = FALSE))
    times$"tm-fast:simple=FALSE" <-
        timings(B, dtm_tm_fast(text, bigrams = FALSE,
                               simple = FALSE))
    do.call(rbind, times)[, 1 : 3]
}

if(FALSE) {
    ## Experiment 2.
    ## Benchmark timings of Various dtms, neither stemming nor bigrams.
    
    install.packages(c("corpus", "quanteda", "tidytext"))
    
    text <- readRDS("texts_prideprejudice.rds")
    stop_words <- corpus::stopwords_en

    require("magrittr")
    require("dplyr")

    B <- 42
    times <- list()
    times$corpus <-
        timings(B, dtm_corpus(text, stem = FALSE, bigrams = FALSE))
    times$quanteda <-
        timings(B, dtm_quanteda(text, stem = FALSE, bigrams = FALSE))
    times$tidytext <- 
        timings(B, dtm_tidytext(text, stem = FALSE, bigrams = FALSE))
    ## times$"tm-slow" <-
    ##     timings(B, dtm_tm_slow(text, stem = FALSE, bigrams = FALSE))
    times$"tm-fast:simple=TRUE" <-
        timings(B, dtm_tm_fast(text, stem = FALSE, bigrams = FALSE))
    times$"tm-fast:simple=FALSE" <-
        timings(B, dtm_tm_fast(text, stem = FALSE, bigrams = FALSE,
                               simple = FALSE))
    do.call(rbind, times)[, 1 : 3]
}

if(FALSE) {
    ## Experiment 3.
    ## Profile dtm for SimpleCorpus and VCorpus.

    text <- readRDS("texts_prideprejudice.rds")
    stop_words <- corpus::stopwords_en

    B <- 42
    
    Rprof("Rprof_dtm_simple=y_stem=y.out")
    for(i in 1 : B)
        dtm_tm_fast(text, stem = TRUE)
    Rprof()
    Rprof("Rprof_dtm_simple=y_stem=n.out")
    for(i in 1 : B)
        dtm_tm_fast(text, stem = FALSE)
    Rprof()

    summaryRprof("Rprof_dtm_simple=y_stem=y.out")
    summaryRprof("Rprof_dtm_simple=y_stem=n.out")    
    
    Rprof("Rprof_dtm_simple=n_stem=y.out")
    for(i in 1 : B)
        dtm_tm_fast(text, stem = TRUE, simple = FALSE)
    Rprof()
    Rprof("Rprof_dtm_simple=n_stem=n.out")
    for(i in 1 : B)
        dtm_tm_fast(text, stem = FALSE, simple = FALSE)
    Rprof()

    summaryRprof("Rprof_dtm_simple=n_stem=y.out")
    summaryRprof("Rprof_dtm_simple=n_stem=n.out")
}
