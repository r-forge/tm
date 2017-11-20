# Coercion functions to TIF (https://github.com/ropensci/tif) objects

require("Matrix")

as.tif_corpus_character <-
function(x)
    UseMethod("as.tif_corpus_character")
as.tif_corpus_character.SimpleCorpus <-
function(x)
    content(x)

as.tif_corpus_df <-
function(x)
    UseMethod("as.tif_corpus_df")
as.tif_corpus_df.VCorpus <-
function(x) {
    doc2row <- function(x) {
        m <- unclass(meta(x))
        m <- m[!names(m) %in% "id"]
        if (any(lengths(m) > 1))
            warning("id ", meta(x, "id"), ": ignoring multi-element entries: ",
                    paste0(names(m[lengths(m) > 1]), collapse = ","))
        m <- replace(m, which(lengths(m) != 1), NA)

        data.frame(doc_id = meta(x, "id"), text = as.character(x), m,
                   stringsAsFactors = FALSE)
    }
    df <- cbind(do.call(rbind, lapply(x, doc2row)), meta(x))
    row.names(df) <- NULL
    df
}

as.tif_dtm <-
function(x)
    UseMethod("as.tif_dtm")
# slam/work/Matrix.R
as.tif_dtm.DocumentTermMatrix <-
function(x) {
    ind <- order(x$j, x$i)
    new("dgCMatrix",
        i = x$i[ind] - 1L,
        p = c(0L, cumsum(tabulate(x$j[ind], x$ncol))),
        x = as.double(x$v[ind]),
        Dim = c(x$nrow, x$ncol),
        Dimnames = if (is.null(x$dimnames)) list(NULL, NULL) else x$dimnames)
}
