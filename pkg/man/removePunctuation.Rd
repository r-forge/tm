\name{removePunctuation}
\alias{removePunctuation}
\alias{removePunctuation.character}
\alias{removePunctuation.PlainTextDocument}
\title{Remove Punctuation Marks from a Text Document}
\description{
  Remove punctuation marks from a text document.
}
\usage{
\method{removePunctuation}{character}(x,
                  preserve_intra_word_contractions = FALSE,
                  preserve_intra_word_dashes = FALSE)
\method{removePunctuation}{PlainTextDocument}(x, \dots)
}
\arguments{
  \item{x}{A character or text document.}
  \item{preserve_intra_word_contractions}{a logical specifying whether
    intra-word contractions should be kept.}
  \item{preserve_intra_word_dashes}{a logical specifying whether
    intra-word dashes should be kept.}
  \item{\dots}{passed over arguments \code{preserve_intra_word_contractions} and
    \code{preserve_intra_word_dashes}.}
}
\value{
  The character or text document \code{x} without punctuation marks
  (besides intra-word contractions (\samp{'}) and intra-word dashes (\samp{-})
  if \code{preserve_intra_word_contractions} and
  \code{preserve_intra_word_dashes} are set, respectively).
}
\seealso{
  \code{\link{getTransformations}} to list available transformation
  (mapping) functions.

  \code{\link{regex}} shows the class \code{[:punct:]} of punctuation
  characters.
}
\examples{
data("crude")
inspect(crude[[14]])
inspect(removePunctuation(crude[[14]]))
inspect(removePunctuation(crude[[14]],
                          preserve_intra_word_contractions = TRUE,
                          preserve_intra_word_dashes = TRUE))
}
