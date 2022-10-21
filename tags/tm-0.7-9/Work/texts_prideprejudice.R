library("dplyr", warn.conflicts = FALSE)
library("ggplot2")
library("magrittr")
library("methods")
library("stringr")

install.packages("janeaustenr")

lines <- (data_frame(text = janeaustenr::prideprejudice)
          %>% mutate(
    linenumber = row_number(),
    chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                            ignore_case = TRUE)))))
texts <- c(tapply(lines$text, lines$chapter, paste, collapse = "\n"))

saveRDS(texts, file = "texts_prideprejudice.rds")
