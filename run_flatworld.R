library(mallet)
library(rJava)
library(readr)
library(cleanNLP)
library(xlsx)
library(stringi)
library(dplyr)
library(topicmodels)
library(tidytext)

source("scripts.R")
cnlp_init_spacy()

# Create filter list
filter_list <- c(LETTERS, letters, "^", "_", "\\",
                 stop_words$word)
filter_list <- data_frame(lemma = filter_list)

# Get the text files
fw <- dir("flatworld", full.names=TRUE)
text <- rep(NA_character_, length(fw))
for (i in seq_along(text)) {
  text[i] <- paste(readLines(fw[i], warn=FALSE), collapse="\n")
}
text <- stri_replace_all(text, "'", fixed="\x91")
text <- stri_replace_all(text, "'", fixed="\x92")
text <- stri_replace_all(text, "'", fixed="\x93")
text <- stri_replace_all(text, "'", fixed="\x94")

text_nl <- text
text <- stri_replace_all(text, " ", fixed = "\n")

# Annotate the text and clean the tokens
if (FALSE) {
  anno <- cnlp_annotators(text, as_strings = TRUE)
  write_rds(anno, "flatworld.rds")
} else {
  anno <- read_rds("flatworld.rds")
}

toks <- cnlp_get_token(anno, combine = TRUE)
toks <- anti_join(toks, filter_list, by = c("lemma" = "lemma"))
toks <- anti_join(toks, filter_list, by = c("word" = "lemma"))
toks <- filter(toks, lemma != "-PRON-")
toks <- filter(toks, upos != "PROPN")
# ents <- filter(toks, !is.na(entity), !(entity_type %in% c("TIME", "DATE", "CARDINAL", "ORDINAL")))
# toks <- filter(toks, is.na(entity_type))
# ents$lemma <- stri_replace_all(ents$entity, "-", fixed = " ")
# toks$lemma[toks$upos == "PROPN"] <- toks$word[toks$upos == "PROPN"]
# toks <- bind_rows(ents, toks)

toks <- split(toks, toks$id)
toks <- lapply(toks, getElement, "lemma")
toks <- as.character(unlist(lapply(toks, paste, collapse = " ")))

mallet_obj <- learn_topics_oah(toks, ntopics = 15, seed = 7)

# Get Metadata
meta <- read_csv("fw_metadata.csv")
names(meta)[1] <- "id"
meta$id <- stri_replace_all(meta$id, "", fixed = "_0001.pdf")
meta$id <- stri_replace_all(meta$id, "", fixed = ".pdf")
meta <- meta[!duplicated(meta$id),]

desc <- data_frame(id = stri_sub(basename(fw), 1, -5))
desc <- left_join(desc, meta)
desc$title[is.na(desc$title)] <- "Unavailable"
desc$desc <- sprintf("<b>Interviewee:</b> %s<br><b>Interviewer:</b> %s",
                      desc$`interviewee_1 (real name)`,
                      desc$interviewer_1)
txt <- stri_replace_all(text_nl, "<br>", fixed = "\n")
txt <- stri_replace_all(txt, "", regex="[^a-zA-Z0-9 \\.!?\\'<>]")
desc$desc <- sprintf("%s\n<br><b>Sample Text:</b><br>... %s ...", desc$desc,
                     stri_sub(txt, 1000, 2000))


# BUILD WEBSITE
build_webpage("fwriters", mallet_obj, desc$desc, desc$title)





