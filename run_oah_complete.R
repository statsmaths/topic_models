library(coreNLP)
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

#######################################################################
# oah, paper abstracts

filter_list <- c(LETTERS, letters, "^", "_", "\\", "paper", "part",
                 "project", "study", "who", "history", "historian",
                 "presentation", "way", "what", "research", "panel",
                 "panelist", "book", "audience", "field", "scholarship",
                 stop_words$word)
filter_list <- data_frame(lemma = filter_list)

prop <- xlsx::read.xlsx("~/gd/oah/2018 AM Proposal Data_10.11.2017.xlsx",
                        sheetIndex = 1, , stringsAsFactors = FALSE)
prop <- prop[nchar(as.character(prop$Proposal.Abstract)) > 100,]
prop <- prop[!is.na(nchar(as.character(prop$Proposal.Abstract))),]
paper <- xlsx::read.xlsx("~/gd/oah/2018 AM Paper Data _10.11.2017.xlsx",
                         sheetIndex = 1, stringsAsFactors = FALSE)
paper <- paper[nchar(as.character(paper$Paper.Abstract)) > 100,]
paper <- paper[!is.na(nchar(as.character(paper$Paper.Abstract))),]
paper <- paper[!duplicated(paper$Paper.Abstract),]
paper <- paper[!duplicated(paper$Paper.Title),]

init_spaCy("en")

title <- as.character(paper$Paper.Title)
title <- stri_replace_all(title, "", fixed = "\t")
title <- stri_replace_all(title, "", fixed = "\r")
title <- stri_replace_all(title, "", fixed = "\n")
title <- stri_replace_all(title, "", fixed = "‘")
title <- stri_replace_all(title, "", fixed = "’")
title <- stri_replace_all(title, "", fixed = "“")
title <- stri_replace_all(title, "", fixed = "”")
title <- stri_replace_all(title, "", fixed = "\"")
title <- stri_trans_totitle(title)
title <- stri_replace_all(title, "II", fixed = "Ii")
title <- stri_replace_all(title, "U.S.", fixed = "U.s.")
title <- stri_replace_all(title, "CA", fixed = "Ca ")
title <- stri_replace_all(title, "A.E.F.", fixed = "A.e.f.")
title <- stri_replace_all(title, "WW", fixed = "Ww")
title <- stri_replace_all(title, "WWII", fixed = "WWii")
title <- stri_replace_all(title, "20th C.", fixed = "20Th C.")
title <- stri_replace_all(title, "0s", fixed = "0S")

text <- paper$Paper.Abstract
text <- stri_replace_all(text, "", fixed = "\t")
text <- stri_replace_all(text, "", fixed = "\r")
text <- stri_replace_all(text, "", fixed = "\n")
text <- stri_replace_all(text, "", fixed = "“")
text <- stri_replace_all(text, "", fixed = "”")
text <- stri_replace_all(text, "", fixed = "‘")
text <- stri_replace_all(text, "", fixed = "’")
text <- stri_replace_all(text, "&quot;", fixed = "\"")
text <- stri_replace_all(text, "&quot;", fixed = "\'")



paper$Participant.TItle[is.na(paper$Participant.TItle)] <- ""
paper$Participant.First.Name[is.na(paper$Participant.First.Name)] <- ""
paper$Participant.Middle.Name[is.na(paper$Participant.Middle.Name)] <- ""
paper$Participant.Last.Name[is.na(paper$Participant.Last.Name)] <- ""
paper$Participant.Suffix[is.na(paper$Participant.Suffix)] <- ""
paper$Participant.Affiliation[is.na(paper$Participant.Affiliation)] <- ""

author <- sprintf("%s %s %s %s (%s %s)", paper$Participant.TItle,
                                       paper$Participant.First.Name,
                                       paper$Participant.Middle.Name,
                                       paper$Participant.Last.Name,
                                       paper$Participant.Suffix,
                                       paper$Participant.Affiliation)
author <- stri_replace_all(author, " ", regex = "[ ]+")
author <- stri_replace_all(author, "", regex = "$ ")
author <- stri_replace_all(author, "", regex = " ^")
author <- stri_replace_all(author, "(", fixed = "( ")
author <- stri_replace_all(author, ")", fixed = " )")
author[stri_length(author) < 10] <- ""

time <- sprintf("%s to %s", stri_sub(as.character(paper$Start.Time), 12, 16),
                            stri_sub(as.character(paper$End.Time), 12, 16))

desc <- sprintf("<h5><i>%s</i></h5><p><h5><b>Date:</b> %s</h5><p><h5><b>Time:</b> %s</h5><p><h5><b>Location:</b> <a href=\'http://www.oah.org/meetings-events/2018/sessions/\' style=\'color:#6CADC7\'>OAH Schedule</a></h5><p>%s",
                author, paper$Date, time, text)

anno <- run_annotators(text, as_strings = TRUE)

toks <- get_token(anno)
toks <- anti_join(toks, filter_list, by = c("lemma" = "lemma"))
toks <- anti_join(toks, filter_list, by = c("word" = "lemma"))
toks <- filter(toks, upos %in% c("NOUN"))
toks <- split(toks, toks$id)
toks <- lapply(toks, getElement, "lemma")
toks <- as.character(unlist(lapply(toks, paste, collapse = " ")))

mallet_obj <- learn_topics_oah(toks, ntopics = 14, seed = 7)

build_webpage("oah12", mallet_obj, desc, title)



