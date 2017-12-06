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

# PREPARE PROPOSAL DATA
prop <- xlsx::read.xlsx("~/Desktop/2018 AM Proposal Data_10.11.2017.xlsx",
                        sheetIndex = 1, , stringsAsFactors = FALSE)
prop <- prop[nchar(as.character(prop$Proposal.Abstract)) > 100,]
prop <- prop[!is.na(nchar(as.character(prop$Proposal.Abstract))),]
prop <- prop[!duplicated(prop$Proposal.Abstract),]
prop <- prop[!duplicated(prop$Proposal.Title),]
prop$Participant.Title[is.na(prop$Participant.Title)] <- ""
prop$Participant.First.Name[is.na(prop$Participant.First.Name)] <- ""
prop$Participant.Middle.Name[is.na(prop$Participant.Middle.Name)] <- ""
prop$Participant.Last.Name[is.na(prop$Participant.Last.Name)] <- ""
prop$Participant.Suffix[is.na(prop$Participant.Suffix)] <- ""
prop$Participant.Affiliation[is.na(prop$Participant.Affiliation)] <- ""
prop$author <- sprintf("%s %s %s %s (%s %s)", prop$Participant.Title,
                                       prop$Participant.First.Name,
                                       prop$Participant.Middle.Name,
                                       prop$Participant.Last.Name,
                                       prop$Participant.Suffix,
                                       prop$Participant.Affiliation)
prop$author <- stri_replace_all(prop$author, " ", regex = "[ ]+")
prop$author <- stri_replace_all(prop$author, "", regex = "$ ")
prop$author <- stri_replace_all(prop$author, "", regex = " ^")
prop$author <- stri_replace_all(prop$author, "(", fixed = "( ")
prop$author <- stri_replace_all(prop$author, ")", fixed = " )")
prop$author <- stri_replace_all(prop$author, "", fixed = "\"")
prop$author[stri_length(prop$author) < 10] <- ""
prop$time <- sprintf("%s to %s", stri_sub(as.character(prop$Start.Time), 12, 16),
                                 stri_sub(as.character(prop$End.Time), 12, 16))

# PREPARE PAPER DATA
paper <- xlsx::read.xlsx("~/Desktop/2018 AM Paper Data _10.11.2017.xlsx",
                         sheetIndex = 1, stringsAsFactors = FALSE)
paper <- paper[nchar(as.character(paper$Paper.Abstract)) > 100,]
paper <- paper[!is.na(nchar(as.character(paper$Paper.Abstract))),]
paper <- paper[!duplicated(paper$Paper.Abstract),]
paper <- paper[!duplicated(paper$Paper.Title),]
paper$Participant.TItle[is.na(paper$Participant.TItle)] <- ""
paper$Participant.First.Name[is.na(paper$Participant.First.Name)] <- ""
paper$Participant.Middle.Name[is.na(paper$Participant.Middle.Name)] <- ""
paper$Participant.Last.Name[is.na(paper$Participant.Last.Name)] <- ""
paper$Participant.Suffix[is.na(paper$Participant.Suffix)] <- ""
paper$Participant.Affiliation[is.na(paper$Participant.Affiliation)] <- ""
paper$author <- sprintf("%s %s %s %s (%s %s)", paper$Participant.TItle,
                                       paper$Participant.First.Name,
                                       paper$Participant.Middle.Name,
                                       paper$Participant.Last.Name,
                                       paper$Participant.Suffix,
                                       paper$Participant.Affiliation)
paper$author <- stri_replace_all(paper$author, " ", regex = "[ ]+")
paper$author <- stri_replace_all(paper$author, "", regex = "$ ")
paper$author <- stri_replace_all(paper$author, "", regex = " ^")
paper$author <- stri_replace_all(paper$author, "(", fixed = "( ")
paper$author <- stri_replace_all(paper$author, ")", fixed = " )")
paper$author <- stri_replace_all(paper$author, "", fixed = "\"")
paper$author[stri_length(paper$author) < 10] <- ""
paper$time <- sprintf("%s to %s", stri_sub(as.character(paper$Start.Time), 12, 16),
                                  stri_sub(as.character(paper$End.Time), 12, 16))

data <- bind_rows(
          data_frame(title = paper$Paper.Title,
                     session = paper$Proposal.Title,
                     author = paper$author,
                     time = paper$time,
                     text = paper$Paper.Abstract,
                     date = paper$Date,
                     type = "Paper"),
          data_frame(title = prop$Proposal.Title,
                     session = "",
                     author = prop$author,
                     time = prop$time,
                     text = prop$Proposal.Abstract,
                     date = prop$Date,
                     type = prop$Proposal.Type)
        )

clean_title <- function(title) {
  title <- as.character(title)
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
  title <- stri_replace_all(title, "CSU ", fixed = "CSU ")
  title <- stri_replace_all(title, " VJ ", fixed = " Vj ")
  title <- stri_replace_all(title, "20th", fixed = "20Th")
  return(title)
}

data$title <- clean_title(data$title)
data$session <- clean_title(data$session)

data$session_text <- sprintf("<p><h5><b>Session:</b> %s</h5></p>",
                             data$session)
data$session_text[data$session == ""] <- ""

text <- as.character(data$text)
text <- stri_replace_all(text, "", fixed = "\t")
text <- stri_replace_all(text, "", fixed = "\r")
text <- stri_replace_all(text, "", fixed = "\n")
text <- stri_replace_all(text, "", fixed = "“")
text <- stri_replace_all(text, "", fixed = "”")
text <- stri_replace_all(text, "", fixed = "‘")
text <- stri_replace_all(text, "", fixed = "’")
text <- stri_replace_all(text, "&quot;", fixed = "\"")
text <- stri_replace_all(text, "&quot;", fixed = "\'")
data$text <- text

data$desc <- sprintf("<h5><i>%s</i></h5><p><h5><b>Date:</b> %s</h5></p><p><h5><b>Time:</b> %s</h5></p><p><h5><b>Type:</b> %s</h5></p>%s<p>%s</p>",
                    data$author, data$date, data$time, data$type, data$session_text, data$text)

# PARSE WITH SPACY
init_spaCy(model_name = "en_core_web_sm")

anno <- run_annotators(text, as_strings = TRUE)

toks <- get_token(anno)
toks <- anti_join(toks, filter_list, by = c("lemma" = "lemma"))
toks <- anti_join(toks, filter_list, by = c("word" = "lemma"))
toks <- filter(toks, upos %in% c("NOUN"))
toks <- split(toks, toks$id)
toks <- lapply(toks, getElement, "lemma")
toks <- as.character(unlist(lapply(toks, paste, collapse = " ")))

mallet_obj <- learn_topics_oah(toks, ntopics = 14, seed = 7)

# BUILD WEBSITE
build_webpage("oah12", mallet_obj, data$desc, data$title)



