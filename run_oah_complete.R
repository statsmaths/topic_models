library(mallet)
library(rJava)
library(readr)
library(cleanNLP)
library(readxl)
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
prop <- read_excel("~/Desktop/OAH20 Proposals_12.6.2019.xlsx")
names(prop) <- stri_replace_all(names(prop), ".", fixed=" ")
prop <- prop[nchar(as.character(prop$Proposal.Abstract)) > 100,]
prop <- prop[!is.na(nchar(as.character(prop$Proposal.Abstract))),]
prop$Participant.Title[is.na(prop$Participant.Title)] <- ""
prop$Participant.First.Name[is.na(prop$Participant.First.Name)] <- ""
prop$Participant.Middle.Name[is.na(prop$Participant.Middle.Name)] <- ""
prop$Participant.Last.Name[is.na(prop$Participant.Last.Name)] <- ""
prop$Participant.Suffix[is.na(prop$Participant.Suffix)] <- ""
prop$Participant.Affiliation[is.na(prop$Participant.Affiliation)] <- ""
prop$author <- sprintf("%s %s %s %s %s (%s)", prop$Participant.Title,
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
prop$author <- stri_replace_all(prop$author, "", fixed = "())")
prop$author <- stri_replace_all(prop$author, "", fixed = "\"")
prop$author[stri_length(prop$author) < 10] <- ""
prop$time <- sprintf("%s to %s", stri_sub(as.character(prop$Start.Time), 12, 16),
                                 stri_sub(as.character(prop$End.Time), 12, 16))

author <- tapply(prop$author, prop$Proposal.ID,
                 function(v) paste(unique(v), collapse = "; "))
id <- match(prop$Proposal.ID, names(author))
prop$author <- author[id]
prop <- prop[!duplicated(prop$Proposal.Abstract),]
prop <- prop[!duplicated(prop$Proposal.Title),]

# PREPARE PAPER DATA
paper <- read_excel("~/Desktop/OAH20 Papers_12.6.2019.xlsx")
names(paper) <- stri_replace_all(names(paper), ".", fixed=" ")
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
cnlp_init_spacy()

anno <- cnlp_annotate(text)

toks <- anno$token
toks <- anti_join(toks, filter_list, by = c("lemma" = "lemma"))
toks <- anti_join(toks, filter_list, by = c("token" = "lemma"))
toks <- filter(toks, upos %in% c("NOUN", "VERB"))
toks <- split(toks, toks$doc_id)
toks <- lapply(toks, getElement, "lemma")
toks <- as.character(unlist(lapply(toks, paste, collapse = " ")))

mallet_obj <- learn_topics_oah(toks, ntopics = 14, seed = 7)

# BUILD WEBSITE
build_webpage("oah2020", mallet_obj, data$desc, data$title)

# BUILD INDEX PAGE
library(stringi)

x <- readLines("models/oah2020/data/meta.csv")
title <- stri_sub(x, stri_locate(x, fixed = ",\"")[,1] + 2, -1)
type <- stri_sub(sapply(stri_split(x, fixed = "<p>"), getElement, 4), 18, -10)
prop_title <- sapply(stri_split(x, fixed = "<p>"), getElement, 5)
id <- stri_detect(prop_title, fixed = "Session:")
prop_title <- stri_sub(prop_title, 21, -10)
prop_title[!id] <- title[!id]
title[!id] <- ""
title <- stri_replace_all(title, "", fixed = "\"")
prop_title <- stri_replace_all(prop_title, "", fixed = "\"")

id <- which(title == "")
base_string <- "
            <tr>
              <td>%s</td>
              <td><a href=\"https://humanitiesdata.org/oah2020/#/doc/%d\">%s</a></td>
              <td></td>
            </tr>"
out1 <- sprintf(base_string, type, seq_along(type) - 1, prop_title)[id]
sby <- prop_title[id]

id <- which(title != "")
base_string <- "
            <tr>
              <td>%s</td>
              <td>%s</td>
              <td><a href=\"https://humanitiesdata.org/oah2020/#/doc/%d\">%s</a></td>
            </tr>"
out2 <- sprintf(base_string, type, prop_title, seq_along(type) - 1, title)[id]
sby <- c(sby, prop_title[id])

out <- c(out1, out2)
out <- out[order(sby)]

# NEED TO MANUALLY PUT THIS INTO INDEX ON SERVER
# FIND THIS: <div id="doc_view" class="hidden">
cat(out, file = "~/Desktop/temp.txt")


  # <div id="doc_view" class="hidden">
  #   <div id="doc_view_help">
  #     <table class="table table-condensed" id="doc_topics">
  #       <thead>
  #         <tr>
  #           <th>Type</th>
  #           <th class="wide">Proposal Title</th>
  #           <th class="wide">Paper Title</th>
  #         </tr>
  #       </thead>
  #       <tbody>
  #         DATA HERE
  #       </tbody>
  #     </table>
  #   </div><!-- #doc_view_main -->
  # </div>
