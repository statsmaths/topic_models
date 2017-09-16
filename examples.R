library(coreNLP)
library(mallet)
library(rJava)
library(readr)

source("scripts.R")

#######################################################################
# oah, paper abstracts
mdata <- read_csv("~/Desktop/oah/metadata.csv")
links <- rep("#", length(mdata))

filter_list <- c(LETTERS, letters, "^", "_", "\\", "paper", "part", "project", "study", "who",
  "history", "historian", "presentation", "way", "what")
mallet_obj <- learn_topics("~/Desktop/oah/anno", 10, seed = 7,
                            filter_list = filter_list)



build_webpage("oah12", mallet_obj, links, mdata$titles)


build_webpage("oah12", mallet_obj, links, mdata$titles,
              topic_nums = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,
                12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24))


c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17,
18, 19, 20, 21, 22, 23, 24)



#######################################################################
# arXiv.org, statistics articles from 2016-01-01 to 2016-06-01
mdata <- read.csv("~/Desktop/arxiv_abstracts/metadata.csv", as.is=TRUE, quote="", sep="|")
mdata <- mdata[-1935,]
links <- sprintf("https://arxiv.org/abs/%s", mdata$id)

mallet_obj <- learn_topics("~/Desktop/arxiv_abstracts/anno", 30L)
build_webpage("arxiv30", mallet_obj, links, mdata$title)

mallet_obj <- learn_clust_topics("~/files/dataProc/arxiv/anno", max_depth=5)
build_webpage("arxiv32", mallet_obj, links, mdata$title)


#######################################################################
# Stanford encyclopaedia of philosophy
fin <- gsub("\\.Rds", "", dir("~/files/dataProc/sep/anno"))
title <- readRDS("~/files/dataProc/sep/titles.Rds")
links <- sprintf("http://plato.stanford.edu/entries/%s", fin)

mallet_obj <- learn_topics("~/files/dataProc/sep/anno", 30L)
build_webpage("sep30", mallet_obj, links, title)

mallet_obj <- learn_clust_topics("~/files/dataProc/sep/anno", max_depth=5)
build_webpage("sep32", mallet_obj, links, title)


#######################################################################
# 113th Congress
mdata <- read.csv("~/files/dataProc/congress/meta/bills93-114.txt", as.is=TRUE)
fin <- gsub("\\.Rds", "", dir("~/files/dataProc/congress/anno"))
mdata <- mdata[match(fin, mdata$BillID),]
title <- sprintf("%s: %s", mdata$BillID, mdata$Title)

mallet_obj <- learn_topics("~/files/dataProc/congress/anno", 20L)
build_webpage("congress20", mallet_obj, mdata$URL, title)

mallet_obj <- learn_clust_topics("~/files/dataProc/congress/anno", max_depth=4)
build_webpage("congress16", mallet_obj, mdata$URL, title)


#######################################################################
# ASA abstracts
mdata <- read.csv("~/files/dataProc/asa/metadata.csv", as.is=TRUE, quote="", sep="|")
links <- rep("http://www.theasa.net/", nrow(mdata))

mallet_obj <- learn_topics("~/files/dataProc/asa/anno", 20L)
build_webpage("asa20", mallet_obj, links, mdata$title)

mallet_obj <- learn_clust_topics("~/files/dataProc/asa/anno", max_depth=4)
build_webpage("asa16", mallet_obj, links, mdata$title)

