library(cleanNLP)
library(mallet)
library(rJava)

source("scripts.R")

#######################################################################
# American Quarterly journal articles

object <- make_doc("~/files/aq/all_annotations_level0.Rds")
doc <- object$doc
df <- object$df
tf <- object$tf

title <- gsub("\"", "&quot;", df$title[!duplicated(df$id)])
uri <- substr(df$uri[!duplicated(df$id)], 6L, nchar(df$uri[!duplicated(df$id)]) - 4L)
vocab <- object$vocab
links <- sprintf("https://www.jstor.org/stable/%s", uri)


mallet_obj <- learn_topics_clean(doc, uri, 30L)
build_webpage("aq30", mallet_obj, links, title)

mallet_obj <- learn_topics_clean(doc, uri, 60L)
build_webpage("aq60", mallet_obj, links, title)

mallet_obj <- learn_clust_topics_clean(tf, vocab, max_depth = 4L)
build_webpage("aq16", mallet_obj, links, title)

mallet_obj <- learn_clust_topics_clean(tf, vocab, max_depth = 5L)
build_webpage("aq32", mallet_obj, links, title)

mallet_obj <- learn_clust_topics_clean(tf, vocab, max_depth = 6L)
build_webpage("aq64", mallet_obj, links, title)
