# This script builds a website from a mallet model
library(mallet)
library(jsonlite)
library(Matrix)
library(coreNLP)
library(rJava)


build_webpage <- function(name, mallet_obj, links, titles) {

  if (inherits(mallet_obj, "jobjRef")) {
    # pull out objects from the mallet_obj
    docs <- mallet.doc.topics(mallet_obj, smoothed=FALSE, normalized=FALSE)
    words <- mallet.topic.words(mallet_obj, smoothed=FALSE, normalized=FALSE)
    vocab <- mallet_obj$getVocabulary()
  } else {
    docs <- mallet_obj$docs
    words <- mallet_obj$words
    vocab <- mallet_obj$vocab
  }

  outDir <- sprintf("models/%s/data", name)
  dir.create("models", FALSE, TRUE)
  dir.create(sprintf("models/%s", name), FALSE, TRUE)
  dir.create(outDir, FALSE, TRUE)
  file.copy("lib/index.html", sprintf("models/%s/", name))

  # meta.csv
  meta <- cbind(links, titles)
  write.table(meta, sprintf("%s/meta.csv",outDir), sep=",",
      quote=TRUE, col.names=FALSE, row.names=FALSE)

  # topic_scaled.csv
  temp <- scale(t(scale(words)))
  dists <- acos(crossprod(temp) / nrow(temp)) / pi
  diag(dists) <- 0
  topic_scaled <- cmdscale(dists)
  write.table(topic_scaled, sprintf("%s/topic_scaled.csv",outDir),
      sep=",", quote=FALSE, col.names=FALSE, row.names=FALSE)

  # info.json
  info <- fromJSON("lib/info.json")
  info$names <- as.list(sprintf("Topic %03d", 1:ncol(docs)))
  names(info$names) <- as.character(1:ncol(docs))
  info <- jsonlite::toJSON(info, auto_unbox=TRUE, pretty=4)
  writeLines(info, sprintf("%s/info.json",outDir))

  # dt.json
  dtm <- as(docs, "CsparseMatrix")
  dtm <- toJSON(list(i=dtm@i, p=dtm@p, x=dtm@x), digits=4L)
  writeLines(dtm, sprintf("%s/dt.json",outDir))

  # tw.json
  n_top_words <- 50
  alpha <- rep(1, ncol(docs))
  tw <- apply(words, 1, function(v) {
    list(words = vocab[order(v, decreasing=TRUE)[1:n_top_words]],
         weights = sort(v, decreasing=TRUE)[1:n_top_words])
  })
  tw <- list(alpha=alpha, tw=tw)
  tw <- jsonlite::toJSON(tw, dataframe="columns", digits=4L)
  writeLines(tw, sprintf("%s/tw.json",outDir))

}


learn_topics <- function(basedir, ntopics, pos_list = "NOUN",
    filter_list = c(LETTERS, letters, "^", "_", "\\", "p_0", "a_i", "x_i", "y_i", "x_1", "y_1", "|", "datum", "â", "Â")) {

  # initalize java
  mem <- "4g"
  options(java.parameters = paste0("-Xmx", mem))
  rJava::.jinit()

  # read all of the annotations into R
  fin <- dir(basedir, full.names=TRUE)
  z <- lapply(fin, function(file) readRDS(file))

  # extract features
  text <- sapply(z, function(this) {
    tok <- getToken(this)
    these <- tok$lemma[universalTagset(tok$POS) %in% pos_list]
    these <- setdiff(these, filter_list)
    index <- grep("mathequation", these, ignore.case=TRUE)
    if (length(index)) these <- these[-index]
    paste(these, collapse=" ")
  })

  # run mallet
  tf <- tempfile()
  writeLines(c("be", "go", "have", letters, LETTERS), tf)
  inst <- mallet.import(basename(fin), text, tf)
  mallet_obj <- MalletLDA(num.topics = as.double(ntopics))
  mallet_obj$loadDocuments(inst)
  mallet_obj$train(200)
  mallet_obj$maximize(10)
  gc()

  mallet_obj
}


learn_clust_topics <- function(basedir, max_depth, pos_list = "NOUN",
    filter_list = c(LETTERS, letters, "^", "_", "\\", "p_0", "a_i", "x_i", "y_i", "x_1", "y_1", "|", "datum", "â", "Â")) {
  # read all of the annotations into R
  fin <- dir(basedir, full.names=TRUE)
  z <- lapply(fin, function(file) readRDS(file))

  # extract features
  text <- lapply(z, function(this) {
    tok <- getToken(this)
    these <- tok$lemma[universalTagset(tok$POS) %in% pos_list]
    these <- setdiff(these, filter_list)
    index <- grep("mathequation", these, ignore.case=TRUE)
    if (length(index)) these <- these[-index]
    index <- which(these == toupper(these))
    if (length(index)) these <- these[-index]
    these
  })

  vocab <- table(unlist(text))
  coff <- sort(vocab, decreasing=TRUE)[4000]
  vocab <- names(vocab[vocab > coff & vocab < length(text)*0.95])

  # calculate Laplacian matrix
  tf_raw <- t(sapply(text, function(this) { table(factor(this, levels=vocab)) }))
  df_raw <- as.numeric(apply(tf_raw != 0, 2, sum))

  tf <- log(1 + tf_raw)
  idf <- log(nrow(tf_raw) / df_raw)
  tfidf <- t(t(tf) * idf)
  tfidf <- scale(tfidf, center=FALSE)
  A <- tcrossprod(tfidf) / ncol(tfidf)
  diag(A) <- 0
  Dinvroot <- diag(1 / sqrt(apply(A, 1, sum)))

  L <- diag(nrow(A)) - Dinvroot %*% A %*% Dinvroot

  # now, cycle over the second eigenvalues
  groups <- rep(0, nrow(L))
  for (depth in 1:max_depth) {

    new_groups <- groups
    second_vals <- rep(0, length(groups))

    for (g in unique(groups)) {

      index <- which(groups == g)
      e <- eigen(L[index,index])
      vals <- e$vector[,ncol(e$vector)-1]
      m <- 0 #median(vals)

      new_groups[index][vals > m]  <- g * 10 + 1
      new_groups[index][vals <= m] <- g * 10

      second_vals[index] <- vals

      #cat(sprintf("group: %014s  depth: %02d\n", g, depth))
    }
    groups <- new_groups
  }

  ntopics <- length(unique(groups))
  docs <- matrix(0, nrow=nrow(L), ncol=ntopics)
  for (i in 1:length(unique(groups))) {
    docs[groups == unique(groups)[i],i] <- 1
  }

  words <- apply(tfidf, 2, function(v) tapply(v, groups, sum))
  words <- round(words)
  rownames(words) <- NULL
  colnames(words) <- NULL

  list(docs = docs, words = words, vocab = vocab)
}



