set.seed(42)
library(data.table)

data_dir <- "../module_3/final/en_US"
out_file <- "shiny_app/ngrams.rds"

read_sample <- function(file, pct = 0.03) {
  lines <- readLines(file, encoding = "UTF-8", skipNul = TRUE)
  sample(lines, size = floor(length(lines) * pct))
}

cat("Reading data...\n")
corpus <- c(
  read_sample(file.path(data_dir, "en_US.blogs.txt")),
  read_sample(file.path(data_dir, "en_US.news.txt")),
  read_sample(file.path(data_dir, "en_US.twitter.txt"))
)

clean <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z']", " ", x)
  x <- gsub("'+", " ", x)
  x <- gsub("\\s+", " ", trimws(x))
  x
}

cat("Cleaning...\n")
corpus <- clean(corpus)
corpus <- corpus[nchar(corpus) > 1]
tokens <- strsplit(corpus, " ")

make_ngrams <- function(tokens, n) {
  result <- vector("list", length(tokens))
  for (i in seq_along(tokens)) {
    w <- tokens[[i]]
    w <- w[nchar(w) > 0]
    len <- length(w)
    if (len >= n) {
      m <- matrix(nrow = len - n + 1, ncol = n)
      for (j in seq_len(n)) m[, j] <- w[j:(len - n + j)]
      result[[i]] <- m
    }
  }
  do.call(rbind, result)
}

cat("Building bigrams...\n")
bg <- make_ngrams(tokens, 2)
bg_dt <- as.data.table(bg)
setnames(bg_dt, c("w1", "w2"))
bg_dt <- bg_dt[, .N, by = .(w1, w2)][N >= 2]
setorder(bg_dt, w1, -N)
bg_dt <- bg_dt[bg_dt[, .I[seq_len(min(.N, 5))], by = w1]$V1]

cat("Building trigrams...\n")
tg <- make_ngrams(tokens, 3)
tg_dt <- as.data.table(tg)
setnames(tg_dt, c("w1", "w2", "w3"))
tg_dt[, prefix := paste(w1, w2)]
tg_dt[, c("w1", "w2") := NULL]
tg_dt <- tg_dt[, .N, by = .(prefix, w3)][N >= 2]
setorder(tg_dt, prefix, -N)
tg_dt <- tg_dt[tg_dt[, .I[seq_len(min(.N, 5))], by = prefix]$V1]

cat("Building unigrams...\n")
ug <- sort(table(unlist(tokens)), decreasing = TRUE)
top_words <- names(ug[1:100])

cat("Converting to lookup lists...\n")
bg_list <- split(bg_dt$w2, bg_dt$w1)
tg_list <- split(tg_dt$w3, tg_dt$prefix)

model <- list(bigrams = bg_list, trigrams = tg_list, top_words = top_words)

dir.create("shiny_app", showWarnings = FALSE)
saveRDS(model, out_file, compress = "xz")
cat("Done. Model saved to", out_file, "\n")
