library(dplyr)
library(curl)
library(quanteda)
library(Rcpp)

enBlog = readLines('en_US.blogs.txt')
enNews = readLines('en_US.news.txt')
enTwitter = readLines('en_US.twitter.txt')
enAll = c(enBlog, enNews, enTwitter)

set.seed(1)
n = 1/100
enBlog_train = sample(enBlog, length(enBlog) * n)
enNews_train = sample(enNews, length(enNews) * n)
enTwitter_train = sample(enTwitter, length(enTwitter) * n)
enAll_train = sample(enAll, length(enAll) * n)

makeCorpus = function(x) {
  corpus(unlist(segment(x, 'sentences')))
}

enBlog_train = makeCorpus(enBlog_train)
enNews_train = makeCorpus(enNews_train)
enTwitter_train = makeCorpus(enTwitter_train)
enAll_train = makeCorpus(enAll_train)

makeToken = function(x, ngramSize = 1, simplify = T) {
  toLower(
    quanteda::tokenize(x,
                       removeNumbers = T,
                       removePunct = T,
                       removeSeparators = T,
                       removeTwitter = T,
                       ngrams = ngramSize,
                       concatenator = " ",
                       simplify = simplify
    )
  )
}


enBlog_train1 = makeToken(enBlog_train)
enBlog_train2 = makeToken(enBlog_train, 2)
enBlog_train3 = makeToken(enBlog_train, 3)

enNews_train1 = makeToken(enNews_train)
enNews_train2 = makeToken(enNews_train, 2)
enNews_train3 = makeToken(enNews_train, 3)

enTwitter_train1 = makeToken(enTwitter_train)
enTwitter_train2 = makeToken(enTwitter_train, 2)
enTwitter_train3 = makeToken(enTwitter_train, 3)

enAll_train1 = makeToken(enAll_train)
enAll_train2 = makeToken(enAll_train, 2)
enAll_train3 = makeToken(enAll_train, 3)

getFrequency = function(x, minCount = 1) {
  x = x %>%
    group_by(NextWord) %>%
    summarize(count = n()) %>%
    filter(count >= minCount)
  x = x %>%
    mutate(freq = count / sum(x$count)) %>%
    select(-count) %>%
    arrange(desc(freq))
}

enBlog_dfTrain1 = data_frame(NextWord = enBlog_train1)
enBlog_dfTrain1 = getFrequency(enBlog_dfTrain1)

enBlog_dfTrain2 = data_frame(NextWord = enBlog_train2)
enBlog_dfTrain2 = getFrequency(enBlog_dfTrain2) %>%
  separate(NextWord, c('word1', 'NextWord'), " ")

enBlog_dfTrain3 = data_frame(NextWord = enBlog_train3)
enBlog_dfTrain3 = getFrequency(enBlog_dfTrain3) %>%
  separate(NextWord, c('word1', 'word2', 'NextWord'), " ")

enNews_dfTrain1 = data_frame(NextWord = enNews_train1)
enNews_dfTrain1 = getFrequency(enNews_dfTrain1)

enNews_dfTrain2 = data_frame(NextWord = enNews_train2)
enNews_dfTrain2 = getFrequency(enNews_dfTrain2) %>%
  separate(NextWord, c('word1', 'NextWord'), " ")

enNews_dfTrain3 = data_frame(NextWord = enNews_train3)
enNews_dfTrain3 = getFrequency(enNews_dfTrain3) %>%
  separate(NextWord, c('word1', 'word2', 'NextWord'), " ")

enTwitter_dfTrain1 = data_frame(NextWord = enTwitter_train1)
enTwitter_dfTrain1 = getFrequency(enTwitter_dfTrain1)

enTwitter_dfTrain2 = data_frame(NextWord = enTwitter_train2)
enTwitter_dfTrain2 = getFrequency(enTwitter_dfTrain2) %>%
  separate(NextWord, c('word1', 'NextWord'), " ")

enTwitter_dfTrain3 = data_frame(NextWord = enTwitter_train3)
enTwitter_dfTrain3 = getFrequency(enTwitter_dfTrain3) %>%
  separate(NextWord, c('word1', 'word2', 'NextWord'), " ")

enAll_dfTrain1 = data_frame(NextWord = enAll_train1)
enAll_dfTrain1 = getFrequency(enAll_dfTrain1)

enAll_dfTrain2 = data_frame(NextWord = enAll_train2)
enAll_dfTrain2 = getFrequency(enAll_dfTrain2) %>%
  separate(NextWord, c('word1', 'NextWord'), " ")

enAll_dfTrain3 = data_frame(NextWord = enAll_train3)
enAll_dfTrain3 = getFrequency(enAll_dfTrain3) %>%
  separate(NextWord, c('word1', 'word2', 'NextWord'), " ")

saveRDS(enBlog_dfTrain1, file = "enBlog_dfTrain1.rds")
saveRDS(enBlog_dfTrain2, file = "enBlog_dfTrain2.rds")
saveRDS(enBlog_dfTrain3, file = "enBlog_dfTrain3.rds")

saveRDS(enNews_dfTrain1, file = "enNews_dfTrain1.rds")
saveRDS(enNews_dfTrain2, file = "enNews_dfTrain2.rds")
saveRDS(enNews_dfTrain3, file = "enNews_dfTrain3.rds")

saveRDS(enTwitter_dfTrain1, file = "enTwitter_dfTrain1.rds")
saveRDS(enTwitter_dfTrain2, file = "enTwitter_dfTrain2.rds")
saveRDS(enTwitter_dfTrain3, file = "enTwitter_dfTrain3.rds")

saveRDS(enAll_dfTrain1, file = "enAll_dfTrain1.rds")
saveRDS(enAll_dfTrain2, file = "enAll_dfTrain2.rds")
saveRDS(enAll_dfTrain3, file = "enAll_dfTrain3.rds")