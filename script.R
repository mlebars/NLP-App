library(dplyr)
library(curl)
library(quanteda)
library(readr)
library(caTools)
library(tidyr)

enBlog = readLines('en_US.blogs.txt')
enNews = readLines('en_US.news.txt')
enTwitter = readLines('en_US.twitter.txt')
enAll = c(enBlog, enNews, enTwitter)

set.seed(1)
n = 1/1000
enBlog_sample = sample(enBlog, length(enBlog) * n)
enNews_sample = sample(enNews, length(enNews) * n)
enTwitter_sample = sample(enTwitter, length(enTwitter) * n)
enAll_sample = sample(enAll, length(enAll) * n)

enBlog_split = sample.split(enBlog_sample, 0.8)
enBlog_train = subset(enBlog_sample, enBlog_split == T)
enBlog_valid = subset(enBlog_sample, enBlog_split == F)

enNews_split = sample.split(enNews_sample, 0.8)
enNews_train = subset(enNews_sample, enNews_split == T)
enNews_valid = subset(enNews_sample, enNews_split == F)

enTwitter_split = sample.split(enTwitter_sample, 0.8)
enTwitter_train = subset(enTwitter_sample, enTwitter_split == T)
enTwitter_valid = subset(enTwitter_sample, enTwitter_split == F)

enAll_split = sample.split(enAll_sample, 0.8)
enAll_train = subset(enAll_sample, enAll_split == T)
enAll_valid = subset(enAll_sample, enAll_split == F)

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

getType1 = function(type) {
  if(type == "Blog") {
    dfTrain1 = enBlog_dfTrain1
  } else if(type == "News") {
    dfTrain1 = enNews_dfTrain1
  } else if(type == "Twitter") {
    dfTrain1 = enTwitter_dfTrain1
  } else {
    dfTrain1 = enAll_dfTrain1
  }
  return(dfTrain1)
}

getType2 = function(type) {
  if(type == "Blog") {
    dfTrain2 = enBlog_dfTrain2
  } else if(type == "News") {
    dfTrain2 = enNews_dfTrain2
  } else if(type == "Twitter") {
    dfTrain2 = enTwitter_dfTrain2
  } else {
    dfTrain2 = enAll_dfTrain2
  }
  return(dfTrain2)
}

getType3 = function(type) {
  if(type == "Blog") {
    dfTrain3 = enBlog_dfTrain3
  } else if(type == "News") {
    dfTrain3 = enNews_dfTrain3
  } else if(type == "Twitter") {
    dfTrain3 = enTwitter_dfTrain3
  } else {
    dfTrain3 = enAll_dfTrain3
  }
  return(dfTrain3)
}


getInput = function(x) {

  if(x == "") {
    input1 = data_frame(word = "")
    input2 = data_frame(word = "")
  }

  if(length(x) ==1) {
    y = data_frame(word = makeToken(corpus(x)))
    
  }

  if (nrow(y) == 1) {
    input1 = data_frame(word = "")
    input2 = y
  
  }   else if (nrow(y) >= 1) {
    input1 = tail(y, 2)[1, ]
    input2 = tail(y, 1)
  }

  inputs = data_frame(words = unlist(rbind(input1,input2)))
  return(inputs)
}

predictNextWord = function(x, y, n = 100, dfTrain1, dfTrain2, dfTrain3) {

  if(x == "" & y == "") {
    prediction[1:n, ] = ""
    
  } else if(x %in% dfTrain3$word1 & y %in% dfTrain3$word2) {
    prediction = dfTrain3 %>%
      filter(word1 %in% x & word2 %in% y) %>%
      select(NextWord, freq)
    
  } else if(y %in% dfTrain2$word1) {
    prediction = dfTrain2 %>%
      filter(word1 %in% y) %>%
      select(NextWord, freq)
    
  } else{
    prediction = dfTrain1 %>%
      select(NextWord, freq)
  }
  
  return(na.omit(prediction[1:n, ]))
}