library(dplyr)
library(curl)
library(quanteda)
library(Rcpp)

enBlog_dfTrain1 =  readRDS(file = 'enBlog_dfTrain1.rds')
enBlog_dfTrain2 =  readRDS(file = 'enBlog_dfTrain2.rds')
enBlog_dfTrain3 =  readRDS(file = 'enBlog_dfTrain3.rds')

enNews_dfTrain1 =  readRDS(file = 'enNews_dfTrain1.rds')
enNews_dfTrain2 =  readRDS(file = 'enNews_dfTrain2.rds')
enNews_dfTrain3 =  readRDS(file = 'enNews_dfTrain3.rds')

enTwitter_dfTrain1 =  readRDS(file = 'enTwitter_dfTrain1.rds')
enTwitter_dfTrain2 =  readRDS(file = 'enTwitter_dfTrain2.rds')
enTwitter_dfTrain3 =  readRDS(file = 'enTwitter_dfTrain3.rds')

enAll_dfTrain1 =  readRDS(file = 'enAll_dfTrain1.rds')
enAll_dfTrain2 =  readRDS(file = 'enAll_dfTrain2.rds')
enAll_dfTrain3 =  readRDS(file = 'enAll_dfTrain3.rds')

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
  }
  
  else if (nrow(y) >= 1) {
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