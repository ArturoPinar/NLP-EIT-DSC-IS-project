# Developer: Arturo Pinar Adán

# LIBRARIES REQUIRED

if(!require(rJava)){
  install.packages("rJava")
}
library(rJava)
.jinit(parameters="-Xmx4g")


if(!require(NLP)) {
  install.packages("NLP")
}
library(NLP)
if(!require(openNLP)) {
  install.packages("NLP")
}
if(!require(openNLP)) {
  install.packages("openNLP")
}
library(openNLP) 
if(!require(openNLPmodels.en)) {
  install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at")
}
library(openNLPmodels.en)
if(!require("tm")){
  install.packages("tm")
}
library(tm)
#install.packages("superml") #for TF-IDF vectorization
if(!require("superml")) {
  install.packages("superml")
}
library(superml)
if(!require("lsa")){
install.packages("lsa")
}
library(lsa) # For cosine similarity

if(!require("text2vec")) {
install.packages("text2vec")
}
library(text2vec)

if(!require("pracma")) {
install.packages("pracma")
}
library(pracma)

# GLOBAL CONFIGURATION
currentDir = ""
corpusPath = "data/data_corpus.txt"
corpusFolder = "data"



# AUXILIAR FUNCTIONS
getAnnotationsFromDocument = function(phrases){
  f=as.String(phrases)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  sent_word <- annotate(f, list(sent_token_annotator, word_token_annotator))
  pos <- annotate(f, pos_tag_annotator, sent_word)
  parse_annotator <- Parse_Annotator()
  result <- annotate(f, parse_annotator, pos)
  return(result)  
}

CreateCorpus <- function (dataset){
  
  if(!file.exists(corpusFolder)){
    dir.create(file.path(currentDir, corpusFolder))
  }
  
  else if(file.exists(corpusPath)) {
    file.remove(corpusPath) # Avoid duplicate entries
  }
  
  df <- read.csv(dataset)
  
  for(row in df$content){
    write.table(row, file=corpusPath, append = TRUE)
  }
  source.pos = DirSource(corpusFolder)
  corpus = Corpus(source.pos)
  return(corpus)
  
}

getAnnotatedPlainTextDocument = function(document,annotations){
  x=as.String(document)
  a = AnnotatedPlainTextDocument(x,annotations)
  return(a)  
} 

tokenizeSentence <- function (corpus) {
  annotations = lapply(corpus, getAnnotationsFromDocument)
  message("(=====-----) (process: 50%)")
  corpus.tagged = Map(getAnnotatedPlainTextDocument, corpus, annotations)
  sentences_tokens = sents(corpus.tagged[[1]])
  return (sentences_tokens)
}

processCorpus <- function (corpusPreprocessing) {
  corpusPreprocessing <- tm_map(corpusPreprocessing, removeWords, stopwords())
  
  corpusPreprocessing <- tm_map(corpusPreprocessing, stripWhitespace)

  corpusPreprocessing <- tm_map(corpusPreprocessing, removePunctuation)
 
  corpusPreprocessing <- tm_map(corpusPreprocessing, stemDocument)
  
  customedSWords = c(stopwords(), "What", "Where", "How", "Who", "Why")
  corpusPreprocessing <- tm_map(corpusPreprocessing, removeWords, customedSWords)
  return (corpusPreprocessing)
}

evaluateCosine <- function (cosine_similarity_matrix) {
  
  maxIndex <- -1
  maxVal <- -1
  index <- 2
  
  while(index < length(cosine_similarity_matrix)){
    val <- cosine_similarity_matrix[index,]
    if (val > maxVal) {
      maxIndex <- index
      maxVal <- val
    }
    index = index + 1
  }
  if(maxVal > 0.1) { # 10% of error
    return (maxIndex)
  }
  else{
    return(-1)
  }
  
  
}

cleanMessage <- function (text) {
  
  nLoop <- lengths(text)
  v <- length(nLoop)
 
  j = 1
  
  if(v != 0){
  while (j < nLoop) {
    if(text[[1]][[j]] == "\"" || 
       text[[1]][[j]] == "x" ||
       text[[1]][[j]] == "1"){
      
      text[[1]][[j]] <- ''
    }
    j = j + 1
    
  }
  }
  
  
  resultUserFinal <- paste(unlist(text), collapse = ' ')
  resultUserFinal <- gsub("\\s+"," ",resultUserFinal)
  return (resultUserFinal)
}
evaluateSimilarity <- function (corpusIdf, sentences_tokens_display) {
  
  tfv <- TfIdfVectorizer$new(remove_stopwords = FALSE)
  tf_matrix <- tfv$fit_transform(corpusIdf$content)
  
  n <- nrow(tf_matrix)
  
  cosine_similarity <- sim2(tf_matrix, t(tf_matrix[n,]), method = c("cosine"))
  
  maxIndex <- evaluateCosine(cosine_similarity)
  
  if(maxIndex != -1) {
  result <- sentences_tokens_display[maxIndex]
  outputUser <- cleanMessage(result)
  }
  else{
    outputUser <- "Sorry, I couldn't find what are you searching for."
  }
  return (outputUser)
  
}

selectNew <- function (dataset) {
  df2 <- read.csv(dataset)
  
  k = 1
  for(new in df2$title) {
    message(paste(k, ": ", new))
    k = k + 1
  }
  inputUser2 = -1
  while(as.integer(inputUser2) < 1 || as.integer(inputUser2) >= k){
  message(paste("Write the number of the new that you want to be sumarized between 1 and ", 
                k - 1))
  inputUser2 <- readline("Write the number here > ")
  inputUser2 <- as.integer(inputUser2)
  }
  
  content <- df2$content[inputUser2]
  content <- as.String(content)
  return(content)
  
}

obtainPipelineEntityRecognition <- function (content) {
  word_ann <- Maxent_Word_Token_Annotator()
  sent_ann <- Maxent_Sent_Token_Annotator()
  
  person_ann <- Maxent_Entity_Annotator(kind = "person")
  location_ann <- Maxent_Entity_Annotator(kind = "location")
  organization_ann <- Maxent_Entity_Annotator(kind = "organization")
  date_ann <- Maxent_Entity_Annotator(kind = "date")
  
  pipeline <- list(sent_ann, 
                   word_ann, 
                   person_ann, 
                   location_ann,
                   organization_ann,
                   date_ann)
  
  return (pipeline)
  
}

recognizeEntities <- function (text_annotations.l1, kind, content, w) {
  if(kind == "person") {
    people = content[text_annotations.l1[w == kind]]
    return (unique(people))
    
  }
  if(kind == "location") {
    locations = content[text_annotations.l1[w == kind]]
    return (unique(locations))
  }
  if(kind == "organization") {
    organizations = content[text_annotations.l1[w == kind]]
    return (unique(organizations))
  }
  if(kind == "date") {
    dates = content[text_annotations.l1[w == kind]]
    return (unique(dates))
  }
}


showEntities <- function (pipeline, content){
  text_annotations.l1 = NLP::annotate(content, pipeline)
  typeof(text_annotations.l1)
  
  k <- sapply(text_annotations.l1$features, `[[`, "kind")
  
  people <- recognizeEntities(text_annotations.l1, "person", content, k)
  places <- recognizeEntities(text_annotations.l1, "location", content, k)
  organizations <- recognizeEntities(text_annotations.l1, "organization", content, k)
  dates <- recognizeEntities(text_annotations.l1, "date", content, k)
  message("(==========) (process: 100%)")
  message("")
  message("")
  
  message("Here is the summary of your new: ")
  message("")
  message("")
  
  message("---- PLACES REFERENCED ----")
  message(cleanMessage((places)))
  message("---- PEOPLE REFERENCED ----")
  message(cleanMessage((people)))
  message("---- ORGANIZATIONS REFERENCED ----")
  message(cleanMessage((organizations)))
  message("---- DATES REFERENCED ----")
  message(cleanMessage((dates)))
  
}



# MAIN FUNCTIONS

summary <- function (dataset) {
  message("Select the new you want to be sumarized")
  content <- selectNew(dataset)
  message("(----------) (process: 0%)")
  
  pipeline <- obtainPipelineEntityRecognition()
  message("(======----) (process: 60%)")
  
  showEntities(pipeline, content)
}

answerQuestion <- function (dataset, inputUser) {
  
  message("(----------) (process: 0%)")
  corpus <- CreateCorpus(dataset)
  message("(==--------) (process: 20%)")
  sentences_tokens <- tokenizeSentence(corpus)
  message("(======----) (process: 60%)") 
  sentences_tokens_display = sentences_tokens
  
  sentences_tokens <- append(sentences_tokens, inputUser)
  corpusPreprocessing <- as.VCorpus(sentences_tokens)
  message("(=======---) (process: 70%)")
  corpusProcessed <- processCorpus(corpusPreprocessing)
  message("(========--) (process: 80%)")
  outputUser <- evaluateSimilarity(corpusProcessed, sentences_tokens_display)
  message("(==========) (process: 100%)")
  
  if(file.exists(corpusPath)) {
    file.remove(corpusPath)
  }
  return (outputUser)
}

# MAIN: START HERE

message("Welcome to newsSummary App what can I do for you? ")

message("First of all I need to know where is located the dataset FILE .CSV that I can use please")

okFile = FALSE
okFolder = FALSE
currentDir <- ""
dataset <- ""
while(okFolder == FALSE || okFile == FALSE) {
  
  if(okFolder == FALSE) {
    message("A valid path is for example: 
      C:/Users/apina/OneDrive/Escritorio/MASTER/UPM/IS/NLP/assignment")
    message("IMPORTANT: only a path of a folder is valid")
    currentDir <- readline(prompt = ">")
    if(file.exists(currentDir)) {
      okFolder <- TRUE
      setwd(currentDir)
    }
    else{
      message("This folder seems to not exist")
    }
  }
  
  if(okFile == FALSE) {
    message("Tell me the name of the dataset (including extension) that must be
      located in the folder that you have specified before")
    message("Example: news.csv")
    dataset <- readline(prompt = ">")
    if(file.exists(dataset)) {
      okFile <- TRUE
    }
    else{
      message("This dataset seems to not exist")
    }
  }
}
message("Say summary if you want to see a summary regarding names, locations, organizations
      and dates from a new that you can select.")
message("Also, you can ask me anything :)")

userInput <- ""

while(userInput != "quit"){
  message("Write quit to exit the application")
  userInput <- ""
  userInput <- readline(prompt = "Tell me :) ")
  
  userInput <- tolower(userInput)
  if(userInput == "summary") {
    summary(dataset)
    message("")
    message("")
  }
  else if(userInput == "hello" || userInput == "hi" ||
          userInput == "good morning" || userInput == "good afternoon") {
    message("Hello!")
  }
  else if (userInput == "bye" || userInput == "goodbye") {
    message("Goodbye!")
  }else{
    if(userInput != "quit"){
      message("I will give you an answer searching in my corpus.")
      message("This may take long time depending on the amount of data, thank you for waiting :)")
      output <- answerQuestion(dataset, userInput)
      message("I've found the following sentence: ")
      message(output)
    }
  }
}

message("Goodbye")
