score.sentiment <- function(sentence,pos,neg){
  require(plyr)
  require(stringr)

  sentence = gsub('[^A-z ]','', sentence)
  sentence = tolower(sentence)
  word.list = str_split(sentence, '\\s+')
  words = unlist(word.list)
  
  pos.matches = match(words, pos)
  neg.matches = match(words, neg)
  
  pos.matches = !is.na(pos.matches)
  neg.matches = !is.na(neg.matches)
  score = sum(pos.matches) - sum(neg.matches)
  return(score)
  
}

score.sentiment <- function(model,df,trained_Y,Number_of_predictors){
  
  RMSE = function(predictions, actual){
    sqrt(mean((predictions - actual)^2))
  }
  mae <- function(predictions,actual){
    error <- predictions - actual
    mean(abs(error))
  }
  mse <- function(residuals) { 
    mse <- mean(residuals^2)
    return(mse)
  }
  r2 <- function(predictions,trained_Y) { 
    SSR  <- sum((predictions - mean(trained_Y))^2)
    SST  <- sum((trained_Y - mean(trained_Y))^2)
    R2  <- SSR/SST
    return(R2)
  }
  Residualstandarderror <-function(model,trained_Y,predictors){
    
    SumSq <- sum((fitted(model)-trained_Y)^2)
    return(sqrt(SumSq/(length(trained_Y)-(predictors+1))))
  }
  fStat <- function(predictions,actual,predictors){
    
    TSS <- sum((actual -mean(actual))^2)
    RSS <- sum((actual-predictions)^2)
    FSTAT <- ((TSS-RSS)/predictors)/(RSS/(length(actual)-predictors-1))
    return(FSTAT)
  }
  adjustedr2 <- function(predictions,actual,predictors){
    n <- length(actual)
    rsqared <- r2(predictions,actual)
    return(1-(1-rsqared) * ((n-1)/(n-predictors-1)))
    
  }
  
  # calcuate predictions
  predictions <- predict(model,df)
  
  # functions
  res_error <- Residualstandarderror(model,trained_Y,Number_of_predictors)
  root_mean_square_error <- RMSE(fitted(model),trained_Y)
  mean_absolute_error <- mae(fitted(model),trained_Y)
  mean_square_error <- mse(resid(model))
  rsquared <- r2(predictions,trained_Y)
  fstatistic <- fStat(predictions,trained_Y,Number_of_predictors)
  n_rows <- nrow(df)
  degreesFreedom <- nrow(df) - Number_of_predictors
  adjustedr2 <- adjustedr2(predictions,trained_Y,Number_of_predictors)
  
  results <- data.frame(
    n_rows,
    degreesFreedom,
    fstatistic,
    res_error,
    root_mean_square_error,
    mean_absolute_error,
    mean_square_error,
    rsquared,
    adjustedr2)
  
  results = as.data.frame(t(results))
  results$Metric <- rownames(results)
  rownames(results) <- NULL
  colnames(results) <- c('Value','Metric')
  results <- results[,c(2,1)]
  results$Value = round(results$Value,3)
  
  return(results)
  
}

Static_Missing_Vars <- function (df) {
  
  list = c()
  for(i in 1:length(colnames(df))){
    
    # Agg data
    Cases = table(df[colnames(df)[i]])
    Cases = sort(Cases,decreasing=T)
    
    # Drop Variables that are > 95% missing
    if(sum(is.na(df[colnames(df)[i]]))/nrow(df) >.95)
    {
      list = c(list , c=colnames(df)[i])
    }
    # Drop variables that are close to static (Warning sometimes the minroity help explain variance)  
    else if(Cases[1]/nrow(df)>.95)
    {
      list = c(list , c=colnames(df)[i])     
    }
    else if(Cases[1]/nrow(df)>.8)
    {
      print(paste('Variable',colnames(df)[i],'was not dropped, but',Cases[1]/nrow(df),'of the cases are static'))
    }
  }
  if(length(list)>0){
    print('The following variables will be dropped')
    print(as.data.frame(list))
    df = df[, !(colnames(df) %in% c((list)))]
  }
  
  return(df)
}

changeclass <- function(df) {
  
  for (z in 1:length(colnames(df))) {
    if (nrow(df) < 32) {
      print("please change column types manually; record count < 32, stopping operation")
      break
    }
    len <- nrow(unique(df[colnames(df)[z]]))
    ## Turn off exponential notation otherwise regular expressions will identify these records as character
    options(scipen = 999)
    total <- length(grep("[A-z]|\\d{4}-\\d{2}-\\d{2}", df[[colnames(df)[z]]]))
    
    tbl <- table(grepl("\\d{4}-\\d{2}-\\d{2}", df[colnames(df)[z]][sample(nrow(df), 0.25 * (nrow(df))), ]))
    if (length(tbl) == 1 && names(tbl) == "FALSE") {
      tbl <- matrix(c(4, 1), ncol = 2, byrow = TRUE)
      tbl <- as.table(tbl)
    } else if (length(tbl) == 1 && names(tbl) == "TRUE") {
      tbl <- matrix(c(1, 4), ncol = 2, byrow = TRUE)
      tbl <- as.table(tbl)
    }
    
    if (tbl[1][[1]] < tbl[2][[1]] & class(df[[colnames(df)[z]]]) != "Date") {
      print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to date", sep = ""))
      df[colnames(df)[z]] <- as.Date(df[, colnames(df)[z]], format = "%Y-%m-%d")
    } else if (len > 32 & total == 0 & class(df[[colnames(df)[z]]]) %in% c("character", "factor", "Date")) {
      print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to numeric", sep = ""))
      df[, colnames(df)[z]] <- as.numeric(as.character(df[, colnames(df)[z]]))
    } else if (len > 32 & total > 1 & class(df[[colnames(df)[z]]]) != "character" & class(df[[colnames(df)[z]]]) != "Date") {
      print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to character", sep = ""))
      df[, colnames(df)[z]] <- as.character(df[[colnames(df)[z]]])
    } else if (len <= 32 & class(df[[colnames(df)[z]]]) != "factor" & class(df[[colnames(df)[z]]]) != "Date") {
      print(paste("changing ", colnames(df)[z], ":", class(df[[colnames(df)[z]]]), " to factor", sep = ""))
      df[, colnames(df)[z]] <- as.factor(df[, colnames(df)[z]])
    }
  }
  print("If warning messages were output please check date classes")
  return(df)
}

word.count <- function(sentence){
  sentence = gsub('[^A-z ]','', sentence)
  sentence = tolower(sentence);
  word.list = str_split(sentence, '\\s+');
  words = unlist(word.list);
  return(length(words))
}

read_airbnb <- function(){
  df <- read.csv('Y:/Airbnb_Texas_Rentals.csv')
  
}

earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  return(d)
}

process_airbnb <- function(df){
  
  ## Simple clean up
  df$url <- NULL
  df$X <- NULL
  df$average_rate_per_night <- as.integer(gsub('[$]','',df$average_rate_per_night))
  df$date_of_listing <- as.Date(paste(df$date_of_listing,'1'), format = "%B %Y %d")
  df$approxDaysOnMarket <- as.integer(Sys.Date() - df$date_of_listing)
  df <- df[complete.cases(df), ]
  
  ## Read in sentiment dictionaries
  pos = readLines('Y:/positive-words.txt')
  neg = readLines('Y:/negative-words.txt')
  
  ## clean up line breaks
  df$description <- as.character(df$description)
  df$description = gsub("[']", " ", df$description)
  df$description = gsub('[\\]', "191x", df$description) ## handling newlines, work around because \\n was removing n as well
  df$description = gsub('191xn', " ", df$description)
  
  df$description <- as.character(df$description)
  df$title = gsub("[']", " ", df$title)
  df$title = gsub('[\\]', "191x", df$title) ## handling newlines, work around because \\n was removing n as well
  df$title = gsub('191xn', " ", df$title)
  
  ## word count and Sentiment
  df$description_Score <- apply(as.matrix(df$description),1,score.sentiment,pos,neg)
  df$description_word_ct <- apply(as.matrix(df$description),1,word.count)
  
  df$title_Score <- apply(as.matrix(df$title),1,score.sentiment,pos,neg)
  df$title_word_ct <- apply(as.matrix(df$title),1,word.count)
  
  ## Check on Static-Missing vars
  df <- Static_Missing_Vars(df)
  
  ## Ensure proper data classes
  df <- changeclass(df)
  df$title_Score <- as.integer(df$title_Score)     ## switched to factor because of number of levels
  df$title_word_ct <- as.integer(df$title_word_ct) ## switched to factor because of number of levels
  
  
  ## Add features
  df$condo <- as.integer(grepl("condo",df$description,ignore.case=TRUE))
  df$lake <- as.integer(grepl("lake",df$description,ignore.case=TRUE))
  df$pool <- as.integer(grepl("pool",df$description,ignore.case=TRUE))
  df$wifi <- as.integer(grepl("wifi|wi-fi",df$description,ignore.case=TRUE))
  df$private <- as.integer(grepl("private",df$description,ignore.case=TRUE))
  
  df$PopularCity <- ifelse(df$city %in% names(head(abs(sort(-table(df$city))),10)),1,0)
  return(df)
  
}

trim <-  function(x){ require(stringr)
  
  x <- gsub('  ',' ',x) 
  x <- gsub('  ',' ',x) 
  x <- gsub('  ',' ',x) 
  x <- gsub('  ',' ',x) 
  x <- str_trim(x, side = c("both"))
  return(x)}

normalize <- function(x) { 
  x <- as.matrix(x)
  minAttr=apply(x, 2, min)
  maxAttr=apply(x, 2, max)
  x <- sweep(x, 2, minAttr, FUN="-") 
  x=sweep(x, 2,  maxAttr-minAttr, "/") 
  attr(x, 'normalized:min') = minAttr
  attr(x, 'normalized:max') = maxAttr
  return (x)
} 

wordcloud <- function(text){
  
  require(tm)
  require(wordcloud)
  require(RColorBrewer)
  
  ## Create corpus
  my_corpus = Corpus(VectorSource(text))
  my_corpus <- sapply(my_corpus,function(row) iconv(row, "latin1", "ASCII", sub=""))
  my_corpus = Corpus(VectorSource(my_corpus))
  
  ## tdm
  tdm = TermDocumentMatrix(
    my_corpus,
    control = list(
      removePunctuation = TRUE,
      stopwords = c("via", "amp","http", stopwords("english")),
      removeNumbers = TRUE, tolower = TRUE)
  )
  
  
  m = as.matrix(tdm)
  # get word counts in decreasing order
  word_freqs = sort(rowSums(m), decreasing = TRUE) 
  # create a data frame with words and their frequencies
  dm = data.frame(word = names(word_freqs), freq = word_freqs)
  
  return(dm)
  
}

rfImp <- function(df,target,returnval){
  
  # Extract predictors
  predictors <- names(df)[names(df) != target]
  
  # Train a model across all the training data and plot the variable importance
  model_rf <- randomForest(df[,predictors], df[,target], ntree=500, importance=TRUE)
  
  # Export Variable importance
  dat<-importance(model_rf, type=1)
  
  # Convert to frame
  featureImportance <- data.frame(dat)
  featureImportance$Var<-rownames(featureImportance)
  rownames(featureImportance) <- 1:nrow(featureImportance)
  featureImportance$Var<-as.character(featureImportance$Var)
  colnames(featureImportance)[1] <- 'IncMSE'
  ## Var Plot
  featureImportance<-sqldf("select * from featureImportance order by IncMSE desc limit 20")
  plt <- ggplot(featureImportance, aes(x=reorder(Var, order(IncMSE, decreasing = F)), y=IncMSE)) +
    geom_bar(stat="identity", fill="#E8AFAF") +
    coord_flip() + 
    theme_light(base_size=20) +
    xlab("Variable") +
    ylab("Importance") + 
    ggtitle("Random Forest Feature Importance") +
    theme(plot.title=element_text(size=18))
  
  # Select top twenty variables
  featureImportanceVals <- sqldf("select Var, IncMSE from featureImportance order by IncMSE desc limit 20")
  featureImportance <- sqldf("select Var from featureImportance order by IncMSE desc limit 20")
  
  RfSelectedVariables <- c()
  for(x in 1:nrow(featureImportance))
  {
    RfSelectedVariables <- c(RfSelectedVariables , c=featureImportance[[1]][x])    
  }
  
  
  if(returnval == 1){
    print('Limited to top twenty variables')
    return(featureImportanceVals)
  }
  if(returnval == 2){
    return(RfSelectedVariables)
  }
  if(returnval == 3){
    return(plt)
  }
}

save(score.sentiment,score.sentiment,Static_Missing_Vars,changeclass,word.count,read_airbnb,earth.dist,trim,process_airbnb,normalize,wordcloud,rfImp,file='functions.rds')



