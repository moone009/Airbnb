library(tm)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(reshape2)
library(caret)
library(ggmap)
library(randomForest)
load('functions.rds')

## ___________________________________________________________________________________________________________________
# Data Prep
## ___________________________________________________________________________________________________________________

## read file
df <- read_airbnb()

## process file
df <- process_airbnb(df)


## ___________________________________________________________________________________________________________________
# Exploratory Analysis
## ___________________________________________________________________________________________________________________

## Create summary stats
Stats =  df %>% 
  group_by(city)%>% 
  summarise(
    Mean=mean(average_rate_per_night),
    Min=min(average_rate_per_night),
    Max=max(average_rate_per_night),
    Median=median(average_rate_per_night), 
    Std=sd(average_rate_per_night),
    Count = n()) %>% arrange(-Count) %>% top_n(10)

## Analyze top freq cities
data <- subset(melt(Stats, id=c("city")),variable %in% c('Mean','Median', 'Std'))
ggplot(data, aes(city, value))+
geom_bar(stat = "identity", aes(fill = variable), position = "dodge") +
ggtitle("Top Ten Cities by Number of Rentals")+
labs(y="Rental Cost USD")


## Analyze Sentiments
ggplot(df, aes(x=description_Score))+
  geom_histogram(color="darkblue", fill="lightblue") +
  ggtitle("Descrition Sentiment Score") +
  labs(x="Sentiment Score (Postive is better)") 

ggplot(df, aes(x=title_Score))+
  geom_histogram(color="darkblue", fill="salmon") +
  ggtitle("Title Sentiment Score") +
  labs(x="Sentiment Score (Postive is better)") 

## ___________________________________________________________________________________________________________________
# Text Analysis
## ___________________________________________________________________________________________________________________

dm <- wordcloud(df$description)
## word freq cutoff
min <- dm$freq[as.integer(nrow(df)*.05)]

ggplot(head(dm,10), aes(x=reorder(word,-freq), y = freq,fill=word)) + 
  geom_bar(stat="identity") + 
  guides(fill=FALSE) +
  labs(x="") +
  ggtitle("Most Frequent Description Words")


## Title text Analysis
dm <- wordcloud(df$title)

## word freq cutoff
min <- dm$freq[as.integer(nrow(df)*.05)]

ggplot(head(dm,10), aes(x=reorder(word,-freq), y = freq,fill=word)) + 
  geom_bar(stat="identity") + 
  guides(fill=FALSE) +
  labs(x="") +
  ggtitle("Most Frequent Title Words")


## ___________________________________________________________________________________________________________________
# Knn Modeling
## ___________________________________________________________________________________________________________________
options(digits=9) ## avoid truncation 

downtown.center <- read.table(header=T,sep=',',text = "
                  'City','Lon','lat'  
                  'Houston',-95.361102,29.759557
                  'Austin',30.275610, -97.743783
                  'San Antonio',29.419157, -98.490170
                  'Dallas',32.782246, -96.799829
                  'Fort Worth',32.752094, -97.331701",colClasses = c('character','character','character'))

knnresults <- data.frame()

for(x in 1:5){
  df_ <- subset(df,city == trim( as.character( downtown.center$City[x])))
  
  model_df <- df_[,c(1:3,6:7,9:18)]
  model_df$bedrooms_count <- as.integer(model_df$bedrooms_count )
  model_df$city <- NULL
  model_df$Z <- scale(model_df$average_rate_per_night)
  model_df <- subset(model_df,Z < 3)
  model_df$Z <-  NULL
  
  ## Calculate Distance From Downtown
  model_df$DownTown_Distance <- earth.dist(as.numeric(downtown.center$Lon[x]),as.numeric(downtown.center$lat[x]),model_df$longitude,model_df$latitude)
   
  model_df$longitude <-  NULL
  model_df$latitude <-  NULL
   
  
  model_df[,c(2:7,13)] <-normalize(model_df[,c(2:7,13)])
  ind <- sample(2,nrow(model_df),replace=TRUE,prob=c(0.7,0.3))
  trainData <- model_df[ind==1,]
  testData <- model_df[ind==2,]
  
  for(i in 1:25){
  # Build Model
  fit <- knnreg(trainData[,c(2:12)], trainData[,c(1)], k = i)
  
  # Model get probability
  Predictions <- predict(fit, testData[,c(2:12)])
  mae_ <- mae(Predictions,testData$average_rate_per_night)
  knnresults <- rbind(knnresults,data.frame(City = trim( as.character( downtown.center$City[x])),k = i , MAE = mae_))
  
  }

}

ggplot(data = knnresults) +
  geom_line(aes(x =  k,y =  MAE,color= City))  + 
  ylab("MAE") +
  xlab("Number of Neighbors") +
  ggtitle("Knn Model Results")

## ___________________________________________________________________________________________________________________
# RandomForest Modeling
## ___________________________________________________________________________________________________________________

## Variable Importance, better understand what is driving the model
rfImp(df[,c(1,2,9:18)],'average_rate_per_night',3)


randomForestResults <- data.frame()
for(x in 1:5){
  df_ <- subset(df,city == trim(as.character(downtown.center$City[x])))
  
  model_df <- df_[,c(1:3,6:7,9:18)]
  model_df$bedrooms_count <- as.integer(model_df$bedrooms_count )
  model_df$city <- NULL
  model_df$Z <- scale(model_df$average_rate_per_night)
  model_df$Z <-  NULL
  
  ## Calculate Distance From Downtown
  model_df$DownTown_Distance <- earth.dist(as.numeric(downtown.center$Lon[x]),as.numeric(downtown.center$lat[x]),model_df$longitude,model_df$latitude)
  
  model_df$longitude <-  NULL
  model_df$latitude <-  NULL
  
  ind <- sample(2,nrow(model_df),replace=TRUE,prob=c(0.7,0.3))
  trainData <- model_df[ind==1,]
  testData <- model_df[ind==2,]
  
  rf <- randomForest(average_rate_per_night ~ ., data=trainData, ntree=500)
  
  Predictions <- predict(rf, testData)
  mae_ <- mae(Predictions,testData$average_rate_per_night)
  randomForestResults <- rbind(randomForestResults,data.frame(City =  trim(as.character(downtown.center$City[x])), mae=mae_))
  
}

 


