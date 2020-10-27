movies <- read.csv("movie_records_metadata.csv")
summary(movies)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

for(i in 1:ncol(movies)){
  if (class(movies[,i]) == "numeric" | class(movies[,i]) == "integer")
    movies[is.na(movies[,i]), i] <- mean(movies[,i], na.rm = TRUE)
  else
    movies[movies[,i] == "",i] <- Mode(movies[,i])}


#Visualization imdb score histogram using ggplot
library(ggplot2)
ggplot(movies, aes(movies$imdb_score)) +
geom_histogram(bins = 40) + 
geom_vline(xintercept = mean(movies$imdb_score,na.rm = TRUE),colour = "blue") +
ylab("Movies Frequency") +
xlab("IMDB Score") + 
annotate("text",x=6.2,y=100, label="Average IMDB Score", col="yellow",angle=90) +
ggtitle("Histogram for IMDB Scores")
#The distribution of the ratings looks normal here, 
#movies with ratings 8.0 to 10.0 span the best 250 on the IMDB website



#Extraction of Genres
genres <- as.character(movies$genres)
movies$movie_title <- as.character(movies$movie_title)
genresTokens <- unlist(strsplit(genres[i], "[|]"))
df <- data.frame(name=character(), genre=character(), rating=numeric(),gross=numeric(), stringsAsFactors = F)
for (i in 1:length(genres)){
  w <- unlist(strsplit(genres[i], "[|]"))
  genresTokens <- c(genresTokens,w)
  for (j in 1:length(w)){
    df[nrow(df)+1,] = list(movies$movie_title[i], w[j],movies$imdb_score[i], movies$gross[i])
  }
}

#Visualization1: Genre Frequency
barchart(df$genre, main="Movie Genres Frequency",
         ylab="Genre", xlab="Frequency")#tokens are parsed genres

#Visualization2: Genre vs Avg Rating
AvgRatingPerGenre <- aggregate(df$rating,list(df$genre),mean)
barplot(AvgRatingPerGenre$x, ylim=c(0,10),las=2,
        names.arg=AvgRatingPerGenre$Group.1,
        main="Average IMDB Score per Genre", ylab="IMDB Score")
#We can observe that the average rating for all genres is the same 
#except for Reality-TV or Fame-Show, so a production company might want to think twice,
#before investing in one of these

#Visualization3: Genre vs Gross
y <- aggregate(as.numeric(df$gross),list(df$genre),sum)
dotchart(y$x, labels = y$Group.1, las=2, 
         main="Genre vs Gross", xlab="Gross")



#Visualization1: Content Rating Frequency
barchart(movies$content_rating, main="Content Rating Frequency",
         ylab="Content Rating", xlab="Frequency")

#Visualization2: Content Rating vs Average Rating
movies <- movies[movies$content_rating != "",]
AvgRatingPerContentRating <- aggregate(movies$imdb_score, 
                                       list(movies$content_rating), mean)
barplot(AvgRatingPerContentRating$x, ylim=c(0,10),las=2,
        names.arg=AvgRatingPerContentRating$Group.1,
        main="Content Rating vs Avg IMDB Score", ylab="IMDB Score")

#Visualization3: Content Rating vs Gross
x <- aggregate(as.numeric(movies$gross),list(movies$content_rating),sum)
dotchart(x$x, labels = x$Group.1,
         main="Content Rating vs Gross", xlab="Gross")



#Correlogram
#install.packages("corrplot")
library(corrplot)
moviedbNumerics <- movies[,c(26,14,4,3,5,6,8,25,9,13,16,19,23,24,28)]
names(movies)
names(moviedbNumerics)
corrplot(cor(moviedbNumerics), method="ellipse")
#Visualization: year vs score
boxplot(movies$imdb_score~movies$title_year, data = fit,las=2, 
        xlab="Year", ylab="IMDB Score", main="IMDB Scores vs Movie year"
        , col="lightgray")
#Observation: For the last 2 decades movies, there has been some drops in the scores,
#as the number of produced movies increased



#Visualization: countries vs score
##################################
bymedian <- with(movies, reorder(movies$country, -movies$imdb_score, median))
boxplot(movies$imdb_score ~ bymedian, data = movies,
        xlab = "Countries", ylab = "IMDB score",
        main = "IMDB Scores vs Countries",las = 2,
        col = "lightgray") 
#Eventhough countries like USA and UK dominated the number production of movies 
#the median scores are not the highest among other developing countries like 
#Egypt, Brazil or Mexico



#Visualization: Facebook likes vs score
#######################################
plot(movies$imdb_score, movies$movie_facebook_likes,
     main="IMDB Score vs Facebook likes", xlab="IMDB Score"
     ,ylab = "Facebook likes")



#Fitting Linear Regression Model
################################

fitData <- data.frame(movies[,c("imdb_score",
                             "duration",
                             "director_facebook_likes",
                             "actor_1_facebook_likes",
                             "actor_2_facebook_likes",
                             "actor_3_facebook_likes",
                             "facenumber_in_poster",
                             "budget",
                             "title_year",
                             "color")])

fit <- lm(imdb_score ~ director_facebook_likes + duration
          + actor_1_facebook_likes + actor_2_facebook_likes
          + actor_3_facebook_likes + facenumber_in_poster
          + budget + title_year + color, data=fitData)
summary(fit)
predictedValue <- fit$coefficients[1] +  
fit$coefficients[2]*5000 + fit$coefficients[3]*180 + fit$coefficients[4]*1000 + 
fit$coefficients[5]*1000 + fit$coefficients[6]*855 + fit$coefficients[7]*5 + 
fit$coefficients[8]*250000000 + fit$coefficients[9]*2000 + fit$coefficients[10]*1 
predictedValue



#Visualization for Directors with Most Movies
#install.packages("wordcloud")
library(wordcloud)
movies <- read.csv("C:/data/movie_metadata.csv")
directors <- table(movies$director_name)
freqTable <- as.data.frame(directors)
set.seed(1234)
wordcloud(words = freqTable$Var1, freq = freqTable$Freq, min.freq = 8,
          max.words=200, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))



#Visualization for percentage of colored movies
movieColors <- factor(movies$color[movies$color != ""])
colorTable <- apply(data.frame(movieColors), 2, table)
slices <- c(colorTable[1], colorTable[2])
lbls <- c("Black and White", "Color")
pct <- round(slices/sum(slices) *100,2)
lbls <- paste(lbls, pct) 
lbls <- paste(lbls,"%",sep="") 
pie(slices, labels = lbls, main="Pie Chart of Colored Movies")


