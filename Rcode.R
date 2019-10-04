##############Movie Recommendation System###########

####Loading Of Libraries####
library(tidyverse)
library(data.table)
library(caret)
library(dslabs)
library(matrixStats)
library(dplyr)
library(lubridate)
####Downloading of Dataset####

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))
movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)

colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Converting Timestamp column into time difference with respect to first Rating of movie
movielens<-movielens%>%mutate(date=as_date(timestamp))
first_rat<-movielens%>%group_by(movieId)%>%summarize(fir_rat=min(date))
movielens<-movielens%>%left_join(first_rat,by='movieId')%>%mutate(rat_dur=as.numeric(date-fir_rat))



####Splitting of Dataset into Train and Test Set####

set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]


# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")
# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)


### 1.simple model using overall average of Ratings###

u<-mean(edx$rating)
naive_rsme<-RMSE(edx$rating,u)
rmse_res2<-data_frame(method="Just the average",RMSE=round(naive_rsme,4))
rmse_res2



### 2.Movie Specific Effect
mov_avg<-edx%>%group_by(movieId)%>%summarize(b_movie=mean(rating-u))


#Observation of Movie effect on ratings

mov_avg%>%qplot(b_movie,geom = "histogram",bins=30,data=.,color="black") 

predicted_rat<-u+validation%>%left_join(mov_avg,by='movieId')%>%pull(b_movie)
rmse_movie_effect<-RMSE(predicted_rat,validation$rating)
rmse_res2<-bind_rows(rmse_res2,data_frame(method="movie effect+average",RMSE=round(rmse_movie_effect,4)))
rmse_res2


### 3.User specific effect 


#Observation of User Specific Effect

edx%>%group_by(userId) %>%summarize(b_u = mean(rating)) %>%filter(n()>=300) %>%ggplot(aes(b_u))+geom_histogram(bins = 30, color = "black")

use_avg<-edx%>%left_join(mov_avg,by='movieId')%>%group_by(userId)%>%summarize(b_user=mean(rating-b_movie-u))

pred_rat<-validation%>%left_join(use_avg,by='userId')%>%left_join(mov_avg,by='movieId')%>%mutate(pred=u+b_user+b_movie)%>%pull(pred)

rmse_mov_use<-RMSE(pred_rat,validation$rating)
rmse_mov_use
rmse_res2<-bind_rows(rmse_res2,data_frame(method="movie+user+average",RMSE=round(rmse_mov_use,4)))
rmse_res2

### 4.Genres Specific effect

gen_avg<-edx%>%left_join(mov_avg,by='movieId')%>%left_join(use_avg,by='userId')%>%group_by(genres)%>%summarize(b_genre=mean(rating-u-b_movie-b_user))

#Observation of Genres Specific Effect###

edx%>%group_by(genres)%>%summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%filter(n >= 10000) %>% 
  mutate(genres = reorder(genres, avg)) %>%ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + geom_point() +
  geom_errorbar() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
pre_rat_amug<-validation%>%left_join(mov_avg,by='movieId')%>%left_join(use_avg,by='userId')%>%left_join(gen_avg,by='genres')%>%mutate(pred=u+b_user+b_movie+b_genre)%>%pull(pred)
rmse_amug<-RMSE(pre_rat_amug,validation$rating)
rmse_amug
rmse_res2<-bind_rows(rmse_res2,data_frame(method="movie+user+average+genres",RMSE=round(rmse_amug,4)))
rmse_res2

###  5.Effect of Rating Timestamp
edx[1:1000,]%>%ggplot(aes(rat_dur,rating))+geom_point()+geom_smooth()
t_rat<-edx%>%left_join(mov_avg,by='movieId')%>%left_join(use_avg,by='userId')%>%left_join(gen_avg,by='genres')%>%mutate(b_time=rating-u-b_user-b_movie-b_genre)

#Use of linear model for relation between time difference and Rating residual
fit_lm<-lm(b_time~rat_dur,data=t_rat)

fit_lm$coefficients


pre_rat_amugt<-validation%>%left_join(mov_avg,by='movieId')%>%left_join(use_avg,by='userId')%>%left_join(gen_avg,by='genres')%>%mutate(pred=u+b_user+b_movie+b_genre+fit_lm$coefficients[2]*rat_dur+fit_lm$coefficients[1])%>%pull(pred)
rmse_amugt<-RMSE(pre_rat_amugt,validation$rating)
rmse_amugt
rmse_res2<-bind_rows(rmse_res2,data_frame(method="movie+user+average+genres+ratingdelay",RMSE=round(rmse_amugt,4)))
rmse_res2