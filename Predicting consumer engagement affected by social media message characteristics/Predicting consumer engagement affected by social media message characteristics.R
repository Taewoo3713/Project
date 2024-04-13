# Ensure that all required packages are installed on your system before proceeding
# Refer to the 'requirements.txt' or the packages used under '# load packages' in this R script for the necessary packages

# load packages
library(stringr)
library(dplyr)
library(ggplot2)
library(ggcorrplot)
library(car)
library(vcd)
library(pscl)
library(MASS)
library(caret)
library(pROC)

rm(list=ls())
getwd()

# load dataset
# set working directory to the cloned repository. Change directory path as needed.
setwd("C:/Users/taewu/Desktop/MSA Gatech/1학기/MGT 6203/Group Project/data")
twts <- readRDS("twts")
brands <- readRDS("brands")

# ================== 1. Data Preprocessing ================= #
# extract organic tweets only
twts <- twts[twts$organic == 'organic',]

# add 'visual_presence' column to mark presence of visuals
for (i in 1:nrow(twts)) {
  # reset media_url to NA at the start of each iteration
  media_url <- NA
  
  # extract the media URL
  tryCatch({
    urls <- twts$entities[[i]]$media$media_url
    media_url <- if (length(urls) > 0) urls[1] else NA
  }, error = function(e) {
    # print an error message if an error occurs
    message(paste("Error in row", i, ":", e$message))
  })
  
  # assign the media URL to the dataframe
  twts$media_url[i] <- media_url
  
  # test for non-NA and non-empty strings, and that it contains "http"
  if (!is.na(media_url) && nzchar(media_url) && str_detect(media_url, "http")) {
    twts$visual_presence[i] <- 1
  } else {
    twts$visual_presence[i] <- 0
  }
}

table(twts$visual_presence)

# add 'followers_count' column by joining twts & brands data
# match screen_name 'Allbirds' before joining (this is the only brand with an unmatching name)
twts <- twts %>%
  mutate(screen_name = replace(screen_name, screen_name == "allbirds", "Allbirds"))
brands_selected <- dplyr::select(brands, screen_name, followers_count)

twts <- left_join(twts, brands_selected, by = "screen_name")

# # check na in 'followers_count' column
# sum(is.na(twts$followers_count))   # 0

# add 'norm_fav' column, which is favorite_count / followers_count
twts <- twts %>%
  mutate(norm_fav = ifelse(followers_count != 0, favorite_count / followers_count, NA))

# create a new df containing the variables we are interested in
data <- twts[c("created_at", "text", "visual_presence", "followers_count", "favorite_count", 
               "norm_fav", "screen_name", "word_count", 
               "syuzhet", "anger", "anticipation", "disgust", 
               "fear", "joy", "sadness", "surprise", "trust", 
               "negative", "positive")]

# remove brands, twts dataset
rm(brands)
rm(twts)

# add 'scaled_norm_like' column to prevent errors in Poisson-related models
data <- data %>%
  mutate(scaled_norm_fav = round(norm_fav, 3) *1000)

# add 'fav_thres' column to indicate the 75% threshold for favorite_counts
fav_75quantile = quantile(data$favorite_count, 0.75) # 75% = 4

data <- data %>%
  mutate(fav_thres = ifelse(favorite_count >= fav_75quantile, 1, 0))

# add 'norm_fav_thres' column to indicate the 75% threshold for normalized_like
norm_fav_75quantile <- quantile(data$norm_fav, 0.75) # 75% = 3.949447e-04

data <- data %>%
  mutate(norm_fav_thres = ifelse(norm_fav >= norm_fav_75quantile, 1, 0))

# add log, sqrt transformed word_count, syuzhet, norm_fav, scaled_norm_fav columns for convenience during test
data$sqrt_word_count <- sqrt(data$word_count)
data$sqrt_syuzhet <- sqrt(data$syuzhet + 4.5)
data$sqrt_favorite_count <- sqrt(data$favorite_count + 1)
data$sqrt_norm_fav <- sqrt(data$norm_fav + 1)
data$sqrt_scaled_norm_fav <- sqrt(data$scaled_norm_fav + 1)
data$log_word_count <- log(data$word_count + 1)
data$log_syuzhet <- log(data$syuzhet + 4.5)
data$log_favorite_count <- log(data$favorite_count + 1)
data$log_norm_fav <- log(data$norm_fav + 1)
data$log_scaled_norm_fav <- log(data$scaled_norm_fav + 1)

# add log, sqrt transformed word_count, syuzhet, norm_fav, scaled_norm_fav columns for convenience during test

data$sqrt_word_count <- sqrt(data$word_count)
data$sqrt_syuzhet <- sqrt(data$syuzhet + 4.5)
data$sqrt_favorite_count <- sqrt(data$favorite_count + 1)
data$sqrt_norm_fav <- sqrt(data$norm_fav + 1)
data$sqrt_scaled_norm_fav <- sqrt(data$scaled_norm_fav + 1)
data$log_word_count <- log(data$word_count + 1)
data$log_syuzhet <- log(data$syuzhet + 4.5)
data$log_favorite_count <- log(data$favorite_count + 1)
data$log_norm_fav <- log(data$norm_fav + 1)
data$log_scaled_norm_fav <- log(data$scaled_norm_fav + 1)

# check null values
sapply(data, function(x) sum(is.na(x)))

str(data)

# create 'recent_data' dataframe with the most recent data (2022 - 2023)
recent_data <- data %>%
  filter(created_at >= as.POSIXct("2022-01-01"))

# create 'data_nonzero' dataframe which excludes rows with favorite_count = 0 and favorite_count > 100,000 (outliers)
# (moved from Data Exploration section for the future convenience)
data_nonzero <- data[data$favorite_count != 0 & data$favorite_count < 100000, ]

# check new quantile
# quantile(data_nonzero$favorite_count) #    1     1     3    13    42899 
# quantile(data_nonzero$norm_fav) # 75%: 9.165903e-04

# edit data_nonzero's 'fav_thres' values
nzro_fav_75quantile <- quantile(data_nonzero$favorite_count, 0.75) # 75%: 13
data_nonzero <- data_nonzero %>%
  mutate(fav_thres <- ifelse(data_nonzero$favorite_count >= nzro_fav_75quantile, 1, 0))

# edit data_nonzero's 'norm_fav_thres'
nzro_norm_fav_75quantile <- quantile(data_nonzero$norm_fav, 0.75) # 75%: 9.165903e-04
data_nonzero <- data_nonzero %>%
  mutate(norm_fav_thres = ifelse(norm_fav >= nzro_norm_fav_75quantile, 1, 0))

# create 'recent_data_nonzero' dataframe with the most recent data (2022 - 2023)
recent_data_nonzero <- data_nonzero %>%
  filter(created_at >= as.POSIXct("2022-01-01"))

# === The code below is for extracting commonly used words related to business and the environment. === #
# === Please note that this is for reference only and does not need to be executed to continue with the analysis. ===== #

# # load package for analyzing texts
# library(tm)

# # select the text column
# text <- recent_data$text

# # transform to lower-case (text normalization)
# text <- tolower(text)

# # create a text corpus
# corpus <- Corpus(VectorSource(text))

# # clean the text
# corpus <- tm_map(corpus, removePunctuation)
# corpus <- tm_map(corpus, removeWords, stopwords("en"))

# # create a document-term matrix
# dtm <- TermDocumentMatrix(corpus) 
# m <- as.matrix(dtm) 
# v <- sort(rowSums(m),decreasing=TRUE)
# d <- data.frame(word = names(v), frequency=v)

# # export the frequent words as a csv file
# #write.csv(d, "frequent_words.csv")

# add 'biz_presence', 'eco_presence' columns to indicate the presence of top 10 biz/eco keywords
#ten keywords
# assign the keywords onto different groups
biz_words_10 <- c("new", "shop", "comfortable", "now", "collection", "online", "love", "review", "sale", "just") 

#eco_words <- c("vegan", "sustainable", "sustainability", "organic") 
eco_words_10 <- c("vegan", "sustainab", "organic", "cotton", "good", "nature", "eco", "earth", "change", "recycle")

# add two new binary variables: "biz_presence", "eco_presence"

pattern <- regex(paste(biz_words_10, collapse = "|"), ignore_case = TRUE)
# recent_data$biz_presence <- str_detect(recent_data$text, pattern) 
recent_data$biz_presence <- ifelse(str_detect(recent_data$text, pattern), 1, 0)

pattern <- regex(paste(eco_words_10, collapse = "|"), ignore_case = TRUE)
# recent_data$eco_presence <- str_detect(recent_data$text, pattern) 
recent_data$eco_presence <- ifelse(str_detect(recent_data$text, pattern), 1, 0)

# count the cases biz_word_presence == True
print(sum(recent_data$biz_presence == 1))

# count the cases biz_word_presence == True
print(sum(recent_data$eco_presence == 1))

# change variables to factors
recent_data$biz_presence <- factor(recent_data$biz_presence, levels = c(1, 0))
recent_data$eco_presence <- factor(recent_data$eco_presence, levels = c(1, 0))

str(recent_data)


# ===================== 2. EDA ========================== #

# ==== 2-1. EDA - Distribution ==== #

# == A. Original Data == #
# basic summary of y variable
summary(data$favorite_count)
cat("Mean of favorite_count:", mean(data$favorite_count), "\n")
cat("Variance of favorite_count:", var(data$favorite_count))

# check the histogram of the response variable 'favorite_count'
hist(data$favorite_count, 
     xlab = "Favorite count",
     ylab = "Frequency",
     main = "Histogram of 'favorite_count'")

# check the histogram of the response variable 'log_favorite_count'
hist(data$log_favorite_count, 
     xlab = "Log_favorite count",
     ylab = "Frequency",
     main = "Histogram of 'log_favorite_count'")

# check QQ-Plot
qqnorm(data$favorite_count); qqline(data$favorite_count)

# check QQ-Plot
qqnorm(data$log_favorite_count); qqline(data$log_favorite_count)

# proportion of 0 counts
# we can see that the majority of data falls on 0 count(45%) and it's extremely right skewed
quantiles <- quantile(data$favorite_count)
cat(sprintf("Proportion of '0' count: %.2f%%\n", 100 * nrow(data[data$favorite_count == 0, ])/nrow(data)))
cat("Quantiles:\n")
cat(sprintf("0%%: %.2f\n25%%: %.2f\n50%%: %.2f\n75%%: %.2f\n100%%: %.2f\n", 
            quantiles[1], quantiles[2], quantiles[3], quantiles[4], quantiles[5]))

# check if y variable follows Poisson distribution
# test results indicate y variable doesn't follow Poisson distribution
fav_fit <- goodfit(data$favorite_count, type="poisson")
summary(fav_fit)

# basic summary of word_count
summary(data$word_count)

# check the histogram of the predictor variable 'word_count'
# word count is slightly right skewed
hist(data$word_count,
     xlab = "Word count",
     ylab = "Frequency",
     main = "Histogram of 'word_count'")

# check QQ-Plot
qqnorm(data$word_count); qqline(data$word_count)

# basic summary of syuzhet 
summary(data$syuzhet)

# check the histogram of the predictor variable 'syuzhet'
# syuzhet is slightly right skewed
hist(data$syuzhet,
     xlab = "syuzhet",
     ylab = "Frequency",
     main = "Histogram of 'syuzhet'")

# check QQ-Plot
qqnorm(data$syuzhet); qqline(data$syuzhet)

# basic summary of visual_presence 
table(data$visual_presence)

# bar plot - visual_presence
ggplot(data, aes(visual_presence)) + 
  geom_bar() +
  labs(x = "visual_presence", y = "Frequency", title = "Bar plot of 'visual_presence'") +
  theme_minimal()

# === B. 'data_nonzero' === #

# basic summary of y variable
summary(data_nonzero$favorite_count)
cat("Mean of favorite_count:", mean(data_nonzero$favorite_count), "\n")
cat("Variance of favorite_count:", var(data_nonzero$favorite_count))

# check the histogram of the response variable 'favorite_count'
hist(data_nonzero$favorite_count, 
     xlab = "Favorite count",
     ylab = "Frequency",
     main = "Histogram of 'favorite_count'")

# check the histogram of the response variable 'log_favorite_count'
hist(data_nonzero$log_favorite_count, 
     xlab = "Log_favorite count",
     ylab = "Frequency",
     main = "Histogram of 'log_favorite_count'")

# check QQ-Plot
qqnorm(data_nonzero$favorite_count); qqline(data_nonzero$favorite_count)

# check QQ-Plot
qqnorm(data_nonzero$log_favorite_count); qqline(data_nonzero$log_favorite_count) 

# check if y variable follows Poisson distribution
# test results indicate y variable doesn't follow Poisson distribution
fav_fit1 <- goodfit(data_nonzero$favorite_count, type="poisson")
summary(fav_fit1)

# basic summary of word_count
summary(data_nonzero$word_count)

# check the histogram of the predictor variable 'word_count'
# word count is slightly right skewed
hist(data_nonzero$word_count,
     xlab = "Word count",
     ylab = "Frequency",
     main = "Histogram of 'word_count'")

# check QQ-Plot
qqnorm(data_nonzero$word_count); qqline(data_nonzero$word_count)

# basic summary of syuzhet 
summary(data_nonzero$syuzhet)

# check the histogram of the predictor variable 'syuzhet'
# syuzhet is slightly right skewed
hist(data_nonzero$syuzhet,
     xlab = "syuzhet",
     ylab = "Frequency",
     main = "Histogram of 'syuzhet'")

# check QQ-Plot
qqnorm(data_nonzero$syuzhet); qqline(data_nonzero$syuzhet)

# basic summary of visual_presence 
table(data_nonzero$visual_presence)

# bar plot - visual_presence
ggplot(data_nonzero, aes(visual_presence)) + 
  geom_bar() +
  labs(x = "visual_presence", y = "Frequency", title = "Bar plot of 'visual_presence'") +
  theme_minimal()

# === C. 'recent_data' === #

# basic summary of y variable
summary(recent_data$favorite_count)
cat("Mean of favorite_count:", mean(recent_data$favorite_count), "\n")
cat("Variance of favorite_count:", var(recent_data$favorite_count))

# check the histogram of the response variable 'favorite_count'
hist(recent_data$favorite_count, 
     xlab = "Favorite count",
     ylab = "Frequency",
     main = "Histogram of 'favorite_count'")

# check the histogram of the response variable 'log_favorite_count'
hist(recent_data$log_favorite_count, 
     xlab = "Log_favorite count",
     ylab = "Frequency",
     main = "Histogram of 'log_favorite_count'")

# check QQ-Plot
qqnorm(recent_data$favorite_count); qqline(recent_data$favorite_count)

# check QQ-Plot
qqnorm(recent_data$log_favorite_count); qqline(recent_data$log_favorite_count)

# check if y variable follows Poisson distribution
# test results indicate y variable doesn't follow Poisson distribution
fav_fit2 <- goodfit(recent_data$favorite_count, type="poisson")
summary(fav_fit2)

# basic summary of y variable
summary(recent_data$scaled_norm_fav)
cat("Mean of scaled_norm_fav:", mean(recent_data$scaled_norm_fav), "\n")
cat("Variance of scaled_norm_fav:", var(recent_data$scaled_norm_fav))

# check the histogram of the response variable 'scaled_norm_fav'
hist(recent_data$scaled_norm_fav, 
     xlab = "Scaled_norm_fav count",
     ylab = "Frequency",
     main = "Histogram of 'scaled_norm_fav'")

# check the histogram of the response variable 'log_scaled_norm_fav'
hist(recent_data$log_scaled_norm_fav, 
     xlab = "Log_scaled_norm_fav",
     ylab = "Frequency",
     main = "Histogram of 'log_scaled_norm_fav'")

# check QQ-Plot
qqnorm(recent_data$scaled_norm_fav); qqline(recent_data$scaled_norm_fav)

# check QQ-Plot
qqnorm(recent_data$log_scaled_norm_fav); qqline(recent_data$log_scaled_norm_fav)

# basic summary of word_count
summary(recent_data$word_count)

# check the histogram of the predictor variable 'word_count'
# word count is slightly right skewed
hist(recent_data$word_count,
     xlab = "Word count",
     ylab = "Frequency",
     main = "Histogram of 'word_count'")

# check QQ-Plot
qqnorm(recent_data$word_count); qqline(recent_data$word_count)

# basic summary of syuzhet 
summary(recent_data$syuzhet)

# check the histogram of the predictor variable 'syuzhet'
# syuzhet is slightly right skewed
hist(recent_data$syuzhet,
     xlab = "syuzhet",
     ylab = "Frequency",
     main = "Histogram of 'syuzhet'")

# check QQ-Plot
qqnorm(recent_data$syuzhet); qqline(recent_data$syuzhet)

# basic summary of visual_presence 
table(recent_data$visual_presence)

# bar plot - visual_presence
ggplot(recent_data, aes(visual_presence)) + 
  geom_bar() +
  labs(x = "visual_presence", y = "Frequency", title = "Bar plot of 'visual_presence'") +
  theme_minimal()

# basic summary of biz_presence 
table(recent_data$biz_presence)
summary(recent_data$favorite_count[recent_data$biz_presence==1])
summary(recent_data$favorite_count[recent_data$biz_presence==0])

# bar plot - biz_presence
ggplot(recent_data, aes(biz_presence)) + 
  geom_bar() +
  labs(x = "biz_presence", y = "Frequency", title = "Bar plot of 'biz_presence'") +
  theme_minimal()

# basic summary of eco_presence 
table(recent_data$eco_presence)
summary(recent_data$favorite_count[recent_data$eco_presence==1])
summary(recent_data$favorite_count[recent_data$eco_presence==0])

# bar plot - eco_presence
ggplot(recent_data, aes(eco_presence)) + 
  geom_bar() +
  labs(x = "eco_presence", y = "Frequency", title = "Bar plot of 'eco_presence'") +
  theme_minimal()

# ==== 2-2. EDA - Linearity ==== #

# == A. original data == #
# scatter plot: word_count vs favorite_count
ggplot(data, aes(x=word_count, y=favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Word count", y = "Favorite count", title = "Scatter plot of 'word_count' & 'favorite_count'")

# scatter plot: syuzhet vs favorite_count
ggplot(data, aes(x=syuzhet, y=favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Syuzhet (sentiment score)", y = "Favorite count", title = "Scatter plot of 'syuzhet' & 'favorite_count'")

# scatter plot: log_word_count vs log_favorite_count
ggplot(data, aes(x=log_word_count, y=log_favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Log_word count", y = "Log_favorite count", title = "Scatter plot of 'log_word_count' & 'log_favorite_count'")

# scatter plot: log_syuzhet vs log_favorite_count
ggplot(data, aes(x=log_syuzhet, y=log_favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Log_syuzhet (sentiment score)", y = "Log_favorite count", title = "Scatter plot of 'log_syuzhet' & 'log_favorite_count'")

# Boxplot: visual_presence vs favorite_count
ggplot(data, aes(x=visual_presence, y=favorite_count)) + 
  geom_boxplot(aes(group = visual_presence)) + 
  labs(x = "Visual Presence", y = "Favorite count", title = "Boxplot of 'visual_presence' & 'favorite_count'")

# Boxplot: visual_presence vs log_favorite_count
ggplot(data, aes(x=visual_presence, y=log_favorite_count)) + 
  geom_boxplot(aes(group = visual_presence)) + 
  labs(x = "Visual Presence", y = "Log_favorite count", title = "Boxplot of 'visual_presence' & 'log_favorite_count'")

# == B. 'data_nonzero' == #
# scatter plot: word_count vs favorite_count
ggplot(data_nonzero, aes(x=word_count, y=favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Word count", y = "Favorite count", title = "Scatter plot of 'word_count' & 'favorite_count'")

# scatter plot: syuzhet vs favorite_count
ggplot(data_nonzero, aes(x=syuzhet, y=favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Syuzhet (sentiment score)", y = "Favorite count", title = "Scatter plot of 'syuzhet' & 'favorite_count'")

# scatter plot: log_word_count vs log_favorite_count
ggplot(data_nonzero, aes(x=log_word_count, y=log_favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Log_word count", y = "Log_favorite count", title = "Scatter plot of 'log_word_count' & 'log_favorite_count'")

# scatter plot: log_syuzhet vs log_favorite_count
ggplot(data_nonzero, aes(x=log_syuzhet, y=log_favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Log_syuzhet (sentiment score)", y = "Log_favorite count", title = "Scatter plot of 'log_syuzhet' & 'log_favorite_count'")

# Boxplot: visual_presence vs favorite_count
ggplot(data_nonzero, aes(x=visual_presence, y=favorite_count)) + 
  geom_boxplot(aes(group = visual_presence)) + 
  labs(x = "Visual Presence", y = "Favorite count", title = "Boxplot of 'visual_presence' & 'favorite_count'")

# Boxplot: visual_presence vs log_favorite_count
ggplot(data_nonzero, aes(x=visual_presence, y=log_favorite_count)) + 
  geom_boxplot(aes(group = visual_presence)) + 
  labs(x = "Visual Presence", y = "Log_favorite count", title = "Boxplot of 'visual_presence' & 'log_favorite_count'")

# == C. 'recent_data' == #
# scatter plot: word_count vs favorite_count
ggplot(recent_data, aes(x=word_count, y=favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Word count", y = "Favorite count", title = "Scatter plot of 'word_count' & 'favorite_count'")

# scatter plot: syuzhet vs favorite_count
ggplot(recent_data, aes(x=syuzhet, y=favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Syuzhet (sentiment score)", y = "Favorite count", title = "Scatter plot of 'syuzhet' & 'favorite_count'")

# scatter plot: log_word_count vs log_favorite_count
ggplot(recent_data, aes(x=log_word_count, y=log_favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Log_word count", y = "Log_favorite count", title = "Scatter plot of 'log_word_count' & 'log_favorite_count'")

# scatter plot: log_syuzhet vs log_favorite_count
ggplot(recent_data, aes(x=log_syuzhet, y=log_favorite_count)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Log_syuzhet (sentiment score)", y = "Log_favorite count", title = "Scatter plot of 'log_syuzhet' & 'log_favorite_count'")

# scatter plot: word_count vs scaled_norm_fav
ggplot(recent_data, aes(x=word_count, y=scaled_norm_fav)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Word count", y = "Scaled_norm_fav", title = "Scatter plot of 'word_count' & 'scaled_norm_fav'")

# scatter plot: syuzhet vs scaled_norm_fav
ggplot(recent_data, aes(x=syuzhet, y=scaled_norm_fav)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Syuzhet (sentiment score)", y = "Scaled_norm_fav", title = "Scatter plot of 'syuzhet' & 'scaled_norm_fav'")

# scatter plot: log_word_count vs log_scaled_norm_fav
ggplot(recent_data, aes(x=log_word_count, y=log_scaled_norm_fav)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE,) +
  labs(x = "Log_word count", y = "Log_scaled_norm_fav", title = "Scatter plot of 'log_word_count' & 'log_scaled_norm_fav'")

# scatter plot: log_syuzhet vs log_scaled_norm_fav
ggplot(recent_data, aes(x=log_syuzhet, y=log_scaled_norm_fav)) + geom_point() + 
  scale_colour_hue(l=50) + 
  geom_smooth(method=lm, formula = y ~ x, se=FALSE, fullrange=TRUE) +
  labs(x = "Log_syuzhet (sentiment score)", y = "Log_scaled_norm_fav count", title = "Scatter plot of 'log_syuzhet' & 'Log_scaled_norm_fav'")

# Boxplot: visual_presence vs favorite_count
ggplot(recent_data, aes(x=visual_presence, y=favorite_count)) + 
  geom_boxplot(aes(group = visual_presence)) + 
  labs(x = "Visual Presence", y = "Favorite count", title = "Boxplot of 'visual_presence' & 'favorite_count'")

# Boxplot: visual_presence vs log_favorite_count
ggplot(recent_data, aes(x=visual_presence, y=log_favorite_count)) + 
  geom_boxplot(aes(group = visual_presence)) + 
  labs(x = "Visual Presence", y = "Log_favorite count", title = "Boxplot of 'visual_presence' & 'log_favorite_count'")

# Boxplot: biz_presence vs favorite_count
ggplot(recent_data, aes(x=biz_presence, y=favorite_count)) + 
  geom_boxplot(aes(group = biz_presence)) + 
  labs(x = "Biz_presence", y = "Favorite count", title = "Boxplot of 'biz_presence' & 'favorite_count'")

# Boxplot: biz_presence vs log_favorite_count
ggplot(recent_data, aes(x=biz_presence, y=log_favorite_count)) + 
  geom_boxplot(aes(group = biz_presence)) + 
  labs(x = "Biz_presence", y = "Log_favorite count", title = "Boxplot of 'biz_presence' & 'log_favorite_count'")

# Boxplot: eco_presence vs favorite_count
ggplot(recent_data, aes(x=eco_presence, y=favorite_count)) + 
  geom_boxplot(aes(group = eco_presence)) + 
  labs(x = "Eco_presence", y = "Favorite count", title = "Boxplot of 'eco_presence' & 'favorite_count'")

# Boxplot: eco_presence vs log_favorite_count
ggplot(recent_data, aes(x=eco_presence, y=log_favorite_count)) + 
  geom_boxplot(aes(group = eco_presence)) + 
  labs(x = "Eco_presence", y = "Log_favorite count", title = "Boxplot of 'eco_presence' & 'log_favorite_count'")


# ==== 2-3. Outlier ==== #

boxplot(data$favorite_count)
boxplot(data$word_count)

# predictor variables seem to be fine, but as y variable showed extreme skewed distribution, we can perhaps exclude possible outliers and re-check it
# there are 8 data points with extreme y value over 100,000  
sum(data$favorite_count >= 100000)

# check the histogram of the response variable 'log_favorite_count' w/o outliers
hist(data$log_favorite_count[data$favorite_count < 100000], 
     xlab = "Log_favorite count",
     ylab = "Frequency",
     main = "Histogram of 'log_favorite_count'")

# check the histogram of the response variable 'log_favorite_count'
hist(data$log_favorite_count, 
     xlab = "Log_favorite count",
     ylab = "Frequency",
     main = "Histogram of 'log_favorite_count'")

# ==== 2-4. Correlation ==== #

# correlation matrix - original data
cor_matrix <- cor(data[, c("favorite_count", "followers_count", "word_count", "syuzhet")])

ggcorrplot(cor_matrix, hc.order = TRUE, lab = TRUE, ggtheme = ggplot2::theme_gray, 
           colors = c("#6D9EC1", "white", "#E46726"), method = "circle")

# correlation matrix - data_nonzero 
cor_matrix_2 <- cor(data_nonzero[,c("favorite_count", "followers_count", "word_count", "syuzhet")])
ggcorrplot(cor_matrix_2, lab = TRUE, ggtheme = ggplot2::theme_gray, 
           colors = c("#6D9EC1", "white", "#E46726"), method = "circle")

# correlation matrix - recent_data 
cor_matrix_3 <- cor(recent_data[,c("favorite_count", "followers_count", "word_count", "syuzhet")])
ggcorrplot(cor_matrix_3, lab = TRUE, ggtheme = ggplot2::theme_gray, 
           colors = c("#6D9EC1", "white", "#E46726"), method = "circle")


# ===================== 3. Preliminary Modeling ========================== #

#### Note: This section contains code written for the progress report. 
#### Feel free to skip ahead to Section 4. Final Modeling.

# ==== 3-1. Linear Regression ==== #
# function for linear regression model performance comparison

lm_eval <- function(models) {
  mse_values <- sapply(models, function(mod) mean(resid(mod)^2))
  r2_values <- sapply(models, function(mod) summary(mod)$r.squared)
  adjusted_r2_values <- sapply(models, function(mod) summary(mod)$adj.r.squared)
  aic_values <- sapply(models, AIC)
  
  model_stats <- data.frame(
    MSE = mse_values,
    R_Squared = r2_values,
    Adjusted_R_Squared = adjusted_r2_values,
    AIC = aic_values
  )
  return(model_stats)
}

# ========= linear regression models w/ 'favorite_count' as Y variable ========= #

# model 1. linear-linear | Y: favorite_count |  R^2 0.0002564
lm_mod1 <- lm(favorite_count ~ word_count + syuzhet + visual_presence, data = data)

# model 2. log-linear | Y: favorite_count | R^2 0.185
lm_mod2 <- lm(log(favorite_count + 1) ~ word_count + syuzhet + visual_presence, data = data)

# model 3. linear-log | Y: favorite_count | R^2 0.000355
lm_mod3 <- lm(favorite_count ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data)

# model 4. log-log | Y: favorite_count | R^2 0.1853
lm_mod4 <- lm(log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data)

# model 5. log-log | Y: favorite_count | without 'visual_presence' | R^2 0.02126
lm_mod5 <- lm(log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5), data=data)

# model 6. linear-sqrt | Y: favorite_count | R^2 0.0003001
lm_mod6 <- lm(favorite_count ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data)

# model 7. log-sqrt | Y: favorite_count | R^2 0.1851
lm_mod7 <- lm(log(favorite_count+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data)


# ========= w/ filtered dataset 'data_nonzero' ========= #

# model 8. linear-linear | Y: favorite_count | 
lm_mod8 <- lm(favorite_count ~ word_count + syuzhet + visual_presence, data = data_nonzero)

# model 9. log-linear | Y: favorite_count | 
lm_mod9 <- lm(log(favorite_count + 1) ~ word_count + syuzhet + visual_presence, data = data_nonzero)

# model 10. linear-log | Y: favorite_count | 
lm_mod10 <- lm(favorite_count ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data_nonzero)

# model 11. log-log | Y: favorite_count |
lm_mod11 <- lm(log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data_nonzero)

# model 12. log-log | Y: favorite_count | without 'visual_presence' | 
lm_mod12 <- lm(formula= log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5), data=data_nonzero)

# model 13. linear-sqrt | Y: favorite_count | 
lm_mod13 <- lm(favorite_count ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data_nonzero)

# model 14. log-sqrt | Y: favorite_count | 
lm_mod14 <- lm(log(favorite_count+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data_nonzero)

lm_model_names <- paste("lm_mod", 1:14, sep="")
lm_models <- mget(lm_model_names)
lm_eval(lm_models)

# ========= linear regression models w/ 'norm_fav' as Y variable ========= #

# model 1. linear-linear | Y: norm_fav |
normfav_lm_mod1 <- lm(norm_fav ~ word_count + syuzhet + visual_presence, data = data)

# model 2. log-linear | Y: norm_fav |
normfav_lm_mod2 <- lm(log(norm_fav + 1) ~ word_count + syuzhet + visual_presence, data = data)

# model 3. linear-log | Y: norm_fav |
normfav_lm_mod3 <- lm(norm_fav ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data)

# model 4. log-log | Y: norm_fav |
normfav_lm_mod4 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data)

# model 5. log-log | Y: norm_fav | without 'visual_presence' |
normfav_lm_mod5 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5), data=data)

# model 6. linear-sqrt | Y: norm_fav |
normfav_lm_mod6 <- lm(norm_fav ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data)

# model 7. log-sqrt | Y: norm_fav |
normfav_lm_mod7 <- lm(log(norm_fav+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data)

# ========= w/ filtered dataset 'data_nonzero' ========= #

# model 8. linear-linear | Y: norm_fav | 
normfav_lm_mod8 <- lm(norm_fav ~ word_count + syuzhet + visual_presence, data = data_nonzero)

# model 9. log-linear | Y: norm_fav | 
normfav_lm_mod9 <- lm(log(norm_fav + 1) ~ word_count + syuzhet + visual_presence, data = data_nonzero)

# model 10. linear-log | Y: norm_fav | 
normfav_lm_mod10 <- lm(norm_fav ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data_nonzero)

# model 11. log-log | Y: norm_fav |
normfav_lm_mod11 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = data_nonzero)

# model 12. log-log | Y: norm_fav | without 'visual_presence' | 
normfav_lm_mod12 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5), data=data_nonzero)

# model 13. linear-sqrt | Y: norm_fav | 
normfav_lm_mod13 <- lm(norm_fav ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data_nonzero)

# model 14. log-sqrt | Y: norm_fav | 
normfav_lm_mod14 <- lm(log(norm_fav+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = data_nonzero)

# performance comparison

normfav_lm_model_names <- paste("normfav_lm_mod", 1:14, sep="")
normfav_lm_models <- mget(normfav_lm_model_names)
lm_eval(normfav_lm_models)

quantile(data_nonzero$norm_fav)

summary(lm_mod4)

lm_mod15 <- lm(log(favorite_count+1) ~ log(word_count+1) + visual_presence, data = data)
summary(lm_mod15)

# lm_mod4 residuals vs fitted plot
plot(lm_mod4$fitted.values, resid(lm_mod4),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# lm_mod4 Q-Q plot
qqnorm(resid(lm_mod4))
qqline(resid(lm_mod4), col = "red")

# lm_mod15 residuals vs fitted plot
plot(lm_mod15$fitted.values, resid(lm_mod15),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# lm_mod15 Q-Q plot
qqnorm(resid(lm_mod15))
qqline(resid(lm_mod15), col = "red")

# ========= w/ recent data

# ========= linear regression models w/ 'favorite_count' as Y variable ========= #

# model 1. linear-linear | Y: favorite_count | 
recent_lm_mod1 <- lm(favorite_count ~ word_count + syuzhet + visual_presence, data = recent_data)

# model 2. log-linear | Y: favorite_count |
recent_lm_mod2 <- lm(log(favorite_count + 1) ~ word_count + syuzhet + visual_presence, data = recent_data)

# model 3. linear-log | Y: favorite_count |
recent_lm_mod3 <- lm(favorite_count ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data)

# model 4. log-log | Y: favorite_count |
recent_lm_mod4 <- lm(log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data)

# model 5. log-log | Y: favorite_count | without 'visual_presence' |
recent_lm_mod5 <- lm(log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5), data=data)

# model 6. linear-sqrt | Y: favorite_count |
recent_lm_mod6 <- lm(favorite_count ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data)

# model 7. log-sqrt | Y: favorite_count |
recent_lm_mod7 <- lm(log(favorite_count+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data)


# ========= w/ filtered dataset 'data_nonzero' ========= #

# model 8. linear-linear | Y: favorite_count | 
recent_lm_mod8 <- lm(favorite_count ~ word_count + syuzhet + visual_presence, data = recent_data_nonzero)

# model 9. log-linear | Y: favorite_count | 
recent_lm_mod9 <- lm(log(favorite_count + 1) ~ word_count + syuzhet + visual_presence, data = recent_data_nonzero)

# model 10. linear-log | Y: favorite_count | 
recent_lm_mod10 <- lm(favorite_count ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 11. log-log | Y: favorite_count |
recent_lm_mod11 <- lm(log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 12. log-log | Y: favorite_count | without 'visual_presence' | 
recent_lm_mod12 <- lm(formula= log(favorite_count+1) ~ log(word_count+1) + log(syuzhet+4.5), data=recent_data_nonzero)

# model 13. linear-sqrt | Y: favorite_count | 
recent_lm_mod13 <- lm(favorite_count ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 14. log-sqrt | Y: favorite_count | 
recent_lm_mod14 <- lm(log(favorite_count+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# performance comparison

recent_lm_model_names <- paste("recent_lm_mod", 1:14, sep="")
recent_lm_models <- mget(recent_lm_model_names)
lm_eval(recent_lm_models)

summary(recent_lm_mod4)

# recent_lm_mod4 residuals vs fitted plot
plot(recent_lm_mod4$fitted.values, resid(recent_lm_mod4),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# recent_lm_mod4 Q-Q plot
qqnorm(resid(recent_lm_mod4))
qqline(resid(recent_lm_mod4), col = "red")


# ========= w/ recent data

# ========= linear regression models w/ 'norm_fav' as Y variable ========= #

# model 1. linear-linear | Y: norm_fav |
recent_normfav_lm_mod1 <- lm(norm_fav ~ word_count + syuzhet + visual_presence, data = recent_data)

# model 2. log-linear | Y: norm_fav |
recent_normfav_lm_mod2 <- lm(log(norm_fav + 1) ~ word_count + syuzhet + visual_presence, data = recent_data)

# model 3. linear-log | Y: norm_fav |
recent_normfav_lm_mod3 <- lm(norm_fav ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data)

# model 4. log-log | Y: norm_fav |
recent_normfav_lm_mod4 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data)

# model 5. log-log | Y: norm_fav | without 'visual_presence' |
recent_normfav_lm_mod5 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5), data=recent_data)

# model 6. linear-sqrt | Y: norm_fav |
recent_normfav_lm_mod6 <- lm(norm_fav ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data)

# model 7. log-sqrt | Y: norm_fav |
recent_normfav_lm_mod7 <- lm(log(norm_fav+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data)

# ========= w/ filtered dataset 'data_nonzero' ========= #

# model 8. linear-linear | Y: norm_fav | 
recent_normfav_lm_mod8 <- lm(norm_fav ~ word_count + syuzhet + visual_presence, data = recent_data_nonzero)

# model 9. log-linear | Y: norm_fav | 
recent_normfav_lm_mod9 <- lm(log(norm_fav + 1) ~ word_count + syuzhet + visual_presence, data = recent_data_nonzero)

# model 10. linear-log | Y: norm_fav | 
recent_normfav_lm_mod10 <- lm(norm_fav ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 11. log-log | Y: norm_fav |
recent_normfav_lm_mod11 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 12. log-log | Y: norm_fav | without 'visual_presence' | 
recent_normfav_lm_mod12 <- lm(log(norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5), data=recent_data_nonzero)

# model 13. linear-sqrt | Y: norm_fav | 
recent_normfav_lm_mod13 <- lm(norm_fav ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 14. log-sqrt | Y: norm_fav | 
recent_normfav_lm_mod14 <- lm(log(norm_fav+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# performance comparison

recent_normfav_lm_model_names <- paste("recent_normfav_lm_mod", 1:14, sep="")
recent_normfav_lm_models <- mget(recent_normfav_lm_model_names)
lm_eval(recent_normfav_lm_models)

# ========= w/ recent data

# ========= linear regression models w/ 'scaled_norm_fav' as Y variable ========= #

# model 1. linear-linear | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod1 <- lm(scaled_norm_fav ~ word_count + syuzhet + visual_presence, data = recent_data)

# model 2. log-linear | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod2 <- lm(log(scaled_norm_fav + 1) ~ word_count + syuzhet + visual_presence, data = recent_data)

# model 3. linear-log | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod3 <- lm(scaled_norm_fav ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data)

# model 4. log-log | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod4 <- lm(log(scaled_norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data)

# model 5. log-log | Y: scaled_norm_fav | without 'visual_presence' |
recent_scaled_normfav_lm_mod5 <- lm(log(scaled_norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5), data=recent_data)

# model 6. linear-sqrt | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod6 <- lm(scaled_norm_fav ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data)

# model 7. log-sqrt | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod7 <- lm(log(scaled_norm_fav+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data)

# ========= w/ filtered dataset 'data_nonzero' ========= #

# model 8. linear-linear | Y: scaled_norm_fav | 
recent_scaled_normfav_lm_mod8 <- lm(scaled_norm_fav ~ word_count + syuzhet + visual_presence, data = recent_data_nonzero)

# model 9. log-linear | Y: scaled_norm_fav | 
recent_scaled_normfav_lm_mod9 <- lm(log(scaled_norm_fav + 1) ~ word_count + syuzhet + visual_presence, data = recent_data_nonzero)

# model 10. linear-log | Y: scaled_norm_fav | 
recent_scaled_normfav_lm_mod10 <- lm(scaled_norm_fav ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 11. log-log | Y: scaled_norm_fav |
recent_scaled_normfav_lm_mod11 <- lm(log(scaled_norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 12. log-log | Y: scaled_norm_fav | without 'visual_presence' | 
recent_scaled_normfav_lm_mod12 <- lm(log(scaled_norm_fav+1) ~ log(word_count+1) + log(syuzhet+4.5), data=recent_data_nonzero)

# model 13. linear-sqrt | Y: scaled_norm_fav | 
recent_scaled_normfav_lm_mod13 <- lm(scaled_norm_fav ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# model 14. log-sqrt | Y: scaled_norm_fav | 
recent_scaled_normfav_lm_mod14 <- lm(log(scaled_norm_fav+1) ~ sqrt(word_count+1) + sqrt(syuzhet+4.5) + visual_presence, data = recent_data_nonzero)

# performance comparison
recent_scaled_normfav_lm_model_names <- paste("recent_scaled_normfav_lm_mod", 1:14, sep="")
recent_scaled_normfav_lm_models <- mget(recent_scaled_normfav_lm_model_names)
lm_eval(recent_scaled_normfav_lm_models)

# function for logistic regression model performance comparison

glm_eval <- function(models) {
  aic_values <- sapply(models, function(mod) summary(mod)$aic)
  residual_dev_values <- sapply(models, function(mod) summary(mod)$deviance)
  
  model_stats <- data.frame(
    AIC = aic_values,
    Residual_Deviance = residual_dev_values
  )
  return(model_stats)
}

# =========  y: fav_thres | dataset: data  ========= #

# model 1. X: sqrt(word_count)
glm_mod1 <- glm(fav_thres~sqrt(word_count), data=data, family = "binomial")

# model 2. X: sqrt(syuzhet)
glm_mod2 <- glm(fav_thres~sqrt(syuzhet+4.5), data=data, family = "binomial")

# model 3. X: sqrt(word_count), sqrt(syuzhet)
glm_mod3 <- glm(fav_thres~sqrt(word_count)+sqrt(syuzhet+4.5), data=data, family = "binomial")

# model 4. X: sqrt(word_count), sqrt(syuzhet), visual_presence
glm_mod4 <- glm(fav_thres~sqrt(word_count) + sqrt(syuzhet+4.5) + visual_presence, data=data, family = "binomial")

# model 5. X: word_count, syuzhet, visual_presence 
glm_mod5 <- glm(fav_thres~word_count + syuzhet + visual_presence, data=data, family = "binomial")

# model 6. X: word_count, visual_presence 
glm_mod6 <- glm(fav_thres~word_count + visual_presence, data=data, family = "binomial")

# model 7. X: log(word_count+1), log(syuzhet+4.5), visual_presence 
glm_mod7 <- glm(fav_thres~log(word_count+1) + log(syuzhet+4.5) + visual_presence, data=data, family = "binomial")

# model 8. X: log(word_count+1), visual_presence 
glm_mod8 <- glm(fav_thres~log(word_count+1) + visual_presence, data=data, family = "binomial")

# evaluate
glm_model_names <- paste("glm_mod", 1:7, sep="")
glm_models <- mget(glm_model_names)
glm_eval(glm_models)

# model 7 plot
ggplot(data, aes(x=log(word_count+1), y=fav_thres)) + 
  geom_point(aes(size=log(syuzhet+4.5), color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  scale_size_continuous(name="log(syuzhet+4.5)") + 
  theme_minimal() 

# model 8 plot
ggplot(data, aes(x=log(word_count+1), y=fav_thres)) + 
  geom_point(aes(color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  theme_minimal()

# check multicollinearity
print(vif(glm_mod7))
print(vif(glm_mod8)) 
# we do not seem to have an issue with mulicollinearity in independent variables

# =========  y: norm_fav_thres | dataset: data  ========= #

# model 1. X: sqrt(word_count) 
glm_norm_mod1 <- glm(norm_fav_thres~sqrt(word_count), data=data, family = "binomial")

# model 2. X: sqrt(syuzhet) 
glm_norm_mod2 <- glm(norm_fav_thres~sqrt(syuzhet+4.5), data=data, family = "binomial")

# model 3. X: sqrt(word_count), sqrt(syuzhet)
glm_norm_mod3 <- glm(norm_fav_thres~sqrt(word_count)+sqrt(syuzhet+4.5), data=data, family = "binomial")

# model 4. X: sqrt(word_count), sqrt(syuzhet), visual_presence
glm_norm_mod4 <- glm(norm_fav_thres~sqrt(word_count) + sqrt(syuzhet+4.5) + visual_presence, data=data, family = "binomial")

# model 5. X: word_count, syuzhet, visual_presence
glm_norm_mod5 <- glm(norm_fav_thres~word_count + syuzhet + visual_presence, data=data, family = "binomial")

# model 6. X: word_count, visual_presence 
glm_norm_mod6 <- glm(norm_fav_thres~word_count + visual_presence, data=data, family = "binomial")

# model 7. X: log(word_count+1), log(syuzhet+4.5), visual_presence
glm_norm_mod7 <- glm(norm_fav_thres~log(word_count+1) + log(syuzhet+4.5) + visual_presence, data=data, family = "binomial")

# model 8. X: log(word_count+1), visual_presence 
glm_norm_mod8 <- glm(norm_fav_thres~log(word_count+1) + visual_presence, data=data, family = "binomial")

glm_norm_model_names <- paste("glm_norm_mod", 1:8, sep="")
glm_norm_models <- mget(glm_norm_model_names)
glm_eval(glm_norm_models)

# model 7 plot
ggplot(data, aes(x=log(word_count+1), y=norm_fav_thres)) + 
  geom_point(aes(size=log(syuzhet+4.5), color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  scale_size_continuous(name="log(syuzhet+4.5)") + 
  theme_minimal() 

# model 8 plot
ggplot(data, aes(x=log(word_count+1), y=norm_fav_thres)) + 
  geom_point(aes(color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  theme_minimal()

# =========  y: fav_thres | dataset: data_nonzeero ========= #

# model 1. X: sqrt(word_count) 
glm_nzro_mod1 <- glm(fav_thres~sqrt(word_count), data=data_nonzero, family = "binomial")

# model 2. X: sqrt(syuzhet) 
glm_nzro_mod2 <- glm(fav_thres~sqrt(syuzhet+4.5), data=data_nonzero, family = "binomial")

# model 3. X: sqrt(word_count), sqrt(syuzhet)
glm_nzro_mod3 <- glm(fav_thres~sqrt(word_count)+sqrt(syuzhet+4.5), data=data_nonzero, family = "binomial")

# model 4. X: sqrt(word_count), sqrt(syuzhet), visual_presence 
glm_nzro_mod4 <- glm(fav_thres~sqrt(word_count) + sqrt(syuzhet+4.5) + visual_presence, data=data_nonzero, family = "binomial")

# model 5. X: word_count, syuzhet, visual_presence 
glm_nzro_mod5 <- glm(fav_thres~word_count + syuzhet + visual_presence, data=data_nonzero, family = "binomial")

# model 6. X: word_count, visual_presence 
glm_nzro_mod6 <- glm(fav_thres~word_count + visual_presence, data=data_nonzero, family = "binomial")

# model 7. X: log(word_count+1), log(syuzhet+4.5), visual_presence 
glm_nzro_mod7 <- glm(fav_thres~log(word_count+1) + log(syuzhet+4.5) + visual_presence, data=data_nonzero, family = "binomial")

# model 8. X: log(word_count+1), visual_presence 
glm_nzro_mod8 <- glm(fav_thres~log(word_count+1) + visual_presence, data=data_nonzero, family = "binomial")

glm_nonzero_model_names <- paste("glm_nzro_mod", 1:8, sep="")
glm_nonzero_models <- mget(glm_nonzero_model_names)
glm_eval(glm_nonzero_models)

# model 7 plot
ggplot(data_nonzero, aes(x=log(word_count+1), y=fav_thres)) + 
  geom_point(aes(size=log(syuzhet+4.5), color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  scale_size_continuous(name="log(syuzhet+4.5)") + 
  theme_minimal() 

# model 8 plot
ggplot(data_nonzero, aes(x=log(word_count+1), y=fav_thres)) + 
  geom_point(aes(color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  theme_minimal()

# =========  y: norm_fav_thres | dataset: data_nonzero========= #

# model 1. X: sqrt(word_count) 
glm_nzro_norm_mod1 <- glm(norm_fav_thres~sqrt(word_count), data=data_nonzero, family = "binomial")

# model 2. X: sqrt(syuzhet)
glm_nzro_norm_mod2 <- glm(norm_fav_thres~sqrt(syuzhet+4.5), data=data_nonzero, family = "binomial")

# model 3. X: sqrt(word_count), sqrt(syuzhet) 
glm_nzro_norm_mod3 <- glm(norm_fav_thres~sqrt(word_count)+sqrt(syuzhet+4.5), data=data_nonzero, family = "binomial")

# model 4. X: sqrt(word_count), sqrt(syuzhet), visual_presence 
glm_nzro_norm_mod4 <- glm(norm_fav_thres~sqrt(word_count) + sqrt(syuzhet+4.5) + visual_presence, data=data_nonzero, family = "binomial")

# model 5. X: word_count, syuzhet, visual_presence
glm_nzro_norm_mod5 <- glm(norm_fav_thres~word_count + syuzhet + visual_presence, data=data_nonzero, family = "binomial")

# model 6. X: word_count, visual_presence
glm_nzro_norm_mod6 <- glm(norm_fav_thres~word_count + visual_presence, data=data_nonzero, family = "binomial")

# model 7. X: log(word_count+1), log(syuzhet+4.5), visual_presence
glm_nzro_norm_mod7 <- glm(norm_fav_thres~log(word_count+1) + log(syuzhet+4.5) + visual_presence, data=data_nonzero, family = "binomial")

# model 8. X: log(word_count+1), visual_presence
glm_nzro_norm_mod8 <- glm(norm_fav_thres~log(word_count+1) + visual_presence, data=data_nonzero, family = "binomial")

glm_nonzero_norm_model_names <- paste("glm_nzro_norm_mod", 1:8, sep="")
glm_nonzero_norm_models <- mget(glm_nonzero_norm_model_names)
glm_eval(glm_nonzero_norm_models)

# model 7 plot
ggplot(data_nonzero, aes(x=log(word_count+1), y=norm_fav_thres)) + 
  geom_point(aes(size=log(syuzhet+4.5), color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  scale_size_continuous(name="log(syuzhet+4.5)") + 
  theme_minimal() 

# model 8 plot
ggplot(data_nonzero, aes(x=log(word_count+1), y=norm_fav_thres)) + 
  geom_point(aes(color=as.factor(visual_presence)), alpha=0.2) +
  stat_smooth(method=glm, method.args=list(family = "binomial"), se=FALSE, color="blue") +
  labs(color="Visual Presence") + 
  theme_minimal()

# ===================== 4. Final Modeling ========================== #

# data split
set.seed(1)

train_ind <- sample(seq_len(nrow(recent_data)), size = floor(0.8 * nrow(recent_data)))

recent_data_train <- recent_data[train_ind, ]
recent_data_test <- recent_data[-train_ind, ]

# ====== 4.1 Linear Regression ====== #
lm_eval <- function(models, y_ind) {
  results <- sapply(models, function(mod) {
    actual_values <- recent_data_test[, y_ind]
    predicted <- predict(mod, newdata = recent_data_test)
    # undo transformation for predicted values
    y_variable <- as.character(formula(mod)[[2]])        
    if (grepl("sqrt", y_variable)) {
      predicted_values <- (predicted^2) - 1
    } else if (grepl("log", y_variable)){
      predicted_values <- exp(predicted) - 1
    } else {
      predicted_values <- predicted
    }
    
    mse <- mean((actual_values - predicted_values)^2)
    rmse <- sqrt(mse)
    mae <- mean(abs(actual_values - predicted_values))
    r2 <- summary(mod)$r.squared
    adj_r2 <- summary(mod)$adj.r.squared
    aic <- AIC(mod)
    log_likelihood <- logLik(mod)
    c(RMSE = rmse, MAE = mae, R_Squared = r2, Adjusted_R_Squared = adj_r2, AIC = aic, Log_Likelihood = log_likelihood)
  })
  
  model_stats <- data.frame(t(results))
  
  return(model_stats)
}
# ========= w/ recent data

# ========= linear regression models w/ 'scaled_norm_fav' as Y variable ========= #

# model 1. linear-linear
recent_lm_mod1 <- lm(scaled_norm_fav ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 2. linear-linear | visual_presence only
recent_lm_mod2 <- lm(scaled_norm_fav ~ visual_presence, data = recent_data_train)

# model 3. linear-log
recent_lm_mod3 <- lm(scaled_norm_fav ~ log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 4. linear-sqrt
recent_lm_mod4 <- lm(scaled_norm_fav ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 5. log-linear
recent_lm_mod5 <- lm(log_scaled_norm_fav ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 6. log-linear | visual_presence only
recent_lm_mod6 <- lm(log_scaled_norm_fav ~ visual_presence, data = recent_data_train)

# model 7. log-log
recent_lm_mod7 <- lm(log_scaled_norm_fav ~ log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 8. log-sqrt
recent_lm_mod8 <- lm(log_scaled_norm_fav ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 9. sqrt-linear
recent_lm_mod9 <- lm(sqrt_scaled_norm_fav ~  word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 10. sqrt-log
recent_lm_mod10 <- lm(sqrt_scaled_norm_fav ~  log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 11. sqrt-sqrt
recent_lm_mod11 <- lm(sqrt_scaled_norm_fav ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

which(names(recent_data_test) == 'scaled_norm_fav') # 20

# performance comparison
recent_lm_model_names <- paste("recent_lm_mod", 1:11, sep="")
recent_lm_models <- mget(recent_lm_model_names)
lm_eval(recent_lm_models, 20) # scaled_norm_fav column index = 20

summary(recent_lm_mod7)

vif(recent_lm_mod7)

# recent_lm_mod7 residuals vs fitted plot
plot(recent_lm_mod7$fitted.values, resid(recent_lm_mod7),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# recent_lm_mod7 Q-Q plot
qqnorm(resid(recent_lm_mod7))
qqline(resid(recent_lm_mod7), col = "red")

# ========= linear regression models w/ 'favorite_count' as Y variable ========= #

# model 1. linear-linear
fav_recent_lm_mod1 <- lm(favorite_count ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 2. linear-linear | visual_presence only
fav_recent_lm_mod2 <- lm(favorite_count ~ visual_presence, data = recent_data_train)

# model 3. linear-log
fav_recent_lm_mod3 <- lm(favorite_count ~ log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 4. linear-sqrt
fav_recent_lm_mod4 <- lm(favorite_count ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 5. log-linear
fav_recent_lm_mod5 <- lm(log_favorite_count ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 6. log-linear | visual_presence only
fav_recent_lm_mod6 <- lm(log_favorite_count ~ visual_presence, data = recent_data_train)

# model 7. log-log
fav_recent_lm_mod7 <- lm(log_favorite_count ~ log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 8. log-sqrt
fav_recent_lm_mod8 <- lm(log_favorite_count ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 9. sqrt-linear
fav_recent_lm_mod9 <- lm(sqrt_favorite_count ~  word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 10. sqrt-log
fav_recent_lm_mod10 <- lm(sqrt_favorite_count ~  log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

# model 11. sqrt-sqrt
fav_recent_lm_mod11 <- lm(sqrt_favorite_count ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train)

which(names(recent_data_test) == 'favorite_count') # 5

# performance comparison
fav_recent_lm_model_names <- paste("fav_recent_lm_mod", 1:11, sep="")
fav_recent_lm_models <- mget(fav_recent_lm_model_names)
lm_eval(fav_recent_lm_models, 5) # favorite_count column index = 5

summary(fav_recent_lm_mod7)

vif(fav_recent_lm_mod7)

# fav_recent_lm_mod7 residuals vs fitted plot
plot(fav_recent_lm_mod7$fitted.values, resid(fav_recent_lm_mod7),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "red")

# fav_recent_lm_mod7 Q-Q plot
qqnorm(resid(fav_recent_lm_mod7))
qqline(resid(fav_recent_lm_mod7), col = "red")


# ====== 4.2 Logistic Regression ====== #

glm_eval_detail <- function(models, predictors, actual, threshold) {
  
  acc_values <- numeric(length(models))
  auc_values <- numeric(length(models))
  sensitivity_values <- numeric(length(models))
  specificity_values <- numeric(length(models))
  precision_values <- numeric(length(models))
  f1_scores <- numeric(length(models))
  logLik_values <- numeric(length(models))
  pseudoR2_values <- numeric(length(models))
  adjR2_values <- numeric(length(models))
  
  aic_values <- sapply(models, AIC)
  residual_dev_values <- sapply(models, function(mod) mod$deviance)
  
  for (i in seq_along(models)) {
    model <- models[[i]]
    predicted_probs <- predict(model, newdata = predictors, type = "response")
    predicted_classes <- ifelse(predicted_probs > threshold, 1, 0)
    
    actual_factor <- factor(actual, levels = c(1, 0)) # added to prevent warning messages
    predicted_classes_factor <- factor(predicted_classes, levels = c(1, 0))
    
    confusion_matrix <- caret::confusionMatrix(predicted_classes_factor, actual_factor)
    acc_values[i] <- confusion_matrix$overall['Accuracy']
    sensitivity_values[i] <- confusion_matrix$byClass['Sensitivity']
    specificity_values[i] <- confusion_matrix$byClass['Specificity']
    precision_values[i] <- confusion_matrix$byClass['Precision']
    f1_scores[i] <- 2 * (precision_values[i] * sensitivity_values[i]) / (precision_values[i] + sensitivity_values[i])
    roc_obj <- pROC::roc(actual, predicted_probs, quiet = TRUE)
    auc_values[i] <- pROC::auc(roc_obj)
    
    # log likelihood
    logLik_values[i] <- logLik(model)
    
    # pseudo R-squared
    null_deviance <- model$null.deviance
    residual_deviance <- model$deviance
    pseudoR2_values[i] <- 1 - (residual_deviance/null_deviance)
    
    # adjusted R-squared
    n <- length(actual)
    p <- length(model$coefficients) - 1
    adjR2_values[i] <- 1 - ((1 - pseudoR2_values[i]) * (n - 1) / (n - p - 1))
  }
  
  results_df <- data.frame(
    AIC = aic_values,
    Residual_Deviance = residual_dev_values,
    Accuracy = acc_values,
    Sensitivity = sensitivity_values,
    Specificity = specificity_values,
    Precision = precision_values,
    F1_Score = f1_scores, 
    AUC = auc_values,
    Log_Likelihood = logLik_values,
    Pseudo_R2 = pseudoR2_values,
    Adjusted_R2 = adjR2_values
  )
  
  return(results_df)
}

# # =========  y: fav_thres | dataset: recent_data_train  ========= #

# model 1. X: all
glm_mod1 <- glm(fav_thres ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train, family = "binomial")

# model 2. X: all except syuzhet
glm_mod2 <- glm(fav_thres ~ word_count + visual_presence + biz_presence + eco_presence, data = recent_data_train, family = "binomial")

# model 3. X: word_count, syuzhet, visual_presence
glm_mod3 <- glm(fav_thres ~ word_count + syuzhet + visual_presence, data = recent_data_train, family = "binomial")

# model 4. X: word_count, visual_presence
glm_mod4 <- glm(fav_thres ~ word_count + visual_presence, data = recent_data_train, family = "binomial")

# model 5. X: all w/ log_word_count & log_syuzhet
glm_mod5 <- glm(fav_thres ~ log_word_count + log_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train, family = "binomial")

# model 6. X: all w/ log_word_count, except syuzhet
glm_mod6 <- glm(fav_thres ~ log_word_count + visual_presence + biz_presence + eco_presence, data = recent_data_train, family = "binomial")

# model 7. X: log_word_count, visual_presence 
glm_mod7 <- glm(fav_thres ~ log_word_count + visual_presence, data = recent_data_train, family = "binomial")

# model 8. X: all w/ sqrt_word_count & sqrt_syuzhet
glm_mod8 <- glm(fav_thres ~ sqrt_word_count + sqrt_syuzhet + visual_presence + biz_presence + eco_presence, data = recent_data_train, family = "binomial")

# model 9. X: all w/ sqrt_word_count, except syuzhet
glm_mod9 <- glm(fav_thres ~ sqrt_word_count + visual_presence + biz_presence + eco_presence, data = recent_data_train, family = "binomial")

# model 10. X: sqrt_word_count, visual_presence 
glm_mod10 <- glm(fav_thres ~ sqrt_word_count + visual_presence, data = recent_data_train, family = "binomial")

which(names(recent_data_test) == 'fav_thres')

glm_model_names <- paste("glm_mod", 1:10, sep="")
glm_models <- mget(glm_model_names)

glm_eval_detail(glm_models, recent_data_test[,-21], recent_data_test[,21], 0.2) # fav_thres index = 21

glm_eval_detail(glm_models, recent_data_test[,-21], recent_data_test[,21], 0.3) # fav_thres index = 21

glm_eval_detail(glm_models, recent_data_test[,-21], recent_data_test[,21], 0.4) # fav_thres index = 21

glm_eval_detail(glm_models, recent_data_test[,-21], recent_data_test[,21], 0.5) # fav_thres index = 21

glm_eval_detail(glm_models, recent_data_test[,-21], recent_data_test[,21], 0.6) # fav_thres index = 21

summary(glm_mod1)

summary(glm_mod2)

# ====== 4-3. Zero-Inflated Models ====== #

# zero-inflated poisson model
zip_model <- zeroinfl(scaled_norm_fav ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence | 1, data = recent_data_train, dist = "poisson")

# zero-inflated negative binomial model
zinb_model <- zeroinfl(scaled_norm_fav ~ word_count + syuzhet + visual_presence + biz_presence + eco_presence | 1, data = recent_data_train, dist = "negbin")

# create a function for RMSE
model.fit.evaluate.rmse <- function(model, test_data, test_values) {
  output = predict(model, test_data)
  rmse = sqrt(sum((output - test_values)^2)/length(test_values))
}

# test values
recent_test_values <- recent_data_test$scaled_norm_fav

# Summary of ZIP model and statistical results
summary(zip_model)

# evaluate ZIP model
cat("< ZIP Model >", "\n",
    "AIC:", AIC(zip_model), "\n",
    "BIC:", BIC(zip_model), "\n",
    "Log-likelihood:", logLik(zip_model),"\n")

null_model_zip <- zeroinfl(scaled_norm_fav ~ 1 | 1, data = recent_data_train, dist = "poisson")
pseudo_r_squared = 1 - (logLik(zip_model)/logLik(null_model_zip))
cat("Pseudo R-Squared:", pseudo_r_squared, "\n") 

rmse.zip = model.fit.evaluate.rmse(zip_model, recent_data_test, recent_test_values)
cat("RMSE: ", rmse.zip) 

# Summary of ZINB model and statistical results
summary(zinb_model)

# evaluate ZINB model
cat("< ZINB Model >", "\n",
    "AIC:", AIC(zinb_model), "\n",
    "BIC:", BIC(zinb_model), "\n",
    "Log-likelihood:", logLik(zinb_model), "\n")

null_model_zinb <- zeroinfl(scaled_norm_fav ~ 1 | 1, data = recent_data_train, dist = "negbin")
pseudo_r_squared = 1 - (logLik(zinb_model)/logLik(null_model_zinb))
cat("Pseudo R-Squared:", pseudo_r_squared, "\n")

rmse.zinb = model.fit.evaluate.rmse(zinb_model, recent_data_test, recent_test_values)
cat("RMSE:", rmse.zinb) 

# revised zero-inflated negative binomial model
zinb_model_revised <- zeroinfl(scaled_norm_fav ~ word_count + syuzhet + visual_presence | 1, data = recent_data_train, dist = "negbin")

# Summary of revised ZINB model and statistical results
summary(zinb_model_revised)

# evaluate ZINB model
cat("< ZINB Model - Revised >", "\n",
    "AIC:", AIC(zinb_model_revised), "\n", 
    "BIC:", BIC(zinb_model_revised), "\n",
    "Log-likelihood:", logLik(zinb_model_revised), "\n") 

null_model_zinb <- zeroinfl(scaled_norm_fav ~ 1 | 1, data = recent_data_train, dist = "negbin")
pseudo_r_squared = 1 - (logLik(zinb_model_revised)/logLik(null_model_zinb))
cat("Pseudo R-suqared:", pseudo_r_squared, "\n") 

rmse.zinb.rev = model.fit.evaluate.rmse(zinb_model_revised, recent_data_test, recent_test_values)
cat("RMSE: ", rmse.zinb.rev)
