---
title: 'Amazon Books Review Analysis 
author: "C. ZHANG"
---


library(lubridate)
library(readxl)
library(plyr)
library(tidytext)
library(textdata)
library(dplyr)
library(tidyr)
library(ggplot2)


#### 
#### 1. Please provide us with a statistical summary/review of the star_ratings for the books in your dataset while incorporating a sentiment analysisof the review.
#### (a)  Using books with at least 50 reviews, identify the 2 top-rated and 2 lowest rated books as determined by and ordered by their mean star_rating.
#### load the data

load("/Users/kirklandtomato/Desktop/R_studio/AmazonFinal3.RData")


#### combine the data

Amazon <- inner_join(Amazon3A, Amazon3B, by=c("customer_id", "review_id"))


#### check the data

summary(Amazon)

#### data is quite clean, the class is normal and there are 450193 objectives and 13 variables. Overall, it is the dataset about Amazon book reviews and related character such as star rating, total votes, etc.


#### introduce review numbers and mean of star rating

a1<- Amazon %>% 
  select(product_title, star_rating) %>% 
  group_by(product_title) %>% 
  summarise(review_number=n(), meanstar=mean(star_rating)) %>% 
  arrange(desc(review_number)) 

#### using books with at least 50 reviews

a2<- Amazon %>% 
  select(product_title, star_rating) %>% 
  group_by(product_title) %>% 
  summarise(review_number=n(), meanstar=mean(star_rating)) %>% 
  arrange(desc(review_number)) %>% 
  filter(review_number>50)


#### top 2 rated books

top_n(a2,2,meanstar)

#### lowest 2 rated books

top_n(a2,-2, meanstar)

#### identify the 4 books

top2<- top_n(a2,2, meanstar)
lower2<- top_n(a2,-2, meanstar)
identified<- rbind(top2, lower2)
identified %>% arrange(desc(meanstar))


#### with books that at least 50 reviews, the Top-2-Rated (determined by mean star_rating) is: 
#### 1) Rush Revere and the American Revolution: Time-Travel Adventures With Exceptional Americans 
#### 2) A Higher Call: An Incredible True Story of Combat and Chivalry in the War-Torn Skies of World War II
#### The Lower-2-rated (determined by mean star_rating) is
#### 1) Allegiant (Divergent Series)                     
#### 2) It Could Happen To Anyone: Why Battered Women Stay


#### 1. (b)  For each identified book (so 4 titles total), please perform a detailed analysis of the reviews by examining the words used by reviewers and their associated sentiment scores.

#### a3 only contains 4books data:

a3<- Amazon %>% 
  select_all() %>% 
  filter(product_title %in% 
        c("Rush Revere and the American Revolution: Time-Travel Adventures With Exceptional Americans",
          "A Higher Call: An Incredible True Story of Combat and Chivalry in the War-Torn Skies of World War II", 
          "Allegiant (Divergent Series)", 
          "It Could Happen To Anyone: Why Battered Women Stay"))


#### sentiment for the 4book

sentiment<- a3 %>%
  unnest_tokens(word, review_body) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>%
  summarize(sentiment = mean(value), words = n()) %>%
  filter(words >= 1) %>%
  arrange(desc(sentiment))


#### The 4 book whole data

sentiment2 <- left_join(sentiment, a3, by =  "review_id")


#### The 4 book whole data, with factor shown star rating levels and filtered by the verified purchase. we also need the helpful vote to above 0.


sentiment3<- sentiment2 %>%
  mutate(star_rating_levels = factor(star_rating,
                            levels = c("5","4", "3","2", "1"))) %>%
  select_all%>%
  filter(verified_purchase %in% c("Y", "N") & helpful_votes>0) %>% 
  na.omit()


#### check the data we are going to use and it is ok

summary(sentiment3)


#### Also, we could get the seperate data on different 4 books as book1, book2, book3 and book4.


book1<- sentiment3 %>% 
  select_all() %>% 
  filter(product_title %in% 
        c("Rush Revere and the American Revolution: Time-Travel Adventures With Exceptional Americans"))
summary(book1)



book2<- sentiment3 %>% 
  select_all() %>% 
  filter(product_title %in% 
        c("A Higher Call: An Incredible True Story of Combat and Chivalry in the War-Torn Skies of World War II"))
summary(book2)



book3<- sentiment3 %>% 
  select_all() %>% 
  filter(product_title %in% 
        c("Allegiant (Divergent Series)"))
summary(book3)



book4<- sentiment3 %>% 
  select_all() %>% 
  filter(product_title %in% 
        c("It Could Happen To Anyone: Why Battered Women Stay"))
summary(book4)


#### we could use facet_wrap to discuss all books in one graphic, pls see below:
#### fig.width=10
library(ggplot2)
ggplot(sentiment3, aes(x = log(words), 
                       y = sentiment)) +
  geom_hex(color= "dark grey") +
  scale_fill_gradientn(colours = rainbow(4)) +
  facet_wrap(~product_title) +
  geom_smooth(method = "lm") +
  ggtitle("The correlation between words and sentiment level in different books") +
  theme_classic() +
  xlab("Words") +
  ylab("Sentiment Level")


#### For each identified book (so 4 titles total), please perform a detailed analysis of the reviews by examining the words used by reviewers and their associated sentiment scores.

#### 1) For top-rated-1-rated book (Rush Revere and the American Revolution: Time-Travel Adventures With Exceptional Americans), the correlation between words and sentiment level is negative, it use less words but express more positive (high sentiment level) reviews. The words used in majority of the reviews are less than mean of word in 4 book dataset. All the reviews are positive.

#### 2) For top-rated-2-book (A Higher Call: An Incredible True Story of Combat and Chivalry in the War-Torn Skies of World War II), the correlation between words and sentiment level is significantly negative, the less words express more positive reviews (high sentiment). Most of the reviews are positive. 


#### 3) For lower-rated-1-book (Allegiant (Divergent Series)), the correlation between words and sentiment level is a little bit negative, but not significate(we could see the line has a very slight angle). The words used are common length (from 0 to 5) and it is a normal distribution. The majority reviews express neutral sentiment since most of them are around 0 sentiment level. 


#### 4) For lower-rated-2-book (It Could Happen To Anyone: Why Battered Women Stay), the correlation between words and sentiment is not significate(the line is flat with no angle). The majority reviews express negative sentiment since most of them are under 0 sentiment level. 


#### Also, we could see the relation between words and sentiment using another similar graphic which having star rating levels introduced:

library(ggplot2)
ggplot(sentiment3, aes(x = log(words), 
                       y = sentiment)) +
  geom_hex(color= "dark grey") +
  scale_fill_gradientn(colours = rainbow(4)) +
  facet_wrap(~star_rating_levels)+
  geom_smooth(method = "lm") +
  ggtitle("The correlation between words and sentiment level in different star rating levels") +
  theme_classic() +
  xlab("Words") +
  ylab("Sentiment Level")



#### 2. we could introduce more larger data set when we come to Q2
#### introduce the review number, calculate the mean of star rating

b1<- Amazon %>% 
  select_all() %>% 
  group_by(product_title) %>% 
  summarise(review_number=n(), meanstar=mean(star_rating)) %>% 
  arrange(desc(review_number)) 


#### calculate the sentiment level (at least 2 words)

sentiment_amazon<- Amazon %>%
  unnest_tokens(word, review_body) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>%
  summarize(sentiment = mean(value), words = n()) %>%
  filter(words >= 2) %>%
  arrange(desc(sentiment))


#### adding sentiment

sentiment_amazon_2 <- left_join(sentiment_amazon, Amazon, by =  "review_id")


#### introduce b1, so we could see review numbers and mean of star rating

sentiment_amazon_3 <- left_join(sentiment_amazon_2, b1, by =  "product_title")


#### process dataset into the way we want(with different star rating levels, purchased or not)

sentiment_amazon_4<- sentiment_amazon_3 %>%
      select_all %>%
      mutate(star_rating_levels = factor(star_rating,levels = c("5","4", "3","2", "1"))) %>%
      mutate(review_year = factor(year(review_date), levels = c("2012","2013", "2014","2015"))) %>%
      filter(verified_purchase %in% c("Y", "N")) %>%
    na.omit()


#### check the data and it is fine and clean

summary(sentiment_amazon_4)



#### Here is the model with whole data sentiment_amazon_4

library(ggplot2)
ggplot(sentiment_amazon_4, aes(x = star_rating_levels, 
                               y= sentiment,
                               color = verified_purchase)) +
  geom_boxplot(aes(fill=verified_purchase), colour="black", outlier.shape = NA) +
  facet_wrap(~verified_purchase) +
  ggtitle("The correlation between star rating level and sentiment") +
  theme_classic() +
  xlab("Star Rating Level") +
  ylab("Sentiment Level")



#### The above graphic communicates star rating levels and sentiment: 
#### 1) The star rating level (1-5) and sentiment level is positively correlated; also, in different status of verified purchase, the sentiment level is different. 
#### 2) The higher star rating level, the higher sentimiment level will present. 
#### 3) With the same star rating level, when the verified purchase is yes, the sentiment level is higher.
#### 4) People are more likely to buy the book(verified purchase is yes) when they noticed the higher star rating or more positive words(sentiment level is high) in the reviews. 
 
 
#### we could also take a look at the previous(Q1) 4 book package using the same model: 

library(ggplot2)
ggplot(sentiment3, aes(x = star_rating_levels, 
                         y= sentiment,
                         color = verified_purchase)) +
  geom_boxplot(aes(fill=verified_purchase), colour="black", outlier.shape = NA) +
  facet_wrap(~verified_purchase) +
  ggtitle("star rating levels and sentiment") +
  theme_classic() +
  xlab("Star Rating Level") +
  ylab("Sentiment Level")



#### The above graphic communicates star rating levels and sentiment using 4 book(tep 2 and lower 2 data)
#### The correlation is similar compared with previous data: 
#### 1) The star rating level (1-5) and sentiment level is positively correlated; also, in different status of verified purchase, the sentiment level is different. 
#### 2) The higher star rating level, the higher sentimiment level will present. 
#### 3) With the same star rating level, when the verified purchase is yes, the sentiment level is higher.
#### 4) It seems more samples in both star level 5 and level 1 since we pick the top 2 and lower 2 rated books. Also, people prefer to buy the book when sentiment level is higher. 



#### Question 3 There are several aspects of the reviews – for example, the text of the review, the title of the review, the time of the review, the number of other reviews written by a customer and the votes a review and book receives

#### Data select or proceed new variable:
#### a) the time of the review(review_year)
#### b) the text of the review(intoduce "feelings" represent sentiment level as factors "happy" or "sad")
#### c) the votes(both total_votes and helpful_votes to be used to calculate helpful_vote_rate)

#### Hypothese1: The correlation betwwen review numbers and helpful vote rate (helpful votes in total votes percentage),check if it is related with sentiment level influence (happy or sad)

#### Hypothese2: The time series graphic of helpful vote rate (helpful votes in total votes percentage), check if it is related with sentiment level influence (happy or sad) and star rating levels


#### Methods: outline the steps to operate idea
#### 1) find the top 10 books

top10<- top_n(a1,10,meanstar)
top10


#### 2) find the lower 10 books

lower10<- top_n(a1,-10,meanstar)
lower10

#### 3) identify the 20 books

identified20<- rbind(top10, lower10)
identified20 %>% arrange(desc(meanstar))


#### 4) get all other variables from original dataset

a_identified20<- Amazon %>% 
  select_all() %>% 
  filter(product_title %in% 
        (identified20$product_title))%>% 
  group_by(product_title) 


#### 5) get the sentiment 

 s_i20<- a_identified20 %>%
  unnest_tokens(word, review_body) %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(review_id) %>%
  summarize(sentiment = mean(value), words = n()) %>%
  filter(words >= 1) %>%
  arrange(desc(sentiment))


#### 6) The 20 book whole data

s_i20_1<- left_join(s_i20, a_identified20, by ="review_id")



s_i20_whole<- left_join(s_i20_1, a1, by="product_title")


#### 7) The 20 book ready to use data: create review year, intoduce "feelings" represent sentiment level as factors "happy" or "sad", the votes (both total_votes and helpful_votes to be used to calculate helpful_vote_rate), clean and organize


s_i20_feelings<- s_i20_whole %>%
  mutate(star_rating_levels = factor(star_rating,
                            levels = c("5","4", "3","2", "1"))) %>%
  mutate(review_year = factor(year(review_date),
                            levels = c("2012","2013", "2014","2015"))) %>%
  mutate(helpful_votes_rate= helpful_votes/total_votes) %>%
  mutate(feelings1 <- ifelse(sentiment>0,0,1), 
         feelings = factor(feelings1, label = c("happy", "sad"))) %>%
  select_all%>%
  filter(verified_purchase %in% c("Y", "N")) %>% 
  na.omit()


#### 8) check the data

summary(s_i20_feelings)


#### Hypothese1: The correlation betwwen review numbers and helpful vote rate (helpful votes in total votes percentage),check if it is related with sentiment level influence (happy or sad)

library(ggplot2)
ggplot(s_i20_feelings, aes(x = review_number, 
                            y = helpful_votes_rate, outlier.shape = NA)) +
  geom_hex(color= "dark grey") +
  scale_fill_gradientn(colours = rainbow(4)) +
  facet_wrap(~feelings)+
  geom_smooth(method = "lm") +
  ggtitle("The correlation betwwen review numbers and helpful vote rate") +
  theme_classic() +
  xlab("review number") +
  ylab("Helpful votes rate")
  
  
#### This graphic is quite interesting:
#### 1) when the review sentiment is "happy" (positive words, sentiment> 0): the correlation betwwen review numbers and helpful vote rate (helpful votes in total votes percentage) is negetively correlated. it mean, the more reviews, the helpful vote rate is lower in the happy sentiment review mood.
#### 2) when the review sentiment is "sad" (negative words, sentiment< 0): the correlation betwwen review numbers and helpful vote rate (helpful votes in total votes percentage) is positively correlated, and it is significate(line angle is high).  it mean, the more reviews, the helpful vote rate is higher in the sad sentiment review mood.



#### Hypothese2: The time series graphic of helpful vote rate (helpful votes in total votes percentage), check if it is related with sentiment level influence (happy or sad) and star rating levels

library(ggplot2)
ggplot(s_i20_feelings, aes(x = review_year, 
                               y= helpful_votes_rate,
                               color = feelings)) +
  geom_boxplot(aes(fill=feelings), colour="black", outlier.shape = NA) +
  facet_wrap(~star_rating_levels) +
  ggtitle("The helpful votes rate in time series") +
  theme_classic() +
  xlab("year") +
  ylab("helpful_votes_rate")


#### The time series graphic of helpful vote rate:
#### 1) from 2012 to 2015, when it conntected with the 5-star rated books, helpful rate is always very high and satisfied (almost reach 1), and the helpful rate range for happy mood is wider than the sad mood.
#### 2) from 2012 to 2015, when it conntected with the (1 star) lower star rated books, helpful rate is always lower than the 5-star rated books, also, the helpful rate range for both mood becomes wider as yime goes on.

#### produce some pseudocode based on your ideas
#### for further explore, based on the current model and variables, what i need most is the sold number of each top and lower rated books. So the sum could do the "lm" model or other model check to see if any of the variable mentioned above has a significate correlation with the number of books sold, so we could make some strategetic suggestion or smart tip for either amazon marketing team or individual writers.




