require(rtweet)
require(httpuv)
# twitter_token<-create_token('placeholder','Redacted','Redacted' )
get_followers('@realDoanldTrump')
dat<-search_tweets("trump", n=1500, token=twitter_token)
tweets<-dat$tweets
tweets$words<-clean_tweets(tweets$text)

# scraping the tweets

pos.words<-read.table('~/Documents/opinion-lexicon-English/positive-words.txt')
colnames(pos.words)<-c("words")
neg.words<-read.table('~/Documents/opinion-lexicon-English/negative-words.txt')
colnames(neg.words)<-c("words")

# reading in positive and negative lexicons


tweets$sentiment<-rep(0, nrow(tweets))

# create a dummy column that we will then use 

for (i in 1:nrow(tweets)){
  
  tweets$sentiment[[i]]<-sum(tweets$words[[i]] %in% pos.words$words)-sum(tweets$words[[i]] %in% neg.words$words)
  
}

# this is really simple. Sentiment=the sum of matches in positive words- the sum of matches in negative words. 

sum(!is.na(tweets$place_name))
plot(density(tweets$sentiment))
mean(tweets$sentiment)
sd(tweets$sentiment)
require(ggplot2)
ggplot(tweets, aes(y=sentiment, x=created_at)) + geom_smooth()
ggplot(tweets, aes(y=sentiment, x=created_at)) + geom_point()
ggplot(tweets, aes(y=sentiment, x=created_at, size=retweet_count, color=sentiment)) + geom_point()
users<-dat$users
users$created_at<-as.POSIXct(users$created_at)
tweets$created_at<-as.POSIXct(tweets$created_at)
users<-as.data.frame(users)
tweets<-as.data.frame(tweets)
require(dplyr)
full_dat<-left_join(users, tweets, by="user_id")
full_dat<-full_dat[!is.na(full_dat$location),]
require(ggmap)
full_dat$geocodes<-geocode(full_dat$location, output="latlon")
full_dat$lat<-full_dat$geocodes$lat
full_dat$lon<-full_dat$geocodes$lon
full_dat<-full_dat[!is.na(full_dat$lat),]
full_dat<-full_dat[!is.na(full_dat$lon),]
us_map<-get_map(location="USA", zoom=4)
require(ggrepel)
map<-ggmap(us_map)+geom_jitter(data = full_dat, aes(x=lon, y=lat, size=retweet_count, color=sentiment)) + scale_color_gradient2(low="blue",mid="grey", high="red") + geom_label_repel(data=full_dat[full_dat$favorite_count>1000,],aes(x=lon, y=lat, label=screen_name))
