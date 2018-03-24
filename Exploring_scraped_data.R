library(ggplot2)
library(dplyr)

setwd('C:\\Users\\Kenny\\Desktop\\NYCDSA\\meetup_scrape')

# Load scraped
meetup <- read.csv('meetup.csv', stringsAsFactors = F)
meetup$X <- NULL
meetup$Founded <- as.Date(meetup$Founded)
meetup$Topics <- strsplit(meetup$Topics, ", ")



# Create function to expand each group row by the number of topics associated with it

funky <- function(test){
  # Initialize empty df
  blanky = data.frame()
  
  # For each row of our dataframe
  for (i in 1:nrow(test)){
    # and for each topic in our topic list IN that row
    for (j in 1:length(test$Topics[[i]])){
      # iterate through the jth element of the list [[i]][j]
      Topic = test$Topics[[i]][j]
      # cbind the original row of columns with (test[i,]) with new column value (topic)
      blanky <- rbind(blanky, cbind(test[i,], Topic))
    }
  }
  blanky$Topics = NULL
  return (blanky)
}

topicDF <- funky(meetup)

write.csv(topicDF, 'topicDF.csv')





####################################################################################
library(scales)

# create DF for overall tech group creations by date
groupTime <- meetup %>%
  mutate(month = months(Founded),
         year = substr(Founded,1,4),
         Founded = as.Date(paste(month, '1', year),'%B %d %Y')) %>%
  group_by(month, year, Founded) %>%
  summarise(numGroups = n())

# plot timeTest
ggplot(groupTime, aes(x = Founded, y = numGroups)) +
  geom_line(color = 'orangered') +
  ggtitle('Number Of Tech Groups Created Since 2002') +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(color = 'grey'),
        plot.title = element_text(size = 17, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 12),
        text = element_text(size = 12)) +
  labs(x = 'Date Created', y = 'Number of Tech Groups') +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))




            
####################################################################################

# Visualize topic trends

topicDF <- read.csv('topicDF.csv')
topicDF$X <- NULL 
topicDF$Founded <- as.Date(topicDF$Founded)


topicTime <- topicDF %>% 
  group_by(Founded, Topic) %>%
  summarise(Count = n())

# since 2002
topicFreq <- topicDF %>%
  group_by(Topic) %>%
  summarise(Count = n()) %>%
  arrange(-Count)


# only 2013
topicFreq2013 <- topicDF %>%
  #filter(Founded >= '2017-01-01' & Founded <= '2017-12-31') %>%
  group_by(Topic) %>%
  summarise(Count = n()) %>%
  arrange(-Count) %>%
  head(10) # only let the top 20 topics?



# TOP TOPICS PLOT BY HOW MANY GROUPS FALL UNDER THAT TOPIC

# TOP TOPICS BY GROUP SIZE 2013
topoverall <- ggplot(topicFreq2013, aes(x = reorder(Topic, -Count), y = Count)) +
  geom_bar(stat = 'identity', aes(fill = Topic)) +
  ggtitle('Overall Top 10 Topics') +
  geom_text(aes(label = Count , vjust = -0.5)) +
  theme(axis.text.x = element_text(angle = 10),
        panel.background = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 12),
        text = element_text(size = 15),
        legend.position = 'none') +
  labs(x = 'Topic', y = 'Number of Groups Under Topic') + 
  scale_fill_manual(values = c('#A50F15','#CB181D','#EF3B2C','#FB6A4A','#FC9272','#FCBBA1','#FEE0D2','#FFF5F0','#67000D','grey'))


# Filter to last year 2017-2018
topicFreq2017 <- topicDF %>%
  filter(Founded >= '2017-01-01' & Founded <= '2017-12-31') %>%
  group_by(Topic) %>%
  summarise(Count = n()) %>%
  arrange(-Count) %>%
  head(5)

# TOP TOPICS BY GROUP SIZE 2017
ggplot(topicFreq2017, aes(x = reorder(Topic, -Count), y = Count)) +
  geom_bar(stat = 'identity', aes(fill = Topic)) +
  ggtitle('2017') +
  geom_text(aes(label = Count , vjust = -0.5)) +
  theme(axis.text.x = element_text(angle = 15),
        panel.background = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 12),
        text = element_text(size = 15),
        legend.position = 'none') +
  labs(x = 'Topic', y = 'Number of Groups Under Topic') + 
  scale_fill_manual(values = c('#A50F15','#CB181D','#EF3B2C','#FB6A4A','#FC9272','#FCBBA1','#FEE0D2','#FFF5F0','#67000D','grey'))


#####################################################################
# ANY VARIATION OF BIG DATA, MACHINESE LEARNING AND DATA SCIENCE TOPICS

groupsAnykeyTopic <- topicDF %>%
  filter(grepl('Machine Learning|Data Science|Big Data', Topic)) %>%
  mutate(month = months(Founded),
         year = substr(Founded,1,4),
         Founded_Date = as.Date(paste(month, '1', year),'%B %d %Y')) %>%
  group_by(Founded_Date, Topic) %>%
  summarise(Groups = n())


# explore data
length(groupsAnykeyTopic$Topic)
sum(groupsAnykeyTopic$Groups)

min(groupsAnykeyTopic$Founded_Date)
max(groupsAnykeyTopic$Founded_Date)


###########################################################################################

# SPECIFIC 3 TOPICS
topicTimedata <- topicDF %>%
  #filter(grep('Machine Learning|Data Science|Big Data', Topic)) %>%
  filter(Topic == 'Machine Learning'|
           Topic == 'Big Data'|
           Topic == 'Data Science') %>%
           #Topic == 'Data Analytics') %>%
  mutate(month = months(Founded),
         year = substr(Founded,1,4),
         Founded_Date = as.Date(paste(month, '1', year),'%B %d %Y')) %>%
  group_by(Founded_Date, Topic) %>%
  summarise(Groups = n())

# Explore topic
min(topicTimedata$Founded_Date)
max(topicTimedata$Founded_Date)
sum(topicTimedata$Groups)

# create dummy data frame to clean area chart
dummy1 <- data.frame(rep(seq.Date(as.Date('2003-03-01'), as.Date('2018-02-01'), 'months'),each = 3) )
dummy2 <- data.frame(rep(c('Big Data','Machine Learning','Data Science'), 180))
dummy <- cbind(dummy1,dummy2)

colnames(dummy) <- c('Founded_Date', 'Topic')
colnames(dummy)
dummy$Founded_Date <- as.Date(dummy$Founded_Date)

# join dummy df with topicTimedata to have full set of dates
topicTimedata <- topicTimedata %>% full_join(dummy, by = c('Founded_Date', 'Topic'))
topicTimedata[is.na(topicTimedata)] <- 0

ggplot(topicTimedata, aes(x = Founded_Date, y = Groups)) +
  geom_area(aes(fill = Topic), alpha = .75) +
  ggtitle('Timeline Trend Of Interested Topic') +
  theme(plot.title = element_text(size = 17, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 15),
        text = element_text(size = 12),
        legend.position = c(.5,.95),
        legend.direction = 'horizontal',
        legend.title = element_blank(),
        panel.background = element_blank(),
        panel.grid.major.x = element_line(color = 'grey'),
        panel.grid.major.y = element_line(color = 'grey')) +
  labs(x = 'Date Created', y = 'Number of Tech Groups') +
  scale_x_date(breaks = date_breaks("years"), labels = date_format("%Y"))

########################################################################

# Topics that have the most MEMBERS overall, not just groups created under it

keyTopicstitles <- topicDF %>%
  filter(Topic == 'Machine Learning'|
           Topic == 'Big Data'|
           Topic == 'Data Science') %>%
  select(Title) %>%
  unique()


# this give all the TOP GROUPS that have 'Data Science', 'Machine Learning' or 'Big Data'  under their topic list
keyGroupsdata <-
  meetup[meetup$Title %in% keyTopicstitles$Title,] %>%
  arrange(-Members)

# clean blank titles from DF
keyGroupsdata <- keyGroupsdata[keyGroupsdata$Title != '',]

# top groups overall
keyGroups <- keyGroupsdata %>%
  arrange(-Members) %>%
  head(5)

# 2014 groups created with key topics
keyGroups2014 <- keyGroupsdata %>%
  filter(Founded >= '2014-01-01' & Founded <= '2014-12-31') %>%
  arrange(-Members) %>%
  head(5)
  


# 2016 groups created with key topics
keyGroups2016 <- keyGroupsdata %>%
  filter(Founded >= '2016-01-01' & Founded <= '2016-12-31') %>%
  arrange(-Members) %>%
  head(5)

# 2017 groups created with key topics
keyGroups2017 <- keyGroupsdata %>%
  filter(Founded >= '2017-01-01' & Founded <= '2017-12-31') %>%
  arrange(-Members) %>%
  head(10)


# PLOT 2014 AND 2016 TOP GROUPS BY MEMBER SIZE

ggplot(keyGroups2014, aes(x = reorder(Title, Members), y = Members)) +
  geom_bar(stat = 'identity', aes(fill = Title)) +
  geom_text(aes(label = Members, hjust = -.25)) +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 30),
        text = element_text(size = 15),
        legend.position = 'none') +
  labs(x = '', y = '') + 
  scale_fill_manual(values = c('#A50F15','#CB181D','#EF3B2C','#FB6A4A','#FC9272','#FCBBA1','#FEE0D2','#FFF5F0','#67000D','grey'))




ggplot(keyGroups2016, aes(x = reorder(Title, Members), y = Members)) +
  geom_bar(stat = 'identity', aes(fill = Title)) +
  geom_text(aes(label = Members, hjust = -.25)) +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(size = 30, face = 'bold', hjust = 0.5),
        axis.title = element_text(size = 30),
        text = element_text(size = 15),
        legend.position = 'none') +
  labs(x = '', y = '') + 
  scale_fill_manual(values = c('#A50F15','#CB181D','#EF3B2C','#FB6A4A','#FC9272','#FCBBA1','#FEE0D2','#FFF5F0','#67000D','grey'))



###########################################################################3

# word cloud of the About Me by topic

library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)

# create filter to obtain only the rows where topic is relevant

# VectorSource func. turns each element of About vector into a 'document'
# Corpus func. then turns the vector of documents into a class called Corpus, which is a collection of documents
aboutCorpus <- Corpus(VectorSource(keyGroups2014$About))
aboutCorpus <- Corpus(VectorSource(keyGroups2016$About))
aboutCorpus <- Corpus(VectorSource(keyGroups2017$About))

# remove puncutaions and stopwords 
aboutCorpus <- tm_map(aboutCorpus, removePunctuation)
aboutCorpus <- tm_map(aboutCorpus, removeWords, stopwords('english'))
aboutCorpus <- tm_map(aboutCorpus, content_transformer(tolower))
aboutCorpus <- tm_map(aboutCorpus, removeNumbers)
aboutCorpus <- tm_map(aboutCorpus, stripWhitespace)

# Remove common words
aboutCorpus <- tm_map(aboutCorpus, removeWords, c('','meetups','like','get','york','nyc','event','events','interested','want','best', 'new','can','data', 'learn', 'group', 'meetup','will','this','discuss','the'))


#Term document matrix
tdm <- TermDocumentMatrix(aboutCorpus)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing = T)
d <- data.frame(word = names(v), freq=v)
head(d,10)

# plot wordcloud
wordcloud(aboutCorpus, max.words = 100, random.order = F, colors = brewer.pal(10, 'Dark2'))





#######################################################################################

# Create Scatter chart of groups that fall under key topics.

install.packages('plotly')
library(plotly)

# plot by member size and group reviews
ggplotly(
  ggplot(keyGroupsdata, aes(x = Members, y = Group_Reviews, text = Title)) +
    geom_point(aes(color = Members)) +
    ggtitle('Data Science Related Group Performances') +
    theme(plot.title = element_text(size = 18, face = 'bold', hjust = 0.5),
          legend.position = 'none',
          panel.background = element_blank(),
          panel.grid.major = element_line(color = 'grey'),
          panel.grid.minor = element_line(color = 'grey'),
          axis.title = element_text(size = 15),
          text = element_text(size = 12)) +
    labs(x = 'Member Size', y = 'Group Reviews')
)


median(meetup$Members)
mean(meetup$Group_Reviews)
