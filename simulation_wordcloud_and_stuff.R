library(dplyr)
library(stringr)
library(tidytext)
library(textreadr)
library(janeaustenr)
library(textdata)
library(magrittr)
library(rvest)
library(twitteR)
library(tm)
library(ggplot2)
library(reshape2)
library(tidyverse)


library("twitteR")
library("tm")
library(dplyr)
library(tidyr)
library(tidytext)
library(tidyverse)


file_s <- read_document(file="simulation1.docx")

a <- length(file_s)/6 #how many observations to you have    17
b <- 6 #how many variables do you have
my_df <- as.data.frame(matrix(nrow=a, ncol=b))

for(z in 1:b){
  for(i in 1:a){
    my_df[i,z]<- file_s[i*b+z-b]
  }#closing z loop
}#closing i loop

my_txt1 <- my_df$V1
my_txt2 <- my_df$V2
my_txt3 <- my_df$V3
my_txt4 <- my_df$V4
my_txt5 <- my_df$V5
my_txt6 <- my_df$V6


mydf1 <- data_frame(line=1:a, text=my_txt1)
mydf2 <- data_frame(line=1:a, text=my_txt2)
mydf3 <- data_frame(line=1:a, text=my_txt3)
mydf4 <- data_frame(line=1:a, text=my_txt4)
mydf5 <- data_frame(line=1:a, text=my_txt5)
mydf6 <- data_frame(line=1:a, text=my_txt6)


mydf_total <-  bind_cols(mutate(mydf1),
                         mutate(mydf2),
                         mutate(mydf3),
                         mutate(mydf4),
                         mutate(mydf5),
                         mutate(mydf6))

mydf_total$line1 <- NULL
mydf_total$line2 <- NULL
mydf_total$line3 <- NULL
mydf_total$line4 <- NULL
mydf_total$line5 <- NULL

mydf_total$all <- paste(mydf_total$text,mydf_total$text1,mydf_total$text2,mydf_total$text3,mydf_total$text4,mydf_total$text5)

mydf_total$text <- NULL
mydf_total$text1 <- NULL
mydf_total$text2 <- NULL
mydf_total$text3<- NULL
mydf_total$text4 <- NULL
mydf_total$text5 <- NULL

colnames(mydf_total)[2] <- 'text'

binary <- c(1,1,0,1,1,1,1,0,1,0,0,0,1,1,0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0)
binary <- data_frame(line = 1:length(binary), binary = binary)

mydf_binary <- bind_cols(mydf_total, binary)
mydf_binary$line1 <- NULL


mydf_total <-  bind_rows(mutate(mydf1, question = "First"),
                         mutate(mydf2, question = "Second"),
                         mutate(mydf3, question = "Third"),
                         mutate(mydf4, question = "Fourth"),
                         mutate(mydf5, question = "Fifth"))%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words)



########################################
# what according to you is considered as being environmental friendly?
########################################

# sum of development 2016
sum_dev16 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise('Sum' = sum(value))
#count(word, value, sort=T)


# mean of development 2016
mean_dev16 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))
#count(word, value, sort=T)

# standard deviation of development 2016
sd_dev16 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sd(value))
#count(word, value, sort=T)

# printing the results of the three calcuations above
print(c(sum_dev16, mean_dev16, sd_dev16))


# tell us about a time you think you positively impacted climate change?
# sum of development 2020
sum_dev20 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum(value))
#count(word, value, sort=T)


# mean of development 2020
mean_dev20 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))
#count(word, value, sort=T)

# standard deviation of development 2020
sd_dev20 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sd(value))
#count(word, value, sort=T)

# printing the results of the three calcuations above
print(c(sum_dev20, mean_dev20, sd_dev20))




#########################
# what do you think you can do better to help avert climate?
#########################

# sum of projection 2016
sum_proj16 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum(value))
#count(word, value, sort=T)


# mean of projection 2016
mean_proj16 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))
#count(word, value, sort=T)

# standard deviation of projection 2016
sd_proj16 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sd(value))
#count(word, value, sort=T)

# printing the results of the three calcuations above
print(c(sum_proj16, mean_proj16, sd_proj16))


# what do you think has the most amount of negative impact in the environment?
# sum of projection 2020
sum_proj20 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum_1 = sum(value), mean_1 = mean(value))
#count(word, value, sort=T)


# mean of projection 2020
mean_proj20 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))
#count(word, value, sort=T)

# standard deviation of projection 2020
sd_proj20 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sd(value))
#count(word, value, sort=T)

# printing the results of the three calcuations above
print(c(sum_proj20, mean_proj20, sd_proj20))


##############################
# have you heard about the term bleaching?
##############################

# sum of monetary policy 2016
sum_mon16 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum(value))
#count(word, value, sort=T)


# mean of monetary policy 2016
mean_mon16 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
 #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))
#count(word, value, sort=T)

# standard deviation of monetary policy 2016
sd_mon16 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sd(value))
#count(word, value, sort=T)

# printing the results of the three calcuations above
print(c(sum_mon16, mean_mon16, sd_mon16))

# do you think the world can resolve the issue of climate change in the coming decade?
# sum of monetary policy 2020
sum_mon20 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sum(value))
#count(word, value, sort=T)


# mean of monetary policy 2020
mean_mon20 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(mean(value))
#count(word, value, sort=T)

# standard deviation of monetary policy 2020
sd_mon20 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  #anti_join(my_junk)%>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sd(value))
#count(word, value, sort=T)

# printing the results of the three calcuations above
print(c(sum_mon20, mean_mon20, sd_mon20))

######################################
# TOPIC AND DTMS / Machine learning
######################################



#calling the Latent Dirichlet Allocation algorithm
dtm

ap_lda <- LDA(mydf_total$word, k=3, control=list(seed=123))
ap_lda

#now we are looking for the per topic per word probabilities aka. beta
#beta - what is the probability that "this term" will be generated by "this topic"
library(tidytext)
ap_topics <- tidy(ap_lda, matrix="beta")
ap_topics
library(ggplot2)
library(dplyr)

top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()


library(topicmodels)

by_question_dtm <- mydf_total%>%
  unnest_tokens(word, text)%>%
  anti_join(stop_words) %>%
  count(question, word, sort = TRUE) %>%
  cast_dtm(question, word, n)

question_lda <- LDA(by_question_dtm, k=4, control = list(seed=123))
question_lda

question_topic <- tidy(question_lda, matrix="beta")
question_topic 

top_terms <- question_topic %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
top_terms

#lets plot the term frequencies by topic
top_terms %>%
  mutate(term=reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend=FALSE) +
  facet_wrap(~topic, scales = "free") +
  coord_flip()


question_gamma %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_boxplot()+
  facet_wrap(~topic)

question_classifications <- question_gamma %>%
  group_by(topic, document) %>%
  top_n(1, gamma) %>%
  ungroup()

question_classifications


top_terms <- question_gamma %>%
  group_by(document) %>%
  top_n(3, gamma) %>%
  ungroup() %>%
  arrange(topic, -gamma)
top_terms

#######################################################################
# what according to you is considered as being environmental friendly?
#######################################################################
library(wordcloud)
data(stop_words)

tidy_1 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_1 #look at trump - he is positive!!! :)
library(plotly)

tidy_1 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_1 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,, scale=c(1.0, 1.0), fixed.asp=TRUE, title.size=1)


#########################################################################
# tell us about a time you think you positively impacted climate change?
############################################################################

tidy_2 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_2 #look at trump - he is positive!!! :)

tidy_2 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_2 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,, scale=c(1.0, 1.0), fixed.asp=TRUE, title.size=1)

############################################################
# what do you think you can do better to help avert climate?
###########################################################

tidy_3 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_3 #look at trump - he is positive!!! :)

tidy_3 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_3 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,, scale=c(1.0, 1.0), fixed.asp=TRUE, title.size=1)

################################################################################
# what do you think has the most amount of negative impact in the environment?
#################################################################################

tidy_4 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_4 #look at trump - he is positive!!! :)

tidy_4 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_4 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,, scale=c(1.0, 1.0), fixed.asp=TRUE, title.size=1)


#########################################
# have you heard about the term bleaching?
#########################################

tidy_5 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_5 #look at trump - he is positive!!! :)

tidy_5 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_5 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,, scale=c(1.0, 1.0), fixed.asp=TRUE, title.size=1)


##########################################################################################
# do you think the world can resolve the issue of climate change in the coming decade?
############################################################################################
tidy_6 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk) %>%
  anti_join(stop_words) %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

tidy_6 #look at trump - he is positive!!! :)

tidy_6 %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()

tidy_6 %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort=TRUE) %>%
  acast(word ~sentiment, value.var="n", fill=0) %>%
  comparison.cloud(colors = c("grey20", "gray80"),
                   max.words=100,, scale=c(1.0, 1.0), fixed.asp=TRUE, title.size=1)




#let's look at the data
original_question <- mydf_total %>%
  count(question, word, sort=TRUE) %>%
  ungroup()

total_words <- original_question %>%
  group_by(question) %>%
  summarize(total=sum(n))

question_words <- left_join(original_question, total_words)


######################################
########## ZIPF's law ################
######################################

freq_by_rank <- question_words %>%
  group_by(question) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)


###################################################
################# TF_IDF ##########################
###################################################

question_words <- question_words %>%
  bind_tf_idf(word, question, n)


question_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?


#############
# looking at the graphical apprach:
question_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

#########################################

tidy_11 <- mydf1 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk)
  #anti_join(stop_words) %>%
  #inner_join(get_sentiments("bing")) %>%
  #count(word, sentiment, sort=T) %>%
  #ungroup()

tidy_22 <- mydf2 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk)
  #anti_join(stop_words) %>%
  #inner_join(get_sentiments("bing")) %>%
  #count(word, sentiment, sort=T) %>%
  #ungroup()

tidy_33 <- mydf3 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk)

tidy_44 <- mydf4 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk)

tidy_55 <- mydf5 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk)

tidy_66 <- mydf6 %>%
  unnest_tokens(word, text) %>%
  anti_join(my_junk)

mydf_total <-  bind_rows(mutate(mydf1, question = "First"),
                         mutate(mydf2, question = "Second"),
                         mutate(mydf3, question = "Third"),
                         mutate(mydf4, question = "Fourth"),
                         mutate(mydf5, question = "Fifth"),
                         mutate(mydf6, question = "Sixth"))

report_bigrams <- mydf_total %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2)

report_bigrams #We want to see the bigrams (words that appear together, "pairs")

report_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words from the bigram data, we need to use the separate function:
#library(tidyr)
bigrams_separated <- report_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts


bigram_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep=" ") #we need to unite what we split in the previous section

bigram_tf_idf <- bigram_united %>%
  count(question, bigram) %>%
  bind_tf_idf(bigram, question, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

library(igraph)
bigram_graph <- bigram_counts %>%
  filter(n>1) %>%
  graph_from_data_frame()

bigram_graph

#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)




library(quanteda)
library(RColorBrewer)
library(caret)

#install.packages('caret', dependencies = TRUE)
#we need to convert the VCorpus from the previous point to
#a regular corpus using the corpus() function.
sim_corpus <- corpus(mydf_binary$text) #creating the corpus on the $text var
msg.dfm <- dfm(sim_corpus, tolower = TRUE) #generating document 
msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 11, min_docfreq = 0)
msg.dfm <- dfm_weight(msg.dfm)

head(msg.dfm)
#let's split the docs into training and testing data
msg.dfm.train<-msg.dfm[1:39,]
msg.dfm.test<-msg.dfm[40:45,]


#building the Naive Bayes model:
NB_classifier <- textmodel_nb(msg.dfm.train, mydf_binary$binary[1:39]) # we need to tell which 1 and 0 to use
NB_classifier
summary(NB_classifier)

# predicting the testing data
pred <- predict(NB_classifier, msg.dfm.test)
pred

c <- confusionMatrix(table(mydf_binary$binary[40:45], pred), mode = 'prec_recall')




my_junk <- data_frame(
  word= c("environmentally", "friendly"),
  lexicon = "junk"
)

#let's look at the data
original_question <- mydf_total %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)%>%
  anti_join(my_junk)%>%
  count(question, word, sort=TRUE) %>%
  ungroup()

total_words <- original_question %>%
  group_by(question) %>%
  summarize(total=sum(n))

question_words <- left_join(original_question, total_words)

print(question_words)

library(ggplot2)
ggplot(question_words, aes(n/total, fill = question))+
  geom_histogram(show.legend=FALSE, bins = 10)+
  #xlim(NA, 0.001) +
  facet_wrap(~question, ncol=2, scales="free_y")
#what do the tails represent? 
#answer: exremely common words! 
# we are really interested in the not so common words. 

######################################
########## ZIPF's law ################
######################################

freq_by_rank <- question_words %>%
  group_by(question) %>%
  mutate(rank = row_number(),
         `term frequency` = n/total)
freq_by_rank

#let's plot ZIPF's Law
freq_by_rank %>%
  ggplot(aes(rank, `term frequency`, color=question))+
  #let's add a tangent line , the first derivative, and see what the slop is
  geom_abline(intercept=-0.62, slope= -1.1, color='gray50', linetype=2)+
  geom_line(size= 1.1, alpha = 0.8, show.legend = FALSE)+
  scale_x_log10()+
  scale_y_log10()

###################################################
################# TF_IDF ##########################
###################################################

question_words <- question_words %>%
  bind_tf_idf(word, question, n)

question_words # we get all the zeors because we are looking at stop words ... too common

question_words %>%
  arrange(desc(tf_idf))
#what can we say about these words?


#############
# looking at the graphical apprach:
question_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(question) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=question))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~question, ncol=2, scales="free")+
  coord_flip()

