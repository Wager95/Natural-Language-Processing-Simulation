#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)


header <- dashboardHeader(title = 'Team 8 Dashboard')

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem(text = 'Introduction',
                 tabName = 'intro'),
        
        menuItem(text = "Statistical Summary",
                 tabName = "summary"
        ), 
        menuItem(text = "ZIPF's Law and TF_IDF", 
                 tabName='zip'
        ),
        menuItem(text='Bigrams', tabName = 'bigram'
                 ),
        menuItem(text = "LDA", 
                 tabName = "lda"
        ),
        menuItem(text = 'Prediction', 
                 tabName = 'pred'),
        
        menuItem(text = 'Conclusion', 
                 tabName = "conclusion")
    )
    , collapsed=TRUE
)



body <- dashboardBody(
    tabItems(
        tabItem(tabName= "intro", fluidRow(imageOutput("intro")
                                           )
                ),
        tabItem(tabName = 'conclusion', fluidRow(imageOutput('conc'))
                ),
        tabItem(tabName = "summary", 
                tabBox(
                    tabPanel("Question 1", 
                             fluidRow(
                                 box("What according to you is considered as being environmentally friendly?", status = 'primary',width = 12),
                                valueBox(
                                    value = 36,
                                    subtitle = "Sum of AFINN Sentiment"
                                ),
                                valueBox(
                                    value = 0.89,
                                    subtitle = "Mean of AFINN Sentiment"
                                    
                                ),
                                valueBox(
                                    value = 1.72,
                                    subtitle="Standard Deviation of AFINN Sentiment"
                                    
                                ),
                                plotOutput("plot")
                                )
                             ),
                    tabPanel("Question 2", 
                             fluidRow(
                                 box("Tell us about a time you think you positively impacted climate change?", status = 'primary',width = 12),
                                 valueBox(
                                     value = 19,
                                     subtitle = "Sum of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = 0.73,
                                     subtitle = "Mean of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = 1.78,
                                     subtitle="Standard Deviation of AFINN Sentiment"
                                     
                                 ), 
                                 plotOutput("plot3")
                             )
                    ),
                    tabPanel("Question 3", 
                             fluidRow(
                                 box("What do you think you can do better to help avert climate change?", status = 'primary',width = 12),
                                 valueBox(
                                     value = -20,
                                     subtitle = "Sum of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = -0.62,
                                     subtitle = "Mean of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = 1.76,
                                     subtitle="Standard Deviation of AFINN Sentiment"
                                     
                                 ), 
                                 plotOutput("plot4")
                             )
                    ),
                    tabPanel("Question 4", 
                             fluidRow(
                                 box("What do you think has the most amount of negative impact on the environment? ", status = 'primary',width = 12),
                                 valueBox(
                                     value = -33,
                                     subtitle = "Sum of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = -1.06,
                                     subtitle = "Mean of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = 2.02,
                                     subtitle="Standard Deviation of AFINN Sentiment"
                                     
                                 ), 
                                 plotOutput("plot5")
                             )
                    ),
                    tabPanel("Question 5", 
                             box("Have you heard about the term 'bleaching'?",status = 'primary',width = 12),
                             fluidRow(
                                 valueBox(
                                     value = -4,
                                     subtitle = "Sum of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = -0.50,
                                     subtitle = "Mean of AFINN Sentiment"
                                     
                                 ),
                                 valueBox(
                                     value = 2.45,
                                     subtitle="Standard Deviation of AFINN Sentiment"
                                     
                                 ), 
                                 plotOutput("plot6")
                             )
                    ),
                    
                    width = 12 )
                ),
        
        tabItem(tabName = "bigram", 
                plotOutput("plot2")
                ),
        
        tabItem(tabName="lda", 
                mainPanel(
                    fluidRow(
                        box(
                            title = "Inputs",status = 'warning',
                            width = 3,
                            sliderInput(
                                "k", "How many clusters do you want?", min = 2, max = 4, value=3)
                            ), 
                        box(
                            title = 'Different Clusters', 
                            status='primary',
                            width = 9,
                            plotOutput("plot8")
                            )
                        )
                    )
                ),
        
        tabItem(
            tabName = 'zip', 
                fluidPage(
                    plotOutput('plot7')
                    )
                ),
        tabItem(
            tabName = "pred", box(
                verbatimTextOutput("pred"), status='primary' ,subtitle = 'Prediction'
                ),
        ))
)


# Define UI for application that draws a histogram
ui <- dashboardPage(header = header,
                    sidebar = sidebar,
                    body = body)



# Define server for outputs in UI
server <- function(input, output) {
    
    # "importing" packages
    
    library(dplyr)
    library(stringr)
    library(tidytext)
    library(textreadr)
    library(textdata)
    library(magrittr)
    library(tm)
    library(ggplot2)
    library(reshape2)
    library(tidyverse)
    library(tidyr)

    
    # Loading the file
    file_s <- read_document(file="simulation1.docx")
    
    a <- length(file_s)/6 # how many observations/people
    b <- 6 # number of variables
    my_df <- as.data.frame(matrix(nrow=a, ncol=b))
    
    for(z in 1:b){
        for(i in 1:a){
            my_df[i,z]<- file_s[i*b+z-b]
        }
    }
    
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
    
    # removing duplicate of line number
    mydf_total$line1 <- NULL
    mydf_total$line2 <- NULL
    mydf_total$line3 <- NULL
    mydf_total$line4 <- NULL
    mydf_total$line5 <- NULL
    
    # putting all the answers per individual together
    mydf_total$all <- paste(mydf_total$text,mydf_total$text1,mydf_total$text2,mydf_total$text3,mydf_total$text4,mydf_total$text5)
   
    # removing the original answers, because now we have one line with all the answers 
    mydf_total$text <- NULL
    mydf_total$text1 <- NULL
    mydf_total$text2 <- NULL
    mydf_total$text3<- NULL
    mydf_total$text4 <- NULL
    mydf_total$text5 <- NULL
    
    # chaning column name to text for later prediction
    colnames(mydf_total)[2] <- 'text'
    
    # creating a binary / success for our data
    binary <- c(1,1,0,1,1,1,1,0,1,0,0,0,1,1,0,1,1,1,1,1,0,0,0,0,1,1,1,1,0,1,0,0,1,0,1,1,1,1,1,1,0,0,0,0,0)
    binary <- data_frame(line = 1:length(binary), binary = binary)
    
    #putting it together
    mydf_binary <- bind_cols(mydf_total, binary)
    #removing duplicate line numbers
    mydf_binary$line1 <- NULL
    
    # data for the main analysis except prediction 
    mydf_total <-  bind_rows(mutate(mydf1, question = "First"),
                             mutate(mydf2, question = "Second"),
                             mutate(mydf3, question = "Third"),
                             mutate(mydf4, question = "Fourth"),
                             mutate(mydf5, question = "Fifth"),
                             mutate(mydf6, question = "Sixth"))
    
    
    ##################################################################
    #                         Prediction Model                       #
    ##################################################################
    output$pred <- renderPrint({
        library(quanteda)
        library(RColorBrewer)
        library(caret)
        #we need to convert the VCorpus from the previous point to
        #a regular corpus using the corpus() function.
        sim_corpus <- corpus(mydf_binary$text) #creating the corpus on the $text var
        msg.dfm <- dfm(sim_corpus, tolower = TRUE) #generating document 
        msg.dfm <- dfm_trim(msg.dfm, min_termfreq = 11, min_docfreq = 0)
        msg.dfm <- dfm_weight(msg.dfm)
        
        #let's split the docs into training and testing data
        msg.dfm.train<-msg.dfm[1:39,]
        msg.dfm.test<-msg.dfm[40:45,]
        
        
        #building the Naive Bayes model:
        NB_classifier <- textmodel_nb(msg.dfm.train, mydf_binary$binary[1:39]) # we need to tell which 1 and 0 to use
        
        
        # predicting the testing data
        pred <- predict(NB_classifier, msg.dfm.test)
        
        ci <- confusionMatrix(table(Prediction = pred, Actual = mydf_binary$binary[40:45]), mode = 'prec_recall')
        
        ci
    })
    
    
    ##########################################################
    # Inputting screenshot of our Introduction and Conclusion#
    ##########################################################
    output$intro <- renderImage({
        library(png)
            return(list(src = "Capture.PNG",contentType = "image/png",alt = "Alignment"))
    }, deleteFile=FALSE)
    
    output$conc <- renderImage({
        library(png)
        return(list(src = "Capture2.PNG",contentType = "image/png",alt = "Alignment"))
    }, deleteFile=FALSE)
    
    
    ############################# Question 1 ################################
    # What according to you is considered as being environmentally friendly?#
    #########################################################################
    output$sum_q1 <- renderTable({
        mydf1 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Sum' = sum(value))%>%
            round(2)
    })
    
    output$mean_q1 <- renderTable({
        mydf1 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Mean' = mean(value))%>%
            round(2)
    })
    
    output$sd_q1 <- renderTable({
        mydf1 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Standard Deviation' = sd(value))%>%
            round(2)
    })
    
    ############################# Question 2 ################################
    # Tell us about a time you think you positively impacted climate change?#
    #########################################################################
    output$sum_q2 <- renderTable({
        mydf2 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Sum' = sum(value))%>%
            round(2)
    })
    
    output$mean_q2 <- renderTable({
        mydf2 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Mean' = mean(value))%>%
            round(2)
    })
    
    output$sd_q2 <- renderTable({
        mydf2 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Standard Deviation' = sd(value))%>%
            round(2)
    })
    
    ############################# Question 3 #############################
    # What do you think you can do better to help avert climate change?  #
    ######################################################################
    output$sum_q3 <- renderTable({
        mydf3 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Sum' = sum(value))%>%
            round(2)
    })
    
    output$mean_q3 <- renderTable({
        mydf3 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Mean' = mean(value))%>%
            round(2)
    })
    
    output$sd_q3 <- renderTable({
        mydf3 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Standard Deviation' = sd(value))%>%
            round(2)
    })
    
    ################################## Question 4 ##################################
    # what do you think has the most amount of negative impact in the environment? #
    ################################################################################
    output$sum_q4 <- renderTable({
        mydf4 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Sum' = sum(value))%>%
            round(2)
    })
    
    output$mean_q4 <- renderTable({
        mydf4 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Mean' = mean(value))%>%
            round(2)
    })
    
    output$sd_q4 <- renderTable({
        mydf4 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Standard Deviation' = sd(value))%>%
            round(2)
    })
    
    #################Question 5#################
    # have you heard about the term bleaching? #
    ############################################
    output$sum_q5 <- renderTable({
        mydf5 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Sum' = sum(value))%>%
            round(2)
    })
    
    output$mean_q5 <- renderTable({
        mydf5 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Mean' = mean(value))%>%
            round(2)
    })
    
    output$sd_q5 <- renderTable({
        mydf5 %>%
            unnest_tokens(word, text) %>%
            anti_join(stop_words) %>%
            inner_join(get_sentiments("afinn"))%>%
            summarise('Standard Deviation' = sd(value))%>%
            round(2)
    })
    
    ######################## CONTRIBUTION TO SENTIMENT ##############################
    output$plot <- renderPlot(
        {
            data(stop_words)
            
            my_junk <- data_frame(
                word= c("environmentally", "friendly"),
                lexicon = "junk"
            )
            
            tidy_1 <- mydf1 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                anti_join(my_junk)%>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_1 
            
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
        }
    )
    ############################ BIGRAM SPIDER WEB######################################
    output$plot2 <- renderPlot(
        {
            my_junk <- data_frame(
                word= c("environmentally","friendly","positively","impacted","bleaching","negative",'change'),
                lexicon = "junk"
            )
            
            mydf_total <-  bind_rows(mutate(mydf1, question = "First"),
                                     mutate(mydf2, question = "Second"),
                                     mutate(mydf3, question = "Third"),
                                     mutate(mydf4, question = "Fourth"),
                                     mutate(mydf5, question = "Fifth"),
                                     mutate(mydf6, question = "Sixth"))
            
            report_bigrams <- mydf_total %>%
                unnest_tokens(bigram, text, token = "ngrams", n=2)
            
            report_bigrams %>%
                count(bigram, sort = TRUE) 
            
           #seperating the bigrams in order to remove stop words, uniting later
            bigrams_separated <- report_bigrams %>%
                separate(bigram, c("word1", "word2"), sep = " ")
            
            bigrams_filtered <- bigrams_separated %>%
                filter(!word1 %in% stop_words$word) %>%
                filter(!word1 %in% my_junk$word) %>%
                filter(!word2 %in% my_junk$word) %>%
                filter(!word2 %in% stop_words$word)
            
            #creating the new bigram, "no-stop-words":
            bigram_counts <- bigrams_filtered %>%
                count(word1, word2, sort = TRUE)
            
            #uniting previous split
            bigram_united <- bigrams_filtered %>%
                unite(bigram, word1, word2, sep=" ") 
            
            bigram_tf_idf <- bigram_united %>%
                count(question, bigram) %>%
                bind_tf_idf(bigram, question, n) %>%
                arrange(desc(tf_idf))
            
            library(igraph)
            bigram_graph <- bigram_counts %>%
                filter(n>1) %>%
                graph_from_data_frame()
            
            
            library(ggraph)
            ggraph(bigram_graph, layout = "fr") +
                geom_edge_link()+
                geom_node_point()+
                geom_node_text(aes(label=name), vjust =1, hjust=1)
        }
    )
    ######################## CONTRIBUTION TO SENTIMENT ##############################
    output$plot3 <- renderPlot(
        {
            
            my_junk <- data_frame(
                word= c("positively"),
                lexicon = "junk"
            )
            
            tidy_2 <- mydf2 %>%
                unnest_tokens(word, text) %>%
                anti_join(my_junk)%>%
                
                anti_join(stop_words) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_2 
            
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
        }
    )
    ######################## CONTRIBUTION TO SENTIMENT ##############################
    output$plot4 <- renderPlot(
        {
            tidy_3 <- mydf3 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_3 
            
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
        }
    )
    ######################## CONTRIBUTION TO SENTIMENT ##############################
    output$plot5 <- renderPlot(
        {
            tidy_4 <- mydf4 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_4 
            
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
        }
    )
    ######################## CONTRIBUTION TO SENTIMENT ##############################
    output$plot6 <- renderPlot(
        {
            tidy_5 <- mydf5 %>%
                unnest_tokens(word, text) %>%
                anti_join(stop_words) %>%
                inner_join(get_sentiments("bing")) %>%
                count(word, sentiment, sort=T) %>%
                ungroup()
            
            tidy_5 
            
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
        }
    )
    ########################## TF-IDF#################################
    output$plot7 <- renderPlot(
        {
            my_junk <- data_frame(
                word= c("environmentally", "friendly"),
                lexicon = "junk"
            )
            
            
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
            
            
            #plotting 
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
        }
    )
    
    
    ####################### LDA #############################
    output$plot8 <- renderPlot({
        library(topicmodels)
        
        my_junk <- data_frame(
            word= c("environmentally", "environment","friendly", 'change', 'don'),
            lexicon = "junk"
        )
        
        
        by_question_dtm <- mydf_total%>%
            unnest_tokens(word, text)%>%
            anti_join(stop_words) %>%
            anti_join(my_junk)%>% 
            count(question, word, sort = TRUE) %>%
            cast_dtm(question, word, n)
        
        question_lda <- LDA(by_question_dtm, k=input$k, control = list(seed=123))
        question_lda
        
        question_topic <- tidy(question_lda, matrix="beta")
        question_topic 
        
        top_terms <- question_topic %>%
            group_by(topic) %>%
            top_n(10, beta) %>%
            ungroup() %>%
            arrange(topic, -beta)
        top_terms
        
        #plotting term frequencies by topic
        top_terms %>%
            mutate(term=reorder(term, beta)) %>%
            ggplot(aes(term, beta, fill = factor(topic))) +
            geom_col(show.legend=FALSE) +
            facet_wrap(~topic, scales = "free") +
            coord_flip()
        
    })
    
}

shinyApp(ui, server)
