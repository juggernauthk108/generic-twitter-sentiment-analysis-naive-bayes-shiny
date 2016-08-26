library(twitteR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(shiny)
library(plyr)
library(e1071)
library(tm)
library(wordcloud)
library(plotrix)

shinyServer(
  function(input,output)
  {
    #shows progress of analysis
    progress <- shiny::Progress$new()
    
    #API credentials for mining twitter
    api_key <- "nVGcGe46qu3uxfitphSxDJj80"
    api_secret <- "FkLJLjzLbcFOZ57v9qyL8Ptgeu2NocBy7OoWkU0TicaBktXBDL"
    access_token <- "707147609392156672-ce4wAsaDReG2szox2Cyv50fZJSe71p6"
    access_token_secret <- "8yaxytl0X7fRIm9PbuHjb0cBSHS1BfL6fye7FGb5B14Bo"
    
    
    progress$set(message = "Authenticating your twitter account", value = 0)
    
    #setup twitter
    setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)
    progress$set(message = "Authentication Successful!", value = 0)
    
    progress$close()
    
    #loading the analysis workspace so as to skip machine learning everytime and execute faster
    load("scriptForAnalysis.RData")
    
    #source to functions used
    source("clean.text.R")
    source("getit.R")
    
    #observe for changes in input
    observe(
    {
      progress <- shiny::Progress$new()
      
      progress$set(message = paste("Harvesting Tweets for",input$tag), value = 0)
      
      s=  as.character(input$startdate)
      e =as.character(input$enddate)
      rt = as.character(input$restype)
      
      text = searchTwitter(input$tag , n = input$count ,  since = s , until = e,lang = 'en',resultType = rt) 
      
      progress$set(message = paste("Tweets harvested for",input$tag), value = 0)
      
      tweet_txt = sapply(text, function(x) x$getText())
   
      Result <- as.data.frame(sentimentScore(tweet_txt, vNegTerms, negTerms, posTerms, vPosTerms))
      colnames(Result) <- c('sentence', 'vNeg', 'neg', 'pos', 'vPos')
      prediction = predict(classifier, Result) #predicting polarity
      Result = cbind(Result,prediction)
      
      #sepearting tweets to make seperate wordclouds
      postive_tweets = clean.text(Result$sentence[Result$prediction == "positive"])
      negative_tweets = clean.text(Result$sentence[Result$prediction == "negative"])
      
      #all the positive sentiment word will be stored in positives
      positives = c()
      
      #all the negative sentiment word will be stored in negatives
      negatives = c()
    
      for(x in 1:length(postive_tweets))
      {
        wlist = str_split(clean.text(postive_tweets[x]),'\\s+')
        indiwords = unlist(wlist)
      
        int1 = match(indiwords,posTerms)
        int2 = match(indiwords,vPosTerms)
      
        positives = cbind(positives,getIt(int1,posTerms))
        positives = cbind(positives,getIt(int2,vPosTerms))
      }
    
      for(x in 1:length(negative_tweets))
      {
        wlist = str_split(clean.text(negative_tweets[x]),'\\s+')
        indiwords = unlist(wlist)
      
        int1 = match(indiwords,negTerms)
        int2 = match(indiwords,vNegTerms)
      
        negatives = cbind(negatives,getIt(int1,negTerms))
        negatives = cbind(negatives,getIt(int2,vNegTerms))
      }
    
    
      progress$set(message = "Rendering results", value = 0)
      
      #make corpus of positive words found in mined tweets 
      pos_corpus = Corpus(VectorSource(positives))
      pos_corpus <- tm_map(pos_corpus, removeWords, stopwords('english'))
      
      #make corpus of negative words found in mined tweets 
      neg_corpus = Corpus(VectorSource(negatives))
      neg_corpus <- tm_map(neg_corpus, removeWords, stopwords('english'))
      
      #make corpus of all sentiment words in mined tweets
      alltweet = Corpus(VectorSource(cbind(positives,negatives)))
    
      
      #following are output plots rendered to ui.R
      output$barp = renderPlot(
      {
        barplot(xtabs(~Result$prediction), col = c(3,2) ,las=1,xlab = "Sentiment",ylab = "Magnitude")
      
      })
     
     #for the tweets table
      Tweets = Result$sentence
     Prediction = Result$prediction
     df_users_sentiment1 <- data.frame(Tweets, Prediction)
     output$tweets_table = renderDataTable(
     {
       df_users_sentiment1
     })
     
     #for the positive tweets
     Positive_Tweets = Result$sentence[Result$prediction == "positive"]
     df_users_sentiment2 <- data.frame(Positive_Tweets)
     output$tweets_tablepos = renderDataTable(
     {
       df_users_sentiment2
     })
     
     #for the negative tweets
     Negative_Tweets = Result$sentence[Result$prediction == "negative"]
     df_users_sentiment3 <- data.frame(Negative_Tweets)
     output$tweets_tableneg = renderDataTable(
     {
       df_users_sentiment3
     })
     
     #for 3D pie diagram
     output$piee = renderPlot({
       
       x <- c(sum(Result$prediction == 'positive'),sum(Result$prediction == 'negative'))
       
       percentlabels<- round(100*sum(Result$prediction == 'positive')/(sum(Result$prediction == 'positive')+sum(Result$prediction == 'negative')), 1)
       pielabels<- paste(percentlabels, "%", sep="")
       
       percentlabels1<- round(100*sum(Result$prediction == 'negative')/(sum(Result$prediction == 'positive')+sum(Result$prediction == 'negative')), 1)
       pielabels1<- paste(percentlabels1, "%", sep="")
       
       labelpie = c(pielabels,pielabels1)
       
       pie3D(x ,labels = labelpie ,
             col = c(3,2))
     })
     
    #negative wordcloud
     output$neg = renderPlot(
      {
        wordcloud(neg_corpus, min.freq=1, colors = c("black","red"))
      }
    )
    
    #positive wordcloud
    output$pos = renderPlot(
      {
        wordcloud(pos_corpus, min.freq=1, colors = rainbow(7))
      }
    )
    
    #all words wordcloud
    output$alltweets = renderPlot(
      {
        wordcloud(alltweet, min.freq=1, colors = rainbow(7))
      }
    )
   
    progress$set(message = "Rendered", value = 0)
    progress$set(message = paste(length(text)," Tweets were analysed"), value = 0)
    progress$close()
    #end
  })
   
  })
  
