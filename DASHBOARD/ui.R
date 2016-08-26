library(shiny)
library(plotrix)
shinyUI(
  fluidPage(
    
  titlePanel(title = "Twitter Sentiment Analysis"),
 #
  sidebarLayout(
    
    sidebarPanel
    (
      textInput("tag","Enter the #TAG",placeholder = "#HASHTAG",value = "#brexit"),
      sliderInput("count","Scroll the number of tweets",min = 25,max = 1500,value = 25),
    selectInput("restype","Type",choices = c("popular","recent","mixed"),selected = "mixed"),
      dateInput("startdate","Since",value = "2016-01-01",format =  "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL),
      dateInput("enddate","Until",value = "2016-08-20",format =  "yyyy-mm-dd", startview = "month", weekstart = 0, language = "en", width = NULL),
     
      submitButton(text = "Analyse" , icon("refresh"))
    
      
    ),
    mainPanel
    (
    
      
      tabsetPanel(
        type = "pills",
        
        tabPanel("Overview",   plotOutput('alltweets'),plotOutput('barp'),plotOutput('piee')),
        tabPanel("All Tweets",dataTableOutput('tweets_table')),
        tabPanel("Positive Tweets",dataTableOutput('tweets_tablepos')),
        tabPanel("Negative Tweets",dataTableOutput('tweets_tableneg')),
        tabPanel("Positive WordCloud", plotOutput('pos')),
        tabPanel("Negative WordCloud", plotOutput('neg'))
        
        )
   
    
      
      
    )
              )
  
              )
)