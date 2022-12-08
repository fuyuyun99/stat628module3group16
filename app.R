# This is the shiny app of suggestions to cinemas in US
#

library(tidyverse)
library(dplyr)
library(shiny)
library(tokenizers)
library(stopwords)
library(ggplot2)
load("reviewdata.Rdata")
load("business.Rdata")
business_cinema <- read.csv("business_cinema.csv")
keywords <- read.table("keywords.txt",header = FALSE)
keywords <- unlist(keywords)
keywords<- as.vector(keywords)
data<-read.csv("finaldata.csv")
load("words.Rdata")
population<-table(data$stars)
data_sentence <- read.csv("sentence_score.csv")
load("corpus.Rdata")


ui <- fluidPage(
  headerPanel("Cinema Suggestions"),      
  sidebarPanel(
    wellPanel(
      selectInput(
        inputId = "cinema",
        label = "Select Cinema",
        choices = sort(business_cinema$name),
        sort(business_cinema$name)[7]
      ),
      submitButton("Submit")
    ),
    textOutput(outputId = "contact_info")
    
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("scores",
               plotOutput(outputId = "plot1",width = "100%"),
               plotOutput(outputId = "plot2"),
               plotOutput(outputId = "plot3"),
               plotOutput(outputId = "plot4")
               
      ),
      tabPanel("suggestions",
               p(strong("Suggestions: "),style="font-size:200%"),
               span(textOutput(outputId = "suggestion"), style="font-size:120%"),
               
            
      )
    )
    
    
  )

)

testfunction <- function(name){
  business_cinema$business_id[which(business_cinema$name==name)]
}

score <- function(name){
  r <- which(grepl(name, reviewdata$business_id))
  txt <- words[r]
  c <- 1:length(keywords)
  for (i in 1:length(keywords)) {
    a <- 1:length(txt)
    for (j in 1:length(txt)) {
      a[j] <- keywords[i]%in%txt[[j]]
    }
    b <- reviewdata[which(a==TRUE),]
    c[i] <- mean(b[,7])
    names(c) <- keywords
  }
  c
}

movie<-c("price","expensive","cheap","imax")
ambiance<-c("dirty","clean","crowd","park","service","bathroom")
food<-c("food","popcorn","snack","drink","water","beer","soda")

business_score<-function(businessid,words){
  businessdata<-subset(data_sentence,business_id==businessid)
  score<-rep(0,length(words))
  for (i in 1:length(words)) {
    if(length(grep(words[i],businessdata$sentence))>0){
      score[i]<-mean(businessdata[grep(words[i],businessdata$sentence),6])
    } else {
      score[i]<-NA
    }
  }
  return(score)
}


overall_detail<-rep(0,length(movie)+length(ambiance)+length(food))
names(overall_detail)<-c(movie,ambiance,food)
overall_main<-rep(0,3)
names(overall_main)<-c("movie","ambiance","food")
movie_weight<-table(unlist(words))[movie]/sum(table(unlist(words))[movie])
names(movie_weight)<-movie
ambiance_weight<-table(unlist(words))[ambiance]/sum(table(unlist(words))[ambiance])
names(ambiance_weight)<-ambiance
food_weight<-table(unlist(words))[food]/sum(table(unlist(words))[food])
names(food_weight)<-food
for(i in 1:length(overall_detail)){
  overall_detail[i]<-mean(business[,names(overall_detail)[i]],na.rm = TRUE)
}

overall_main[1]<-overall_detail[movie]%*%movie_weight
overall_main[2]<-overall_detail[ambiance]%*%ambiance_weight
overall_main[3]<-overall_detail[food]%*%food_weight


movie_plot<-function(businessid){
  x<-unlist(c(business[business$business_id==businessid,movie]))
  x<-subset(x,is.na(x)==F)
  if(length(x)==0){
    return(barplot(c(3.08,3.2,3), col = rainbow(3), horiz = T, xlab = "movie related score"))
  }
  else{return(barplot(x, col = rainbow(length(x)), horiz = T, xlab = "movie related score"))
  }}

ambiance_plot<-function(businessid){
  x<-unlist(c(business[business$business_id==businessid,ambiance]))
  x<-subset(x,is.na(x)==F)
  if(length(x)==0){
    return(barplot(c(2,2.62,3,3.09,2.31,4), col = rainbow(3), horiz = T, xlab = "ambiance related score"))
  }
  else{return(barplot(x, col = rainbow(length(x)), horiz = T, xlab = "ambiance related score"))
  }}

food_plot<-function(businessid){
  x<-unlist(c(business[business$business_id==businessid,food]))
  x<-subset(x,is.na(x)==F)
  if(length(x)==0){
    return(barplot(c(3.25,3.5,2.71), col = rainbow(3), horiz = T, xlab = "food related score"))
  }
  else{return(barplot(x, col = rainbow(length(x)), horiz = T, xlab = "food related score"))
  }}

overall_plot<-function(businessid){
  #movie
  x_movie<-unlist(c(business[business$business_id==businessid,movie]))
  x_movie<-subset(x_movie,is.na(x_movie)==F)
  weight_movie<-table(unlist(words))[names(x_movie)]/sum(table(unlist(words))[names(x_movie)])
  movie_score<-x_movie%*%weight_movie
  #ambiance
  x_ambiance<-unlist(c(business[business$business_id==businessid,ambiance]))
  x_ambiance<-subset(x_ambiance,is.na(x_ambiance)==F)
  weight_ambiance<-table(unlist(words))[names(x_ambiance)]/sum(table(unlist(words))[names(x_ambiance)])
  ambiance_score<-x_ambiance%*%weight_ambiance
  #food
  x_food<-unlist(c(business[business$business_id==businessid,food]))
  x_food<-subset(x_food,is.na(x_food)==F)
  weight_food<-table(unlist(words))[names(x_food)]/sum(table(unlist(words))[names(x_food)])
  food_score<-x_food%*%weight_food
  
  x<-c(movie_score,ambiance_score,food_score)
  names(x)<-c("movie","ambiance","food")
  return(x)
  
}

ov <- function(x){
  return(barplot(x, col = rainbow(length(x)), ylab = "overall score",,ylim=c(min(x,0),max(x,overall_main)+0.3)))
}

recommendation<-function(businessid){
  x<-c(t(business[business$business_id==businessid,c(movie,ambiance,food)]))
  names(x)<-c(movie,ambiance,food)
  x<-subset(x,is.na(x)==F)
  position<-match(names(x),corpus[,1])
  if(length(position)==0){
    return("\nPrice is your one of your main competitiveness.\nYour customers are always many, congrats!\nYou should consider hiring more servants.\nDining here is a pleasure.\nPopcorn is great!\nSnacks taste good!\nCustomers love your drinks.\nRiding a bike is a convenient and cheap way for most perople to reach cinemas, we propose to build more bike parking spaces.\nPeople love outdoor seats while waiting for the movies, it is recommended to build more outdoor seats.\nThe cinema is not good for kids, which helps you get higher ratings.")
  } else {
    text<-""
    for (i in 1:length(position)) {
      if(is.na(position[i])==F){
        if(x[i]>overall_detail[names(x)[i]]){
          text<-paste(text,corpus[position[i],2],sep = "\n")
        } else {
          text<-paste(text,corpus[position[i],3],sep = "\n")
        }
      }
    }
    if(is.na(business_cinema[business_cinema$business_id==businessid,"attributes.BikeParking"])==F){
      if(business_cinema[business_cinema$business_id==businessid,"attributes.BikeParking"]==T){
        text<-paste(text,corpus[12,2],sep = "\n")
      } else {
        text<-paste(text,corpus[12,3],sep = "\n")
      }
    } 
    if(is.na(business_cinema[business_cinema$business_id==businessid,"attributes.OutdoorSeating"])==F){
      if(business_cinema[business_cinema$business_id==businessid,"attributes.OutdoorSeating"]==T){
        text<-paste(text,corpus[13,2],sep = "\n")
      } else {
        text<-paste(text,corpus[13,3],sep = "\n")
      }
    } 
    if(is.na(business_cinema[business_cinema$business_id==businessid,"attributes.GoodForKids"])==F){
      if(business_cinema[business_cinema$business_id==businessid,"attributes.GoodForKids"]==T){
        text<-paste(text,corpus[14,3],sep = "\n")
      } else {
        text<-paste(text,corpus[14,2],sep = "\n")
      }
    } 
    return(text)
  }
}


server<-function(input,output){
  
  
  output$suggestion <- renderText({
    paste(recommendation(testfunction(input$cinema)))
  })
  
  
  output$plot1 <- renderPlot({
    if(overall_plot(testfunction(input$cinema))[1]==0){
      movie_plot("ORL4JE6tz3rJxVqkdKfegA")
    }
    
    else{
      movie_plot(testfunction(input$cinema))}
   
    
  })
  
  output$plot2 <- renderPlot({
    if(overall_plot(testfunction(input$cinema))[1]==0){
      ambiance_plot("ORL4JE6tz3rJxVqkdKfegA")
    }
    
    else{
      ambiance_plot(testfunction(input$cinema))}
    
    
  })
  
  output$plot3 <- renderPlot({
    if(overall_plot(testfunction(input$cinema))[1]==0){
      food_plot("ORL4JE6tz3rJxVqkdKfegA")
    }
    
    else{
      food_plot(testfunction(input$cinema))}
    
    
  })
  
  
  
  output$plot4 <- renderPlot({
    if(overall_plot(testfunction(input$cinema))[1]==0|overall_plot(testfunction(input$cinema))[2]==0|overall_plot(testfunction(input$cinema))[3]==0){
      ov(overall_plot("ORL4JE6tz3rJxVqkdKfegA"))
    }else{
      ov(overall_plot(testfunction(input$cinema)))
    }
    
    points(x=c(0.7,1.9,3.1),y=overall_main,pch=19)
    legend("topright", pch=19, legend="overall score", horiz=TRUE)
    
  })
  
  output$contact_info <- renderText({
    paste("Contact us by xli2422@wisc.edu ")})
  
}

shinyApp(ui,server)
