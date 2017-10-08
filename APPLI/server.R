library(shiny)
library(shinythemes)
require (Rfacebook)
library(devtools)
library(httr) 
library(Rfacebook)
library(httpuv) 
library(Rook)
library(rjson)
library(plyr)
library(reshape2)
library(DT)
fb_oauth <- fbOAuth(app_id="1008151552621835", app_secret="d630eeef72f473974f0d33db3dac2e21",extended_permissions = TRUE) 
shinyServer(function(input, output) {
  
 
output$value <- renderPrint({ input$text }) 

data<-reactive({
page <- getPage(page=input$text, token=fb_oauth, n=100)
return(page)
})

output$table<-DT::renderDataTable(
  DT::datatable(data(),class='compact',options=list( initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#3182bd', 'color': '#fff'});",
    "}")     
  )))
dataaa<-reactive({
  pages<-searchPages(string = "zara",token=fb_oauth,n=10)
  return(pages)
})

output$tablee<-DT::renderDataTable(
  DT::datatable(dataaa(),class='compact',options=list( initComplete = JS(
    "function(settings, json) {",
    "$(this.api().table().header()).css({'background-color': '#3182bd', 'color': '#fff'});",
    "}")     
  )))

output$like<-renderPrint({
  sum(data()$likes_count)
})


output$comment<-renderPrint({
  sum(data()$comments_count)
})

output$share<-renderPrint({
  sum(data()$shares_count)
})


output$summary<-renderPrint({
  data()[which.max(data()$likes_count),]
  })

output$summary1<-renderPrint({
data()[which.max(data()$comments_count),]
})

output$summary2<-renderPrint({
data()[which.max(data()$shares_count),]
})

posti <- reactive({
  summary <- data()[which.max(data()$likes_count),]
pos<-getPost(summary$id[1], token=fb_oauth, comments = FALSE, n.likes=2000)
poos<-as.data.frame(pos)
  return(poos)

})


output$tab<-DT::renderDataTable(
  DT::datatable(posti(),class='compact',options=list( initComplete = JS(
    "function(settings, json) {",
   "$(this.api().table().header()).css({'background-color': '#3182bd', 'color': '#fff'});",
   "}")     
 )))

##17. Extract Reactions for most recent post
output$postii<- renderPrint({
  getReactions(post=data()$id[1], token=fb_oauth)

})


output$posty<- renderPrint({
  data()[1,]
  
}
)




})

