
library(shiny)
library(shinythemes)
library(DT)
library(devtools)


shinyUI(fixedPage(theme=shinytheme("cerulean"),
                  textInput("text", label = h3("Entrez le nom de la boutique"), value = "",width=800),
                  hr(),
                  verbatimTextOutput("value"),
                  submitButton("Run"),
          tabsetPanel(
            
      tabPanel("Data users",
                h4("Comportement des utilisateurs sur la page choisie"),
                DT::dataTableOutput("table")),
       tabPanel("Data pages",
                h4("Affichage des differentes pages de notre produit"),
                DT::dataTableOutput("tablee")),
       tabPanel("Reaction of users",
                h4("Les differentes reactions sur la derniere publication"),
                verbatimTextOutput("postii"),
                verbatimTextOutput("posty")),
       tabPanel("Interactions"  ,
                h4("Somme des 'likes' sur la page choisie" ),
      verbatimTextOutput("like"),
        h4("somme des 'comments' sur la page choisie"),
       verbatimTextOutput("comment" ),
      h4("somme des 'shares' sur la page choisie"),
       verbatimTextOutput("share"),
       h4("La publciation ayant le maximum de nombres de 'Likes'"),       
       verbatimTextOutput("summary"),
       h4("La publication ayant le maximun de nombres de 'comments'"),
       verbatimTextOutput("summary1"),
       h4("La publication ayant le maximum de nombres de 'shares'"),
       verbatimTextOutput("summary2"))
        
    )
        
) )