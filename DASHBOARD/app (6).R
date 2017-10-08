options(shiny.maxRequestSize=10000000000*1024^2)
library(shinydashboard) #shiny
library(shiny) #shiny
library(shinyWidgets)  # for the pickerInput
library(datasets)
library(data.table)
library(leaflet)
library(gplots)
library(plotly)
library(ggplot2)
library(devtools) #plots
library(Information) #IV and WOE
library(ClustOfVar)  #variable reduction #tree

library(ape)  # Phylogenetic trees

library(translateR)

library(Hmisc)  # %nin%
library(DT) # DT table

 library(stats) # Model: Stepwise Logistic Regression Model
library(googleVis)
 library(rpart)
Sys.setlocale("LC_ALL","Arabic")
require(tm)
require(tm.plugin.webmining)
require(SnowballC)

library(plotly)
library(maptools)
library(sp)
library(shapefiles)




###############################################################################
###############################################################################
Sys.setenv('MAPBOX_TOKEN' = 'your_mapbox_token_here')



ui <- dashboardPage( skin = "purple",
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu( 
      
      menuItem("Dashboard",tabName="dash", icon=icon("tachometer") ),
      menuItem("Import data",tabName="import", icon=icon("upload") ),
      menuItem("Settings",tabName="settings", icon=icon("upload") ),
      menuItem("Data Visualization",tabName="dataviz", icon=icon("upload") ),
  
    
      menuItem("Tweets", tabName = "tweets", icon = icon("twitter")),
      menuItem("worldcloud",tabName = "worldcloud",icon=icon("cloud")),
      menuItem("Frequency",tabName = "frequency",icon = icon("bar-chart"))
      
      )
  
  ),
  dashboardBody( 
    tabItems(
    
    tabItem(tabName = "import",
            fluidPage(
            tabBox( id="tabset1", height = "100%",width = "100%" ,
                    tabPanel("Upload and settings", 
            box(width = 5,
                title = " ", status = "primary",
                solidHeader = TRUE ,
                collapsible = TRUE,
                fileInput('file1', 'Choose CSV Fille' , 
                          accept=c('text/csv', 'text/comma-separated-values,text/plain',
                                   '.csv') ),
                actionButton(inputId = "clicks", label="uploadf",icon = icon("refresh")),
                
                tags$hr(),
                
                checkboxInput('header', 'Header', TRUE),
                checkboxInput('stringasfactor', 'Strings as Factors', TRUE),
                radioButtons('sep', 'Separator', c(Comma=',',
                                                   Semicolon=';',
                                                   Tab='\t', space=''), ';'), 
                
                radioButtons('quote', 'Quote',
                             c(None='',
                               'Double Quote'='"',
                               'Single Quote'="'"),
                             '') ,
                uiOutput("tb")
                
            ),
            
            box(title = "chose variables ", status = "warning", solidHeader = TRUE, uiOutput("allvars")),
            
            box(title = " ", status = "primary", solidHeader = TRUE,
                "The variables that has one factor:", 
                
                uiOutput("onefactorvar")
                
            ) ,
            
            
            
           
              fluidPage(
              box(width = 12,
                  title = "Information about the types of variables", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE, collapsed = TRUE, 
                  verbatimTextOutput('strr') 
            ) ),
            
           fluidPage(
              
              box( width=12,
                   title = "summary", status = "primary", solidHeader = TRUE,
                   collapsible = TRUE,collapsed = TRUE,
                   verbatimTextOutput('stats')
              ) 
            
            )),
            tabPanel("Data",
                    fluidPage( 
                     box(title = "Add/Remove a filter for categorical variables ", status = "success", solidHeader = TRUE,
                         
                         helpText("Do you want to add a filter for your data?"),
                         hr(),
                         
                         
                         column(6, actionButton('addFilter', 'Add filter')),
                         offset = 6,
                         
                         tags$div(id = 'placeholderAddRemFilt'),
                         tags$div(id = 'placeholderFilter'),
                         
                         
                         verbatimTextOutput("alot")
                         
                         
                     ),
                     
                     box(width=5,  background = "purple",
                         radioButtons("dist", "Applicate the changes?",
                                      c("No" = "no",
                                        "Yes" = "yes"
                                        
                                      ))),
                     hr(),
                     br(),
                     
                     box(width=12,status = "success",
                     div(style = 'overflow-x: scroll', DT::dataTableOutput('dataimp1') )
                     
                     )
                     )
            )
            
            )
            )
    ),
    #####################################################################################
    tabItem(tabName = "settings",
            
            fluidPage(
              box(
                title= "Types of variables:", status = "danger", solidHeader = TRUE,
                uiOutput("catcheck"),
                
                uiOutput("numcheck"),
                
                actionButton(inputId = "factors11", label="Change to factors",
                             icon = icon("check") )
                
                
              )
       
       )
    ),
    ###################################################################################
    tabItem(tabName = "dataviz",
            
              tabBox( id="tabviz", height = "100%",width = "100%" ,
                      tabPanel("ALL variables",
                               fluidPage(
              box(width=4,title = "All Variables", status = "primary", solidHeader = TRUE,
                  collapsible = TRUE,
                  selectizeInput("var", "Choose a variable:", 
                                 choices = NULL),
                  br(),
                  "Frequences",
                  div(style = 'overflow-x: scroll', tableOutput('percent_table'))
                  
              ),
              
              box(width=8,title = "Good and Bad Distribution ", status = "danger",solidHeader = TRUE,
                  plotlyOutput("plot_distr1")),
              box(width=8, title = "Good and Bad Distribution ", status = "danger",solidHeader = TRUE,
                  plotlyOutput("plot_distr2"))
              
              
              
              
              
                               )
                      ),
              tabPanel("Les prix",
                       fluidPage(
                         box(width=5,status = "danger", solidHeader = TRUE,
                             uiOutput("box_var_prix")),
                         
                         
                         box(width=5,status = "danger", solidHeader = TRUE,
                             uiOutput("box_var_reg")),
                         
                         box(width=4,status = "danger", solidHeader = TRUE,
                          
                             checkboxGroupInput("var_prix", "Selectionnez la methode",
                                            choices=c("Moyenne"="moy", "Max"="max", "Min"="min"))),
                         box(width=8, status = "danger", solidHeader = TRUE,
                             
                             htmlOutput("edades")
                             )
                         
                         
                        
                         
                         
                        
                       )
                       
                       
                       )
              
              
            )
      
    ),
  
    ###################################################################
    tabItem(tabName = "wordcount",
            sliderInput("topn",label="Select top n highest frequency words to display",min=1,max=100,step=1,value=10),
            actionButton("example", "Example"),
            actionButton("*Clear", "clear"),
            
            
            
            tags$p(" Text input:"),
            fluidRow(
              box(width=11,tags$textarea(id="text",rows="5",style="width:100%",""))
            ),
            fluidRow(
              box(width=11,textOutput("char_count"))
            ),
            fluidRow(
              box(width=11,textOutput("total_count"))
            ),
            tags$p(" Seperate word count:"),
            fluidRow(
              box(width=11,plotOutput("seperate_bar",width="30%",height="100px"))
            )
            
    ),           
    
    
    
    
    ##########second tab content
    
    tabItem(tabName = "tweets",
            fixedRow(
              column(12,box(
                title = "Choose Tweets", width = NULL, solidHeader = TRUE, status = "warning",
                
                
                
                fixedRow(
                  column(3, textInput("word2","Choose a subject")),
                  column(3,radioButtons("displaytweet2",  "Select Number of Tweets to Read", 
                                        list("100", "1000", "5000"))),
                  column(3,sliderInput("freq2","Select Number of Tweets to Mine ",min = 100,  max = 1000, value = 500)),
                  column(6,actionButton("extract","Extract and analyse",icon = icon("twitter")))
                )))),
            fluidRow( 
              box(
                title = "Tweets", width = 1000, solidHeader = TRUE, status = "warning",verbatimTextOutput("data1")))
    ),
    
    
    tabItem(tabName = "worldcloud",
            
            
            fluidPage(
              titlePanel("wordcloud"),
              sidebarLayout(
                sidebarPanel(
                  fileInput("wc","Upload a text file for wordcloud",multiple = F,accept = "text/plain"),
                  actionButton("update","create word cloud")
                ),
                fluidRow(
                  box(plotOutput("wcplot")) )
              )
            )
            
    )
 
    ####################################################################
    
    
   
    
    
   
            
              
                      
                              
            
            
            
)
)


)



server <- function(input, output, session) { 
  dattt=reactive({
    inFile=input$file1
    if(is.null(inFile))
      return(NULL)
    
    mabase=read.csv(inFile$datapath, encoding = "UTF-8",  header=input$header, 
                    sep=input$sep, stringsAsFactors = input$stringasfactor)
    
    
   
    
    
    return(as.data.frame(mabase))
  
    
  })
  
  
  output$allvars=renderUI({
    alvar=names(dattt())
    pickerInput(inputId = "allvar", label = "Select the variables you want", choices = alvar,
                 multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `deselect-all-text` = "None...",
                  `select-all-text` = "Yeah, all !",
                  `none-selected-text` = "zero"
                ))
    
  })
  
  output$onefactorvar=renderUI({
    
    N=NCOL(dattt())
    listnum=c(names(dattt()))
    ll=NULL
    
    for (i in 1:N  ) {
      if (length(levels(factor( as.factor(dattt()[,i]))) ) <2) ll= c(ll , listnum[i]) 
    }
    
    
    checkboxGroupInput("1factvar", "  ",   choices =  ll, selected = ll , inline = TRUE )
    
    
  })
  
  
  
  output$strr=renderPrint({
    str(datas())
  })
  
  output$stats=renderPrint({
    
    summary(datas())
    
  })
  
  sample=reactive({
    return(head(datas(), 50))
    
  })
  
  output$dataimp1 = DT::renderDataTable({
    sample()
  }, options = list( lengthMenu = c(5, 10,25 ) ,pageLength = 10, lengthChange = TRUE, autoWidth = FALSE))
  
  
  
  output$tb=renderUI({
    tableOutput("dataimp1")
  })
  
  headers=reactive({
    l2=NULL
    list_name=c(names(datas()))
    for (i in 1:NCOL(datas())  ) {
      if ( is.factor(datas()[,i]) ==TRUE ) l2= c(l2 , list_name[i]) 
    }
   return( l2 )
  })
  
  filter <- character(0)
  
  makeReactiveBinding("aggregFilterObserver")
  aggregFilterObserver <- list()
  
  observeEvent(input$addFilter, {
    add <- input$addFilter
    filterId <- paste0('Filter_', add)
    colfilterId <- paste0('Col_Filter_', add)
    rowfilterId <- paste0('Row_Filter_', add)
    removeFilterId <- paste0('Remove_Filter_', add)
    
    
    insertUI(
      selector = "#placeholderFilter",
      where = "afterEnd" ,
      ui = tags$div(id = filterId,
                    actionButton(removeFilterId, label = "Remove filter"),
                    selectInput(colfilterId, label = NULL, choices = headers(), selected =headers()[1]),
                    checkboxGroupInput(rowfilterId, label = "Select variable values",
                                       choices = NULL, selected = NULL)
      )
    )
    
    observeEvent(input[[colfilterId]], {
      
      col <- input[[colfilterId]]
      values <- as.list(unique(datas()[col]))[[1]]
      
      updateCheckboxGroupInput(session, rowfilterId , label = "Select variable values", 
                               choices =values, selected = values,  inline = TRUE)
      
      aggregFilterObserver[[filterId]]$col <<- col
      aggregFilterObserver[[filterId]]$rows <<- NULL
    })
    
    observeEvent(input[[rowfilterId]], {
      
      rows <- input[[rowfilterId]]
      
      aggregFilterObserver[[filterId]]$rows <<- rows
      
    })
    
    
    
    observeEvent(input[[removeFilterId]], {
      removeUI(selector = paste0('#', filterId))
      
      aggregFilterObserver[[filterId]] <<- NULL
      
    })
  })
  
  
  
  base_filtering=reactive({
    
    invisible(lapply(aggregFilterObserver, function(filter){
      
      dd <<- datas()[which(datas()[[filter$col]] %in% filter$rows), ]
      
    }))
    dd
    
    
  })
  
  base_filtered=reactive({
    if (any(input$dist=="no"))
      da=datas()
    else 
      da=base_filtering()
    
    return(da)
  })
  
  
  
  #####################################################################################################
  #################################### Settings ##################################################
  
  datas11=reactive({
    if (is.null(input$allvar)==FALSE) {
      onlyvar= dattt()[,input$allvar]
      
      return(as.data.frame(onlyvar) ) }
    else return(as.data.frame(dattt()) )
    
  })
  
  output$numcheck=renderUI({
    
    N=NCOL(datas11())
    listnum=c(names(datas11()))
    ll=NULL
    
    for (i in 1:N  ) {
      if ( is.numeric(datas11()[,i]) ==TRUE ) ll= c(ll , listnum[i]) 
    }
    
    
    selectizeInput( "numerical", label = "Numerical Variables", choices = ll, multiple=TRUE  ,
                    selected=NULL, options = NULL )
    
  })
  
  output$catcheck=renderUI({
    
    N=NCOL(datas11())
    listnum=c(names(datas11()))
    ll=NULL
    
    for (i in 1:N  ) {
      if ( is.factor(datas11()[,i]) ==TRUE )  {ll= c(ll , listnum[i])
      }
      
    }
    ll=c(ll, input$numerical)
    checkboxGroupInput("catego", "Categorical variables ",   choices =  ll, selected = ll , inline = TRUE )
    
    
  })
  
  
  
  datas=reactive ({
    if (is.null(input$numerical)==FALSE) {
      fff=sapply(datas11()[,input$numerical] , as.factor )
      ffff=datas11()[, which(colnames(datas11()) %nin% input$numerical)] 
      
      ff=cbind.data.frame(fff, ffff)
      mm=as.data.frame(ff)
      
      return(as.data.frame(mm) ) }
    if (is.null(input$numerical)==TRUE) {
      ddd=data.frame(datas11())
      return(ddd)
    }
    
    
  })
  
  
  
  
  
 ####################################################################################################
  ############################# Data Viz ########################################################
  
 
  quali_var_list=reactive({
    lists=NULL
    for (i in 1:NCOL(base_filtered())) {
      if (is.factor(base_filtered()[,i])==TRUE)   lists=c(lists, names(base_filtered())[i] )
    }
    
    return(lists)
  })
  
  quanti_var_list=reactive({
    lists=NULL
    for (i in 1:NCOL(base_filtered())) {
      if (is.numeric(base_filtered()[,i])==TRUE)   lists=c(lists, names(base_filtered())[i] )
    }
    
    return(lists)
  })
  
  quali_data=reactive({
    return(base_filtered()[,quali_var_list()])
  })
  
  quanti_data=reactive({
    return(base_filtered()[,quanti_var_list()] )
  })
  
  
  ######################################################################################
 
 
 observe({
   
   list=c(names(base_filtered()))
   
   updateSelectizeInput(session, "var", label = NULL, choices =c("Select a variable", list),
                        selected="Select a variable", server=TRUE)
 })
 
 first_variable <- reactive({
   for (i in 1:NCOL(base_filtered())) {
     if (input$var==names(base_filtered())[i]) return(base_filtered()[,i])
     
     
   }
 }) 
 
 
 pct=reactive({
   tbl <- table(first_variable())
   tbl_pct=cbind( levels(as.factor(first_variable())),tbl,round(prop.table(tbl)*100,2)) 
   
   colnames(tbl_pct) <- c('Count','Frequence', 'Percentage')
   
   return(tbl_pct ) })
 
 output$percent_table=renderTable({
   
   return((pct())) 
 })

 output$plot_distr1=renderPlotly({
   if (is.factor(first_variable() )==TRUE) {
     
     plot_ly( x=  ~first_variable() , name='number of Goods', type = 'scatter',marker = list(size = 10,
                                                                                             color = 'rgba(255, 182, 193, .9)',
                                                                                             line = list(color = 'rgba(152, 0, 0, .8)',
                                                                                                         width = 2))) %>%   
       
       layout(title = 'Styled Scatter',
              yaxis = list(zeroline = FALSE)
       )
   }
   
   else if (is.numeric(first_variable() )==TRUE) {
     
     densi=density(first_variable())
     plot_ly(y= ~densi$y,  name='Density of the variable', type = 'scatter', mode = 'lines', fill = 'tozeroy') %>%
       layout( title = '', xaxis = list(title = input$var )
       ) 
     
   }
   
 })
 
 
 
 output$plot_distr2=renderPlotly({
   
   plot_ly( x= ~first_variable() , name='number of Goods', type = 'histogram', mode = 'lines') %>% 
     
     layout( title = '', xaxis = list(title = input$var))
   
 })
 
 
 ################## prix maping ##############################
 
 
output$box_var_prix=renderUI({
   
   
   
  selectInput("choix_var_prix", label = NULL, choices =quanti_var_list(),
                        selected=quanti_var_list()[1])
 })
 output$box_var_reg=renderUI({
   
   selectInput( "choix_var_reg", label = NULL, choices =quali_var_list(),
                        selected=quali_var_list()[1])
 })
 
 cast.map =reactive({
  l= aggregate(as.numeric(base_filtered()[,input$choix_var_prix]),
                    by=   list(Regions=as.factor(base_filtered()[,input$choix_var_reg])),mean , na.rm=TRUE )
  ll=as.data.frame(l[-1,])
  return(ll)
 })
 
 output$edades=renderGvis({
   
  
   
   map <- gvisGeoChart(cast.map(),"Regions", "x",
                       options=list(region="TN",displayMode="regions", resolution="provinces",
                                    title="Average age per Province",
                                    
                                    titleTextStyle="{color:'black',fontName:'Courier',fontSize:14}"
                                   ) )
  
                         
   
   
 })

 #############################################################################
 ########################## eda ###############################################
 
 
 myword2 <- reactive({as.character(input$word2)}) 
 mycount2 <- reactive({as.numeric(input$freq2) })
 mydisplaycount2 <- reactive({as.numeric(input$displaytweet2)})
 
 tweets2 <- reactive({ tweetmine(myword2(), mycount2())})
 finaldata2 <- reactive({ tweetclean(tweets2())})
 
 output$M<-renderPrint( finaldata3())
 
 
 
 
 output$data1 <- renderPrint({
   head(tweets2(), mydisplaycount2())
 })
 
 
 
 
 

 

  
  
   }

shinyApp(ui, server)
