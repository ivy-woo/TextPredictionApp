library(shiny)
library(shinyWidgets)
library(data.table)
library(markdown)

DT <- readRDS("final.Rds")

lookup <- function(DT, path, ngram=4, nxt=5){
  path <- splitInput(tolower(path))
  
  l <- length(path)
  if(l<(ngram-1)) path <- c("<s>",path)
  else path <- path[(l-ngram+2):l]
  l <- length(path)
  
  output <- list(pred=rep(NA,nxt),key="",nas=1)
  
  output <- searchDT(output,DT,path)
  
  if(!is.na(output$nas)){
    path[[l]] <- "<unk>"
    output <- searchDT(output,DT,path)
  }
  
#  cat("Last path looked up:",output$key, "\n")
  return(output$pred)
}


splitInput <- function(string){
  m <- gregexpr(" |,|\\.",string)
  split <- unlist(regmatches(string, m, invert=NA))
  rmlast <- ifelse(split[length(split)]!="", T, F)
  empty <- which(split==" " | split=="")
  if(length(empty)!=0) split <- split[-empty]
  if(rmlast) return(split[-length(split)])
  else return(split)
}


searchDT <- function(output,DT,path){
  output$key <- paste(c("extrastring",path),collapse=" ")
  
  unk <- FALSE
  while(!is.na(output$nas)){
    chr <- unlist(strsplit(output$key," "))
    if(length(chr)==1) break
    if(unk) output$key <- paste(c("<unk>",chr[-1]),collapse=" ")
    else output$key <- paste(chr[-1],collapse=" ")
    
    rec <- DT[txt==output$key,pred]
    if(length(rec)!=0){
      rec <- unlist(strsplit(rec,"/"))
      rec <- rec[!(rec %in% output$pred)]
      rec <- rec[1:min(sum(is.na(output$pred)),length(rec))] 
      output$pred[1:length(rec)+output$nas-1] <- rec
      output$nas <- match(NA,output$pred)
    }
    unk <- !unk
  }
  
  return(output)
}


ui <- fluidPage(
  
  setBackgroundColor(
    color = c("#F7FBFF", "#AFDAF0"),
    gradient = "linear",
    direction = c("bottom","right")
  ),
  
  tags$style("#reminder {color:dimgrey;}"),
  tags$style("#warning, #warning2 {color:brown;}"),
  tags$style("#pred1, #pred2, #pred3, #pred4, #pred5 
             {font-size:21px; color:darkblue; font-weight:530;}"),
  
  titlePanel("Text Prediction"),
  
  navlistPanel( widths=c(2,10),
    tabPanel("App",
             
             sidebarLayout(
               sidebarPanel(
                 
                 selectInput("npred",
                             h4("How many predictions to output?"),
                             choices=list("1"=1, "2"=2, "3"=3, "4"=4, "5"=5), selected=4),
                 
                 textInput("text","Text input:", placeholder="Type some words here"),
                 
                 htmlOutput("reminder"),
                 htmlOutput("warning"),
                 htmlOutput("warning2"),
               ),
               
               mainPanel(
                 
                 h3("Prediction(s) of the next word:"),
                 textOutput("pred1"),
                 textOutput("pred2"),
                 textOutput("pred3"),
                 textOutput("pred4"),
                 textOutput("pred5"),
               )
             )
    ),
    tabPanel("Information",
             tabsetPanel(type="tabs",
               tabPanel("In-/Output",
                        includeMarkdown("inoutput.md")),
               tabPanel("Support",
                        includeMarkdown("support.md")),
               tabPanel("Scope",
                        includeMarkdown("scope.md"))
             )
    ),
    tabPanel("Contact",
             br(),
             h4("For further documentation/issues reporting please refer to", 
             a("the app's Github repo",href="https://github.com/ivy-woo/TextPredictionApp"),
             "."),
    )
  )

)


server <- function(input, output) {
  
    pred <- reactive({
        lookup(DT,input$text)
    })
    
    output$pred1 <- renderText({ 
      if(length(splitInput(input$text))==0) ""
      else pred()[1] 
    })
    output$pred2 <- renderText({
      if(length(splitInput(input$text))!=0 && input$npred>=2 && !is.na(pred()[2])) pred()[2]
      else ""
    })
    output$pred3 <- renderText({
      if(length(splitInput(input$text))!=0 && input$npred>=3 && !is.na(pred()[3])) pred()[3]
      else ""
    })
    output$pred4 <- renderText({
      if(length(splitInput(input$text))!=0 && input$npred>=4 && !is.na(pred()[4])) pred()[4]
      else ""
    })
    output$pred5 <- renderText({
      if(length(splitInput(input$text))!=0 && input$npred>=5 && !is.na(pred()[5])) pred()[5]
      else ""
    })
    
    output$reminder <- renderUI({
      if(grepl("[A-Za-z]$",input$text)){
        HTML(paste("Remember to add a blank space at the end to let me know the last word is completed!",
                   "(Refer to 'Input' bullet 3 in the 'Information' panel.)",
                   "", "",
                   sep="<br/>"))
      }
    })
      
    output$warning <- renderUI({
       if(grepl("[^A-Za-z,. ]",input$text)){
          HTML(paste("Seems like there is unsupported character in the input.",
                     "Have you typed any non-english word or punctuation that is not comma nor full stop?", 
                     "", "",
                     sep="<br/>"))
      }    
    })
    
    output$warning2 <- renderUI({
      if(grepl("\\.( )*$",input$text)){
          HTML(paste("I see a full stop at the end of your input, 
                     is it the end of a sentence or some kind of abbreviation?",
                     "I do not support prediction for both cases, the output displayed might be funny.",
                     sep="<br/>"))
      }    
    })
    
}


shinyApp(ui = ui, server = server)
