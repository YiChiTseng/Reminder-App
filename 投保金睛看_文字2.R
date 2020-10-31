library(shiny)
library(shinydashboard)
library(shinyjs)
library(readxl)
df1 <- read_excel("C:/Users/franc/Desktop/投保金睛看/0821.xlsx")
vectorBulletList <- function(vector) {
  if(length(vector > 1)) 
  {
    paste0("<ul><li>", 
           paste0(
             paste0(vector, collpase = ""), collapse = "</li><li>"),
           "</li></ul>")   
  }}

#setup
#################################################################################################

#去掉網址標題
tags$head(tags$style(HTML('.b {font-weight:bold;}')))
header <- dashboardHeader(
  title = strong('投保金睛看'))

################################################################################################

#get rid of sidebar 
sidebar <-dashboardSidebar(disable = TRUE)

################################################################################################

body <- dashboardBody(
  fluidRow(
    box(
          column(2,
                selectInput('Item',
                              "商品類型:",
                              c('選項','投資型','非投資型'))),
           column(2,
                  selectInput('Amount',
                              "保額",
                              c('選項','未滿800萬','800萬以上'))),
           column(2,
                  selectInput("Age",
                              "年齡:",
                              c('選項','未滿20歲','20-69歲','70歲以上'))),
           column(2,
                  selectInput("Job",
                              "職業:",
                              c('選項','學生','家管','退休', '無業','其他'))),
           column(2,
                  selectInput("Source",
                              "保費來源:",
                              c('選項','解約','非解約'))),
           column(2,
                  selectInput("Pay",
                              "保費繳納人:",
                              c('選項','要被保險人','第一順位身故受益人','其他'))),
           column(2,
                  submitButton("查詢"))),
           
    fluidRow(
      align = 'center',
      textOutput('Text3'),
      tags$head(tags$style(HTML('
                            #Text3{
                              font-size: 20px;
                              font-weight:bold;
                            ')))), 
    br(),
    fluidRow(
      box( title = span(img(src='check.png', height = 35),strong("財務告知書")),
           width = '4',
           solidHeader = T, 
           collapsible = T, 
           status = 'primary',
           uiOutput("TextA"),
           tags$head(tags$style(HTML("
                           .form-control {
                            border-radius: 4px 4px 4px 4px;}
                            #Text1{
                              font-size: 20px;
                              width: 700px; 
                              max-width: 100%;
                              padding: 6px 12px;
                              white-space: pre-wrap;
                              verflow-y:scroll;
                              max-height: 700px;
                              font-weight:bold;
                            }
                           ")))),
          
      box( title = span(img(src = 'check.png',height = 35), strong("業報書")), 
           width = '4',
           solidHeader = T, 
           collapsible = T, 
           status = 'primary',
           uiOutput("TextB"), 
           tags$head(tags$style(HTML("
                            }.form-control {
                            border-radius: 4px 4px 4px 4px;}
                            #Text2{
                              font-size: 20px;
                              width: 700px; 
                              max-width: 100%;
                              padding: 6px 12px;
                              white-space: pre-wrap;
                              verflow-y:scroll;
                              max-height: 600px;
                              font-weight:bold;
                            ")))), 
      
       box(title = span(img(src = 'check.png',height = 35), strong("這邊要注意")), 
                width = '4',
                solidHeader = T, 
                collapsible = T, 
                status = 'primary',
                uiOutput("TextC"), 
                tags$head(tags$style(HTML("
                            }.form-control {
                            border-radius: 4px 4px 4px 4px;}
                            #Text2{
                              font-size: 20px;
                              width: 700px; 
                              max-width: 100%;
                              padding: 6px 12px;
                              white-space: pre-wrap;
                              verflow-y:scroll;
                              max-height: 600px;
                              font-weight:bold;
                            ")))), 
      box(title = span(img(src = 'check.png',height = 35), strong("貼心小提醒")), 
          width = '4',
          solidHeader = T, 
          collapsible = T, 
          status = 'primary',
          uiOutput("TextD"), 
          tags$head(tags$style(HTML("
                            }.form-control {
                            border-radius: 4px 4px 4px 4px;}
                            #Text2{
                              font-size: 20px;
                              width: 700px; 
                              max-width: 100%;
                              padding: 6px 12px;
                              white-space: pre-wrap;
                              verflow-y:scroll;
                              max-height: 600px;
                              font-weight:bold;
                            ")))), 
           
      )
      )
)


ui <- dashboardPage(header,sidebar,body,title = "投保金睛看",skin = "blue")

###########################################################################################################

server <- function(input, output){
  
  output$TextA <- renderUI({
    if(input$Item != "選項" &  input$Amount!= "選項" & input$Age != "選項" & input$Job != "選項" & input$Source != "選項" & input$Pay != "選項")
    {
        df1 <-df1[df1$編號 == "A",]
        df1 <- df1[df1$條件 == input$Item |
                   df1$條件 == input$Amount |
                   df1$條件 == input$Age |
                   df1$條件 == input$Job |
                   df1$條件 == input$Source |
                   df1$條件 == input$Pay |
                   df1$條件 == "ALL",]
        Text <-vector()
        for (i in (1: nrow(df1)))
        {
          Text <- c(Text,df1[i,3])
        }
        Text <- gsub('o','',Text)
        Text <- gsub('\r\n','',Text)
        Text <- unique(Text)
        return(HTML(vectorBulletList(Text)))
      }
      else
      {
        return("請選擇上方選項")
      }
      })
  
  output$TextB <- renderText({
    if(input$Item != "選項" &  input$Amount!= "選項" & input$Age != "選項" & input$Job != "選項" & input$Source != "選項" & input$Pay != "選項")
    {
      df1 <-df1[df1$編號 == "B",]
      df1 <- df1[df1$條件 == input$Item |
                   df1$條件 == input$Amount |
                   df1$條件 == input$Age |
                   df1$條件 == input$Job |
                   df1$條件 == input$Source |
                   df1$條件 == input$Pay |
                   df1$條件 == "ALL",]
      Text <-vector()
      for (i in (1: nrow(df1)))
      {
        Text <- c(Text,df1[i,3])
      }
      Text <- gsub('o','',Text)
      Text <- gsub('\r\n','',Text)
      Text <- unique(Text)
      return(HTML(vectorBulletList(Text)))
    }
    else
    {
      return("請選擇上方選項")
    }
  })
  
  output$TextC <- renderText({
    if(input$Item != "選項" &  input$Amount!= "選項" & input$Age != "選項" & input$Job != "選項" & input$Source != "選項" & input$Pay != "選項")
    {
      df1 <-df1[df1$編號 == "C",]
      df1 <- df1[df1$條件 == input$Item |
                   df1$條件 == input$Amount |
                   df1$條件 == input$Age |
                   df1$條件 == input$Job |
                   df1$條件 == input$Source |
                   df1$條件 == input$Pay |
                   df1$條件 == "ALL",]
      Text <-vector()
      for (i in (1: nrow(df1)))
      {
        Text <- c(Text,df1[i,3])
      }
      Text <- gsub('o','',Text)
      Text <- gsub('\r\n','',Text)
      Text <- unique(Text)
      return(HTML(vectorBulletList(Text)))
    }
    else
    {
      return("請選擇上方選項")
    }
  })
  output$TextD <- renderText({
    if(input$Item != "選項" &  input$Amount!= "選項"& input$Age != "選項" & input$Job != "選項" & input$Source != "選項" & input$Pay != "選項")
    {
      b <-paste("<ul><li>累計本公司保額1,001萬，請同時約保額生調。</li><li>客戶健告為「是」，請檢附體況相對應的問卷。</li><li>繳款人若有異動，新繳款人若有貸款/保單貸款/解約，須補「財告書」及完成承保前電訪。</li><li>要保人/被保險人/實際繳交保費人，其中一位70歲(含)以上，需完成銷售過程錄音及承保前電訪</li></ul>")
      return(b)
    }
    else 
    {
      return("請選擇上方選項")
    }
  })
  
 
}
 
shinyApp(ui = ui, server = server)