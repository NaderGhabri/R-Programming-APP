library(shiny)
library(shinythemes)
library(highcharter)
library(tidyverse)
library(DT)
library(ggplot2)
library(hflights)

ui<-fluidPage(
  navbarPage( "Analyse Exploratoire des Donnees",
              tabPanel("Statistiques ",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "compagnie",
                                       label="compagnie:",
                                       choices=unique(hflights$UniqueCarrier),
                                       selected = ""),
                           selectInput(inputId = "mois",
                                       label="mois:",
                                       choices=unique(hflights$Month),
                                       selected=1),
                           selectInput(inputId = "Destination",
                                       label="Destination:",
                                       choices=unique(hflights$Dest),
                                       selected=1),
                         ),
                         mainPanel(
                           h4(textOutput(outputId = "var1")),
                           br(),
                           tableOutput(outputId = "stat1"),
                           br(),
                           br(),
                           h4(textOutput(outputId = "var2")),
                           br(),
                           tableOutput(outputId = "stat2"),
                           br(),
                           br(),
                           h4(textOutput(outputId = "var3")),
                           br(),
                           tableOutput(outputId = "stat")
                         )
                         
                       )
                       
              ),
              
              tabPanel("Visualisation des Donnees",
                       sidebarLayout(
                         sidebarPanel(
                           selectInput(inputId = "Destinationn",
                                       label="Destination:",
                                       choices=unique(hflights$Dest),
                                       selected=1),
                           sliderInput(inputId = "observ",
                                       label="Top-n:",
                                       value=5,
                                       min=1,
                                       max=15
                           )
                         ),
                         mainPanel(
                           plotOutput(outputId = "graphe1"),
                           plotOutput(outputId = "graphe2")
                         )
                       )
              ),
              tabPanel("Filtrage des donnees ",
                       sidebarLayout(
                         sidebarPanel(
                           sliderInput(inputId = "moiss",
                                       label="Mois:",
                                       value=3,
                                       min=1,
                                       max=12
                           ),
                           selectInput(inputId = "compagne",
                                       label="Compagne:",
                                       choices=unique(hflights$UniqueCarrier),
                                       selected=1),
                           numericInput(inputId = "observv",
                                        label="Nombre d'observations a afficher:",
                                        value=10),
                         ),
                         mainPanel(
                           h4(textOutput(outputId="observation")),
                           br(),
                           h4(dataTableOutput(outputId = "table")),
                           
                         )
                         
                       )
                       
              ),
  )
)
server<-function(input,output){
  output$var1<-renderText({
    c("Statistiques Descriptive du temps de retard au depart")
  })
  output$var2<-renderText({
    c("Statistiques Descriptive du temps de retard a l'arrive")
  })
 
  output$stat1<-renderTable({
    hflights %>%
      filter(UniqueCarrier==input$compagnie & Month==input$mois & Dest==input$Destination) %>%
      summarise(Moyenne=round(mean(DepDelay,na.rm=T),3),
                Ecart_Type=round(sd(DepDelay,na.rm=T),3),
                Maximum=round(max(DepDelay,na.rm=T),3),
                Minimum=round(min(DepDelay,na.rm=T),3))
  })
  output$stat2<-renderTable({
    hflights %>%
      filter(UniqueCarrier==input$compagnie & Month==input$mois & Dest==input$Destination) %>%
      summarise(Moyenne=round(mean(ArrDelay,na.rm=T),3),
                Ecart_Type=round(sd(ArrDelay,na.rm=T),3),
                Maximum=round(max(ArrDelay,na.rm=T),3),
                Minimum=round(min(ArrDelay,na.rm=T),3))
  })
  

  output$graphe1<-renderPlot({
    hflights %>%
      group_by(UniqueCarrier) %>%
      filter(Dest==input$Destinationn) %>%
      summarise(Mean=mean(DepDelay,na.rm=T)) %>%
      head(n=input$observ) %>%
      ggplot(aes(x=reorder(UniqueCarrier,-
                             Mean),y=Mean,fill=UniqueCarrier))+
      geom_col()+
      theme_bw()+
      ggtitle("Les 5 Compagnies ayant le retards au depart les plus longs")
  })
  output$graphe2<-renderPlot({
    hflights %>%
      group_by(UniqueCarrier) %>%
      filter(Dest==input$Destinationn) %>%
      summarise(Mean2=mean(ArrDelay,na.rm=T)) %>%
      head(n=input$observ) %>%
      arrange(desc(Mean2)) %>%
      ggplot(aes(x=reorder(UniqueCarrier,-
                             Mean2),y=Mean2,fill=UniqueCarrier))+
      geom_col()+
      theme_bw()+
      ggtitle("Les 5 Compagnies ayant le retards a l'arrivee les plus longs")
  })
  
 
  output$table<-renderDataTable({
    d<-hflights %>%
      select(UniqueCarrier,Dest,Month,DepDelay,ArrDelay) %>%
      filter(Month==input$moiss & UniqueCarrier==input$compagne) %>%
      arrange(desc(ArrDelay)) %>%
      arrange(desc(DepDelay)) %>%
      head(n=input$observv)
    DT::datatable(d)
  })
  
}

shinyApp(ui=ui,server=server)
