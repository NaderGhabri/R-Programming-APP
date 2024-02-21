library(shiny)
library(dplyr)
library(ggplot2)
library(gapminder)
data(gapminder)
force(gapminder)
pays<-levels(gapminder$country)

ui<-fluidPage(
  titlePanel("Analyse exploratoire des donnees base GAPMINDER"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "pays",
                  label="Choisir le Pays:",
                  choices=levels(gapminder$country),
                  selected = ""),
      textInput(inputId = "text1",
                label="Donner un titre au graphique:",
                value="Figure 1:"),
      textInput(inputId = "text2",
                label="Donner un titre au graphique:",
                value="Figure 2:"),
      colourpicker::colourInput(inputId = "couleur",
                                "choisir le couluer de la ligne",
                                value="blue")
      
    ),
    mainPanel(
      tabsetPanel(
       tabPanel("Graphe1",plotOutput(outputId = "graphe1")),
       tabPanel("Graphe2",plotOutput(outputId = "graphe2")),
       tabPanel("Donnees",dataTableOutput(outputId = "data")))
       
      
    )
  )
)

server<-function(input,output){
  output$graphe1<-renderPlot({
    gapminder %>% 
      filter(country==input$pays) %>% 
      ggplot(aes(x=year,y=lifeExp))+
      geom_line(color=input$couleur)+
      geom_point()+
      ggtitle(input$text1)+
      theme_bw()
  })
  output$graphe2<-renderPlot({
    gapminder %>% 
      filter(country==input$pays) %>% 
      ggplot(aes(x=year,y=gdpPercap))+
      geom_line(color=input$couleur)+
      geom_point()+
      ggtitle(input$text2)+
      theme_bw()
  })
  
  output$data<-renderDataTable({
    data1<-gapminder %>% 
      filter(country==input$pays) %>% 
      select(year, country, lifeExp, gdpPercap)
    DT::datatable(data1)
    
  })
  
}





shinyApp(ui=ui,server=server)