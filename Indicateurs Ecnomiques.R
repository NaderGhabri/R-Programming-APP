
library(shiny)
library(gapminder)
library(ggplot2)
data(gapminder)
force(gapminder)
ui<-fluidPage(shinythemes::themeSelector(),
              titlePanel(h3("Indicateurs Ecnomiques")),
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId = "pays",
                              label="Choisir le pays:",
                              choices=levels(gapminder$country)),
                  textInput(inputId = "title1",
                            label="Titre du graphique 1:",
                            value="Figure1: POPULATION"),
                  textInput(inputId = "title2",
                            label="Titre du graphique 2:",
                            value="Figure2: ESPERANCE DE VIE"),
                  textInput(inputId = "title3",
                            label="Titre du graphique 3:",
                            value="FIGURE3: PIB"),
                  actionButton(inputId = "action",
                               label = "submitt")
                ),
                mainPanel(
                  textOutput(outputId = "titre1"),
                  textOutput(outputId = "titre2"),
                  textOutput(outputId = "titre3"),
                  fluidRow(
                    column(4,plotOutput(outputId = "pop")),
                    column(4,plotOutput(outputId = "life")),
                    column(4,plotOutput(outputId = "gdp")))
                  
                )
              )
)
server<-function(input,output){
  output$titre1<-renderText({
  })
  output$titre2<-renderText({
  })
  output$titre3<-renderText({
  })
  
  output$pop<-renderPlot({
    input$action
    isolate(gapminder %>%
              filter(country==input$pays) %>%
              ggplot(aes(x=year,y=pop))+
              geom_line(color="red")+
              geom_point()+
              ggtitle(req(c(input$title1)))+
              theme_bw())
  })
  output$life<-renderPlot({
    input$action
    isolate(gapminder %>%
              filter(country==input$pays) %>%
              ggplot(aes(x=year,y=lifeExp))+
              geom_line(color="blue")+
              geom_point()+
              ggtitle(req(c(input$title2)))+
              theme_bw())
  })
  output$gdp<-renderPlot({
    input$action
    isolate(gapminder %>%
              filter(country==input$pays) %>%
              ggplot(aes(x=year,y=gdpPercap))+
              geom_line(color="green")+
              geom_point()+
              ggtitle(req(c(input$title3)))+
              theme_bw())
    
  })
}
shinyApp(ui,server)