library(shiny)
ui<-fluidPage(
  titlePanel("Calcul de l'indice de masse corporelle"),
  sidebarLayout(
    sidebarPanel(
      helpText("Parametres:"),
      sliderInput(inputId = "poids",
                  label="Poids en Kg:",
                  value=70,
                  min=20,
                  max=100
      ),
      sliderInput(inputId = "taille",
                  label="Taille en cm:",
                  value=100,
                  min=40,
                  max=250
      ),
      actionButton(inputId = "action",
                   label="GO!")
    ),
    mainPanel(
      h4(textOutput(outputId = "Poids")),
      br(),
      h4(textOutput(outputId = "Taille")),
      br(),
    
      h4(textOutput(outputId = "imc"))
    )
  )
)
server<-function(input,output){
  output$Poids<-renderText({
    input$action
    isolate(c("Votre poids est:",input$poids))
  })
  output$Taille<-renderText({
    input$action
    isolate(c("Votre taille est:",input$taille))
  })
  
  
  output$imc<-renderText({
    input$action
    isolate(c("Votre IMC est:", (input$poids/((input$taille/100)*(input$taille/100)))))
  })
   
}
shinyApp(ui=ui, server=server)