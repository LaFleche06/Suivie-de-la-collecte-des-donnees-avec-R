library(shiny)
library(colourpicker)

# Define server logic
server <- function(input, output, session) {
  
  output$distPlot <- renderPlot({
    # Sélection de la variable choisie par l'utilisateur
    x <- faithful[[input$variable]]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # Dessiner l'histogramme
    hist(x, breaks = bins, col = input$color, border = 'white',
         xlab = input$variable,
         main = input$title)
  })
  
  # Afficher le nombre de classes
  output$numClasses <- renderText({
    paste("Nombre de classes :", input$bins)
  })
  
  # Résumé statistique
  output$summary <- renderPrint({
    summary(faithful)
  })
  
  # Affichage du jeu de données
  output$dataTable <- renderDataTable({
    faithful
  })
  
  # Exporter le graphe
  output$downloadPlot <- downloadHandler(
    filename = function() { "histogramme.jpeg" },
    content = function(file) {
      jpeg(file)
      x <- faithful[[input$variable]]
      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      hist(x, breaks = bins, col = input$color, border = 'white',
           xlab = input$variable, main = input$title)
      dev.off()
    }
  )
}
