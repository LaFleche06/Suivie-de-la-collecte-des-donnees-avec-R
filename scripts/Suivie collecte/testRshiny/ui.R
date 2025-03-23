library(shiny)
library(colourpicker) # Pour le choix de couleur

# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("My first application"),
  
  # Sidebar layout
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      # Input pour choisir un titre
      textInput("title", "Titre de l'histogramme :", "Histogramme de faithful"),
      
      # Radio buttons pour choisir la variable à représenter
      radioButtons("variable", "Choisir la variable :",
                   choices = colnames(faithful),
                   selected = "waiting"),
      
      # Colour input pour choisir la couleur
      colourInput("color", "Choisir une couleur :", value = "darkgray"),
      
      # Bouton pour exporter l'histogramme
      downloadButton("downloadPlot", "Télécharger l'histogramme")
    ),
    
    # Main panel
    mainPanel(
      plotOutput("distPlot"),
      textOutput("numClasses"), # Affiche le nombre de classes
      verbatimTextOutput("summary"),
      dataTableOutput("dataTable") # Affiche le jeu de données
    )
  )
)
