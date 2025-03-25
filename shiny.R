library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tibble)
library(gridExtra)

# Générer une base fictive
set.seed(123)
fake_data <- tibble(
  Age = sample(18:80, 50, replace = TRUE),
  salaire = sample(50000:500000, 50, replace = TRUE),
  depenses = sample(30000:600000, 50, replace = TRUE),
  Nombre = sample(1:15, 50, replace = TRUE)
)

# Sauvegarder la base fictive dans un fichier CSV
write.csv(fake_data, "securite_fictive.csv", row.names = FALSE)

# Charger la base de données par défaut
base_path <- "securite_fictive.csv"
if (file.exists(base_path)) {
  base_data <- read.csv(base_path, sep=",", stringsAsFactors = FALSE)
} else {
  base_data <- fake_data
}

# Interface utilisateur
ui <- fluidPage(
  titlePanel("Suivi de la collecte de données"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Importer un fichier CSV", accept = ".csv"),
      actionButton("detect", "Détecter les incohérences")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Données", DTOutput("table")),
        tabPanel("Graphiques", plotOutput("graphique")),
        tabPanel("Statistiques Descriptives", verbatimTextOutput("stats")),
        tabPanel("Incohérences", verbatimTextOutput("incoherences"))
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  data <- reactive({
    if (is.null(input$file)) {
      return(base_data)
    } else {
      return(read.csv(input$file$datapath, sep=",", stringsAsFactors = FALSE))
    }
  })
  
  output$table <- renderDT({
    req(data())
    datatable(data())
  })
  
  output$graphique <- renderPlot({
    req(data())
    df <- data()
    
    p1 <- ggplot(df, aes(x = as.numeric(Age))) +
      geom_histogram(bins = 20, fill = "blue", alpha = 0.7) +
      labs(title = "Répartition de l'âge du chef de ménage")
    
    p2 <- ggplot(df, aes(y = salaire)) +
      geom_boxplot(fill = "green", alpha = 0.7) +
      labs(title = "Distribution du salaire")
    
    p3 <- ggplot(df, aes(x = depenses)) +
      geom_histogram(bins = 20, fill = "red", alpha = 0.7) +
      labs(title = "Répartition des dépenses")
    
    p4 <- ggplot(df, aes(x = as.factor(Nombre))) +
      geom_bar(fill = "purple", alpha = 0.7) +
      labs(title = "Distribution du nombre de personnes")
    
    grid.arrange(p1, p2, p3, p4, ncol = 2)
  })
  
  output$stats <- renderPrint({
    req(data())
    summary(data())
  })
  
  output$incoherences <- renderPrint({
    req(data())
    df <- data()
    incoherences <- list()
    
    if("Age" %in% names(df)) {
      incoherences$age <- df %>% filter(Age < 18 | Age > 100)
    }
    if("salaire" %in% names(df) & "depenses" %in% names(df)) {
      incoherences$depenses <- df %>% filter(depenses > salaire)
    }
    if("Nombre" %in% names(df)) {
      incoherences$menage <- df %>% filter(Nombre < 0)
    }
    
    if (length(incoherences) == 0) {
      print("Aucune incohérence détectée.")
    } else {
      print(incoherences)
    }
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
