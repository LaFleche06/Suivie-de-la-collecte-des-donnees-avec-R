library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tibble)
library(gridExtra)
library(tidyverse)
library(robotoolbox)

# Configuration de l'API KoboToolbox
kobo_setup(url = "https://kf.kobotoolbox.org", token = Sys.getenv("4d2d05b02a89fc97fe59d9b8a77337b47649ef59") )

# Récupération de l'asset spécifique
asset <- kobo_asset(Sys.getenv("aSAa9CCRcCunWL75V46Ky8"))
df <- asset %>% kobo_data()

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
        tabPanel("Statistiques Descriptives", DTOutput("stats")),
        tabPanel("Incohérences", DTOutput("incoherences"))
      )
    )
  )
)

# Serveur
server <- function(input, output) {
  data <- reactive({
    if (is.null(input$file)) {
      return(df)
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
  
  output$stats <- renderDT({
    req(data())
    datatable(summary(data()), options = list(autoWidth = TRUE))
  })
  
  output$incoherences <- renderDT({
    req(data())
    df <- data()
    incoherences <- tibble(Type = character(), Détails = list())
    
    if("Age" %in% names(df)) {
      incoh_age <- df %>% filter(Age < 18 | Age > 100)
      if (nrow(incoh_age) > 0) {
        incoherences <- add_row(incoherences, Type = "Âge incohérent", Détails = list(incoh_age))
      }
    }
    if("salaire" %in% names(df) & "depenses" %in% names(df)) {
      incoh_dep <- df %>% filter(depenses > salaire)
      if (nrow(incoh_dep) > 0) {
        incoherences <- add_row(incoherences, Type = "Dépenses > Salaire", Détails = list(incoh_dep))
      }
    }
    if("Nombre" %in% names(df)) {
      incoh_menage <- df %>% filter(Nombre < 0)
      if (nrow(incoh_menage) > 0) {
        incoherences <- add_row(incoherences, Type = "Nombre négatif", Détails = list(incoh_menage))
      }
    }
    
    if (nrow(incoherences) == 0) {
      datatable(data.frame(Message = "Aucune incohérence détectée."), options = list(dom = 't'))
    } else {
      datatable(incoherences, options = list(autoWidth = TRUE))
    }
  })
}

# Exécution de l'application
shinyApp(ui = ui, server = server)
