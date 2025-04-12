
library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(DT)
library(ggplot2)
library(R6)

# Classe de Validation 
AlerteEnquete <- R6::R6Class("AlerteEnquete",
                             public = list(
                               initialize = function(zones_autorisees, seuils_validation) {
                                 private$zones <- zones_autorisees
                                 private$seuils <- seuils_validation
                                 private$donnees_enquetes <- data.frame()
                                 private$stats_enqueteurs <- data.frame(
                                   id_enqueteur = character(),
                                   total_soumissions = integer(),
                                   soumissions_valides = integer(),
                                   stringsAsFactors = FALSE
                                 )
                               },
                               
                               # Méthode pour ajouter une nouvelle soumission
                               ajouter_soumission = function(donnees_enqueteur) {
                                 # Validation de la soumission
                                 resultats_validation <- self$valider_soumission(donnees_enqueteur)
                                 
                                 # Préparer la ligne de données avec le statut de validation
                                 nouvelle_ligne <- data.frame(
                                   id_enqueteur = donnees_enqueteur$id_enqueteur,
                                   latitude = donnees_enqueteur$latitude,
                                   longitude = donnees_enqueteur$longitude,
                                   age = donnees_enqueteur$age,
                                   revenu = donnees_enqueteur$revenu,
                                   date_soumission = Sys.time(),
                                   geolocalisation_valide = resultats_validation$geolocalisation_valide,
                                   donnees_coherentes = resultats_validation$donnees_coherentes,
                                   alertes = paste(resultats_validation$alertes, collapse = "; ")
                                 )
                                 
                                 # Mettre à jour les statistiques de l'enquêteur
                                 self$mise_a_jour_stats_enqueteur(
                                   donnees_enqueteur$id_enqueteur, 
                                   resultats_validation$geolocalisation_valide && resultats_validation$donnees_coherentes
                                 )
                                 
                                 # Ajouter à la base de données interne
                                 private$donnees_enquetes <- rbind(private$donnees_enquetes, nouvelle_ligne)
                                 
                                 # Sauvegarder dans un fichier CSV (optionnel)
                                 write.csv(private$donnees_enquetes, "suivi_enquetes.csv", row.names = FALSE)
                                 
                                 return(resultats_validation)
                               },
                               
                               # Nouvelle méthode pour mettre à jour les statistiques par enquêteur
                               mise_a_jour_stats_enqueteur = function(id_enqueteur, soumission_valide) {
                                 # Chercher si l'enquêteur existe déjà
                                 idx <- which(private$stats_enqueteurs$id_enqueteur == id_enqueteur)
                                 
                                 if (length(idx) == 0) {
                                   # Nouvel enquêteur
                                   nouvelle_entree <- data.frame(
                                     id_enqueteur = id_enqueteur,
                                     total_soumissions = 1,
                                     soumissions_valides = as.integer(soumission_valide)
                                   )
                                   private$stats_enqueteurs <- rbind(private$stats_enqueteurs, nouvelle_entree)
                                 } else {
                                   # Enquêteur existant
                                   private$stats_enqueteurs$total_soumissions[idx] <- 
                                     private$stats_enqueteurs$total_soumissions[idx] + 1
                                   
                                   if (soumission_valide) {
                                     private$stats_enqueteurs$soumissions_valides[idx] <- 
                                       private$stats_enqueteurs$soumissions_valides[idx] + 1
                                   }
                                 }
                               },
                               
                               # Méthode pour obtenir les statistiques des enquêteurs
                               obtenir_stats_enqueteurs = function() {
                                 return(private$stats_enqueteurs)
                               },
                               
                               # Méthode pour récupérer les données
                               obtenir_donnees = function() {
                                 return(private$donnees_enquetes)
                               },
                               
                               # Validation principale
                               valider_soumission = function(donnees_enqueteur) {
                                 resultats <- list(
                                   geolocalisation_valide = FALSE,
                                   donnees_coherentes = FALSE,
                                   alertes = c()
                                 )
                                 
                                 # Validation géographique
                                 validation_geo <- self$verifier_localisation(
                                   donnees_enqueteur$latitude, 
                                   donnees_enqueteur$longitude
                                 )
                                 resultats$geolocalisation_valide <- validation_geo$valide
                                 
                                 if (!validation_geo$valide) {
                                   resultats$alertes <- c(
                                     resultats$alertes, 
                                     "Localisation hors de la zone d'enquête"
                                   )
                                 }
                                 
                                 # Validation des données
                                 validation_donnees <- self$verifier_coherence(donnees_enqueteur)
                                 resultats$donnees_coherentes <- validation_donnees$valide
                                 
                                 if (!validation_donnees$valide) {
                                   resultats$alertes <- c(
                                     resultats$alertes, 
                                     validation_donnees$problemes
                                   )
                                 }
                                 
                                 return(resultats)
                               },
                               
                               # Vérification de localisation
                               verifier_localisation = function(latitude, longitude, tolerance_km = 5) {
                                 point <- st_point(c(longitude, latitude))
                                 
                                 dans_zone <- FALSE
                                 for (zone in private$zones) {
                                   if (st_intersects(point, zone, sparse = FALSE)) {
                                     dans_zone <- TRUE
                                     break
                                   }
                                 }
                                 
                                 return(list(
                                   valide = dans_zone,
                                   details = ifelse(dans_zone, "Dans la zone", "Hors zone")
                                 ))
                               },
                               
                               # Vérification de cohérence
                               verifier_coherence = function(donnees) {
                                 problemes <- c()
                                 
                                 if (donnees$age < private$seuils$age_min || 
                                     donnees$age > private$seuils$age_max) {
                                   problemes <- c(problemes, "Âge hors limites")
                                 }
                                 
                                 if (donnees$revenu < 0) {
                                   problemes <- c(problemes, "Revenu négatif invalide")
                                 }
                                 
                                 return(list(
                                   valide = length(problemes) == 0,
                                   problemes = problemes
                                 ))
                               }
                             ),
                             
                             private = list(
                               zones = NULL,
                               seuils = NULL,
                               donnees_enquetes = NULL,
                               stats_enqueteurs = NULL
                             )
)

# Préparation des zones autorisées
library(sf)
# Préparation des zones autorisées
zones_autorisees <- list(
  st_polygon(list(rbind(
    c(-4.0175, 5.3364),  # Longitude, Latitude
    c(-3.9, 5.4),
    c(-3.9, 5.2),
    c(-4.0175, 5.3364)   # Doit être identique au premier point pour fermer le polygone
  ))),
  st_polygon(list(rbind(
    c(-4.1, 5.3),        # Deuxième zone plus réaliste
    c(-4.0, 5.4),
    c(-4.0, 5.2),
    c(-4.1, 5.3)         # Doit être identique au premier point
  )))
)

# Définition des seuils de validation
seuils_validation <- list(
  age_min = 18,
  age_max = 120,
  revenu_min = 0
)

# Initialisation du système d'alerte
systeme_alerte <- AlerteEnquete$new(
  zones_autorisees, 
  seuils_validation
)

# Interface utilisateur Shiny
ui <- dashboardPage(
  dashboardHeader(title = "Suivi d'Enquêtes"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de Bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte des Enquêtes", tabName = "carte", icon = icon("map-marker")),
      menuItem("Détails des Enquêtes", tabName = "details", icon = icon("table")),
      menuItem("Statistiques Enquêteurs", tabName = "enqueteurs", icon = icon("users"))
    )
  ),
  dashboardBody(
    tabItems(
      # Tableau de Bord
      tabItem(tabName = "dashboard",
              fluidRow(
                # Statistiques globales
                box(title = "Statistiques Globales", status = "primary", solidHeader = TRUE,
                    width = 12,
                    infoBoxOutput("total_enquetes"),
                    infoBoxOutput("enquetes_valides"),
                    infoBoxOutput("enquetes_invalides")
                ),
                
                # Graphiques
                box(title = "Répartition des Enquêtes", status = "warning", solidHeader = TRUE,
                    plotOutput("plot_validite")
                )
              )
      ),
      
      # Carte des Enquêtes
      tabItem(tabName = "carte",
              leafletOutput("carte_enquetes"),
              absolutePanel(
                top = 10, right = 10,
                selectInput("filtre_carte", "Filtrer par Validité",
                            choices = c("Tous", "Valides", "Invalides"))
              )
      ),
      
      # Détails des Enquêtes
      tabItem(tabName = "details",
              DTOutput("table_enquetes"),
              actionButton("ajouter_enquete", "Ajouter une Enquête")
      ),
      
      # Nouvel onglet pour les statistiques des enquêteurs
      tabItem(tabName = "enqueteurs",
              box(title = "Statistiques par Enquêteur", status = "success", solidHeader = TRUE,
                  width = 12,
                  DTOutput("table_stats_enqueteurs")
              )
      )
    )
  )
)

# Logique serveur Shiny
server <- function(input, output, session) {
  # Réactive pour stocker les données
  donnees_reactives <- reactiveVal(data.frame())
  
  # Observateur pour ajouter des enquêtes
  observeEvent(input$ajouter_enquete, {
    showModal(modalDialog(
      title = "Ajouter une Nouvelle Enquête",
      textInput("id_enqueteur", "ID Enquêteur"),
      numericInput("latitude", "Latitude", value = 5.3364),
      numericInput("longitude", "Longitude", value = -4.0175),
      numericInput("age", "Âge", value = 30, min = 0, max = 120),
      numericInput("revenu", "Revenu", value = 50000),
      footer = tagList(
        modalButton("Annuler"),
        actionButton("soumettre_enquete", "Soumettre")
      )
    ))
  })
  
  # Soumettre une nouvelle enquête
  observeEvent(input$soumettre_enquete, {
    req(input$id_enqueteur, input$latitude, input$longitude, input$age, input$revenu)
    
    nouvelle_donnee <- list(
      id_enqueteur = input$id_enqueteur,
      latitude = input$latitude,
      longitude = input$longitude,
      age = input$age,
      revenu = input$revenu
    )
    
    # Validation et ajout
    resultats <- systeme_alerte$ajouter_soumission(nouvelle_donnee)
    
    # Mettre à jour les données réactives
    donnees_actuelles <- systeme_alerte$obtenir_donnees()
    donnees_reactives(donnees_actuelles)
    
    # Fermer le modal
    removeModal()
  })
  
  # Mise à jour des données
  data_filtree <- reactive({
    req(donnees_reactives())
    
    donnees <- donnees_reactives()
    
    if (input$filtre_carte == "Valides") {
      donnees <- donnees[donnees$geolocalisation_valide & donnees$donnees_coherentes, ]
    } else if (input$filtre_carte == "Invalides") {
      donnees <- donnees[!donnees$geolocalisation_valide | !donnees$donnees_coherentes, ]
    }
    
    donnees
  })
  
  # Statistiques
  output$total_enquetes <- renderInfoBox({
    infoBox("Total Enquêtes", nrow(donnees_reactives()), icon = icon("list"))
  })
  
  output$enquetes_valides <- renderInfoBox({
    valides <- sum(donnees_reactives()$geolocalisation_valide & donnees_reactives()$donnees_coherentes)
    infoBox("Enquêtes Valides", valides, icon = icon("check"), color = "green")
  })
  
  output$enquetes_invalides <- renderInfoBox({
    invalides <- sum(!donnees_reactives()$geolocalisation_valide | !donnees_reactives()$donnees_coherentes)
    infoBox("Enquêtes Invalides", invalides, icon = icon("times"), color = "red")
  })
  
  # Graphique de répartition
  output$plot_validite <- renderPlot({
    req(nrow(donnees_reactives()) > 0)
    
    validite <- data.frame(
      Statut = c("Valides", "Invalides"),
      Nombre = c(
        sum(donnees_reactives()$geolocalisation_valide & donnees_reactives()$donnees_coherentes),
        sum(!donnees_reactives()$geolocalisation_valide | !donnees_reactives()$donnees_coherentes)
      )
    )
    
    ggplot(validite, aes(x = Statut, y = Nombre, fill = Statut)) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = c("Valides" = "green", "Invalides" = "red")) +
      theme_minimal() +
      labs(title = "Répartition des Enquêtes", y = "Nombre d'Enquêtes")
  })
  
  # Carte des enquêtes
  output$carte_enquetes <- renderLeaflet({
    req(nrow(data_filtree()) > 0)
    
    leaflet(data_filtree()) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude, 
        color = ifelse(data_filtree()$geolocalisation_valide & data_filtree()$donnees_coherentes, 
                       "green", "red"),
        popup = ~paste("ID:", id_enqueteur, 
                       "<br>Validité Géo:", geolocalisation_valide,
                       "<br>Données Cohérentes:", donnees_coherentes)
      )
  })
  
  # Table des enquêtes
  output$table_enquetes <- renderDT({
    req(nrow(donnees_reactives()) > 0)
    datatable(donnees_reactives(), 
              options = list(pageLength = 10),
              filter = "top")
  })
  
  # Nouvelle sortie pour les statistiques des enquêteurs
  output$table_stats_enqueteurs <- renderDT({
    stats_enqueteurs <- systeme_alerte$obtenir_stats_enqueteurs()
    
    # Calculer le pourcentage de questionnaires valides
    stats_enqueteurs$pourcentage_valides <- 
      round(stats_enqueteurs$soumissions_valides / stats_enqueteurs$total_soumissions * 100, 2)
    
    datatable(stats_enqueteurs, 
              colnames = c(
                "ID Enquêteur", 
                "Total Soumissions", 
                "Soumissions Valides", 
                "% Valides"
              ),
              options = list(pageLength = 10),
              filter = "top")
  })
}

# Lancer l'application
shinyApp(ui, server)