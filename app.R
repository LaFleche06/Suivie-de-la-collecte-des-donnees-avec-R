# Chargement des bibliothèques nécessaires
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(DT)
library(shinydashboard)
library(shinyalert)
library(dplyr)

# -- Configuration API KoboToolbox
token <- "6f8bb50a807ae9e7fda9fba40566da0f9c3383c5"
form_id <- "a9okT245N3K6EBHaW5kEWu"
url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/", form_id, "/data/?format=json")

# -- Fonction pour récupérer les données depuis l'API KoboToolbox
fetch_data <- function() {
  tryCatch({
    response <- GET(url, add_headers(Authorization = paste("Token", token)))
    
    if (status_code(response) == 200) {
      content_text <- content(response, "text", encoding = "UTF-8")
      data <- fromJSON(content_text, flatten = TRUE)
      
      if ("results" %in% names(data) && length(data$results) > 0) {
        return(as.data.frame(data$results))
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  }, error = function(e) {
    return(NULL)
  })
}

# -- Fonction pour vérifier les incohérences dans les données
check_incoherence <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  data$alertes <- "Aucune incohérence"
  
  # Vérification de la présence des coordonnées GPS
  data$alertes[is.na(data$`_geolocation1`) | is.na(data$`_geolocation2`)] <- "Coordonnées GPS manquantes"
  
  # Vérification des dépenses supérieures aux revenus
  data$alertes[data$depenses > data$revenus] <- "Dépenses supérieures aux revenus"
  
  # Vérification de la cohérence des dates
  data$alertes[as.Date(data$date_debut) > as.Date(data$date_fin)] <- "Dates incohérentes"
  
  # Vérification de la cohérence des âges
  data$alertes[data$age < 0 | data$age > 120] <- "Âge incohérent"
  
  return(data)
}

# -- Interface utilisateur
ui <- dashboardPage(
  dashboardHeader(title = "Suivi d'Enquête KoboToolbox"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte", tabName = "map", icon = icon("map")),
      menuItem("Données brutes", tabName = "rawdata", icon = icon("table"))
    )
  ),
  dashboardBody(
    useShinyalert(),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_submissions"),
                valueBoxOutput("valid_submissions"),
                valueBoxOutput("invalid_submissions")
              ),
              fluidRow(
                box(title = "Tableau des questionnaires soumis", status = "primary", DTOutput("data_table"))
              )
      ),
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Carte des questionnaires", status = "primary", leafletOutput("map", height = 600))
              )
      ),
      tabItem(tabName = "rawdata",
              fluidRow(
                box(title = "Données brutes", status = "warning", DTOutput("raw_data_table"))
              )
      )
    )
  )
)

# -- Serveur
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  refresh_data <- function() {
    new_data <- fetch_data()
    if (!is.null(new_data)) {
      new_data <- check_incoherence(new_data)
      
      # Renommer les colonnes pour correspondre à ce que Leaflet attend
      if ("_geolocation" %in% colnames(new_data)) {
        geoloc <- do.call(rbind, new_data$`_geolocation`)
        new_data$latitude <- geoloc[, 1]
        new_data$longitude <- geoloc[, 2]
      }
      
      data(new_data)
      
      # Déclencher des alertes instantanées pour les soumissions invalides
      invalid_data <- new_data %>% filter(alertes != "Aucune incohérence")
      
      if (nrow(invalid_data) > 0) {
        lapply(1:nrow(invalid_data), function(i) {
          shinyalert(
            title = "Alerte d'incohérence détectée!",
            text = paste("Enquêteur :", invalid_data$`_uuid`[i], "<br>",
                         "Problème :", invalid_data$alertes[i], "<br>",
                         "Localisation : Latitude =", invalid_data$latitude[i], ", Longitude =", invalid_data$longitude[i]),
            type = "error",
            html = TRUE
          )
        })
      }
    }
  }
  
  # Rafraîchissement des données toutes les 30 secondes
  autoInvalidate <- reactiveTimer(30000)
  observe({
    autoInvalidate()
    refresh_data()
  })
  
  output$total_submissions <- renderValueBox({
    req(data())
    valueBox(nrow(data()), "Total des questionnaires", icon = icon("file"), color = "blue")
  })
  
  output$valid_submissions <- renderValueBox({
    req(data())
    valid_count <- sum(data()$alertes == "Aucune incohérence")
    valueBox(valid_count, "Questionnaires valides", icon = icon("check"), color = "green")
  })
  
  output$invalid_submissions <- renderValueBox({
    req(data())
    invalid_count <- sum(data()$alertes != "Aucune incohérence")
    valueBox(invalid_count, "Questionnaires invalides", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$data_table <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
  
  output$map <- renderLeaflet({
    req(data())
    valid_data <- data() %>% filter(!is.na(longitude) & !is.na(latitude))
    
    leaflet(valid_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~ifelse(alertes == "Aucune incohérence", "green", "red"),
        popup = ~alertes
      )
  })
  
  output$raw_data_table <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
}

# -- Lancer l'application Shiny
shinyApp(ui = ui, server = server)
