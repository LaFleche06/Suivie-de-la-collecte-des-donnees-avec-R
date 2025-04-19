# Chargement des bibliothèques nécessaires
library(shiny)
library(httr)
library(leaflet)
library(DT)
library(shinydashboard)
library(shinyalert)
library(dplyr)
library(writexl)
library(ggplot2)
library(tidyr)
library(plotly)
library(shinyWidgets)
library(lubridate)
library(officer)
library(knitr)
library(kableExtra)
library(ggplot2)
library(dplyr)
library(tidyr)
library(plotly)
library(lubridate)
library(scales)
library(officer)
library(flextable)
library(docxtractr)  # Pour la conversion en PDF


# Configuration API KoboToolbox
token <- Sys.getenv("token")
form_id <- Sys.getenv("form_id")
url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/", form_id, "/data/?format=json")

# Fonction pour récupérer les données depuis l'API KoboToolbox
fetch_data <- function() {
  tryCatch({
    response <- GET(url, add_headers(Authorization = paste("Token", token)))
    if (status_code(response) == 200) {
      content_text <- content(response, "text", encoding = "UTF-8")
      data <- fromJSON(content_text, flatten = TRUE)
      if ("results" %in% names(data) && length(data$results) > 0) return(as.data.frame(data$results))
    }
    return(NULL)
  }, error = function(e) NULL)
}

# Vérification des incohérences dans les données
check_incoherence <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  # Initialisation des colonnes d'alerte
  data$alertes <- "Aucune incohérence"
  data$details_incoherence <- NA
  data$nb_incoherences <- 0
  
  # Liste des vérifications à effectuer
  checks <- list(
    list(
      condition = function(d) {
        if (!"_geolocation" %in% colnames(d)) return(rep(FALSE, nrow(d)))
        if (is.character(d$`_geolocation`)) {
          return(is.na(d$`_geolocation`) | d$`_geolocation` == "")
        } else {
          return(sapply(d$`_geolocation`, function(x) is.null(x) || length(x) == 0 || 
                          (length(x) > 0 && (is.na(x[1]) || is.na(x[2])))))
        }
      },
      alert = "Coordonnées GPS manquantes",
      details = "Les coordonnées GPS n'ont pas été enregistrées lors de la collecte."
    ),
    list(
      condition = function(d) {
        cols <- c("Quel_est_la_d_pense_mensuelle_du_m_nage_", "Quelle_est_la_d_pens_e_du_m_nage_exact_")
        if (!all(cols %in% colnames(d))) return(rep(FALSE, nrow(d)))
        
        for (col in cols) {
          if (!is.numeric(d[[col]])) d[[col]] <- as.numeric(as.character(d[[col]]))
        }
        
        return(!is.na(d[[cols[1]]]) & !is.na(d[[cols[2]]]) & d[[cols[2]]] > (d[[cols[1]]] * 2))
      },
      alert = "Dépenses exactes supérieures aux dépenses mensuelles estimées",
      details = function(d) {
        cols <- c("Quel_est_la_d_pense_mensuelle_du_m_nage_", "Quelle_est_la_d_pens_e_du_m_nage_exact_")
        paste0("Dépense exacte (", d[[cols[2]]], 
               ") est significativement supérieure à la dépense mensuelle estimée (", 
               d[[cols[1]]], ")")
      }
    ),
    list(
      condition = function(d) {
        col <- "Age_du_CM"
        if (!col %in% colnames(d)) return(rep(FALSE, nrow(d)))
        
        if (!is.numeric(d[[col]])) d[[col]] <- as.numeric(as.character(d[[col]]))
        return(!is.na(d[[col]]) & (d[[col]] < 15 | d[[col]] > 120))
      },
      alert = "Âge incohérent",
      details = function(d) {
        paste0("L'âge du chef de ménage (", d$Age_du_CM, 
               ") est ", ifelse(d$Age_du_CM < 15, 
                                "inférieur à 15 ans", 
                                "supérieur à 120 ans"), 
               ", ce qui est improbable.")
      }
    ),
    list(
      condition = function(d) {
        cols <- c("Combien_de_personnes_vent_dans_le_m_nage_", "Combien_de_femmes_vivent_dans_le_m_nage_")
        if (!all(cols %in% colnames(d))) return(rep(FALSE, nrow(d)))
        
        for (col in cols) {
          if (!is.numeric(d[[col]])) d[[col]] <- as.numeric(as.character(d[[col]]))
        }
        
        return(!is.na(d[[cols[1]]]) & !is.na(d[[cols[2]]]) & d[[cols[1]]] < d[[cols[2]]])
      },
      alert = "Nombre de femmes supérieur au total de personnes",
      details = function(d) {
        cols <- c("Combien_de_personnes_vent_dans_le_m_nage_", "Combien_de_femmes_vivent_dans_le_m_nage_")
        paste0("Le nombre de femmes (", d[[cols[2]]], 
               ") est supérieur au nombre total de personnes dans le ménage (", 
               d[[cols[1]]], ").")
      }
    )
  )
  
  # Vérification des données manquantes obligatoires
  champs_obligatoires <- c("Age_du_CM", "Combien_de_personnes_vent_dans_le_m_nage_")
  for (champ in champs_obligatoires) {
    if (champ %in% colnames(data)) {
      check <- list(
        condition = function(d) is.na(d[[champ]]) | d[[champ]] == "",
        alert = paste0("Données manquantes: ", champ),
        details = paste0("La valeur pour le champ '", champ, "' est manquante alors qu'elle est obligatoire.")
      )
      checks <- c(checks, list(check))
    }
  }
  
  # Application des vérifications
  for (check in checks) {
    condition_result <- check$condition(data)
    
    if (any(condition_result)) {
      # Mettre à jour le compteur d'incohérences
      data$nb_incoherences[condition_result] <- data$nb_incoherences[condition_result] + 1
      
      # Mettre à jour les alertes pour les nouvelles incohérences
      is_first_alert <- data$alertes[condition_result] == "Aucune incohérence"
      data$alertes[condition_result & is_first_alert] <- check$alert
      
      # Pour les rangées ayant déjà des alertes, ajouter à la liste
      data$alertes[condition_result & !is_first_alert] <- paste0(data$alertes[condition_result & !is_first_alert], 
                                                                 ", ", check$alert)
      
      # Mise à jour des détails
      if (is.function(check$details)) {
        details <- check$details(data[condition_result, , drop = FALSE])
        data$details_incoherence[condition_result] <- ifelse(
          is.na(data$details_incoherence[condition_result]),
          details,
          paste0(data$details_incoherence[condition_result], "; ", details)
        )
      } else {
        data$details_incoherence[condition_result] <- ifelse(
          is.na(data$details_incoherence[condition_result]),
          check$details,
          paste0(data$details_incoherence[condition_result], "; ", check$details)
        )
      }
    }
  }
  
  return(data)
}

# Extraction des coordonnées géographiques
extract_coordinates <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # Initialiser les colonnes de latitude et longitude
  data$latitude <- NA
  data$longitude <- NA
  
  if ("_geolocation" %in% colnames(data)) {
    if (is.character(data$`_geolocation`)) {
      valid_coords <- !is.na(data$`_geolocation`) & data$`_geolocation` != ""
      if (any(valid_coords)) {
        coord_split <- strsplit(as.character(data$`_geolocation`[valid_coords]), ",")
        data$latitude[valid_coords] <- sapply(coord_split, function(x) if(length(x) >= 1) as.numeric(trimws(x[1])) else NA)
        data$longitude[valid_coords] <- sapply(coord_split, function(x) if(length(x) >= 2) as.numeric(trimws(x[2])) else NA)
      }
    } else if (is.list(data$`_geolocation`)) {
      for (i in 1:nrow(data)) {
        geo <- data$`_geolocation`[[i]]
        if (!is.null(geo) && length(geo) >= 2 && !is.na(geo[1]) && !is.na(geo[2])) {
          data$latitude[i] <- geo[1]
          data$longitude[i] <- geo[2]
        }
      }
    }
  }
  
  # Conversion de la date de soumission au format Date
  if ("_submission_time" %in% colnames(data)) {
    data$submission_date <- as.Date(data$`_submission_time`)
  }
  
  return(data)
}

# Interface utilisateur améliorée
ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Suivi d'enquête KoboToolbox"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte", tabName = "map", icon = icon("map")),
      menuItem("Incohérences", tabName = "incoherences", icon = icon("exclamation-triangle")),
      menuItem("Données brutes", tabName = "rawdata", icon = icon("table")),
      menuItem("Visualisations", tabName = "visualizations", icon = icon("chart-bar"))
    ),
    div(
      style = "padding: 15px;",
      actionButton("refresh_button", "Rafraîchir les données", icon = icon("refresh"), 
                   style = "width: 100%; background-color: #3c8dbc; color: white;")
    )
  ),
  dashboardBody(
    useShinyalert(force=TRUE),
    # CSS personnalisé pour améliorer l'apparence
    tags$head(
      tags$style(HTML('
        .skin-blue .main-header .logo {
          font-weight: bold;
        }
        .box-title {
          font-size: 18px;
        }
        .small-box {
          border-radius: 5px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
        .small-box h3 {
          font-size: 28px;
        }
      '))
    ),
    tabItems(
      # Onglet Tableau de bord amélioré
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_submissions", width = 3),
                valueBoxOutput("valid_submissions", width = 3),
                valueBoxOutput("invalid_submissions", width = 3),
                valueBoxOutput("completion_rate", width = 3)
              ),
              fluidRow(
                box(title = "Activité récente", status = "primary", width = 8,
                    plotlyOutput("activity_chart", height = 250)),
                box(title = "Téléchargement", status = "info", width = 4,
                    downloadButton("download_xls", "Télécharger en XLS", 
                                   style = "width: 100%; margin-bottom: 10px;"),
                    downloadButton("download_csv", "Télécharger en CSV", 
                                   style = "width: 100%; margin-bottom: 10px;"),
                    downloadButton("download_incoherences", "Télécharger incohérences",
                                   style = "width: 100%; margin-bottom: 10px;"),
                    downloadButton("download_report", "Télécharger le rapport",
                                   style = "width: 100%; background-color: #673AB7; color: white;")
                )
              ),
              fluidRow(
                box(title = "Régions avec le plus d'incohérences", status = "warning", width = 6,
                    plotlyOutput("regions_issues"))
              )
      ),
      
      # Onglet Carte amélioré
      tabItem(tabName = "map",
              fluidRow(
                column(3,
                       box(title = "Filtres", status = "primary", width = 12,
                           selectInput("map_filter_region", "Filtrer par région", choices = c("Toutes" = ""), multiple = FALSE),
                           dateRangeInput("map_date_range", "Période", start = NULL, end = NULL),
                           checkboxGroupInput("map_show_status", "Afficher statut", 
                                              choices = c("Valide" = "valid", "Invalide" = "invalid"),
                                              selected = c("valid", "invalid")),
                           actionButton("reset_map_filters", "Réinitialiser filtres", 
                                        style = "width: 100%; margin-top: 10px;")
                       )
                ),
                column(9,
                       box(title = "Carte des questionnaires", status = "primary", width = 12, height = 600,
                           leafletOutput("map", height = 550))
                )
              )
      ),
      
      # Onglet Incohérences amélioré
      tabItem(tabName = "incoherences",
              fluidRow(
                column(12,
                       box(title = "Filtres des incohérences", status = "primary", width = 12, collapsible = TRUE,
                           fluidRow(
                             column(3, selectInput("incoherence_type", "Type d'incohérence", 
                                                   choices = c("Tous" = ""))),
                             column(3, selectInput("incoherence_region", "Région", 
                                                   choices = c("Toutes" = ""))),
                             column(3, dateRangeInput("incoherence_date_range", "Période", start = NULL, end = NULL)),
                             column(3, actionButton("reset_incoherence_filters", "Réinitialiser", 
                                                    style = "margin-top: 25px; width: 100%;"))
                           )
                       )
                )
              ),
              fluidRow(
                box(title = "Analyse des incohérences", status = "warning", width = 12,
                    DTOutput("incoherences_table"))
              ),
              fluidRow(
                box(title = "Répartition des types d'incohérences", status = "danger", width = 6,
                    plotlyOutput("incoherences_summary", height = 350)),
                box(title = "Tendance temporelle des incohérences", status = "info", width = 6,
                    plotlyOutput("incoherences_trend", height = 350))
              )
      ),
      
      # Onglet Données brutes amélioré
      tabItem(tabName = "rawdata",
              fluidRow(
                box(title = "Filtres des données", status = "primary", width = 12, collapsible = TRUE,
                    fluidRow(
                      column(3, selectInput("data_filter_region", "Région", choices = c("Toutes" = ""))),
                      column(3, selectInput("data_filter_status", "Statut", 
                                            choices = c("Tous" = "", "Valide" = "valid", "Invalide" = "invalid"))),
                      column(3, dateRangeInput("data_date_range", "Période", start = NULL, end = NULL)),
                      column(3, actionButton("reset_data_filters", "Réinitialiser", 
                                             style = "margin-top: 25px; width: 100%;"))
                    )
                )
              ),
              fluidRow(
                box(title = "Données brutes", status = "warning", width = 12,
                    DTOutput("raw_data_table"))
              )
      ),
      
      # Nouvel onglet Visualisations
      tabItem(tabName = "visualizations",
              fluidRow(
                box(title = "Distribution de l'âge des chefs de ménage par région", status = "primary", width = 6,
                    plotlyOutput("age_distribution_region", height = 300)),
                box(title = "Taille des ménages par région", status = "info", width = 6,
                    plotlyOutput("household_size", height = 300))
              ),
              fluidRow(
                box(title = "Comparaison des dépenses", status = "warning", width = 12,
                    plotlyOutput("expenses_comparison", height = 350))
              )
      )
    )
  )
)

# Serveur
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  # Fonction de rafraîchissement des données
  refresh_data <- function() {
    new_data <- fetch_data()
    if (!is.null(new_data)) {
      new_data <- extract_coordinates(new_data) %>% check_incoherence()
      data(new_data)
      
      # Mise à jour des options des filtres
      if ("Region" %in% colnames(new_data)) {
        regions <- sort(unique(na.omit(new_data$Region)))
        updateSelectInput(session, "map_filter_region", choices = c("Toutes" = "", regions))
        updateSelectInput(session, "incoherence_region", choices = c("Toutes" = "", regions))
        updateSelectInput(session, "data_filter_region", choices = c("Toutes" = "", regions))
      }
      
      # Mise à jour des options de type d'incohérence
      alert_types <- unique(unlist(strsplit(new_data$alertes[new_data$alertes != "Aucune incohérence"], ", ")))
      updateSelectInput(session, "incoherence_type", choices = c("Tous" = "", alert_types))
      
      # Mise à jour des plages de dates
      if ("submission_date" %in% colnames(new_data)) {
        date_range <- range(na.omit(new_data$submission_date))
        updateDateRangeInput(session, "map_date_range", start = date_range[1], end = date_range[2])
        updateDateRangeInput(session, "incoherence_date_range", start = date_range[1], end = date_range[2])
        updateDateRangeInput(session, "data_date_range", start = date_range[1], end = date_range[2])
      }
      
      # Alertes pour les soumissions invalides
      invalid_data <- new_data %>% filter(alertes != "Aucune incohérence")
      if (nrow(invalid_data) > 0) {
        shinyalert(
          title = "Alertes d'incohérences détectées!",
          text = paste0(nrow(invalid_data), " questionnaires présentent des incohérences. Consultez l'onglet 'Incohérences' pour plus d'informations."),
          type = "warning"
        )
      }
    } else {
      shinyalert(
        title = "Erreur de connexion",
        text = "Impossible de récupérer les données depuis KoboToolbox. Vérifiez votre connexion et les paramètres d'API.",
        type = "error"
      )
    }
  }
  
  # Données filtrées pour la carte
  filtered_map_data <- reactive({
    req(data())
    d <- data()
    
    # Filtrage par région
    if (!is.null(input$map_filter_region) && input$map_filter_region != "") {
      d <- d %>% filter(Region == input$map_filter_region)
    }
    
    # Filtrage par date
    if (!is.null(input$map_date_range)) {
      d <- d %>% filter(submission_date >= input$map_date_range[1] & 
                          submission_date <= input$map_date_range[2])
    }
    
    # Filtrage par statut
    if (!is.null(input$map_show_status) && length(input$map_show_status) > 0) {
      if (all(c("valid", "invalid") %in% input$map_show_status)) {
        # Ne rien faire, tout afficher
      } else if ("valid" %in% input$map_show_status) {
        d <- d %>% filter(alertes == "Aucune incohérence")
      } else if ("invalid" %in% input$map_show_status) {
        d <- d %>% filter(alertes != "Aucune incohérence")
      }
    }
    
    return(d)
  })
  
  # Données filtrées pour les incohérences
  filtered_incoherence_data <- reactive({
    req(data())
    d <- data() %>% filter(alertes != "Aucune incohérence")
    
    # Filtrage par type d'incohérence
    if (!is.null(input$incoherence_type) && input$incoherence_type != "") {
      d <- d %>% filter(grepl(input$incoherence_type, alertes))
    }
    
    # Filtrage par région
    if (!is.null(input$incoherence_region) && input$incoherence_region != "") {
      d <- d %>% filter(Region == input$incoherence_region)
    }
    
    # Filtrage par date
    if (!is.null(input$incoherence_date_range)) {
      d <- d %>% filter(submission_date >= input$incoherence_date_range[1] & 
                          submission_date <= input$incoherence_date_range[2])
    }
    
    return(d)
  })
  
  # Données filtrées pour les données brutes
  filtered_raw_data <- reactive({
    req(data())
    d <- data()
    
    # Filtrage par région
    if (!is.null(input$data_filter_region) && input$data_filter_region != "") {
      d <- d %>% filter(Region == input$data_filter_region)
    }
    
    # Filtrage par statut
    if (!is.null(input$data_filter_status) && input$data_filter_status != "") {
      if (input$data_filter_status == "valid") {
        d <- d %>% filter(alertes == "Aucune incohérence")
      } else if (input$data_filter_status == "invalid") {
        d <- d %>% filter(alertes != "Aucune incohérence")
      }
    }
    
    # Filtrage par date
    if (!is.null(input$data_date_range)) {
      d <- d %>% 
        filter(submission_date >= input$data_date_range[1] & 
                 submission_date <= input$data_date_range[2])
    }
    
    return(d)
  })
  
  # Réinitialisation des filtres
  observeEvent(input$reset_map_filters, {
    req(data())
    date_range <- range(na.omit(data()$submission_date))
    updateSelectInput(session, "map_filter_region", selected = "")
    updateDateRangeInput(session, "map_date_range", start = date_range[1], end = date_range[2])
    updateCheckboxGroupInput(session, "map_show_status", selected = c("valid", "invalid"))
  })
  
  observeEvent(input$reset_incoherence_filters, {
    req(data())
    date_range <- range(na.omit(data()$submission_date))
    updateSelectInput(session, "incoherence_type", selected = "")
    updateSelectInput(session, "incoherence_region", selected = "")
    updateDateRangeInput(session, "incoherence_date_range", start = date_range[1], end = date_range[2])
  })
  
  observeEvent(input$reset_data_filters, {
    req(data())
    date_range <- range(na.omit(data()$submission_date))
    updateSelectInput(session, "data_filter_region", selected = "")
    updateSelectInput(session, "data_filter_status", selected = "")
    updateDateRangeInput(session, "data_date_range", start = date_range[1], end = date_range[2])
  })
  
  # Rafraîchissement initial et périodique
  observe({ refresh_data() }, priority = 1000)
  observeEvent(reactiveTimer(120000)(), { refresh_data() })
  observeEvent(input$refresh_button, { refresh_data() })
  
  # Value boxes
  output$total_submissions <- renderValueBox({
    req(data())
    valueBox(nrow(data()), "Total des questionnaires", icon = icon("file"), color = "blue")
  })
  
  output$valid_submissions <- renderValueBox({
    req(data())
    valueBox(sum(data()$alertes == "Aucune incohérence", na.rm = TRUE), 
             "Questionnaires valides", icon = icon("check"), color = "green")
  })
  
  output$invalid_submissions <- renderValueBox({
    req(data())
    valueBox(sum(data()$alertes != "Aucune incohérence", na.rm = TRUE), 
             "Questionnaires invalides", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$completion_rate <- renderValueBox({
    req(data())
    valid_percent <- round(100 * sum(data()$alertes == "Aucune incohérence", na.rm = TRUE) / nrow(data()), 1)
    valueBox(paste0(valid_percent, "%"), "Taux de validité", icon = icon("percent"), color = "purple")
  })
  
  # Graphique d'activité
  output$activity_chart <- renderPlotly({
    req(data())
    if (!"submission_date" %in% colnames(data())) return(NULL)
    
    # 1. Préparation rigoureuse des données
    activity_data <- data() %>%
      mutate(
        date = as.Date(submission_date),  # Conversion explicite en Date
        status = factor(ifelse(alertes == "Aucune incohérence", "Valide", "Invalide"),
                        levels = c("Valide", "Invalide"))
      ) %>%
      count(date, status, .drop = FALSE)  # Garde toutes les combinaisons
    
    # 2. Création du graphique avec plot_ly pour plus de contrôle
    p <- plot_ly(activity_data) %>%
      add_bars(
        x = ~date, 
        y = ~n, 
        color = ~status,
        colors = c("Valide" = "#4CAF50", "Invalide" = "#F44336"),
        width = 86400000,  # Largeur en ms (1 jour = 86400000 ms)
        hoverinfo = "text",
        text = ~paste("Date:", format(date, "%d %b %Y"),
                      "<br>Statut:", status,
                      "<br>Count:", n)
      ) %>%
      layout(
        barmode = "group",  # Barres côte à côte
        xaxis = list(
          type = "date",
          tickformat = "%d %b",  # Format jour + mois abrégé
          tickmode = "auto",
          nticks = 10,  # Nombre de ticks approximatif
          title = ""
        ),
        yaxis = list(title = "Nombre de questionnaires"),
        hovermode = "x unified",
        showlegend = TRUE
      )
    
    # 3. Ajustement dynamique si peu de données
    if (nrow(activity_data) < 10) {
      p <- p %>% layout(
        xaxis = list(
          dtick = 86400000,  # 1 jour entre chaque tick
          tick0 = min(activity_data$date)
        )
      )
    }
    
    return(p)
  })
  
  # Carte
  output$map <- renderLeaflet({
    map_data <- filtered_map_data() %>% filter(!is.na(latitude) & !is.na(longitude))
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = -17.44, lat = 14.70, zoom = 10) %>%
               addPopups(lng = -17.44, lat = 14.70, popup = "Aucune coordonnée GPS valide disponible"))
    }
    
    popups <- paste0(
      "<strong>ID:</strong> ", substr(map_data$`_uuid`, 1, 8), "...<br>",
      "<strong>Région:</strong> ", map_data$Region, "<br>",
      "<strong>Localité:</strong> ", map_data$localit, "<br>",
      "<strong>Date:</strong> ", format(map_data$submission_date, "%d/%m/%Y"), "<br>",
      "<strong>Alerte:</strong> ", map_data$alertes
    )
    
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~ifelse(alertes == "Aucune incohérence", "green", "red"),
        radius = 8, fillOpacity = 0.8, stroke = FALSE, popup = popups,
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("Valide", "Invalide"),
        title = "Statut des questionnaires"
      )
  })
  
  # Tableau des incohérences
  output$incoherences_table <- renderDT({
    incoherences_data <- filtered_incoherence_data() %>%
      select(
        ID = `_uuid`,
        Region = `Region`, 
        Localite = `localit`,
        Date_Soumission = `submission_date`,
        Type_Alerte = `alertes`,
        Details = `details_incoherence`
      )
    
    if(nrow(incoherences_data) == 0) {
      return(datatable(data.frame(Message = "Aucune incohérence détectée dans les données."),
                       options = list(dom = 't')))
    }
    
    datatable(incoherences_data, 
              options = list(scrollX = TRUE, pageLength = 10),
              selection = 'single',
              rownames = FALSE
    ) %>% formatStyle(
      'Type_Alerte',
      backgroundColor = styleEqual(
        c("Coordonnées GPS manquantes", 
          "Dépenses exactes supérieures aux dépenses mensuelles estimées",
          "Âge incohérent", "Nombre de femmes supérieur au total de personnes",
          "Données manquantes: Age_du_CM", "Données manquantes: Combien_de_personnes_vent_dans_le_m_nage_"),
        c('#ff9999', '#ffcc99', '#99ccff', '#ffff99', '#ddddff', '#ddddff')
      )
    )
  })
  
  # Téléchargement XLS
  output$download_xls <- downloadHandler(
    filename = function() paste("donnees_kobo_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) writexl::write_xlsx(data(), path = file)
  )
  
  # Graphique des types d'incohérences
  # Remplacer renderPlot par renderPlotly pour incoherences_summary
  output$incoherences_summary <- renderPlotly({
    req(data())
    incoherences_data <- data() %>% filter(alertes != "Aucune incohérence")
    
    if(nrow(incoherences_data) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "Aucune incohérence détectée", 
                               showarrow = FALSE, font = list(size = 16)))
    }
    
    # Extraire tous les types d'incohérences
    all_alerts <- unlist(strsplit(incoherences_data$alertes, ", "))
    summary_data <- data.frame(alerte = all_alerts) %>%
      count(alerte) %>%
      arrange(n)  # Tri pour un meilleur affichage horizontal
    
    # Création de l'histogramme horizontal
    plot_ly(summary_data, 
            y = ~reorder(alerte, n),  # Inversion des axes
            x = ~n, 
            type = "bar",
            orientation = "h",  # Orientation horizontale
            marker = list(color = "#FF7043",
                          line = list(color = "#E65100", width = 1))) %>%
      layout(title = "Répartition des types d'incohérences",
             yaxis = list(title = "", tickfont = list(size = 12)),
             xaxis = list(title = "Nombre d'occurrences"),
             margin = list(l = 200)  # Marge gauche plus large pour les libellés
      )
  })
  
  # Ajouter les graphiques manquants
  output$regions_issues <- renderPlotly({
    req(data())
    issues_by_region <- data() %>%
      filter(alertes != "Aucune incohérence") %>%
      count(Region) %>%
      arrange(desc(n))
    
    if(nrow(issues_by_region) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "Aucune incohérence par région", 
                               showarrow = FALSE, font = list(size = 16)))
    }
    
    plot_ly(issues_by_region, x = ~Region, y = ~n, type = "bar", 
            marker = list(color = "#FF5722")) %>%
      layout(title = "Incohérences par région",
             xaxis = list(title = ""),
             yaxis = list(title = "Nombre d'incohérences"))
  })
  
  output$incoherences_types <- renderPlotly({
    req(data())
    
    # Extraire tous les types d'incohérences
    incoherences_data <- data() %>% filter(alertes != "Aucune incohérence")
    
    if(nrow(incoherences_data) == 0) {
      return(plot_ly() %>% 
               add_annotations(text = "Aucune incohérence détectée", 
                               showarrow = FALSE, font = list(size = 16)))
    }
    
    all_alerts <- unlist(strsplit(incoherences_data$alertes, ", "))
    incoherence_counts <- table(all_alerts)
    pie_data <- data.frame(
      type = names(incoherence_counts),
      count = as.numeric(incoherence_counts)
    )
    
    plot_ly(pie_data, labels = ~type, values = ~count, type = 'pie',
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors = colorRampPalette(c("#FF5722", "#FFEB3B"))(nrow(pie_data)))) %>%
      layout(title = "Répartition des types d'incohérences",
             showlegend = FALSE)
  })
  
  output$incoherences_trend <- renderPlotly({
    req(data())
    if(!"submission_date" %in% colnames(data())) return(NULL)
    
    trend_data <- data() %>%
      mutate(status = ifelse(alertes == "Aucune incohérence", "Valide", "Invalide")) %>%
      count(date = submission_date, status) %>%
      pivot_wider(names_from = status, values_from = n, values_fill = list(n = 0)) %>%
      mutate(pourcentage_invalide = ifelse(Valide + Invalide > 0, 
                                           100 * Invalide / (Valide + Invalide), 0))
    
    if(nrow(trend_data) == 0) return(NULL)
    
    plot_ly() %>%
      add_trace(data = trend_data, x = ~date, y = ~pourcentage_invalide, 
                type = 'scatter', mode = 'lines+markers',
                name = "% Invalide", line = list(color = '#E53935')) %>%
      layout(title = "Évolution du taux d'incohérences",
             xaxis = list(title = "Date"),
             yaxis = list(title = "% de questionnaires invalides", rangemode = "tozero"))
  })
  
  # Ajouter les graphiques pour l'onglet Visualisations
 
  output$household_size <- renderPlotly({
    req(data())
    if(!"Combien_de_personnes_vent_dans_le_m_nage_" %in% colnames(data()) || 
       !"Region" %in% colnames(data())) return(NULL)
    
    household_data <- data() %>%
      filter(!is.na(Combien_de_personnes_vent_dans_le_m_nage_), !is.na(Region)) %>%
      group_by(Region) %>%
      summarize(taille_moyenne = mean(as.numeric(Combien_de_personnes_vent_dans_le_m_nage_), na.rm = TRUE))
    
    if(nrow(household_data) == 0) return(NULL)
    
    plot_ly(household_data, x = ~Region, y = ~taille_moyenne, type = "bar",
            marker = list(color = "#26A69A")) %>%
      layout(title = "Taille moyenne des ménages par région",
             xaxis = list(title = "Région"),
             yaxis = list(title = "Nombre moyen de personnes"))
  })
  
  output$expenses_comparison <- renderPlotly({
    req(data())
    expense_cols <- c("Quel_est_la_d_pense_mensuelle_du_m_nage_", 
                      "Quelle_est_la_d_pens_e_du_m_nage_exact_")
    
    if(!all(expense_cols %in% colnames(data()))) return(NULL)
    
    expenses_data <- data() %>%
      filter(!is.na(Quel_est_la_d_pense_mensuelle_du_m_nage_) & 
               !is.na(Quelle_est_la_d_pens_e_du_m_nage_exact_)) %>%
      mutate(
        estimee = as.numeric(Quel_est_la_d_pense_mensuelle_du_m_nage_),
        exacte = as.numeric(Quelle_est_la_d_pens_e_du_m_nage_exact_)
      ) %>%
      select(Region, estimee, exacte) %>%
      group_by(Region) %>%
      summarize(
        Dépense_estimée = mean(estimee, na.rm = TRUE),
        Dépense_exacte = mean(exacte, na.rm = TRUE)
      ) %>%
      pivot_longer(cols = c(Dépense_estimée, Dépense_exacte), 
                   names_to = "Type", values_to = "Montant")
    
    if(nrow(expenses_data) == 0) return(NULL)
    
    plot_ly(expenses_data, x = ~Region, y = ~Montant, color = ~Type, type = "bar",
            colors = c("#7986CB", "#4DB6AC")) %>%
      layout(title = "Comparaison des dépenses estimées et exactes par région",
             xaxis = list(title = "Région"),
             yaxis = list(title = "Montant moyen (FCFA)"),
             barmode = "group")
  })
  
  # Téléchargement CSV
  output$download_csv <- downloadHandler(
    filename = function() paste("donnees_kobo_", Sys.Date(), ".csv", sep = ""),
    content = function(file) write.csv(data(), file, row.names = FALSE)
  )
  
  # Téléchargement des incohérences
  output$download_incoherences <- downloadHandler(
    filename = function() paste("incoherences_kobo_", Sys.Date(), ".xlsx", sep = ""),
    content = function(file) {
      incoherences_data <- data() %>% 
        filter(alertes != "Aucune incohérence") %>%
        select(ID = `_uuid`, Region, localit, submission_date, alertes, details_incoherence)
      writexl::write_xlsx(incoherences_data, path = file)
    }
  )
  
  # Graphique en camembert 
  output$validity_piechart <- renderPlot({
    req(data())
    validity_data <- data() %>%
      mutate(Statut = ifelse(alertes == "Aucune incohérence", "Valide", "Non valide")) %>%
      count(Statut) %>%
      mutate(Percentage = n/sum(n)*100,
             Label = paste0(Statut, "\n", n, " (", round(Percentage, 1), "%)"))
    
    ggplot(validity_data, aes(x = "", y = n, fill = Statut)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar("y", start = 0) +
      scale_fill_manual(values = c("Valide" = "#4CAF50", "Non valide" = "#F44336")) +
      geom_text(aes(label = Label), position = position_stack(vjust = 0.5),
                color = "white", size = 5) +
      labs(title = "Proportion des questionnaires\nvalides et non valides") +
      theme_void() +
      theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
            legend.position = "none")
  })
  
  # Tableau de données brutes
  output$raw_data_table <- renderDT({
    req(filtered_raw_data())
    
    # Sélection des colonnes à afficher (ajustez selon vos besoins)
    display_data <- filtered_raw_data() %>%
      select(
        ID = `_uuid`,
        Date = submission_date,
        Region,
        Localité = localit,
        `Âge CM` = Age_du_CM,
        `Taille ménage` = Combien_de_personnes_vent_dans_le_m_nage_,
        Statut = alertes,
        `Détails incohérences` = details_incoherence
      )
    
    datatable(
      display_data,
      extensions = c('Buttons', 'Scroller'),
      options = list(
        scrollX = TRUE,
        scrollY = 500,
        scroller = TRUE,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf'),
        pageLength = 25,
        language = list(
          search = "Rechercher:",
          paginate = list(previous = 'Précédent', `next` = 'Suivant')
        )
      ),
      rownames = FALSE,
      filter = 'top'  # Ajoute des filtres individuels pour chaque colonne
    ) 
  })
  # Dans votre serveur Shiny
  output$duree_enquete <- renderPlotly({
    req(data())
    
    # Calcul de la durée (adapté du script HFC)
    donnees <- data() %>%
      mutate(duration = as.numeric(difftime(endtime, starttime, units = "mins")))
    
    # Détection des valeurs aberrantes
    outliers <- scores(donnees$duration[!is.na(donnees$duration)], type = "z")
    
    # Visualisation
    plot_ly(donnees, y = ~duration, type = "box", 
            boxpoints = "all", jitter = 0.3,
            pointpos = 0, name = "Durée (minutes)") %>%
      layout(title = "Distribution des durées d'enquête",
             yaxis = list(title = "Durée (minutes)"))
  })
  output$age_distribution_region <- renderPlotly({
    req(data())
    
    # 1. Vérification des colonnes nécessaires
    if(!"Age_du_CM" %in% names(data()) || !"Region" %in% names(data())) {
      showNotification("Colonnes 'Age_du_CM' ou 'Region' manquantes", type = "error")
      return(NULL)
    }
    
    # 2. Préparation des données avec vérification
    plot_data <- data() %>%
      mutate(
        Age_du_CM = as.numeric(as.character(Age_du_CM)),
        Region = as.character(Region)
      ) %>%
      filter(
        !is.na(Age_du_CM), 
        !is.na(Region),
        Age_du_CM >= 15,
        Age_du_CM <= 100
      )
    
    # 3. Vérification si données disponibles
    if(nrow(plot_data) == 0) {
      showNotification("Aucune donnée valide pour le graphique", type = "warning")
      return(NULL)
    }
    
    # 4. Création du graphique avec paramètres optimisés
    p <- plot_ly(
      data = plot_data,
      y = ~Age_du_CM,
      color = ~Region,
      type = "box",
      boxpoints = "all",
      jitter = 0.3,
      pointpos = 0,
      colors = "Set3"
    ) %>%
      layout(
        title = "Distribution de l'âge par région",
        xaxis = list(title = "Région"),
        yaxis = list(title = "Âge (années)", range = c(15, 100)),
        boxmode = "group",
        margin = list(l = 50, r = 50, b = 100, t = 50)
      )
    
    return(p)
  })
  # Ajouter dans la section du serveur, après les autres gestionnaires de téléchargement
  
  # Fonction pour générer un rapport simplifié mais professionnel
  generate_kobo_report <- function(data, output_file) {
    # Vérifier si les données existent
    if (is.null(data) || nrow(data) == 0) {
      stop("Aucune donnée disponible pour générer le rapport")
    }
    
    # Créer un nouveau document Word
    doc <- officer::read_docx()
    
    # Ajouter un en-tête avec logo et titre
    doc <- doc %>%
      officer::body_add_fpar(officer::fpar(
        officer::ftext("Rapport d'enquête KoboToolbox", 
                       officer::fp_text(font.size = 18, bold = TRUE, font.family = "Arial")))) %>%
      officer::body_add_par("") %>%
      officer::body_add_par(paste("Date du rapport:", format(Sys.Date(), "%d/%m/%Y"))) %>%
      officer::body_add_par("")
    
    # 1. RÉSUMÉ GÉNÉRAL
    doc <- doc %>%
      officer::body_add_fpar(officer::fpar(
        officer::ftext("1. Résumé de l'enquête", 
                       officer::fp_text(font.size = 14, bold = TRUE)))) %>%
      officer::body_add_par("")
    
    # Statistiques de base
    total_submissions <- nrow(data)
    valid_submissions <- sum(data$alertes == "Aucune incohérence", na.rm = TRUE)
    invalid_submissions <- total_submissions - valid_submissions
    validity_rate <- round(100 * valid_submissions / total_submissions, 1)
    
    summary_stats <- data.frame(
      Statistique = c("Nombre total de questionnaires", 
                      "Questionnaires valides", 
                      "Questionnaires invalides", 
                      "Taux de validité"),
      Valeur = c(total_submissions, 
                 valid_submissions, 
                 invalid_submissions, 
                 paste0(validity_rate, "%"))
    )
    
    # Ajouter le tableau de statistiques générales
    doc <- doc %>%
      officer::body_add_par("Statistiques générales de l'enquête:") %>%
      flextable::body_add_flextable(
        flextable::flextable(summary_stats) %>% 
          flextable::set_table_properties(width = 1, layout = "autofit") %>%
          flextable::theme_vanilla() %>%
          flextable::fontsize(size = 10, part = "all")
      ) %>%
      officer::body_add_par("")
    
    # Sauvegarder le document
    print(doc, target = output_file)
    
    return(output_file)
  }
  
  
  output$download_report <- downloadHandler(
    filename = function() {
      paste("rapport_", format(Sys.Date(), "%Y%m%d"), ".pdf", sep = "")
    },
    content = function(file) {
      # Créer un fichier temporaire Rmd
      temp_rmd <- tempfile(fileext = ".Rmd")
      
      # Copier le contenu de votre template
      writeLines(readLines("rapport_template.Rmd"), temp_rmd)
      
      # Paramètres à passer
      params <- list(data = data())
      
      # Générer le PDF
      rmarkdown::render(
        input = temp_rmd,
        output_file = file,
        output_format = "pdf_document",
        params = params,
        envir = new.env(parent = globalenv()),
        clean = TRUE
      )
    },
    contentType = "application/pdf"
  )

}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)