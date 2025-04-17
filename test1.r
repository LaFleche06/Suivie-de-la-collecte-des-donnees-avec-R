# Chargement des bibliothèques nécessaires avec vérification
required_packages <- c("shiny", "httr", "jsonlite", "leaflet", "DT", "shinydashboard", 
                      "shinyalert", "dplyr", "writexl", "ggplot2", "tidyr", "lubridate",
                      "shinycssloaders", "shinyWidgets", "plotly")

# Installer les packages manquants
install_missing <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(install_missing)) install.packages(install_missing)

# Charger les packages
invisible(lapply(required_packages, library, character.only = TRUE))

# Configuration API KoboToolbox (à sécuriser)
token <- Sys.getenv("KOBO_TOKEN", "6f8bb50a807ae9e7fda9fba40566da0f9c3383c5")
form_id <- "a9okT245N3K6EBHaW5kEWu"
api_url <- paste0("https://kf.kobotoolbox.org/api/v2/assets/", form_id, "/data/?format=json")

# Cache pour les données avec expiration (15 minutes)
data_cache <- reactiveValues(
  data = NULL,
  timestamp = NULL
)

# Fonction optimisée pour récupérer les données
fetch_data <- function() {
  tryCatch({
    response <- GET(
      api_url, 
      add_headers(
        Authorization = paste("Token", token),
        "Accept" = "application/json"
      ),
      timeout(30)
    )
    
    if (status_code(response) == 200) {
      data <- content(response, "parsed", encoding = "UTF-8")
      if (!is.null(data$results) && length(data$results) > 0) {
        # Conversion optimisée en dataframe
        df <- map_dfr(data$results, flatten) %>% 
          mutate(across(where(is.list), ~map_chr(., ~paste(., collapse = ", "))))
        return(df)
      }
    } else {
      message("Erreur API: ", status_code(response))
    }
    return(NULL)
  }, error = function(e) {
    message("Erreur fetch_data: ", e$message)
    return(NULL)
  })
}

# Vérification des incohérences avec des règles modulaires
# Correction des erreurs dans le code

check_incoherence <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  # Initialisation des alertes
  data <- data %>% 
    mutate(
      alertes = "Aucune incohérence",
      details_incoherence = NA_character_,
      statut_validation = "Valide"
    )
  
  # Liste des règles de validation
  validation_rules <- list(
    # Règle 1: Coordonnées GPS manquantes
    list(
      condition = ~is.na(`_geolocation`) | `_geolocation` == "",
      message = "Coordonnées GPS manquantes",
      details = "Les coordonnées GPS n'ont pas été enregistrées lors de la collecte."
    ),
    
    # Règle 2: Dépenses exactes > 2x dépenses mensuelles
    list(
      condition = ~suppressWarnings(as.numeric(Quelle_est_la_d_pens_e_du_m_nage_exact_)) > 
        (2 * suppressWarnings(as.numeric(Quel_est_la_d_pense_mensuelle_du_m_nage_))),
      message = "Dépenses exactes supérieures aux dépenses mensuelles estimées",
      details = function(row) {
        paste0("Dépense exacte (", row$Quelle_est_la_d_pens_e_du_m_nage_exact_, 
               ") > 2x dépense mensuelle (", row$Quel_est_la_d_pense_mensuelle_du_m_nage_, ")")
      }
    ),
    
    # Règle 3: Âge incohérent
    list(
      condition = ~suppressWarnings(as.numeric(Age_du_CM)) < 15 | suppressWarnings(as.numeric(Age_du_CM)) > 120,
      message = "Âge incohérent",
      details = function(row) {
        paste0("Âge (", row$Age_du_CM, ") ", 
               ifelse(suppressWarnings(as.numeric(row$Age_du_CM)) < 15, "< 15 ans", "> 120 ans"))
      }
    ),
    
    # Règle 4: Nombre de femmes > total personnes
    list(
      condition = ~suppressWarnings(as.numeric(Combien_de_femmes_vivent_dans_le_m_nage_)) > 
        suppressWarnings(as.numeric(Combien_de_personnes_vivent_dans_le_m_nage_)),
      message = "Nombre de femmes > total personnes",
      details = function(row) {
        paste0("Femmes: ", row$Combien_de_femmes_vivent_dans_le_m_nage_, 
               " > Total: ", row$Combien_de_personnes_vivent_dans_le_m_nage_)
      }
    )
  )
  
  # Application des règles
  for (rule in validation_rules) {
    rows <- which(eval(rule$condition, envir = data))
    
    if (length(rows)) {  # Correction : ajout de la parenthèse fermante
      data$statut_validation[rows] <- "Invalide"
      data$alertes[rows] <- ifelse(
        data$alertes[rows] == "Aucune incohérence",
        rule$message,
        paste(data$alertes[rows], rule$message, sep = "; ")
      )
      
      for (i in rows) {
        details <- if (is.function(rule$details)) {
          rule$details(data[i, ])
        } else {
          rule$details
        }
        
        data$details_incoherence[i] <- ifelse(
          is.na(data$details_incoherence[i]),
          details,
          paste(data$details_incoherence[i], details, sep = "\n")
        )
      }
    }
  }
  
  return(data)
}

# Extraction optimisée des coordonnées
extract_coordinates <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  data %>% mutate(
    latitude = case_when(
      is.na(`_geolocation`) ~ NA_real_,
      `_geolocation` == "" ~ NA_real_,
      grepl(",", `_geolocation`) ~ as.numeric(str_split(`_geolocation`, ",", simplify = TRUE)[,1]),
      TRUE ~ NA_real_
    ),
    longitude = case_when(
      is.na(`_geolocation`) ~ NA_real_,
      `_geolocation` == "" ~ NA_real_,
      grepl(",", `_geolocation`) ~ as.numeric(str_split(`_geolocation`, ",", simplify = TRUE)[,2]),
      TRUE ~ NA_real_
    ),
    popup_content = paste(
      "<strong>ID:</strong>", `_uuid`, "<br>",
      "<strong>Date:</strong>", `_submission_time`, "<br>",
      "<strong>Statut:</strong>", statut_validation, "<br>",
      "<strong>Alertes:</strong>", alertes
    )
  )
}

# UI modernisée avec shinyDashboardPlus
ui <- dashboardPage(
  skin = "blue-light",
  dashboardHeader(
    title = span(tagList(icon("map-marked-alt"), "Suivi d'Enquête KoboToolbox")),
    titleWidth = 300,
    dropdownMenuOutput("notificationMenu")
  ),
  
  dashboardSidebar(
    width = 300,
    sidebarMenu(
      id = "tabs",
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("tachometer-alt")),
      menuItem("Visualisation spatiale", tabName = "map", icon = icon("map")),
      menuItem("Analyse des données", tabName = "analysis", icon = icon("chart-bar"),
               menuSubItem("Qualité des données", tabName = "quality"),
               menuSubItem("Statistiques", tabName = "stats")),
      menuItem("Données brutes", tabName = "rawdata", icon = icon("database"))
    ),
    
    hr(),
    
    awesomeCheckboxGroup(
      inputId = "filter_status",
      label = "Filtrer par statut:", 
      choices = c("Valide", "Invalide"),
      selected = c("Valide", "Invalide"),
      inline = TRUE
    ),
    
    dateRangeInput(
      "date_range",
      "Période:",
      start = Sys.Date() - 30,
      end = Sys.Date(),
      language = "fr"
    ),
    
    actionBttn(
      "refresh_button",
      "Actualiser les données",
      icon = icon("sync"),
      style = "gradient",
      color = "primary",
      block = TRUE
    ),
    
    hr(),
    
    div(
      style = "padding: 10px; text-align: center;",
      downloadBttn(
        "download_report",
        "Générer rapport",
        style = "gradient",
        color = "success"
      )
    )
  ),
  
  dashboardBody(
    useShinyalert(),
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.3/css/all.min.css"),
      tags$style(HTML("
        .skin-blue-light .main-header .logo {background-color: #3c8dbc;}
        .skin-blue-light .main-header .logo:hover {background-color: #3c8dbc;}
        .box.box-solid.box-primary>.box-header {background: #3c8dbc;}
        .box.box-solid.box-danger>.box-header {background: #dd4b39;}
        .box.box-solid.box-success>.box-header {background: #00a65a;}
        .box-title {font-weight: bold;}
        .info-box-icon {height: 90px !important; line-height: 90px !important;}
        .info-box-content {padding-top: 10px; padding-bottom: 10px;}
      "))
    ),
    
    tabItems(
      # Tableau de bord
      tabItem(
        tabName = "dashboard",
        fluidRow(
          infoBoxOutput("total_submissions", width = 3),
          infoBoxOutput("valid_submissions", width = 3),
          infoBoxOutput("invalid_submissions", width = 3),
          infoBoxOutput("completion_rate", width = 3)
        ),
        
        fluidRow(
          box(
            title = "Évolution des soumissions", 
            status = "primary", 
            solidHeader = TRUE,
            width = 8,
            withSpinner(plotlyOutput("submission_trend"))
          ),
          
          box(
            title = "Répartition par région", 
            status = "info", 
            solidHeader = TRUE,
            width = 4,
            withSpinner(plotlyOutput("region_distribution"))
          )
        ),
        
        fluidRow(
          box(
            title = "Dernières soumissions", 
            status = "primary", 
            solidHeader = TRUE,
            width = 12,
            withSpinner(DTOutput("recent_submissions")))
      ),
      
      # Carte
      tabItem(
        tabName = "map",
        box(
          title = "Carte interactive des enquêtes", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          leafletOutput("map", height = "700px") %>% withSpinner(),
          absolutePanel(
            top = 20, right = 20, width = 300,
            draggable = TRUE,
            wellPanel(
              style = "opacity: 0.9;",
              selectInput(
                "map_layer",
                "Couche de fond:",
                choices = c("OpenStreetMap", "Satellite", "Terrain"),
                selected = "OpenStreetMap"
              ),
              checkboxInput("cluster", "Regroupement", TRUE)
            )
          )
        )
      ),
      
      # Analyse qualité
      tabItem(
        tabName = "quality",
        fluidRow(
          box(
            title = "Types d'incohérences", 
            status = "danger", 
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("incoherences_summary"))),
          
          box(
            title = "Proportion valide/invalide", 
            status = "info", 
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("validity_piechart")))
        ),
        
        box(
          title = "Détail des incohérences", 
          status = "warning", 
          solidHeader = TRUE,
          width = 12,
          withSpinner(DTOutput("incoherences_table")))
      ),
      
      # Statistiques
      tabItem(
        tabName = "stats",
        fluidRow(
          box(
            title = "Distribution des âges", 
            status = "primary", 
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("age_distribution"))),
          
          box(
            title = "Dépenses mensuelles", 
            status = "success", 
            solidHeader = TRUE,
            width = 6,
            withSpinner(plotlyOutput("expenses_boxplot")))
        ),
        
        fluidRow(
          box(
            title = "Taille des ménages", 
            status = "info", 
            solidHeader = TRUE,
            width = 12,
            withSpinner(plotlyOutput("household_size")))
        )
      ),
      
      # Données brutes
      tabItem(
        tabName = "rawdata",
        box(
          title = "Données complètes", 
          status = "primary", 
          solidHeader = TRUE,
          width = 12,
          div(
            style = "margin-bottom: 10px;",
            downloadButton("download_xls", "Excel", class = "btn-success"),
            downloadButton("download_csv", "CSV", class = "btn-info"),
            downloadButton("download_rds", "RDS", class = "btn-warning")
          ),
          withSpinner(DTOutput("raw_data_table")))
      )
    )
  )
)

# Serveur optimisé
server <- function(input, output, session) {
  # Reactive values pour stocker les données
  app_data <- reactiveValues(
    raw = NULL,
    processed = NULL,
    last_update = NULL
  )
  
  # Notification menu
  output$notificationMenu <- renderMenu({
    invalid_count <- if (!is.null(app_data$processed)) {
      sum(app_data$processed$statut_validation == "Invalide", na.rm = TRUE)
    } else 0
    
    dropdownMenu(
      type = "notifications", 
      badgeStatus = if (invalid_count > 0) "danger" else "success",
      .list = if (invalid_count > 0) {
        list(notificationItem(
          text = paste(invalid_count, "questionnaires invalides"),
          icon = icon("exclamation-triangle"),
          status = "danger"
        ))
      } else {
        list(notificationItem(
          text = "Tous les questionnaires sont valides",
          icon = icon("check-circle"),
          status = "success"
        ))
      }
    )
  })
  
  # Fonction de rafraîchissement des données avec cache
  refresh_data <- function(force = FALSE) {
    # Vérifier si le cache est encore valide (15 minutes)
    if (!force && !is.null(app_data$last_update) && 
        difftime(Sys.time(), app_data$last_update, units = "mins") < 15) {
      return()
    }
    
    showNotification("Mise à jour des données en cours...", type = "message")
    
    tryCatch({
      raw_data <- fetch_data()
      
      if (!is.null(raw_data)) {
        processed_data <- raw_data %>% 
          check_incoherence() %>% 
          extract_coordinates() %>%
          mutate(
            submission_date = as.Date(`_submission_time`),
            Age_du_CM = as.numeric(Age_du_CM),
            depense_mensuelle = as.numeric(Quel_est_la_d_pense_mensuelle_du_m_nage_),
            taille_menage = as.numeric(Combien_de_personnes_vent_dans_le_m_nage_)
          )
        
        app_data$raw <- raw_data
        app_data$processed <- processed_data
        app_data$last_update <- Sys.time()
        
        showNotification("Données mises à jour avec succès", type = "message")
      } else {
        showNotification("Erreur lors de la récupération des données", type = "error")
      }
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error")
    })
  }
  
  # Rafraîchissement initial et périodique
  observe({
    refresh_data()
    invalidateLater(900000) # Rafraîchissement toutes les 15 minutes
  })
  
  # Rafraîchissement manuel
  observeEvent(input$refresh_button, {
    refresh_data(force = TRUE)
  })
  
  # Filtrer les données selon les sélections
  filtered_data <- reactive({
    req(app_data$processed)
    
    data <- app_data$processed %>%
      filter(
        statut_validation %in% input$filter_status,
        submission_date >= input$date_range[1],
        submission_date <= input$date_range[2]
      )
    
    return(data)
  })
  
  # Value boxes
  output$total_submissions <- renderInfoBox({
    data <- filtered_data()
    infoBox(
      "Total", 
      nrow(data), 
      icon = icon("file-alt"),
      color = "blue",
      fill = TRUE
    )
  })
  
  output$valid_submissions <- renderInfoBox({
    data <- filtered_data()
    count <- sum(data$statut_validation == "Valide", na.rm = TRUE)
    infoBox(
      "Valides", 
      count, 
      icon = icon("check-circle"),
      color = "green",
      fill = TRUE
    )
  })
  
  output$invalid_submissions <- renderInfoBox({
    data <- filtered_data()
    count <- sum(data$statut_validation == "Invalide", na.rm = TRUE)
    infoBox(
      "Invalides", 
      count, 
      icon = icon("exclamation-triangle"),
      color = "red",
      fill = TRUE
    )
  })
  
  output$completion_rate <- renderInfoBox({
    data <- filtered_data()
    rate <- if (nrow(data) > 0) {
      round(sum(data$statut_validation == "Valide") / nrow(data) * 100, 1)
    } else 0
    
    infoBox(
      "Taux de complétude", 
      paste0(rate, "%"), 
      icon = icon("percent"),
      color = if (rate > 90) "green" else if (rate > 70) "yellow" else "red",
      fill = TRUE
    )
  })
  
  # Carte interactive
  output$map <- renderLeaflet({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "Aucune donnée à afficher"))
    
    # Préparation des données spatiales
    spatial_data <- data %>% 
      filter(!is.na(latitude), !is.na(longitude)) %>%
      mutate(
        color = ifelse(statut_validation == "Valide", "green", "red"),
        radius = ifelse(statut_validation == "Valide", 6, 8)
      )
    
    validate(need(nrow(spatial_data) > 0, "Aucune donnée spatiale valide"))
    
    # Choix de la couche de fond
    base_layer <- switch(input$map_layer,
                        "OpenStreetMap" = providers$OpenStreetMap,
                        "Satellite" = providers$Esri.WorldImagery,
                        "Terrain" = providers$Esri.WorldTopoMap)
    
    # Création de la carte
    map <- leaflet(spatial_data) %>%
      addProviderTiles(base_layer) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("Valide", "Invalide"),
        title = "Statut des questionnaires"
      )
    
    # Ajout des marqueurs avec ou sans clustering
    if (input$cluster) {
      map <- map %>%
        addCircleMarkers(
          ~longitude, ~latitude,
          color = ~color,
          radius = ~radius,
          fillOpacity = 0.8,
          stroke = FALSE,
          popup = ~popup_content,
          clusterOptions = markerClusterOptions()
        )
    } else {
      map <- map %>%
        addCircleMarkers(
          ~longitude, ~latitude,
          color = ~color,
          radius = ~radius,
          fillOpacity = 0.8,
          stroke = FALSE,
          popup = ~popup_content
        )
    }
    
    # Ajustement de la vue
    map %>% fitBounds(
      lng1 = min(spatial_data$longitude),
      lat1 = min(spatial_data$latitude),
      lng2 = max(spatial_data$longitude),
      lat2 = max(spatial_data$latitude)
    )
  })
  
  # Graphique des tendances de soumission
  output$submission_trend <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "Aucune donnée à afficher"))
    
    daily_counts <- data %>%
      mutate(date = as.Date(`_submission_time`)) %>%
      count(date, statut_validation) %>%
      complete(date = seq(min(date), max(date), by = "day"), 
                statut_validation, fill = list(n = 0))
    
    plot_ly(daily_counts, x = ~date, y = ~n, color = ~statut_validation, 
            type = 'scatter', mode = 'lines+markers',
            colors = c("Valide" = "#4CAF50", "Invalide" = "#F44336")) %>%
      layout(
        title = "Évolution des soumissions par jour",
        xaxis = list(title = "Date"),
        yaxis = list(title = "Nombre de soumissions"),
        hovermode = "x unified"
      )
  })
  
  # Graphique de distribution par région
  output$region_distribution <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0 && "Region" %in% names(data), "Aucune donnée régionale à afficher"))
    
    region_counts <- data %>%
      count(Region, statut_validation) %>%
      group_by(Region) %>%
      mutate(percentage = n / sum(n) * 100) %>%
      ungroup()
    
    plot_ly(region_counts, x = ~Region, y = ~percentage, color = ~statut_validation,
            type = 'bar', colors = c("Valide" = "#4CAF50", "Invalide" = "#F44336")) %>%
      layout(
        title = "Répartition par région",
        xaxis = list(title = ""),
        yaxis = list(title = "Pourcentage", ticksuffix = "%"),
        barmode = 'stack'
      )
  })
  
  # Tableau des dernières soumissions
  output$recent_submissions <- renderDT({
    data <- filtered_data() %>%
      arrange(desc(`_submission_time`)) %>%
      head(20) %>%
      select(
        ID = `_uuid`,
        Date = `_submission_time`,
        Region,
        Localite = localit,
        Statut = statut_validation,
        Alertes = alertes
      )
    
    datatable(
      data,
      options = list(
        dom = 't',
        scrollX = TRUE,
        pageLength = 10
      ),
      rownames = FALSE
    ) %>% formatStyle(
      'Statut',
      backgroundColor = styleEqual(c("Valide", "Invalide"), c('#4CAF50', '#F44336')),
      color = styleEqual(c("Valide", "Invalide"), c('white', 'white'))
  })
  
  # Graphique des types d'incohérences
  output$incoherences_summary <- renderPlotly({
    data <- filtered_data() %>%
      filter(statut_validation == "Invalide")
    
    validate(need(nrow(data) > 0, "Aucune incohérence détectée"))
    
    # Extraire les types d'incohérences (séparés par ;)
    incoherences <- data %>%
      mutate(incoherence = strsplit(alertes, ";")) %>%
      unnest(incoherence) %>%
      mutate(incoherence = trimws(incoherence)) %>%
      filter(incoherence != "Aucune incohérence") %>%
      count(incoherence) %>%
      arrange(desc(n))
    
    plot_ly(incoherences, x = ~n, y = ~reorder(incoherence, n), type = 'bar',
            orientation = 'h', marker = list(color = '#FF7043')) %>%
      layout(
        title = "Types d'incohérences détectées",
        xaxis = list(title = "Nombre"),
        yaxis = list(title = ""),
        margin = list(l = 150)
      )
  })
  
  # Camembert valide/invalide
  output$validity_piechart <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "Aucune donnée à afficher"))
    
    validity_counts <- data %>%
      count(statut_validation) %>%
      mutate(percentage = n / sum(n) * 100)
    
    plot_ly(validity_counts, labels = ~statut_validation, values = ~n, type = 'pie',
            marker = list(colors = c("#4CAF50", "#F44336")),
            textinfo = 'label+percent',
            hoverinfo = 'text+value',
            text = ~paste(n, "questionnaires")) %>%
      layout(
        title = "Répartition valide/invalide",
        showlegend = FALSE
      )
  })
  
  # Tableau des incohérences
  output$incoherences_table <- renderDT({
    data <- filtered_data() %>%
      filter(statut_validation == "Invalide") %>%
      select(
        ID = `_uuid`,
        Date = `_submission_time`,
        Region,
        Localite = localit,
        Type_Alerte = alertes,
        Details = details_incoherence
      )
    
    validate(need(nrow(data) > 0, "Aucune incohérence détectée"))
    
    datatable(
      data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel')
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      selection = 'single'
    )
  })
  
  # Distribution des âges
  output$age_distribution <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0 && "Age_du_CM" %in% names(data), "Aucune donnée d'âge disponible"))
    
    plot_ly(data, x = ~Age_du_CM, color = ~statut_validation, type = "histogram",
            colors = c("Valide" = "#4CAF50", "Invalide" = "#F44336"),
            nbinsx = 20) %>%
      layout(
        title = "Distribution des âges des chefs de ménage",
        xaxis = list(title = "Âge"),
        yaxis = list(title = "Nombre"),
        barmode = "overlay"
      )
  })
  
  # Boxplot des dépenses
  output$expenses_boxplot <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0 && "depense_mensuelle" %in% names(data), "Aucune donnée de dépenses disponible"))
    
    plot_ly(data, y = ~depense_mensuelle, color = ~statut_validation, type = "box",
            colors = c("Valide" = "#4CAF50", "Invalide" = "#F44336")) %>%
      layout(
        title = "Distribution des dépenses mensuelles",
        yaxis = list(title = "Dépense mensuelle"),
        showlegend = FALSE
      )
  })
  
  # Taille des ménages
  output$household_size <- renderPlotly({
    data <- filtered_data()
    validate(need(nrow(data) > 0 && "taille_menage" %in% names(data), "Aucune donnée sur la taille des ménages"))
    
    plot_ly(data, x = ~taille_menage, color = ~statut_validation, type = "histogram",
            colors = c("Valide" = "#4CAF50", "Invalide" = "#F44336"),
            nbinsx = 10) %>%
      layout(
        title = "Répartition de la taille des ménages",
        xaxis = list(title = "Nombre de personnes"),
        yaxis = list(title = "Nombre de ménages"),
        barmode = "stack"
      )
  })
  
  # Tableau des données brutes
  output$raw_data_table <- renderDT({
    data <- filtered_data()
    validate(need(nrow(data) > 0, "Aucune donnée disponible"))
    
    datatable(
      data,
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        dom = 'Bfrtip',
        buttons = c('copy', 'csv', 'excel', 'pdf')
      ),
      extensions = 'Buttons',
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Téléchargement des données
  output$download_xls <- downloadHandler(
    filename = function() {
      paste("kobo_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(filtered_data(), path = file)
    }
  )
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("kobo_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$download_rds <- downloadHandler(
    filename = function() {
      paste("kobo_data_", Sys.Date(), ".rds", sep = "")
    },
    content = function(file) {
      saveRDS(filtered_data(), file)
    }
  )
  
  # Génération de rapport
  output$download_report <- downloadHandler(
    filename = function() {
      paste("kobo_report_", Sys.Date(), ".html", sep = "")
    },
    content = function(file) {
      # Copier le fichier Rmd temporaire
      temp_report <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", temp_report, overwrite = TRUE)
      
      # Paramètres à passer au rapport
      params <- list(
        data = filtered_data(),
        date_range = input$date_range,
        status_filter = input$filter_status
      )
      
      # Knit le document
      rmarkdown::render(
        temp_report,
        output_file = file,
        params = params,
        envir = new.env(parent = globalenv())
      )
    }
  )
}

# Lancer l'application
shinyApp(ui, server)