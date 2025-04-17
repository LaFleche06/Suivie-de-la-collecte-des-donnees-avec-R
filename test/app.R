# Chargement des bibliothèques nécessaires
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(DT)
library(shinydashboard)
library(shinyalert)
library(dplyr)
library(writexl)  # Pour l'export en Excel

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

check_incoherence <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data.frame())
  
  # Initialisation des colonnes d'alerte
  data$alertes <- "Aucune incohérence"
  data$details_incoherence <- NA
  
  # 1. Vérification des coordonnées GPS
  if ("_geolocation" %in% colnames(data)) {
    if (is.character(data$`_geolocation`)) {
      gps_missing <- is.na(data$`_geolocation`) | data$`_geolocation` == ""
    } else if (is.list(data$`_geolocation`)) {
      gps_missing <- sapply(data$`_geolocation`, function(x) {
        is.null(x) || length(x) == 0 || (length(x) > 0 && (is.na(x[1]) || is.na(x[2])))
      })
    }
    
    if (any(gps_missing)) {
      data$alertes[gps_missing] <- "Coordonnées GPS manquantes"
      data$details_incoherence[gps_missing] <- "Les coordonnées GPS n'ont pas été enregistrées lors de la collecte."
    }
  }
  
  # 2. Vérification des dépenses
  depense_col <- "Quelle_est_la_d_pens_e_du_m_nage_exact_"
  depense_mensuelle_col <- "Quel_est_la_d_pense_mensuelle_du_m_nage_"
  
  if (depense_col %in% colnames(data) && depense_mensuelle_col %in% colnames(data)) {
    # Conversion en numérique
    if (!is.numeric(data[[depense_col]])) {
      data[[depense_col]] <- as.numeric(as.character(data[[depense_col]]))
    }
    if (!is.numeric(data[[depense_mensuelle_col]])) {
      data[[depense_mensuelle_col]] <- as.numeric(as.character(data[[depense_mensuelle_col]]))
    }
    
    # Vérification de la cohérence
    dep_incoherent <- !is.na(data[[depense_col]]) & 
      !is.na(data[[depense_mensuelle_col]]) &
      data[[depense_col]] > (data[[depense_mensuelle_col]] * 10000 * 1.2) # 20% de marge
    
    if (any(dep_incoherent)) {
      data$alertes[dep_incoherent] <- "Dépenses exactes supérieures aux dépenses mensuelles estimées"
      
      for (i in which(dep_incoherent)) {
        data$details_incoherence[i] <- paste0(
          "Dépense exacte (", formatC(data[[depense_col]][i], format="f", big.mark=" ", digits=0), 
          ") est supérieure à la dépense mensuelle estimée (", 
          formatC(data[[depense_mensuelle_col]][i] * 10000, format="f", big.mark=" ", digits=0), 
          ") avec plus de 20% d'écart."
        )
      }
    }
  }
  
  # 3. Vérification de l'âge du chef de ménage
  age_col <- "Age_du_CM"
  if (age_col %in% colnames(data)) {
    if (!is.numeric(data[[age_col]])) {
      data[[age_col]] <- as.numeric(as.character(data[[age_col]]))
    }
    
    age_incoherent <- !is.na(data[[age_col]]) & (data[[age_col]] < 15 | data[[age_col]] > 120)
    
    if (any(age_incoherent)) {
      data$alertes[age_incoherent] <- "Âge incohérent"
      
      for (i in which(age_incoherent)) {
        if (data[[age_col]][i] < 15) {
          data$details_incoherence[i] <- paste0(
            "L'âge du chef de ménage (", data[[age_col]][i], 
            ") est inférieur à 15 ans, ce qui est improbable."
          )
        } else {
          data$details_incoherence[i] <- paste0(
            "L'âge du chef de ménage (", data[[age_col]][i], 
            ") est supérieur à 120 ans, ce qui est improbable."
          )
        }
      }
    }
  }
  
  # 4. Vérification nombre de personnes vs femmes
  personnes_col <- "Combien_de_personnes_vent_dans_le_m_nage_"
  femmes_col <- "Combien_de_femmes_vivent_dans_le_m_nage_"
  
  if (personnes_col %in% colnames(data) && femmes_col %in% colnames(data)) {
    if (!is.numeric(data[[personnes_col]])) {
      data[[personnes_col]] <- as.numeric(as.character(data[[personnes_col]]))
    }
    if (!is.numeric(data[[femmes_col]])) {
      data[[femmes_col]] <- as.numeric(as.character(data[[femmes_col]]))
    }
    
    pers_incoherent <- !is.na(data[[personnes_col]]) & 
      !is.na(data[[femmes_col]]) &
      data[[personnes_col]] < data[[femmes_col]]
    
    if (any(pers_incoherent)) {
      data$alertes[pers_incoherent] <- "Nombre de femmes supérieur au total de personnes"
      
      for (i in which(pers_incoherent)) {
        data$details_incoherence[i] <- paste0(
          "Le nombre de femmes (", data[[femmes_col]][i], 
          ") est supérieur au nombre total de personnes dans le ménage (", 
          data[[personnes_col]][i], ")."
        )
      }
    }
  }
  
  # 5. Vérification des données manquantes obligatoires
  champs_obligatoires <- c("Age_du_CM", "Combien_de_personnes_vent_dans_le_m_nage_")
  
  for (champ in champs_obligatoires) {
    if (champ %in% colnames(data)) {
      manquant <- is.na(data[[champ]]) | data[[champ]] == ""
      if (any(manquant) && data$alertes[manquant] == "Aucune incohérence") {
        data$alertes[manquant] <- paste0("Données manquantes: ", champ)
        data$details_incoherence[manquant] <- paste0(
          "La valeur pour le champ '", champ, "' est manquante alors qu'elle est obligatoire."
        )
      }
    }
  }
  
  return(data)
}

# -- Fonction pour extraire correctement les coordonnées géographiques
extract_coordinates <- function(data) {
  if (is.null(data) || nrow(data) == 0) return(data)
  
  # Initialiser les colonnes de latitude et longitude si elles n'existent pas
  if (!("latitude" %in% colnames(data)) || !("longitude" %in% colnames(data))) {
    data$latitude <- NA
    data$longitude <- NA
  }
  
  # Traiter selon le format des coordonnées
  if ("_geolocation" %in% colnames(data)) {
    if (is.character(data$`_geolocation`)) {
      # Format: "lat,lon" comme chaîne de caractères
      valid_coords <- !is.na(data$`_geolocation`) & data$`_geolocation` != ""
      if (any(valid_coords)) {
        coord_split <- strsplit(as.character(data$`_geolocation`[valid_coords]), ",")
        
        lat_values <- sapply(coord_split, function(x) {
          if (length(x) >= 1) as.numeric(trimws(x[1])) else NA
        })
        lon_values <- sapply(coord_split, function(x) {
          if (length(x) >= 2) as.numeric(trimws(x[2])) else NA
        })
        
        data$latitude[valid_coords] <- lat_values
        data$longitude[valid_coords] <- lon_values
      }
    } else if (is.list(data$`_geolocation`)) {
      # Format: liste de coordonnées [lat, lon]
      for (i in 1:nrow(data)) {
        geo <- data$`_geolocation`[[i]]
        if (!is.null(geo) && length(geo) >= 2 && !is.na(geo[1]) && !is.na(geo[2])) {
          data$latitude[i] <- geo[1]
          data$longitude[i] <- geo[2]
        }
      }
    }
  }
  
  return(data)
}

# -- Interface utilisateur simplifiée et fonctionnelle
ui <- dashboardPage(
  dashboardHeader(title = "Suivi d'Enquête KoboToolbox"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tableau de bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Carte", tabName = "map", icon = icon("map")),
      menuItem("Détails des incohérences", tabName = "incoherences", icon = icon("exclamation-triangle")),
      menuItem("Données brutes", tabName = "rawdata", icon = icon("table"))
    ),
    actionButton("refresh_button", "Rafraîchir les données", icon = icon("refresh"))
  ),
  dashboardBody(
    useShinyalert(force=TRUE),
    tabItems(
      # Onglet Tableau de bord
      tabItem(tabName = "dashboard",
              fluidRow(
                valueBoxOutput("total_submissions"),
                valueBoxOutput("valid_submissions"),
                valueBoxOutput("invalid_submissions")
              ),
              fluidRow(
                box(title = "Tableau des questionnaires soumis", status = "primary", width = 12,
                    DTOutput("data_table"),
                    downloadButton("download_xls", "Télécharger en XLS", 
                                   style = "color: white; background-color: #3c8dbc; margin-top: 10px;")
                )
              )
      ),
      
      # Onglet Carte
      tabItem(tabName = "map",
              fluidRow(
                box(title = "Carte des questionnaires", status = "primary", width = 12,
                    leafletOutput("map", height = 600))
              )
      ),
      
      # Onglet Détails des incohérences - MODIFIÉ
      tabItem(tabName = "incoherences",
              fluidRow(
                box(title = "Analyse des incohérences", status = "warning", width = 12,
                    p("Ce tableau présente uniquement les questionnaires comportant des incohérences, avec des détails sur les problèmes détectés."),
                    DTOutput("incoherences_table"))
              ),
              fluidRow(
                box(title = "Résumé des incohérences", status = "danger", width = 6,
                    plotOutput("incoherences_summary", height = 350)),
                box(title = "Distribution par région", status = "info", width = 6,
                    plotOutput("incoherences_by_region", height = 350))
              ),
              fluidRow(
                box(title = "Évolution des incohérences dans le temps", status = "primary", width = 12,
                    plotOutput("incoherences_timeline", height = 300))
              ),
              fluidRow(
                box(title = "Tableau récapitulatif par région", status = "warning", width = 12,
                    DTOutput("region_summary_table"))
              ),
              fluidRow(
                box(title = "Détail d'un questionnaire", status = "info", width = 12,
                    uiOutput("questionnaire_details"))
              )
      ),
      
      # Onglet Données brutes
      tabItem(tabName = "rawdata",
              fluidRow(
                box(title = "Données brutes", status = "warning", width = 12,
                    DTOutput("raw_data_table"))
              )
      )
    )
  )
)
# -- Serveur
server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  selected_questionnaire <- reactiveVal(NULL)
  
  refresh_data <- function() {
    new_data <- fetch_data()
    if (!is.null(new_data)) {
      # Extraire les coordonnées géographiques
      new_data <- extract_coordinates(new_data)
      
      # Vérification des incohérences
      new_data <- check_incoherence(new_data)
      data(new_data)
      
      # Déclencher des alertes instantanées pour les soumissions invalides
      invalid_data <- new_data %>% filter(alertes != "Aucune incohérence")
      
      if (nrow(invalid_data) > 0) {
        shinyalert(
          title = "Alertes d'incohérences détectées!",
          text = paste0(nrow(invalid_data), " questionnaires présentent des incohérences. Consultez l'onglet 'Détails des incohérences' pour plus d'informations."),
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
  
  send_alert_email <- function(alert_data) {
    alert_text <- paste(
      "Subject: [Bastula] Alerte\n\n",
      "Nouvelle alerte :", alert_data$alertes,
      "\nDétails :", alert_data$details_incoherence,
      "\nID :", alert_data$`_uuid`,
      "\nDate :", Sys.time()
    )
    
    # Solution pour Linux/Unix (utilise le client mail système)
    if (Sys.which("mail") != "") {
      system(paste0(
        'echo "', alert_text, '" | mail -s "[Bastula] Alerte" somaben791@gmail.com'
      ))
    }
    # Solution pour Windows (nécessite Blat ou autre client mail)
    else if (.Platform$OS.type == "windows") {
      temp_file <- tempfile()
      writeLines(alert_text, temp_file)
      system(paste0('blat ', temp_file, ' -to somaben791@gmail.com -subject "[Bastula] Alerte"'))
    }
  }
  
  # Rafraîchissement initial des données
  observe({
    refresh_data()
  }, priority = 1000)
  
  # Rafraîchissement des données toutes les 60 secondes
  autoInvalidate <- reactiveTimer(60000)
  observe({
    autoInvalidate()
    refresh_data()
  })
  
  # Rafraîchissement manuel des données
  observeEvent(input$refresh_button, {
    refresh_data()
  })
  
  output$total_submissions <- renderValueBox({
    req(data())
    valueBox(nrow(data()), "Total des questionnaires", icon = icon("file"), color = "blue")
  })
  
  output$valid_submissions <- renderValueBox({
    req(data())
    valid_count <- sum(data()$alertes == "Aucune incohérence", na.rm = TRUE)
    valueBox(valid_count, "Questionnaires valides", icon = icon("check"), color = "green")
  })
  
  output$invalid_submissions <- renderValueBox({
    req(data())
    invalid_count <- sum(data()$alertes != "Aucune incohérence", na.rm = TRUE)
    valueBox(invalid_count, "Questionnaires invalides", icon = icon("exclamation-triangle"), color = "red")
  })
  
  output$data_table <- renderDT({
    req(data())
    
    # Sélectionner et renommer les colonnes pertinentes
    selected_data <- data() %>%
      select(
        ID = `_uuid`,
        Region = `Regiion`,  # Notez le nouveau nom "Regiion" avec deux 'i'
        Localite = `localit`,
        Age_CM = `Age_du_CM`,
        Nb_Personnes = `Combien_de_personnes_vent_dans_le_m_nage_`,
        Nb_Femmes = `Combien_de_femmes_vivent_dans_le_m_nage_`,
        Depense_Mensuelle = `Quel_est_la_d_pense_mensuelle_du_m_nage_`,
        Depense_Exacte = `Quelle_est_la_d_pens_e_du_m_nage_exact_`,  # Notez le '_' final
        Date_Soumission = `_submission_time`,
        Alertes = `alertes`
      )
    
    datatable(selected_data, 
              options = list(
                scrollX = TRUE,
                pageLength = 10,
                rowCallback = JS(
                  "function(row, data) {
                    if (data[9] !== 'Aucune incohérence') {
                      $('td', row).css('background-color', '#ffdddd');
                    }
                  }"
                )
              )
    ) %>% formatStyle(
      'Alertes',
      backgroundColor = styleEqual(c("Aucune incohérence", "Coordonnées GPS manquantes", 
                                     "Dépenses exactes supérieures aux dépenses mensuelles estimées",
                                     "Âge incohérent", "Nombre de femmes supérieur au total de personnes",
                                     "Données manquantes: Age_du_CM", "Données manquantes: Combien_de_personnes_vent_dans_le_m_nage_"),
                                   c('transparent', '#ff9999', '#ffcc99', '#99ccff', '#ffff99', '#ddddff', '#ddddff'))
    )
  })
  
  # Fonction de téléchargement en XLS
  output$download_xls <- downloadHandler(
    filename = function() {
      paste("donnees_kobo_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      req(data())
      writexl::write_xlsx(data(), path = file)
    }
  )
  
  output$map <- renderLeaflet({
    req(data())
    
    # Filtrer les données avec des coordonnées valides
    map_data <- data() %>% 
      filter(!is.na(latitude) & !is.na(longitude))
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = -17.44, lat = 14.70, zoom = 10) %>%
               addPopups(lng = -17.44, lat = 14.70, popup = "Aucune coordonnée GPS valide disponible"))
    }
    
    # Créer des popups informatifs
    popups <- paste0(
      "<strong>ID:</strong> ", map_data$`_uuid`, "<br>",
      "<strong>Région:</strong> ", map_data$Region, "<br>",
      "<strong>Localité:</strong> ", map_data$localit, "<br>",
      "<strong>Date:</strong> ", map_data$`_submission_time`, "<br>",
      "<strong>Alerte:</strong> ", map_data$alertes
    )
    
    # Créer la carte
    leaflet(map_data) %>%
      addTiles() %>%
      addCircleMarkers(
        ~longitude, ~latitude,
        color = ~ifelse(alertes == "Aucune incohérence", "green", "red"),
        radius = 8,
        fillOpacity = 0.8,
        stroke = FALSE,
        popup = popups,
        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend(
        position = "bottomright",
        colors = c("green", "red"),
        labels = c("Valide", "Invalide"),
        title = "Statut des questionnaires"
      )
  })
  
  # Tableau des incohérences seulement
  output$incoherences_table <- renderDT({
    req(data())
    
    # Filtrer les données pour n'afficher que les incohérences
    incoherences_data <- data() %>%
      filter(alertes != "Aucune incohérence") %>%
      select(
        ID = `_uuid`,
        Region = `Region`, 
        Localite = `localit`,
        Date_Soumission = `_submission_time`,
        Type_Alerte = `alertes`,
        Details = `details_incoherence`
      )
    
    if(nrow(incoherences_data) == 0) {
      return(datatable(data.frame(Message = "Aucune incohérence détectée dans les données."),
                       options = list(dom = 't')))
    }
    
    # Ajouter un bouton pour afficher les détails complets
    datatable(incoherences_data, 
              options = list(
                scrollX = TRUE,
                pageLength = 10
              ),
              selection = 'single',
              rownames = FALSE,
              caption = "Cliquez sur une ligne pour voir les détails complets du questionnaire"
    ) %>% formatStyle(
      'Type_Alerte',
      backgroundColor = styleEqual(c("Coordonnées GPS manquantes", 
                                     "Dépenses exactes supérieures aux dépenses mensuelles estimées",
                                     "Âge incohérent", "Nombre de femmes supérieur au total de personnes",
                                     "Données manquantes: Age_du_CM", "Données manquantes: Combien_de_personnes_vent_dans_le_m_nage_"),
                                   c('#ff9999', '#ffcc99', '#99ccff', '#ffff99', '#ddddff', '#ddddff'))
    )
  })
  
  # Résumé graphique des incohérences
  output$incoherences_summary <- renderPlot({
    req(data())
    
    incoherences_count <- table(data()$alertes)
    
    # Exclure "Aucune incohérence" pour le graphique
    incoherences_count <- incoherences_count[names(incoherences_count) != "Aucune incohérence"]
    
    if(length(incoherences_count) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", 
           main = "Aucune incohérence détectée dans les données.")
      return()
    }
    
    # Créer un graphique à barres
    barplot(incoherences_count, 
            main = "Types d'incohérences détectées",
            ylab = "Nombre de questionnaires",
            col = rainbow(length(incoherences_count)),
            las = 2,
            cex.names = 0.8,
            horiz = TRUE)
  })
  
  # Observer la sélection dans le tableau des incohérences
  observeEvent(input$incoherences_table_rows_selected, {
    req(data())
    
    incoherences_data <- data() %>%
      filter(alertes != "Aucune incohérence")
    
    if(length(input$incoherences_table_rows_selected) > 0 && nrow(incoherences_data) > 0) {
      selected_row <- input$incoherences_table_rows_selected
      
      if(selected_row <= nrow(incoherences_data)) {
        selected_id <- incoherences_data$`_uuid`[selected_row]
        selected_questionnaire(data() %>% filter(`_uuid` == selected_id))
      }
    }
  })
  
  # Afficher les détails du questionnaire sélectionné
  output$questionnaire_details <- renderUI({
    req(selected_questionnaire())
    
    questionnaire <- selected_questionnaire()
    
    if(nrow(questionnaire) == 0) {
      return(p("Sélectionnez un questionnaire dans le tableau ci-dessus pour voir les détails."))
    }
    
    # Formater les détails du questionnaire
    fluidRow(
      column(12,
             box(
               title = paste("Détails du questionnaire:", questionnaire$`_uuid`[1]),
               status = "primary",
               width = 12,
               collapsible = TRUE,
               div(
                 tags$h4(
                   tags$span(style = "color: red;", "Type d'incohérence: "), 
                   questionnaire$alertes[1]
                 ),
                 tags$p(
                   tags$span(style = "font-weight: bold;", "Explication: "), 
                   questionnaire$details_incoherence[1]
                 ),
                 hr(),
                 tags$h4("Informations générales"),
                 tags$ul(
                   tags$li(tags$b("Région: "), if(!is.null(questionnaire$Region[1])) questionnaire$Region[1] else "Non spécifié"),
                   tags$li(tags$b("Localité: "), if(!is.null(questionnaire$localit[1])) questionnaire$localit[1] else "Non spécifié"),
                   tags$li(tags$b("Date de soumission: "), if(!is.null(questionnaire$`_submission_time`[1])) questionnaire$`_submission_time`[1] else "Non spécifié"),
                   tags$li(tags$b("Coordonnées GPS: "), 
                           if(!is.na(questionnaire$latitude[1]) && !is.na(questionnaire$longitude[1])) {
                             paste(questionnaire$latitude[1], questionnaire$longitude[1], sep=", ")
                           } else {
                             "Non disponibles"
                           })
                 ),
                 tags$h4("Données du ménage"),
                 tags$ul(
                   tags$li(tags$b("Âge du chef de ménage: "), 
                           if(!is.null(questionnaire$`Age_du_CM`[1]) && !is.na(questionnaire$`Age_du_CM`[1])) {
                             questionnaire$`Age_du_CM`[1]
                           } else {
                             "Non spécifié"
                           }),
                   tags$li(tags$b("Nombre de personnes dans le ménage: "), 
                           if(!is.null(questionnaire$`Combien_de_personnes_vent_dans_le_m_nage_`[1]) && !is.na(questionnaire$`Combien_de_personnes_vent_dans_le_m_nage_`[1])) {
                             questionnaire$`Combien_de_personnes_vent_dans_le_m_nage_`[1]
                           } else {
                             "Non spécifié"
                           }),
                   tags$li(tags$b("Nombre de femmes dans le ménage: "), 
                           if(!is.null(questionnaire$`Combien_de_femmes_vivent_dans_le_m_nage_`[1]) && !is.na(questionnaire$`Combien_de_femmes_vivent_dans_le_m_nage_`[1])) {
                             questionnaire$`Combien_de_femmes_vivent_dans_le_m_nage_`[1]
                           } else {
                             "Non spécifié"
                           }),
                   tags$li(tags$b("Dépense mensuelle du ménage: "), 
                           if(!is.null(questionnaire$`Quel_est_la_d_pense_mensuelle_du_m_nage_`[1]) && !is.na(questionnaire$`Quel_est_la_d_pense_mensuelle_du_m_nage_`[1])) {
                             formatC(questionnaire$`Quel_est_la_d_pense_mensuelle_du_m_nage_`[1] * 10000, format="f", big.mark=" ", digits=0)
                           } else {
                             "Non spécifié"
                           }),
                   tags$li(tags$b("Dépense exacte du ménage: "), 
                           if(!is.null(questionnaire$`Quelle_est_la_d_pens_e_du_m_nage_exact`[1]) && !is.na(questionnaire$`Quelle_est_la_d_pens_e_du_m_nage_exact`[1])) {
                             formatC(questionnaire$`Quelle_est_la_d_pens_e_du_m_nage_exact`[1], format="f", big.mark=" ", digits=0)
                           } else {
                             "Non spécifié"
                           })
                 ),
                 tags$h4("Recommandations pour correction"),
                 tags$div(
                   style = "background-color: #f0f7fb; border-left: 5px solid #3498db; padding: 10px;",
                   tags$p(
                     get_recommendation(questionnaire$alertes[1], questionnaire)
                   )
                 )
               )
             )
      )
    )
  })
  
  # Fonction pour générer des recommandations spécifiques
  get_recommendation <- function(alerte_type, data) {
    if(alerte_type == "Coordonnées GPS manquantes") {
      return("L'enquêteur doit activer la géolocalisation sur son appareil et s'assurer que les permissions sont accordées à l'application de collecte. Une nouvelle visite peut être nécessaire pour collecter les coordonnées GPS.")
    } else if(alerte_type == "Dépenses exactes supérieures aux dépenses mensuelles estimées") {
      return(paste0("Vérifier les dépenses exactes (", formatC(data$`Quelle_est_la_d_pens_e_du_m_nage_exact`[1], format="f", big.mark=" ", digits=0), 
                    ") par rapport à l'estimation mensuelle (", formatC(data$`Quel_est_la_d_pense_mensuelle_du_m_nage_`[1] * 10000, format="f", big.mark=" ", digits=0), 
                    "). Une des deux valeurs pourrait comporter une erreur de saisie ou de compréhension. Contacter l'enquêteur pour clarification."))
    } else if(alerte_type == "Âge incohérent") {
      return(paste0("L'âge du chef de ménage (", data$`Age_du_CM`[1], ") semble incorrect. Vérifier si c'est une erreur de saisie ou s'il y a eu confusion dans l'identification du chef de ménage."))
    } else if(alerte_type == "Nombre de femmes supérieur au total de personnes") {
      return(paste0("Le nombre de femmes (", data$`Combien_de_femmes_vivent_dans_le_m_nage_`[1], 
                    ") ne peut pas être supérieur au nombre total de personnes dans le ménage (", 
                    data$`Combien_de_personnes_vent_dans_le_m_nage_`[1], 
                    "). Vérifier les deux valeurs et corriger en conséquence."))
    } else {
      return("Vérifier les données et contacter l'enquêteur pour clarification.")
    }
  }
  
  output$raw_data_table <- renderDT({
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
}

# -- Lancer l'application Shiny
shinyApp(ui = ui, server = server)