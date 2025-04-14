# Chargement des bibliothèques nécessaires
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(DT)
library(shinydashboard)
library(shinyalert)
library(dplyr)
library(writexl)
library(ggplot2)
library(tidyr)

# Configuration API KoboToolbox
token <- "6f8bb50a807ae9e7fda9fba40566da0f9c3383c5"
form_id <- "a9okT245N3K6EBHaW5kEWu"
#token <- Sys.getenv("token")
#form_id <- Sys.getenv("form_id")

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
  
  # Vérification des coordonnées GPS
  if ("_geolocation" %in% colnames(data)) {
    gps_missing <- if(is.character(data$`_geolocation`)) {
      is.na(data$`_geolocation`) | data$`_geolocation` == ""
    } else {
      sapply(data$`_geolocation`, function(x) is.null(x) || length(x) == 0 || 
               (length(x) > 0 && (is.na(x[1]) || is.na(x[2]))))
    }
    
    if (any(gps_missing)) {
      data$alertes[gps_missing] <- "Coordonnées GPS manquantes"
      data$details_incoherence[gps_missing] <- "Les coordonnées GPS n'ont pas été enregistrées lors de la collecte."
    }
  }
  
  # Vérification des dépenses mensuelles vs exactes
  depense_cols <- c(mensuelle = "Quel_est_la_d_pense_mensuelle_du_m_nage_", 
                    exacte = "Quelle_est_la_d_pens_e_du_m_nage_exact_")
  
  if (all(depense_cols %in% colnames(data))) {
    # Conversion en numérique
    for (col in depense_cols) {
      if (!is.numeric(data[[col]])) data[[col]] <- as.numeric(as.character(data[[col]]))
    }
    
    # Vérification de la cohérence entre dépenses estimées et exactes
    dep_incoherent <- !is.na(data[[depense_cols["mensuelle"]]]) & 
      !is.na(data[[depense_cols["exacte"]]]) &
      data[[depense_cols["exacte"]]] > (data[[depense_cols["mensuelle"]]] * 2)
    
    if (any(dep_incoherent)) {
      data$alertes[dep_incoherent] <- "Dépenses exactes supérieures aux dépenses mensuelles estimées"
      for (i in which(dep_incoherent)) {
        data$details_incoherence[i] <- paste0("Dépense exacte (", data[[depense_cols["exacte"]]][i], 
                                              ") est significativement supérieure à la dépense mensuelle estimée (", 
                                              data[[depense_cols["mensuelle"]]][i], ")")
      }
    }
  }
  
  # Vérification de l'âge du chef de ménage
  age_col <- "Age_du_CM"
  if (age_col %in% colnames(data)) {
    if (!is.numeric(data[[age_col]])) data[[age_col]] <- as.numeric(as.character(data[[age_col]]))
    
    age_incoherent <- !is.na(data[[age_col]]) & (data[[age_col]] < 15 | data[[age_col]] > 120)
    if (any(age_incoherent)) {
      data$alertes[age_incoherent] <- "Âge incohérent"
      for (i in which(age_incoherent)) {
        data$details_incoherence[i] <- paste0("L'âge du chef de ménage (", data[[age_col]][i], 
                                              ") est ", ifelse(data[[age_col]][i] < 15, 
                                                               "inférieur à 15 ans", 
                                                               "supérieur à 120 ans"), 
                                              ", ce qui est improbable.")
      }
    }
  }
  
  # Vérification nombre de personnes vs femmes
  pers_cols <- c(total = "Combien_de_personnes_vent_dans_le_m_nage_", 
                 femmes = "Combien_de_femmes_vivent_dans_le_m_nage_")
  
  if (all(pers_cols %in% colnames(data))) {
    for (col in pers_cols) {
      if (!is.numeric(data[[col]])) data[[col]] <- as.numeric(as.character(data[[col]]))
    }
    
    pers_incoherent <- !is.na(data[[pers_cols["total"]]]) & 
      !is.na(data[[pers_cols["femmes"]]]) &
      data[[pers_cols["total"]]] < data[[pers_cols["femmes"]]]
    
    if (any(pers_incoherent)) {
      data$alertes[pers_incoherent] <- "Nombre de femmes supérieur au total de personnes"
      for (i in which(pers_incoherent)) {
        data$details_incoherence[i] <- paste0("Le nombre de femmes (", data[[pers_cols["femmes"]]][i], 
                                              ") est supérieur au nombre total de personnes dans le ménage (", 
                                              data[[pers_cols["total"]]][i], ").")
      }
    }
  }
  
  # Vérification des données manquantes obligatoires
  champs_obligatoires <- c("Age_du_CM", "Combien_de_personnes_vent_dans_le_m_nage_")
  for (champ in champs_obligatoires) {
    if (champ %in% colnames(data)) {
      manquant <- is.na(data[[champ]]) | data[[champ]] == ""
      if (any(manquant) && data$alertes[manquant] == "Aucune incohérence") {
        data$alertes[manquant] <- paste0("Données manquantes: ", champ)
        data$details_incoherence[manquant] <- paste0("La valeur pour le champ '", champ, 
                                                     "' est manquante alors qu'elle est obligatoire.")
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
  
  return(data)
}

# Interface utilisateur simplifiée
ui <- dashboardPage(
  dashboardHeader(title = "Suivi d'enquête KoboToolbox"),
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
                box(title = "Téléchargement des données", status = "primary", width = 12,
                    downloadButton("download_xls", "Télécharger les données complètes en XLS", 
                                   style = "color: white; background-color: #3c8dbc; width: 100%; padding: 10px;")
                )
              )
      ),
      
      # Onglet Carte
      tabItem(tabName = "map",
              box(title = "Carte des questionnaires", status = "primary", width = 12,
                  leafletOutput("map", height = 600))
      ),
      
      # Onglet Détails des incohérences
      tabItem(tabName = "incoherences",
              box(title = "Analyse des incohérences", status = "warning", width = 12,
                  p("Ce tableau présente uniquement les questionnaires comportant des incohérences, avec des détails sur les problèmes détectés."),
                  DTOutput("incoherences_table")),
              fluidRow(
                box(title = "Répartition des types d'incohérences", status = "danger", width = 6,
                    plotOutput("incoherences_summary", height = 350)),
                box(title = "Proportion des questionnaires valides/non valides", status = "info", width = 6,
                    plotOutput("validity_piechart", height = 350))
              )
      ),
      
      # Onglet Données brutes
      tabItem(tabName = "rawdata",
              box(title = "Données brutes", status = "warning", width = 12,
                  DTOutput("raw_data_table"))
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
      
      # Alertes pour les soumissions invalides
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
  
  # Rafraîchissement initial et périodique
  observe({ refresh_data() }, priority = 1000)
  observeEvent(reactiveTimer(60000)(), { refresh_data() })
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
  
  # Carte
  output$map <- renderLeaflet({
    req(data())
    map_data <- data() %>% filter(!is.na(latitude) & !is.na(longitude))
    
    if (nrow(map_data) == 0) {
      return(leaflet() %>% 
               addTiles() %>%
               setView(lng = -17.44, lat = 14.70, zoom = 10) %>%
               addPopups(lng = -17.44, lat = 14.70, popup = "Aucune coordonnée GPS valide disponible"))
    }
    
    popups <- paste0(
      "<strong>ID:</strong> ", map_data$`_uuid`, "<br>",
      "<strong>Région:</strong> ", map_data$Region, "<br>",
      "<strong>Localité:</strong> ", map_data$localit, "<br>",
      "<strong>Date:</strong> ", map_data$`_submission_time`, "<br>",
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
    req(data())
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
  output$incoherences_summary <- renderPlot({
    req(data())
    incoherences_data <- data() %>% filter(alertes != "Aucune incohérence")
    
    if(nrow(incoherences_data) == 0) {
      plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "", main = "Aucune incohérence détectée")
      return()
    }
    
    summary_data <- incoherences_data %>%
      count(alertes) %>%
      arrange(desc(n)) %>%
      mutate(alertes = factor(alertes, levels = alertes))
    
    ggplot(summary_data, aes(x = reorder(alertes, n), y = n, fill = alertes)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = n), hjust = -0.3, size = 4) +
      coord_flip() +
      labs(x = NULL, y = "Nombre d'occurrences") +
      ggtitle(" Incohérences détectées") +
      theme_minimal() +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0, size = 14, face = "bold", margin = margin(b = 20)),
            axis.text.y = element_text(size = 10),
            axis.text.x = element_text(size = 9),
            panel.grid.major.y = element_blank())
  })
  
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
    req(data())
    datatable(data(), options = list(scrollX = TRUE))
  })
}

# Lancer l'application Shiny
shinyApp(ui = ui, server = server)