# ============================================================
# PACKAGES NÉCESSAIRES
# ============================================================
library(shiny)
library(leaflet)
library(DT)
library(dplyr)
library(lubridate)
library(base64enc)
library(shinyjs)

# ============================================================
# UI
# ============================================================
ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$meta(name = "viewport", 
              content = "width=device-width, initial-scale=1.0"),
    tags$link(rel = "stylesheet", href = "style.css"),
    tags$script(HTML("
      function getLocation() {
        if (navigator.geolocation) {
          navigator.geolocation.getCurrentPosition(
            function(position) {
              Shiny.setInputValue('gps_lat', position.coords.latitude);
              Shiny.setInputValue('gps_lon', position.coords.longitude);
              Shiny.setInputValue('gps_accuracy', position.coords.accuracy);
              Shiny.setInputValue('gps_status', 'success');
            },
            function(error) {
              Shiny.setInputValue('gps_status', 'error');
            },
            {enableHighAccuracy: true, timeout: 10000}
          );
        }
      }
    "))
  ),
  
  # ---- TITRE ----
  div(class = "app-header",
      h2("🌿 Application Patrouille"),
      p(textOutput("current_datetime"))
  ),
  
  # ---- FORMULAIRE PRINCIPAL ----
  div(class = "form-container",
      
      # ==========================================
      # SECTION 1 : GÉOLOCALISATION
      # ==========================================
      div(class = "section-card",
          h3("📍 Géolocalisation"),
          
          actionButton("btn_gps", 
                       "📡 Obtenir ma position", 
                       class = "btn-primary btn-block"),
          br(),
          
          div(class = "gps-info",
              uiOutput("gps_display")
          ),
          
          leafletOutput("mini_map", height = "200px")
      ),
      
      # ==========================================
      # SECTION 2 : PROVENANCE
      # ==========================================
      div(class = "section-card",
          h3("🏠 Provenance"),
          
          radioButtons("provenance",
                       label    = NULL,
                       choices  = c("Locaux"           = "locaux",
                                    "Région/Alentours" = "region",
                                    "Vacanciers"       = "vacanciers"),
                       selected = character(0))
      ),
      
      # ==========================================
      # SECTION 3 : TYPE DE PUBLIC
      # ==========================================
      div(class = "section-card",
          h3("👥 Type de public"),
          
          div(class = "input-group-custom",
              tags$label("Nombre de personnes"),
              numericInput("nb_personnes",
                           label = NULL,
                           value = NULL,
                           min   = 1,
                           max   = 500,
                           step  = 1)
          ),
          
          tags$label("Catégorie"),
          checkboxGroupInput("type_public",
                             label   = NULL,
                             choices = c("Famille"    = "famille",
                                         "Jeune"      = "jeune",
                                         "Retraité"   = "retraite",
                                         "Adulte"     = "adulte",
                                         "Saisonnier" = "saisonnier"))
      ),
      
      # ==========================================
      # SECTION 4 : SUJETS ABORDÉS
      # ==========================================
      div(class = "section-card",
          h3("💬 Sujets abordés"),
          
          checkboxGroupInput("sujets",
                             label   = NULL,
                             choices = c("Qualité de l'eau"     = "qualite_eau",
                                         "Eau potable"          = "eau_potable",
                                         "Gestion niveau d'eau" = "gestion_niveau",
                                         "Espèces protégées"    = "esp_protegees",
                                         "Espèces invasives"    = "esp_invasives",
                                         "Règles de navigation" = "navigation",
                                         "Cyanobactéries"       = "cyano",
                                         "Activité pétrolière"  = "petrole",
                                         "Hydraviation"         = "hydraviation",
                                         "Sécurité incendie"    = "incendie",
                                         "Gestion des déchets"  = "dechets"))
      ),
      
      # ==========================================
      # SECTION 5 : SUPPORTS DÉLIVRÉS
      # ==========================================
      div(class = "section-card",
          h3("📦 Supports délivrés"),
          
          checkboxGroupInput("supports",
                             label   = NULL,
                             choices = c("Eventail informatif" = "eventail",
                                         "Cartes postales"     = "cartes_postales",
                                         "Cendrier"            = "cendrier",
                                         "Z Cards"             = "z_cards",
                                         "Flyer cyano"         = "flyer_cyano",
                                         "Rien"                = "rien"))
      ),
      
      # ==========================================
      # SECTION 6 : COMMENTAIRES & PHOTOS
      # ==========================================
      div(class = "section-card",
          h3("📝 Observations & Photos"),
          
          textAreaInput("commentaires",
                        label       = "Commentaires",
                        placeholder = "Décrivez votre observation...",
                        rows        = 4,
                        width       = "100%"),
          
          tags$label("📷 Ajouter une photo"),
          fileInput("photo",
                    label       = NULL,
                    accept      = c("image/jpeg", "image/png", "image/gif"),
                    buttonLabel = "Choisir une photo",
                    placeholder = "Aucune photo sélectionnée"),
          
          uiOutput("photo_preview")
      ),
      
      # ==========================================
      # BOUTONS D'ACTION
      # ==========================================
      div(class = "action-buttons",
          
          actionButton("btn_submit",
                       "✅ Enregistrer l'observation",
                       class = "btn-success btn-lg btn-block"),
          br(),
          
          actionButton("btn_reset",
                       "🔄 Nouveau formulaire",
                       class = "btn-warning btn-block"),
          br(),
          
          uiOutput("submit_message")
      )
  ),
  
  # ---- VISUALISATION DES DONNÉES ----
  div(class = "section-card",
      h3("📊 Données collectées"),
      
      downloadButton("btn_export",
                     "⬇️ Exporter CSV",
                     class = "btn-info"),
      br(), br(),
      
      DT::dataTableOutput("data_table")
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # REACTIVE VALUES
  # ----------------------------------------------------------
  rv <- reactiveValues(
    lat       = NULL,
    lon       = NULL,
    accuracy  = NULL,
    data      = data.frame(),
    photo_b64 = NULL
  )
  
  # Chargement données existantes au démarrage
  observe({
    if (file.exists("data/observations.csv")) {
      rv$data <- read.csv("data/observations.csv",
                          stringsAsFactors = FALSE)
    }
  })
  
  # ----------------------------------------------------------
  # DATE & HEURE
  # ----------------------------------------------------------
  output$current_datetime <- renderText({
    invalidateLater(60000)
    format(Sys.time(), "%d/%m/%Y - %H:%M")
  })
  
  # ----------------------------------------------------------
  # GÉOLOCALISATION
  # ----------------------------------------------------------
  observeEvent(input$btn_gps, {
    shinyjs::runjs("getLocation();")
  })
  
  observeEvent(input$gps_lat, {
    rv$lat      <- input$gps_lat
    rv$lon      <- input$gps_lon
    rv$accuracy <- input$gps_accuracy
  })
  
  output$gps_display <- renderUI({
    if (!is.null(rv$lat)) {
      div(class = "gps-success",
          p(paste("✅ Latitude  :", round(rv$lat, 6))),
          p(paste("✅ Longitude :", round(rv$lon, 6))),
          p(paste("🎯 Précision :", round(rv$accuracy, 0), "m"))
      )
    } else if (!is.null(input$gps_status) && 
               input$gps_status == "error") {
      div(class = "gps-error",
          p("❌ Impossible d'obtenir la position")
      )
    } else {
      div(class = "gps-waiting",
          p("⏳ En attente de localisation...")
      )
    }
  })
  
  output$mini_map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = 2.3522, lat = 46.8566, zoom = 5)
  })
  
  observeEvent(rv$lat, {
    req(rv$lat, rv$lon)
    
    leafletProxy("mini_map") %>%
      clearMarkers() %>%
      addMarkers(lng   = rv$lon,
                 lat   = rv$lat,
                 popup = paste("📍 Position actuelle<br>",
                               "Précision:", round(rv$accuracy, 0), "m")) %>%
      setView(lng = rv$lon, lat = rv$lat, zoom = 14)
  })
  
  # ----------------------------------------------------------
  # PHOTO PREVIEW
  # ----------------------------------------------------------
  observeEvent(input$photo, {
    req(input$photo)
    rv$photo_b64 <- base64encode(input$photo$datapath)
  })
  
  output$photo_preview <- renderUI({
    req(input$photo)
    tags$img(src   = input$photo$datapath,
             style = "max-width:100%; border-radius:8px; margin-top:10px;",
             alt   = "Photo de l'observation")
  })
  
  # ----------------------------------------------------------
  # VALIDATION
  # ----------------------------------------------------------
  validate_form <- function() {
    errors <- c()
    
    if (is.null(rv$lat))
      errors <- c(errors, "⚠️ Géolocalisation manquante")
    
    if (is.null(input$provenance) || input$provenance == "")
      errors <- c(errors, "⚠️ Veuillez indiquer la provenance")
    
    if (is.null(input$nb_personnes) || is.na(input$nb_personnes))
      errors <- c(errors, "⚠️ Nombre de personnes manquant")
    
    return(errors)
  }
  
  # ----------------------------------------------------------
  # SOUMISSION
  # ----------------------------------------------------------
  observeEvent(input$btn_submit, {
    
    errors <- validate_form()
    
    if (length(errors) > 0) {
      output$submit_message <- renderUI({
        div(class = "alert alert-danger",
            lapply(errors, function(e) p(e))
        )
      })
      return()
    }
    
    nouvelle_obs <- data.frame(
      id                 = paste0("OBS_", format(Sys.time(), "%Y%m%d_%H%M%S")),
      datetime           = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      date               = format(Sys.Date(), "%Y-%m-%d"),
      heure              = format(Sys.time(), "%H:%M"),
      latitude           = ifelse(!is.null(rv$lat), rv$lat, NA),
      longitude          = ifelse(!is.null(rv$lon), rv$lon, NA),
      precision_gps      = ifelse(!is.null(rv$accuracy), round(rv$accuracy, 0), NA),
      provenance         = ifelse(!is.null(input$provenance), input$provenance, NA),
      nb_personnes       = ifelse(!is.na(input$nb_personnes), input$nb_personnes, NA),
      public_famille     = "famille"    %in% input$type_public,
      public_jeune       = "jeune"      %in% input$type_public,
      public_retraite    = "retraite"   %in% input$type_public,
      public_adulte      = "adulte"     %in% input$type_public,
      public_saisonnier  = "saisonnier" %in% input$type_public,
      sujet_qualite_eau  = "qualite_eau"    %in% input$sujets,
      sujet_eau_potable  = "eau_potable"    %in% input$sujets,
      sujet_gestion_niv  = "gestion_niveau" %in% input$sujets,
      sujet_esp_prot     = "esp_protegees"  %in% input$sujets,
      sujet_esp_inv      = "esp_invasives"  %in% input$sujets,
      sujet_navigation   = "navigation"     %in% input$sujets,
      sujet_cyano        = "cyano"          %in% input$sujets,
      sujet_petrole      = "petrole"        %in% input$sujets,
      sujet_hydraviation = "hydraviation"   %in% input$sujets,
      sujet_incendie     = "incendie"       %in% input$sujets,
      sujet_dechets      = "dechets"        %in% input$sujets,
      support_eventail   = "eventail"       %in% input$supports,
      support_cartes     = "cartes_postales"%in% input$supports,
      support_cendrier   = "cendrier"       %in% input$supports,
      support_zcards     = "z_cards"        %in% input$supports,
      support_flyer_cyano= "flyer_cyano"    %in% input$supports,
      support_rien       = "rien"           %in% input$supports,
      commentaires       = ifelse(!is.null(input$commentaires) && 
                                    input$commentaires != "",
                                  input$commentaires, NA),
      photo_present      = !is.null(input$photo),
      photo_nom          = ifelse(!is.null(input$photo), input$photo$name, NA),
      stringsAsFactors   = FALSE
    )
    
    rv$data <- bind_rows(rv$data, nouvelle_obs)
    
    if (!dir.exists("data")) dir.create("data")
    write.csv(rv$data, "data/observations.csv", row.names = FALSE)
    
    if (!is.null(input$photo) && !is.null(rv$photo_b64)) {
      photo_dir <- "data/photos"
      if (!dir.exists(photo_dir)) dir.create(photo_dir, recursive = TRUE)
      photo_name <- paste0(nouvelle_obs$id, "_", input$photo$name)
      file.copy(input$photo$datapath, file.path(photo_dir, photo_name))
    }
    
    output$submit_message <- renderUI({
      div(class = "alert alert-success",
          p(paste("✅ Observation enregistrée !", nouvelle_obs$id))
      )
    })
  })
  
  # ----------------------------------------------------------
  # RESET
  # ----------------------------------------------------------
  observeEvent(input$btn_reset, {
    updateRadioButtons(session, "provenance", selected = character(0))
    updateNumericInput(session, "nb_personnes", value = NA)
    updateCheckboxGroupInput(session, "type_public", selected = character(0))
    updateCheckboxGroupInput(session, "sujets",      selected = character(0))
    updateCheckboxGroupInput(session, "supports",    selected = character(0))
    updateTextAreaInput(session, "commentaires", value = "")
    
    rv$lat       <- NULL
    rv$lon       <- NULL
    rv$accuracy  <- NULL
    rv$photo_b64 <- NULL
    
    output$submit_message <- renderUI({ NULL })
    
    leafletProxy("mini_map") %>%
      clearMarkers() %>%
      setView(lng = 2.3522, lat = 46.8566, zoom = 5)
    
    showNotification("🔄 Formulaire réinitialisé", type = "message", duration = 3)
  })
  
  # ----------------------------------------------------------
  # TABLEAU
  # ----------------------------------------------------------
  output$data_table <- DT::renderDataTable({
    req(nrow(rv$data) > 0)
    
    DT::datatable(rv$data,
                  options  = list(
                    scrollX    = TRUE,
                    pageLength = 5,
                    language   = list(url = "//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json")
                  ),
                  rownames = FALSE)
  })
  
  # ----------------------------------------------------------
  # EXPORT CSV
  # ----------------------------------------------------------
  output$btn_export <- downloadHandler(
    filename = function() {
      paste0("patrouille_", format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv(rv$data, file, row.names = FALSE)
    }
  )
}

# ============================================================
# LANCEMENT
# ============================================================
shinyApp(ui = ui, server = server)
