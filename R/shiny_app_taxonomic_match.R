# Main Shiny App for Taxonomic Name Standardization
#
# Modular Shiny app that orchestrates all modules for taxonomic name matching

#' Taxonomic Name Standardization App
#'
#' Main Shiny application for standardizing taxonomic names against the backbone database.
#' This app provides a modern, modular interface for matching species names with
#' intelligent fuzzy matching and manual review capabilities.
#'
#' @param data Optional data.frame or reactive, pre-loaded data to standardize
#' @param name_column Optional character, pre-selected column name containing taxa
#' @param language Character, initial language ("en" or "fr"), default: "en"
#' @param min_similarity Numeric, minimum similarity for fuzzy matching (0-1), default: 0.3
#' @param max_suggestions Integer, maximum suggestions per name, default: 10
#' @param mode Character, review mode ("interactive" or "batch"), default: "interactive"
#'
#' @return Shiny app object
#'
#' @keywords internal
#' @import shiny
app_taxonomic_match <- function(
  data = NULL,
  name_column = NULL,
  language = "en",
  min_similarity = 0.3,
  max_suggestions = 10,
  mode = "interactive"
) {

  # Validate parameters
  language <- match.arg(language, c("en", "fr"))
  mode <- match.arg(mode, c("interactive", "batch"))

  # UI
  ui <- shiny::fluidPage(

    # Custom CSS
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        .sidebar-panel {
          background-color: #f8f9fa;
          padding: 15px;
          border-radius: 5px;
        }
        .main-tabs {
          margin-top: 20px;
        }
        .module-title {
          color: #2c3e50;
          border-bottom: 2px solid #3498db;
          padding-bottom: 10px;
          margin-bottom: 20px;
        }
      "))
    ),

    # Language toggle (top right)
    shiny::absolutePanel(
      top = 10,
      right = 20,
      fixed = TRUE,
      draggable = FALSE,
      mod_language_toggle_ui("language")
    ),

    # Title
    shiny::div(
      class = "container-fluid",
      shiny::fluidRow(
        shiny::column(
          width = 12,
          shiny::h1(
            shiny::textOutput("app_title"),
            style = "color: #2c3e50; margin-top: 20px; margin-bottom: 10px;"
          ),
          shiny::p(
            shiny::textOutput("app_subtitle"),
            style = "color: #7f8c8d; font-size: 16px; margin-bottom: 30px;"
          )
        )
      )
    ),

    # Main layout
    shiny::fluidRow(

      # Sidebar
      shiny::column(
        width = 3,
        class = "sidebar-panel",

        # Data Input
        mod_data_input_ui("data_input"),
        shiny::hr(),

        # Column Selection
        mod_column_select_ui("column_select"),
        shiny::hr(),

        # Progress Tracker
        mod_progress_tracker_ui("progress")
      ),

      # Main panel
      shiny::column(
        width = 9,
        shiny::div(
          class = "main-tabs",

          shiny::tabsetPanel(
            id = "main_tabs",
            type = "pills",

            # Auto Match Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_auto_match", inline = TRUE),
              value = "auto_match",
              icon = shiny::icon("magic"),
              shiny::br(),
              mod_auto_matching_ui("auto_match")
            ),

            # Review Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_review", inline = TRUE),
              value = "review",
              icon = shiny::icon("search"),
              shiny::br(),
              mod_name_review_ui("review")
            ),

            # Export Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_export", inline = TRUE),
              value = "export",
              icon = shiny::icon("download"),
              shiny::br(),
              mod_results_export_ui("export")
            ),

            # Traits Enrichment Tab
            shiny::tabPanel(
              title = shiny::textOutput("tab_traits", inline = TRUE),
              value = "traits",
              icon = shiny::icon("database"),
              shiny::br(),
              mod_traits_enrichment_ui("traits")
            )
          )
        )
      )
    ),

    # Footer
    shiny::hr(),
    shiny::div(
      style = "text-align: center; color: #7f8c8d; padding: 20px;",
      shiny::p(
        shiny::icon("leaf"),
        "Taxonomic Name Standardization Tool |",
        "Powered by plotsdatabase package",
        style = "font-size: 14px;"
      )
    )
  )

  # Server
  server <- function(input, output, session) {

    # Stop the server when session ends
    session$onSessionEnded(function() {
      shiny::stopApp()
    })

    # Initialize shiny.i18n Translator
    # Find translation files - works in both installed and development mode
    trans_path <- NULL

    # 1. Try installed package location (after devtools::install())
    installed_path <- system.file("translations", package = "plotsdatabase")
    if (installed_path != "" && dir.exists(installed_path)) {
      trans_path <- installed_path
    }

    # 2. Try development location (with devtools::load_all())
    if (is.null(trans_path)) {
      # Look for inst/translations relative to package source
      dev_paths <- c(
        file.path(getwd(), "inst", "translations"),
        file.path(system.file(package = "plotsdatabase"), "..", "..", "inst", "translations")
      )

      for (p in dev_paths) {
        if (dir.exists(p)) {
          trans_path <- normalizePath(p, winslash = "/", mustWork = FALSE)
          if (dir.exists(trans_path)) {
            break
          }
        }
      }
    }

    if (is.null(trans_path) || !dir.exists(trans_path)) {
      stop(
        "Translation files not found!\n",
        "Searched in:\n",
        "  - Installed: ", installed_path, "\n",
        "  - Development: ", file.path(getwd(), "inst", "translations"), "\n",
        "\nPlease ensure translations exist in inst/translations/ and run:\n",
        "  devtools::install()"
      )
    }

    # Initialize translator
    # Try shiny.i18n first, but fall back to custom translator if it fails
    i18n <- tryCatch({
      translator <- shiny.i18n::Translator$new(
        translation_csvs_path = trans_path,
        separator_csv = ","
      )
      translator$set_translation_language(language)
      message("Using shiny.i18n translator")
      translator
    }, error = function(e) {
      # Fallback: use custom translator with combined CSV
      message("shiny.i18n initialization failed, using custom translator: ", e$message)

      # Read combined translations file
      all_trans <- read.csv(
        file.path(trans_path, "translations.csv"),
        stringsAsFactors = FALSE,
        encoding = "UTF-8"
      )

      # Create custom translator
      translator <- create_custom_translator(all_trans, default_language = language)
      translator
    })

    # Language management
    current_language <- mod_language_toggle_server("language", initial = language)

    # Update translator when language changes
    shiny::observeEvent(current_language(), {
      i18n$set_translation_language(current_language())
    })

    # App title and subtitle
    output$app_title <- shiny::renderText({
      i18n$t("app_title")
    })

    output$app_subtitle <- shiny::renderText({
      i18n$t("app_subtitle")
    })

    # Tab labels (need to be reactive for language switching)
    output$tab_auto_match <- shiny::renderText({
      i18n$t("tab_auto_match")
    })

    output$tab_review <- shiny::renderText({
      i18n$t("tab_review")
    })

    output$tab_export <- shiny::renderText({
      i18n$t("tab_export")
    })

    output$tab_traits <- shiny::renderText({
      i18n$t("tab_traits")
    })

    # Data input module
    user_data <- mod_data_input_server(
      "data_input",
      provided_data = data,
      i18n = i18n
    )

    # Column selection module
    column_info <- mod_column_select_server(
      "column_select",
      data = user_data,
      initial_column = name_column,
      i18n = i18n
    )

    # Auto matching module
    match_results <- mod_auto_matching_server(
      "auto_match",
      data = user_data,
      column_name = shiny::reactive(column_info()$column),
      include_authors = shiny::reactive(column_info()$include_authors),
      min_similarity = min_similarity,
      i18n = i18n
    )

    # Progress tracker module
    mod_progress_tracker_server(
      "progress",
      match_results = match_results,
      i18n = i18n
    )

    # Manual review module
    reviewed_results <- mod_name_review_server(
      "review",
      match_results = match_results,
      mode = mode,
      max_suggestions = max_suggestions,
      min_similarity = min_similarity,
      i18n = i18n
    )

    # Results export module (use reviewed results instead of just matched results)
    mod_results_export_server(
      "export",
      results = reviewed_results,
      original_data = user_data,
      i18n = i18n
    )

    # Traits enrichment module
    mod_traits_enrichment_server(
      "traits",
      results = reviewed_results,
      column_name = shiny::reactive(column_info()$column),
      i18n = i18n
    )
  }

  # Return Shiny app object
  shiny::shinyApp(ui, server)
}
