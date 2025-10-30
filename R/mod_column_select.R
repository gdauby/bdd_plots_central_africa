# Column Selection Module
#
# Allows user to select which column contains taxonomic names

#' Column Select Module - UI
#'
#' @param id Character, module ID
#'
#' @return Shiny UI element
#'
#' @keywords internal
mod_column_select_ui <- function(id) {
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::h4(shiny::textOutput(ns("title"))),
    shiny::uiOutput(ns("column_controls"))
  )
}


#' Column Select Module - Server
#'
#' @param id Character, module ID
#' @param data Reactive data.frame from data input module
#' @param initial_column Character, optional pre-selected column name
#' @param i18n Translator object from shiny.i18n
#'
#' @return Reactive list with $column (selected column name) and $include_authors (logical)
#'
#' @keywords internal
mod_column_select_server <- function(id, data, initial_column = NULL, i18n) {
  shiny::moduleServer(id, function(input, output, session) {

    # Module title
    output$title <- shiny::renderText({
      i18n$t("column_select_title")
    })

    # Column selection controls
    output$column_controls <- shiny::renderUI({
      req(data())

      ns <- session$ns
      df <- data()

      # Get character columns
      char_cols <- names(df)[sapply(df, is.character)]

      if (length(char_cols) == 0) {
        return(
          shiny::div(
            style = "color: red;",
            shiny::p(i18n$t("msg_error"), "No character columns found in data")
          )
        )
      }

      # Determine selected column
      selected_col <- if (!is.null(initial_column) && initial_column %in% char_cols) {
        initial_column
      } else {
        char_cols[1]
      }

      shiny::tagList(
        shiny::selectInput(
          inputId = ns("column_name"),
          label = i18n$t("column_select_name"),
          choices = char_cols,
          selected = selected_col
        ),
        shiny::checkboxInput(
          inputId = ns("include_authors"),
          label = i18n$t("column_match_authors"),
          value = FALSE
        ),
        shiny::helpText(i18n$t("column_match_authors_help"))
      )
    })

    # Return reactive list
    return(
      shiny::reactive({
        list(
          column = input$column_name,
          include_authors = input$include_authors %||% FALSE
        )
      })
    )
  })
}
