# Custom Translator Class
#
# A simple translator class that mimics shiny.i18n::Translator interface
# but works reliably with a combined CSV file containing all translations

#' Custom Translator for Shiny App
#'
#' Creates a translator object that provides i18n functionality
#' compatible with shiny.i18n interface but using a simpler implementation.
#'
#' @param translations_df Data frame with columns: key, en, fr
#' @param default_language Initial language (default: "en")
#'
#' @return A translator object with methods: t(), set_translation_language()
#' @keywords internal
create_custom_translator <- function(translations_df, default_language = "en") {

  # Store translations as nested list
  trans_list <- list(
    en = setNames(as.list(translations_df$en), translations_df$key),
    fr = setNames(as.list(translations_df$fr), translations_df$key)
  )

  # Current language
  current_lang <- default_language

  # Create translator object
  translator <- list(
    translation = trans_list,

    # Method to get translation for a key
    t = function(key) {
      result <- trans_list[[current_lang]][[key]]
      if (is.null(result)) {
        warning("Translation key '", key, "' not found for language '", current_lang, "'")
        return(key)  # Return key itself if translation not found
      }
      return(as.character(result))
    },

    # Method to set current language
    set_translation_language = function(lang) {
      if (!lang %in% c("en", "fr")) {
        stop("Language must be 'en' or 'fr'")
      }
      current_lang <<- lang
    },

    # Method to get current language
    get_translation_language = function() {
      return(current_lang)
    }
  )

  class(translator) <- c("CustomTranslator", "list")
  return(translator)
}
