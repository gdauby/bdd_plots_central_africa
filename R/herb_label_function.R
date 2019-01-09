
herbarium_label <-
  function (dat = NULL, spellcheck = TRUE, theme = c("KFBG", "PE", 
                                                   "KUN", "HU", "GILLES"), outfile = "herblabel.rtf", font = c("Roman", 
                                                                                                     "Arial"), 
            font_size = 1) 
{
  if (is.null(dat)) {
    stop("'dat' should be specified")
  }
  theme <- match.arg(theme)
  font <- match.arg(font)
  herbdat000 <- dat
  herbdat000[herbdat000 == ""] <- NA
  dat$LAT_FLAG <- toupper(dat$LAT_FLAG)
  dat$LON_FLAG <- toupper(dat$LON_FLAG)
  if (any(is.na(herbdat000$HERBARIUM))) {
    stop(paste("\"HERBARIUM\" not provided for row: ", paste(which(is.na(herbdat000$HERBARIUM)) + 
                                                               1, collapse = ", ")))
  }
  if (any(is.na(herbdat000$COLLECTOR))) {
    stop(paste("\"COLLECTOR\" not provided for row: ", paste(which(is.na(herbdat000$COLLECTOR)) + 
                                                               1, collapse = ", ")))
  }
  if (any(is.na(herbdat000$COLLECTOR_NUMBER))) {
    stop(paste("\"COLLECTOR_NUMBER\" not provided for row: ", 
               paste(which(is.na(herbdat000$COLLECTOR_NUMBER)) + 
                       1, collapse = ", ")))
  }
  if (any(is.na(herbdat000$DATE_COLLECTED))) {
    stop(paste("\"DATE_COLLECTED\" not provided for row: ", 
               paste(which(is.na(herbdat000$DATE_COLLECTED)) + 
                       1, collapse = ", ")))
  }
  if (any(is.na(herbdat000$FAMILY))) {
    warning(paste("\"FAMILY\" not provided for row: ", paste(which(is.na(herbdat000$FAMILY)) + 
                                                               1, collapse = ", ")))
  }
  # if (any(is.na(herbdat000$GENUS))) {
  #   warning(paste("\"GENUS\" must be provided for row: ", 
  #                 paste(which(is.na(herbdat000$GENUS)) + 1, collapse = ", ")))
  # }
  if (any(is.na(herbdat000$COUNTRY))) {
    stop(paste("\"COUNTRY\" not provided for row: ", paste(which(is.na(herbdat000$COUNTRY)) + 
                                                             1, collapse = ", ")))
  }
  # if (any(is.na(herbdat000$STATE_PROVINCE))) {
  #   warning(paste("\"STATE_PROVINCE\" not provided for row: ", 
  #                 paste(which(is.na(herbdat000$STATE_PROVINCE)) + 
  #                         1, collapse = ", ")))
  # }
  # if (any(is.na(herbdat000$COUNTY))) {
  #   warning(paste("\"COUNTY\" not provided for row: ", paste(which(is.na(herbdat000$COUNTY)) + 
  #                                                              1, collapse = ", ")))
  #   herbdat000$COUNTY[is.na(herbdat000$COUNTY)] <- " "
  # }
  if (any(is.na(herbdat000$LOCALITY))) {
    warning(paste("\"LOCALITY\" not provided for row: ", 
                  paste(which(is.na(herbdat000$LOCALITY)) + 1, collapse = ", ")))
  }
  if (any(is.na(herbdat000$IDENTIFIED_BY))) {
    warning(paste("\"IDENTIFIED_BY\" not provided for row: ", 
                  paste(which(is.na(herbdat000$IDENTIFIED_BY)) + 1, collapse = ", ")))
  }
  if (any(is.na(herbdat000$DATE_IDENTIFIED))) {
    warning(paste("\"DATE_IDENTIFIED\" not provided for row: ", 
                  paste(which(is.na(herbdat000$DATE_IDENTIFIED)) + 
                          1, collapse = ", ")))
  }
  print(paste(nrow(herbdat000), "herbarium specimen labels to create:"))
  pgenus <- herblabel::pgenus
  formatdate <- function(x) {
    if (!is.na(suppressWarnings(as.integer(x)))) {
      if (!grepl("^darwin", R.version$os)) {
        x <- as.Date(as.integer(x), origin = "1899-12-30")
      }
      else {
        x <- as.Date(as.integer(x), origin = "1904-01-01")
      }
    }
    res <- format(as.Date(x), "%d %B %Y")
    return(res)
  }
  formatdate2 <- function(x) {
    if (!is.na(suppressWarnings(as.integer(x)))) {
      if (!grepl("^darwin", R.version$os)) {
        x <- as.Date(as.integer(x), origin = "1899-12-30")
      }
      else {
        x <- as.Date(as.integer(x), origin = "1904-01-01")
      }
    }
    res <- format(as.Date(x))
    return(res)
  }
  Cap <- function(x) {
    paste(toupper(substring(x, 1, 1)), tolower(substring(x, 
                                                         2)), sep = "")
  }
  Cap2 <- function(x) {
    paste(toupper(substring(x, 1, 1)), substring(x, 2), 
          sep = "")
  }
  replace_space <- function(x) {
    gsub("^[[:space:]]+|[[:space:]]+$", "", x)
  }
  REPLACE <- function(x) {
    if (length(x) > 1) {
      stop("only one string is allowed")
    }
    bbb <- gsub(" +", " ", gsub(",+", ", ", gsub(", +", 
                                                 ",", x)))
    bbb <- gsub("^[[:space:]]+|[[:space:]]+$", "", bbb)
    endchar <- substr(bbb, nchar(bbb), nchar(bbb))
    if (endchar == ",") {
      yyy <- gregexpr(pattern = ",", bbb)
      res <- substr(bbb, start = 1, stop = ifelse(unlist(lapply(yyy, 
                                                                function(x) {
                                                                  max(x) - 1
                                                                })) > 1, unlist(lapply(yyy, function(x) {
                                                                  max(x) - 1
                                                                })), nchar(bbb)))
    }
    else {
      res <- bbb
    }
    res <- gsub("^[[:space:]]+|[[:space:]]+$", "", res)
    return(res)
  }
  latin_source <- herblabel::latin_source
  genera_names <- as.character(pgenus$GENUS)
  italic_latin <- function(x) {
    res.split <- unlist(strsplit(x, split = " "))
    res.split2 <- tolower(gsub(",|\\.", "", res.split))
    res.split3 <- gsub(",|\\.", "", res.split)
    found <- res.split2 %in% latin_source
    genus_found <- Cap(res.split3) %in% genera_names
    res.split[genus_found] <- res.split[genus_found]
    res.split[found] <- paste("\\cf3 \\i ", res.split[found], 
                              "\\i0\\cf0 ", sep = "")
    paste(res.split, collapse = " ", sep = "")
  }
  same_families <- c("Palmae", "Arecaceae", "Gramineae", "Poaceae", 
                     "Leguminosae", "Fabaceae", "Guttiferae", "Clusiaceae", 
                     "Cruciferae", "Brassicaceae", "Labiatae", "Lamiaceae", 
                     "Compositae", "Asteraceae", "Umbelliferae", "Apiaceae")
  herbdat000$FAMILY <- toupper(herbdat000$FAMILY)
  herbdat000$GLOBAL_UNIQUE_IDENTIFIER <- replace_space(herbdat000$GLOBAL_UNIQUE_IDENTIFIER)
  herbdat000$HERBARIUM <- replace_space(herbdat000$HERBARIUM)
  herbdat000$TITLE <- replace_space(herbdat000$TITLE)
  herbdat000$COLLECTOR <- replace_space(herbdat000$COLLECTOR)
  herbdat000$ADDITIONAL_COLLECTOR <- replace_space(herbdat000$ADDITIONAL_COLLECTOR)
  herbdat000$COLLECTOR_NUMBER <- replace_space(herbdat000$COLLECTOR_NUMBER)
  herbdat000$DATE_COLLECTED <- replace_space(herbdat000$DATE_COLLECTED)
  herbdat000$LOCAL_NAME <- replace_space(herbdat000$LOCAL_NAME)
  herbdat000$FAMILY <- replace_space(herbdat000$FAMILY)
  herbdat000$GENUS <- replace_space(herbdat000$GENUS)
  herbdat000$SPECIES <- replace_space(herbdat000$SPECIES)
  herbdat000$AUTHOR_OF_SPECIES <- replace_space(herbdat000$AUTHOR_OF_SPECIES)
  herbdat000$INFRASPECIFIC_RANK <- replace_space(herbdat000$INFRASPECIFIC_RANK)
  herbdat000$INFRASPECIFIC_EPITHET <- replace_space(herbdat000$INFRASPECIFIC_EPITHET)
  herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK <- replace_space(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK)
  herbdat000$COUNTRY <- replace_space(herbdat000$COUNTRY)
  herbdat000$STATE_PROVINCE <- replace_space(herbdat000$STATE_PROVINCE)
  herbdat000$COUNTY <- replace_space(herbdat000$COUNTY)
  herbdat000$LOCALITY <- replace_space(herbdat000$LOCALITY)
  herbdat000$IMAGE_URL <- replace_space(herbdat000$IMAGE_URL)
  herbdat000$RELATED_INFORMATION <- replace_space(herbdat000$RELATED_INFORMATION)
  herbdat000$LAT_DEGREE <- replace_space(herbdat000$LAT_DEGREE)
  herbdat000$LAT_MINUTE <- replace_space(herbdat000$LAT_MINUTE)
  herbdat000$LAT_SECOND <- replace_space(herbdat000$LAT_SECOND)
  herbdat000$LAT_FLAG <- replace_space(herbdat000$LAT_FLAG)
  herbdat000$LON_DEGREE <- replace_space(herbdat000$LON_DEGREE)
  herbdat000$LON_MINUTE <- replace_space(herbdat000$LON_MINUTE)
  herbdat000$LON_SECOND <- replace_space(herbdat000$LON_SECOND)
  herbdat000$LON_FLAG <- replace_space(herbdat000$LON_FLAG)
  herbdat000$ELEVATION <- replace_space(herbdat000$ELEVATION)
  herbdat000$ATTRIBUTES <- replace_space(herbdat000$ATTRIBUTES)
  herbdat000$REMARKS <- replace_space(herbdat000$REMARKS)
  herbdat000$GEOREFERENCE_SOURCES <- replace_space(herbdat000$GEOREFERENCE_SOURCES)
  herbdat000$PROJECT <- replace_space(herbdat000$PROJECT)
  herbdat000$IDENTIFIED_BY <- replace_space(herbdat000$IDENTIFIED_BY)
  herbdat000$DATE_IDENTIFIED <- replace_space(herbdat000$DATE_IDENTIFIED)
  herbdat000$TYPE_STATUS <- replace_space(herbdat000$TYPE_STATUS)
  herbdat000$PROCESSED_BY <- replace_space(herbdat000$PROCESSED_BY)
  herbdat000$DATE_LASTMODIFIED <- replace_space(herbdat000$DATE_LASTMODIFIED)
  herbdat000$LAT_DEGREE <- as.character(as.integer(herbdat000$LAT_DEGREE))
  herbdat000$LAT_MINUTE <- as.character(as.integer(herbdat000$LAT_MINUTE))
  herbdat000$LAT_SECOND <- as.character(round(as.numeric(herbdat000$LAT_SECOND), 
                                              digits = 2))
  herbdat000$LON_DEGREE <- as.character(as.integer(herbdat000$LON_DEGREE))
  herbdat000$LON_MINUTE <- as.character(as.integer(herbdat000$LON_MINUTE))
  herbdat000$LON_SECOND <- as.character(round(as.numeric(herbdat000$LON_SECOND), 
                                              digits = 2))
  comment_genus <- rep("", length(herbdat000$GENUS))
  comment_family <- rep("", length(herbdat000$FAMILY))
  if (spellcheck) {
    sptemp <- paste(ifelse(is.na(herbdat000$GENUS), "", 
                           herbdat000$GENUS), ifelse(is.na(herbdat000$SPECIES), 
                                                     "", herbdat000$SPECIES), ifelse(is.na(herbdat000$AUTHOR_OF_SPECIES), 
                                                                                     "", herbdat000$AUTHOR_OF_SPECIES), ifelse(is.na(herbdat000$INFRASPECIFIC_RANK), 
                                                                                                                               "", herbdat000$INFRASPECIFIC_RANK), ifelse(is.na(herbdat000$INFRASPECIFIC_EPITHET), 
                                                                                                                                                                          "", herbdat000$INFRASPECIFIC_EPITHET), ifelse(is.na(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK), 
                                                                                                                                                                                                                        "", herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK), sep = " ")
    sptemp2 <- c()
    for (i in 1:length(sptemp)) {
      sptemp2[i] <- REPLACE(sptemp[i])
    }
    tplsplist <- herblabel::tplsplist
    ind <- !sptemp2 %in% tplsplist
    if (length(which(ind)) > 0) {
      message_txt <- paste("Species:\n", paste(sptemp2[ind], 
                                               collapse = "\n"), "\nin row", paste(which(ind), 
                                                                                   collapse = ","), "\n are not accepted in The Plant List Database Ver 1.1, \ncheck spelling or synonyms for the scientific names")
    }
    herbdat000$GENUS[ind] <- ifelse(is.na(herbdat000$GENUS[ind]), 
                                    "(Empty Species)", herbdat000$GENUS[ind])
    herbdat000$GENUS[ind] <- paste("\\cf3\\i0 This species can not be found in the embedded database in herblabel package. Check Spelling or Validity at {\\field{\\*\\fldinst{HYPERLINK \"http://www.theplantlist.org/\"}}{\\fldrslt{\\ul\\cf3 http://www.theplantlist.org/}}} or {\\field{\\*\\fldinst{HYPERLINK \"http://frps.eflora.cn/\"}}{\\fldrslt{\\ul\\cf3 http://frps.eflora.cn/}}} for:\\i  ", 
                                   herbdat000$GENUS[ind], sep = "")
    comment_genus[ind] <- paste("This species can not be found in the embedded database in herblabel package", 
                                dat$GENUS[ind])
    herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind] <- paste(ifelse(is.na(herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind]), 
                                                                 "", herbdat000$AUTHOR_OF_INFRASPECIFIC_RANK[ind]), 
                                                          "", sep = "")
  }
  lat_check_ind_1 <- rep(FALSE, nrow(herbdat000))
  lat_check_ind_2 <- rep(FALSE, nrow(herbdat000))
  lat_check_ind_3 <- rep(FALSE, nrow(herbdat000))
  lon_check_ind_1 <- rep(FALSE, nrow(herbdat000))
  lon_check_ind_2 <- rep(FALSE, nrow(herbdat000))
  lon_check_ind_3 <- rep(FALSE, nrow(herbdat000))
  herbdat000$LAT_FLAG <- toupper(herbdat000$LAT_FLAG)
  herbdat000$LON_FLAG <- toupper(herbdat000$LON_FLAG)
  for (i in 1:nrow(herbdat000)) {
    herbdat_temp <- herbdat000[i, ]
    if (any(!is.na(herbdat_temp$LAT_DEGREE), !is.na(herbdat_temp$LAT_MINUTE), 
            !is.na(herbdat_temp$LAT_SECOND)) & is.na(herbdat_temp$LAT_FLAG)) {
      lat_check_ind_1[i] <- TRUE
    }
    if (all(!is.na(herbdat_temp$LAT_DEGREE), !is.na(herbdat_temp$LAT_MINUTE), 
            !is.na(herbdat_temp$LAT_SECOND)) & is.na(herbdat_temp$LAT_FLAG)) {
      lat_check_ind_2[i] <- TRUE
    }
    if (!herbdat_temp$LAT_FLAG %in% c("N", "S", NA)) {
      lat_check_ind_3[i] <- TRUE
    }
    if (any(!is.na(herbdat_temp$LON_DEGREE), !is.na(herbdat_temp$LON_MINUTE), 
            !is.na(herbdat_temp$LON_SECOND)) & is.na(herbdat_temp$LON_FLAG)) {
      lon_check_ind_1[i] <- TRUE
    }
    if (all(!is.na(herbdat_temp$LON_DEGREE), !is.na(herbdat_temp$LON_MINUTE), 
            !is.na(herbdat_temp$LON_SECOND)) & is.na(herbdat_temp$LON_FLAG)) {
      lon_check_ind_2[i] <- TRUE
    }
    if (!herbdat_temp$LON_FLAG %in% c("E", "W", NA)) {
      lon_check_ind_3[i] <- TRUE
    }
  }
  if (any(lat_check_ind_1)) {
    lat_check_ind_1_msg <- paste("Degree, Minutes and Seconds for Latitude not completed in row:", 
                                 paste(which(lat_check_ind_1), collapse = ", "))
    stop(lat_check_ind_1_msg)
  }
  if (any(lat_check_ind_2)) {
    lat_check_ind_2_msg <- paste("LAT_FLAG not specified in row:", 
                                 paste(which(lat_check_ind_2), collapse = ", "))
    stop(lat_check_ind_2_msg)
  }
  if (any(lat_check_ind_3)) {
    lat_check_ind_3_msg <- paste("Only N or S is allowed for the LAT_FLAG in row:", 
                                 paste(which(lat_check_ind_3), collapse = ", "))
    stop(lat_check_ind_3_msg)
  }
  if (any(lon_check_ind_1)) {
    lon_check_ind_1_msg <- paste("Degree, Minutes and Seconds for Longitude not completed in row:", 
                                 paste(which(lon_check_ind_1), collapse = ", "))
    stop(lon_check_ind_1_msg)
  }
  if (any(lon_check_ind_2)) {
    lon_check_ind_2_msg <- paste("LON_FLAG must be specified in row:", 
                                 paste(which(lon_check_ind_2), collapse = ", "))
    stop(lon_check_ind_2_msg)
  }
  if (any(lon_check_ind_3)) {
    lon_check_ind_3_msg <- paste("Only N or S is allowed for the LON_FLAG in row:", 
                                 paste(which(lon_check_ind_3), collapse = ", "))
    stop(lon_check_ind_3_msg)
  }
  if (font == "Roman") {
    fonttab <- "{\\fonttbl{\\f0\\froman\\fcharset134 SimSun;}{\\f1\\froman\\fcharset134 Times New Roman;}}"
  }
  if (font == "Arial") {
    fonttab <- "{\\fonttbl{\\f0\\fswiss\\fcharset134 SimSun;}{\\f1\\fswiss\\fcharset134 Arial;}}"
  }
  temp1 <- paste("{\\rtf1\\ansi\\ansicpg936\\deflangfe2052\\fcharset134\\deff1", 
                 fonttab, "{\\stylesheet{\\*\\cs3 Default Paragraph Font;}}{\\colortbl\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red0\\green255\\blue0;\\red0\\green0\\blue255;}\\paperw12240\\paperh15840\\margl1800\\margr1800\\margt1440\\margb1440\\gutter0\\ftnbj\\aenddoc\\jcompress1\\viewkind4\\viewscale100\\asianbrkrule\\allowfieldendsel\\snaptogridincell\\viewkind4\\sectd\\sbkpage\\pgwsxn11906\\pghsxn16838\\marglsxn600\\margrsxn600\\margtsxn720\\margbsxn10\\guttersxn0\\headery720\\footery720\\pgbrdropt0\\sectdefaultcl\\cols2\\colsx1080\\linebetcol1\\endnhere", 
                 sep = "")
  temp2 <- c()
  temp_count <- seq(0, nrow(herbdat000), by = 5)
  temp_count[1] <- 1
  NEW_DATE_COLLECTED <- as.character(c())
  NEW_DATE_IDENTIFIED <- as.character(c())
  herbdat_row1 <- herbdat000[1, ]
  for (i in 1:nrow(herbdat000)) {
    herbdat <- herbdat000[i, ]
    if (nrow(herbdat000) > 5) {
      if (i %in% temp_count) {
        print(paste("Making label for row: ", i))
      }
    }
    if (spellcheck) {
      temp.genus <- herbdat$GENUS
      if (!grepl("This species can not be found in the embedded database in herblabel package", 
                 as.character(temp.genus))) {
        if (!Cap(as.character(temp.genus)) %in% Cap(as.character(pgenus$GENUS))) {
          if (!grepl("Genus not accepted at The Plant List Website", 
                     as.character(temp.genus))) {
            herbdat$GENUS <- paste("\\cf3 ", as.character(temp.genus), 
                                   "\\cf3 \\i0  (Genus not accepted at The Plant List Website.)", 
                                   sep = "")
            comment_genus[i] <- paste(as.character(temp.genus), 
                                      "(Genus not accepted at The Plant List Website.)", 
                                      sep = "")
          }
        }
      }
      temp.family <- herbdat$FAMILY
      if (!Cap(as.character(temp.family)) %in% c(Cap(as.character(pgenus$FAMILY)), 
                                                 same_families)) {
        if (is.na(temp.family) & !grepl("Empty Family", 
                                        as.character(comment_family[i]))) {
          temp.family <- "(Empty Family)"
        }
        herbdat$FAMILY <- paste("\\cf3 ", as.character(temp.family), 
                                "\\cf3 (Family not accepted at The Plant List Website.) ", 
                                sep = "")
        comment_family[i] <- paste(gsub("\\\\\\cf3", 
                                        "", as.character(temp.family)), "Family not accepted at The Plant List Website.")
      }
      fam.genus.temp <- data.frame(FAMILY = as.character(herbdat$FAMILY), 
                                   GENUS = as.character(herbdat$GENUS))
      fgmerge.temp <- merge(x = fam.genus.temp, y = pgenus, 
                            by.x = "GENUS", by.y = "GENUS", all.x = TRUE, 
                            sort = FALSE)
      if (any(as.character(Cap(fgmerge.temp$FAMILY.x)) != 
              as.character(Cap(fgmerge.temp$FAMILY.y)) & !is.na(as.character(fgmerge.temp$FAMILY.y)))) {
        if (unique(as.character(Cap(fgmerge.temp$FAMILY.x))) %in% 
            as.character(Cap(fgmerge.temp$FAMILY.y)) & 
            !grepl("could also be under", as.character(temp.genus))) {
          herbdat$FAMILY <- paste("\\cf3 ", unique(as.character(fgmerge.temp$FAMILY.x)), 
                                  "  ", sep = "")
          herbdat$GENUS <- paste("\\cf3 ", unique(as.character(fgmerge.temp$GENUS)), 
                                 "\\i0\\cf3  (could also be under \"", toupper(paste(as.character(Cap(fgmerge.temp$FAMILY.y))[!as.character(Cap(fgmerge.temp$FAMILY.y)) %in% 
                                                                                                                                as.character(Cap(fgmerge.temp$FAMILY.x))], 
                                                                                     collapse = "\", \"")), "\" according to The Plant List Website.) ", 
                                 sep = "")
          comment_genus[i] <- paste(unique(as.character(fgmerge.temp$GENUS)), 
                                    "(could also be under ", toupper(paste(as.character(Cap(fgmerge.temp$FAMILY.y))[!as.character(Cap(fgmerge.temp$FAMILY.y)) %in% 
                                                                                                                      as.character(Cap(fgmerge.temp$FAMILY.x))], 
                                                                           collapse = ",")), "according to The Plant List Website.) ", 
                                    sep = "")
        }
        else {
          if (!grepl("should be under", as.character(temp.genus))) {
            comment_family[i] <- paste(unique(gsub("\\\\\\cf3", 
                                                   "", as.character(fgmerge.temp$FAMILY.x))), 
                                       collapse = "")
            if (!toupper(as.character(fgmerge.temp$FAMILY.x)) %in% 
                toupper(same_families)) {
              herbdat$GENUS <- paste("\\cf3 ", unique(as.character(fgmerge.temp$GENUS)), 
                                     "\\i0 \\cf3 (should be under \"", toupper(paste(as.character(fgmerge.temp$FAMILY.y), 
                                                                                     collapse = "\", \"")), "\" according to The Plant List Website.) ", 
                                     sep = "")
              comment_genus[i] <- paste(" (should be under", 
                                        toupper(paste(as.character(fgmerge.temp$FAMILY.y), 
                                                      collapse = ",")), "according to The Plant List Website.)", 
                                        sep = "")
            }
          }
        }
      }
    }
   
    if (theme == "GILLES") {
      res <- c(paste("{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb350\\sa80\\fs", 
                     trunc(font_size * 20), "", herbdat$HERBARIUM, 
                     "\\b0\\par }", sep = ""), 
               ifelse(is.na(herbdat$TITLE), "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\fs", 
                                                                trunc(font_size * 18), "\\qc\\sb10\\sa100\\b ", 
                                                                herbdat$TITLE, "\\b0 \\par }", sep = "")), 
               ifelse(is.na(herbdat$FAMILY), paste("{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100\\fs", 
                                                   trunc(font_size * 18), "\\b ", "\\b0\\qc0 \\par }", 
                                                   sep = ""), paste("{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100\\fs", 
                                                                    trunc(font_size * 18), "\\b ", herbdat$FAMILY, 
                                                                    "\\b0\\qc0 \\par }", sep = "")), 
               ifelse(is.na(herbdat$FULL_NAME),
                      "", paste("{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs", trunc(font_size * 20), "\\b\\i ", herbdat$FULL_NAME, 
                                "\\b0\\par}", sep = "")), 
               paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb120\\sa20\\fs", trunc(font_size * 18), " ", 
                     REPLACE(paste(toupper(ifelse(is.na(herbdat$COUNTRY), "", herbdat$COUNTRY)), ", ", 
                                   ifelse(is.na(herbdat$STATE_PROVINCE), "", herbdat$STATE_PROVINCE), ", ", 
                                   ifelse(is.na(herbdat$COUNTY), "", herbdat$COUNTY), ", ", 
                                   ifelse(is.na(herbdat$LOCALITY), "", as.character(herbdat$LOCALITY)), sep = "")), "\\par}", sep = ""), 
               REPLACE(ifelse(is.na(herbdat$LAT_DEGREE), "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\sb20\\sa150\\fs", trunc(font_size * 18), "\\qj ", 
                                                                   herbdat$LAT_DEGREE, "\\u176;", 
                                                                   herbdat$LAT_MINUTE, "\\u39;", 
                                                                   herbdat$LAT_SECOND, "\\u34;", 
                                                                   herbdat$LAT_FLAG, ", ", 
                                                                   herbdat$LON_DEGREE, "\\u176;", 
                                                                   herbdat$LON_MINUTE, "\\u39;", 
                                                                   herbdat$LON_SECOND, "\\u34;", 
                                                                   herbdat$LON_FLAG, 
                                                                   ifelse(is.na(herbdat$ELEVATION), "", paste("; ", herbdat$ELEVATION, "m", sep = "")), "\\par }", 
                                                                   sep = ""))), 
               ifelse((is.na(herbdat$ATTRIBUTES)) & (is.na(herbdat$REMARKS)), "", italic_latin(gsub("\\.  ", "\\. ", gsub(" \\.", "\\.", gsub("\\. \\.", "\\. ", gsub("\\. +", "\\. ", 
                                                                                                                                                                      REPLACE(paste("{\\pard\\keep\\keepn\\fi0\\li0", paste("\\fs", trunc(font_size * 18), sep = ""), "\\sb60", 
                                                                                                                                                                                    ifelse(is.na(herbdat$ATTRIBUTES), "", Cap2(as.character(herbdat$ATTRIBUTES))), 
                                                                                                                                                                                    ifelse(is.na(herbdat$ATTRIBUTES), "", ". "), 
                                                                                                                                                                                    ifelse(is.na(herbdat$REMARKS), "", Cap2(as.character(herbdat$REMARKS))),"\\sa80\\par}", sep = " ")))))))), 
               ifelse(is.na(herbdat$ADDITIONAL_COLLECTOR), paste("{\\pard\\keep\\keepn\\fi0\\sb200\\sa100\\fs", trunc(font_size * 18), "\\tqr\\tx4850\\b ", 
                                                                 herbdat$COLLECTOR, ", #", 
                                                                 herbdat$COLLECTOR_NUMBER, "\\b0", "  ", 
                                                                 ifelse(nchar(paste(herbdat$COLLECTOR, herbdat$ADDITIONAL_COLLECTOR, ", #", herbdat$COLLECTOR_NUMBER)) > 40, "\\line", "  "), " \\tab ", 
                                                                 tryCatch(formatdate(herbdat$DATE_COLLECTED), 
                                                                          error = function(e) {cat(" ")
                                                                            herbdat$DATE_COLLECTED}), "\\par}", sep = ""), 
                      paste("{\\pard\\keep\\keepn\\fi0\\sb200\\sa100", 
                            paste("\\fs", trunc(font_size * 18), sep = ""), "\\tqr\\tx4850\\b ", 
                            herbdat$COLLECTOR, ", ", 
                            herbdat$ADDITIONAL_COLLECTOR, "  #", 
                            herbdat$COLLECTOR_NUMBER, "\\b0", "  ", 
                            ifelse(nchar(paste(herbdat$COLLECTOR, herbdat$ADDITIONAL_COLLECTOR, ", #", herbdat$COLLECTOR_NUMBER)) > 40, "\\line", "  "), " \\tab ", 
                            tryCatch(formatdate(herbdat$DATE_COLLECTED), error = function(e) {cat(" ") 
                              herbdat$DATE_COLLECTED}), "\\par}", sep = "")), 
               ifelse(is.na(herbdat$PROJECT), "", paste("{\\pard\\keep\\keepn\\fi0\\li0\\sa160\\fs", trunc(font_size * 18), "\\ql\\b ", as.character(herbdat$PROJECT), "\\ql0\\b0\\par }", sep = "")), 
               ifelse(is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER) & is.na(herbdat$TYPE_STATUS) & is.na(herbdat$IDENTIFIED_BY) & is.na(herbdat$DATE_IDENTIFIED), "", 
                      paste("{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\fs", trunc(font_size * 18), "\\tqr\\tx4850 ", 
                            # gsub("_","", ifelse(is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER), "", as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER))), 
                            " \\tab ", 
                            ifelse(is.na(herbdat$TYPE_STATUS), "", herbdat$TYPE_STATUS), 
                            ifelse(is.na(herbdat$IDENTIFIED_BY), "", paste(" Det.: ", herbdat$IDENTIFIED_BY)), 
                            ifelse(is.na(herbdat$DATE_IDENTIFIED), "", ", "), 
                            ifelse(is.na(herbdat$DATE_IDENTIFIED), "", 
                                   tryCatch(formatdate(herbdat$DATE_IDENTIFIED), error = function(e) {cat(" ")
                                     herbdat$DATE_IDENTIFIED})), "\\par}", sep = "")), 
               paste("{\\pard\\keep\\keepn\\sa100\\fs", trunc(font_size * 18), sep = ""), " \\par }", paste("{\\pard\\keep\\qc\\fs", trunc(font_size * 18), sep = ""), "  .                  .                   .\\par}")
    }
    
    NEW_DATE_COLLECTED[i] <- tryCatch(formatdate2(herbdat$DATE_COLLECTED), 
                                      error = function(e) {
                                        herbdat$DATE_COLLECTED
                                      })
    NEW_DATE_IDENTIFIED[i] <- tryCatch(formatdate2(herbdat$DATE_IDENTIFIED), 
                                       error = function(e) {
                                         herbdat$DATE_IDENTIFIED
                                       })
    temp2 <- c(temp2, res)
    herbdat_row1 <- rbind(herbdat_row1, herbdat)
  }
  template <- c(temp1, temp2, "}")
  res <- template[!template %in% ""]
  res <- res[!res %in% " "]
  res <- replace_space(res)
  res <- iconv(x = res, from = "UTF-8", to = "GB18030")
  writeLines(res, outfile)
  cat("Herbarium Labels have been saved to:\n", file.path(getwd(), 
                                                          outfile), "\n", sep = "")
  modified_dat <- herbdat_row1[-1, ]
  modified_dat$GENUS <- comment_genus
  modified_dat$FAMILY <- comment_family
  dat$DATE_COLLECTED <- NEW_DATE_COLLECTED
  dat$DATE_IDENTIFIED <- NEW_DATE_IDENTIFIED
  return(invisible(list(dat = dat, modified_dat = modified_dat)))
}
