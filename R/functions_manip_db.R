
#' Launch shiny app for taxa
#'
#' Allow exploring the database by taxa
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @import shiny
#' @export
query_taxa_app <- function() {

  app <- list(ui = ui <- fluidPage(

    # Application title
    titlePanel("Query plots"),

    sidebarPanel(
      textInput("genus",
                "Enter genus name", value=NULL),

      textInput("species",
                "Enter species name", value=NULL),

      textInput("family",
                "Enter family name", value=NULL),

      checkboxInput("extract_ind", "Extract individuals?")
      # ,
      #
      # actionButton("do", "Launch query")

    ),


    mainPanel(
      DT::DTOutput("table"),

      verbatimTextOutput("test"),

      plotOutput(outputId = "plots_allo", brush = "plot_brush"),

      tableOutput("data_brush")

    )
  ),
              server = function(input, output) {


                extract <- reactiveValues(df = NULL)

                plot_dbh_h <- reactiveValues(df = NULL)

                ids_selected <- reactiveValues(df = NULL)

                data_height_dbh <- reactiveValues(df = NULL)

                # all_val <- reactiveValues(val=NULL)
                # all_val$val <-
                #   paste(input$genus , input$species)

                xxchange <- reactive({
                  paste(input$genus , input$species , input$family)
                })

                observeEvent(xxchange, {

                  # req(input$genus)

                  output$table <- DT::renderDataTable({

                    extract$df <- query_tax_all(genus_searched = input$genus, tax_esp_searched = input$species,
                                                extract_individuals = input$extract_ind, tax_fam_searched = input$family)
                    extract$df

                  })

                })

                observeEvent(xxchange, {
                  #
                  # output$test <- renderPrint({
                  #
                  #   print(input$genus)
                  #   print(input$species)
                  #   print(data_height_dbh$df)
                  #   # print(ids_selected$df)
                  #
                  # })

                  output$plots_allo <-
                    renderPlot({

                      ids_selected$df <-
                        query_tax_all(genus_searched = input$genus, tax_esp_searched = input$species, tax_fam_searched = input$family) %>%
                        dplyr::select(id_n) %>%
                        dplyr::pull()

                      # ids$vec <- id
                      if(length(ids_selected$df)>0) {
                        out_all <-
                          explore_allometric_taxa(id_search = ids_selected$df)

                        data_height_dbh$df <- out_all$data_height_dbh

                        plot_dbh_h$df <- out_all$plot_height_dbh
                        #
                        plot_dbh_h$df
                        # plot(1)
                      }else{
                        return()
                      }
                    })


                  output$data_brush <-
                    renderTable({
                      n <-
                        nrow(brushedPoints(data_height_dbh$df, brush = input$plot_brush))

                      if(n==0) {
                        return()
                      }else{
                        brushedPoints(data_height_dbh$df, brush = input$plot_brush)
                      }

                    })

                })

                # }
                # )
              })
  shiny::runApp(app, launch.browser = TRUE)

}

#' Load the database
#'
#' Load the database and ask for password
#'
#' @return The database is loaded
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
.call.mydb <- function() {

  if(!exists("mydb")) {
    pass <- rstudioapi::askForPassword("Please enter your password")

    mydb <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
                      host = 'localhost', # i.e. 'ec2-54-83-201-96.compute-1.amazonaws.com'
                      port = 5432, # or any other port specified by your DBA
                      user = 'postgres',
                      password = pass)

    assign("mydb", mydb, envir = .GlobalEnv)

    # return(mydb)
  }
}

#' List of countries
#'
#' Provide list of countries where plots occur
#'
#' @return A tibble of all countries
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
###
country_list <- function() {

  if(!exists("mydb")) .call.mydb() # mydb <-

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")
  nn <- dplyr::group_by(nn, country)
  nn <- dplyr::count(nn)
  nn <- dplyr::mutate(nn, n=as.integer(n))
  nn <- dplyr::collect(nn)

  # %>%
  #   dplyr::group_by(country) %>%
  #   dplyr::count() %>%
  #   dplyr::mutate(n=as.integer(n)) %>%
  #   dplyr::collect()

  # dbDisconnect(mydb)

  return(nn)
}



#' List of province
#'
#' Provide list of province where plots occur
#' @param country string
#'
#' @return A tibble of all province
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
province_list <- function(country=NULL) {
  if(!exists("mydb")) .call.mydb()

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")

  if(!is.null(country)) {

    var <- rlang::enquo(country)

    nn <-
      nn %>%
      dplyr::filter(country==!!var)
  }

  nn <-
    nn %>%
    dplyr::group_by(province) %>%
    dplyr::count() %>%
    dplyr::mutate(n=as.integer(n)) %>%
    dplyr::collect()

  # dbDisconnect(mydb)

  return(nn)
}



#' List of method
#'
#' Provide list of method where plots occur
#'
#' @return A tibble of all method
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
method_list <- function() {
  if(!exists("mydb")) .call.mydb()

  nn <-
    dplyr::tbl(mydb, "data_liste_plots") %>%
    dplyr::group_by(method) %>%
    dplyr::count() %>%
    dplyr::mutate(n=as.integer(n)) %>%
    dplyr::collect()

  # dbDisconnect(mydb)
  return(nn)
}





#' List and map selected plots
#'
#' Provide and map list of selected plots
#'
#' @return A tibble of all method
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}

#' @param country string
#' @param province string
#' @param team_lead string
#' @param plot_name string
#' @param method string
#' @param date_y integer
#' @param extract_individuals logical
#' @param map logical
#' @param zoom integer positive values indicating zoom level for ggplot style map
#' @param label logical for ggplot map, whether adding label or not
#' @param try_optimal_label logical
#' @param map_type string mapview or ggplot
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
#' @export
query_plots <- function(team_lead=NULL,
                        plot_name=NULL,
                        country=NULL,
                        province=NULL,
                        method=NULL,
                        date_y=NULL,
                        extract_individuals=FALSE,
                        map=FALSE,
                        zoom=0,
                        label=T,
                        try_optimal_label=F,
                        map_type="mapview") {

  if(!exists("mydb")) .call.mydb()

  query <- 'SELECT * FROM data_liste_plots WHERE MMM'

  if(!is.null(team_lead))
    query <- gsub(pattern = "MMM", replacement = paste0(" team_leader ILIKE '%", team_lead, "%' AND MMM"), x=query)

  # query <- "SELECT * FROM data_liste_plots WHERE  team_leader ILIKE '%Dauby%' AND country IN ('Gabon', 'Cameroun')"

  if(!is.null(country)) {
    if(length(country)==1) {
      query <- gsub(pattern = "MMM", replacement = paste0(" country ILIKE '%", country, "%' AND MMM"), x=query)
    }else{
      query <- gsub(pattern = "MMM", replacement = paste0("country IN ('", paste(country, collapse = "', '"), "') AND MMM"), x=query)
    }
  }

  if(!is.null(province)) {
    if(length(province)==1) {
      query <- gsub(pattern = "MMM", replacement = paste0(" province ILIKE '%", province, "%' AND MMM"), x=query)
    }else{
      query <- gsub(pattern = "MMM", replacement = paste0("province IN ('", paste(province, collapse = "', '"), "') AND MMM"), x=query)
    }
  }

  if(!is.null(method))
    query <- gsub(pattern = "MMM", replacement = paste0(" method ILIKE '%", method, "%' AND MMM"), x=query)

  if(!is.null(plot_name))
    query <- gsub(pattern = "MMM", replacement = paste0(" plot_name ILIKE '", plot_name, "%' AND MMM"), x=query)

  query <- gsub(pattern = "AND MMM", replacement = "", query)

  if(grepl("WHERE MMM", query)) query <- gsub(pattern = " WHERE MMM", replacement = "", query)

  rs <- DBI::dbSendQuery(mydb, query)
  res <- DBI::dbFetch(rs)
  DBI::dbClearResult(rs)
  res <- dplyr::as_tibble(res)
  res <-
    res %>%
    dplyr::select(-id_old)

  if(map) {

    world_map <- spData::world

    if(any(is.na(res$ddlat)) | any(is.na(res$ddlon))) {
      cat("\n removing following plots because missing coordinates")
      print(res %>%
              dplyr::filter(is.na(ddlat), is.na(ddlon)))
    }

    res <-
      res %>%
      dplyr::filter(!is.na(ddlat), !is.na(ddlon)) %>%
      dplyr::select(-id_senterre_db)

    data_sf <- sf::st_as_sf(res, coords = c("ddlon", "ddlat"), crs = 4326)
    bbox_data <- sf::st_bbox(data_sf)

    if(map_type=="ggplot") {

      outputmap <-
        ggplot2::ggplot() +
        ggplot2::geom_sf(data = world_map, alpha=0.8)

      if(label) {
        if(try_optimal_label) {
          outputmap <-
            outputmap +
            ggrepel::geom_text_repel(ggplot2::aes(x= res$ddlon, y= res$ddlat, label= data_sf$plot_name), hjust=0, vjust=0)
        }else{
          outputmap <-
            outputmap +
            ggplot2::geom_text(ggplot2::aes(x= res$ddlon, y= res$ddlat, label= data_sf$plot_name), hjust=0, vjust=0)
        }
      }

      outputmap <- outputmap +
        ggplot2::geom_sf(data = data_sf)

      outputmap <-
        outputmap +
        ggplot2::coord_sf(xlim = c(bbox_data[1]-zoom, bbox_data[3]+zoom), ylim = c(bbox_data[2]-zoom, bbox_data[4]+zoom))


    }

    if(map_type=="mapview") {
      print(map_type)
      outputmap <-  mapview::mapview(data_sf)
    }
    print(outputmap)
  }

  if(extract_individuals) {
    # res <-
    #   tbl(mydb, "data_individuals") %>%
    #   filter(id_table_liste_plots_n %in% res$id_liste_plots) %>%
    #
    #   collect()

    specimens_id_diconame <-
      dplyr::tbl(mydb, "specimens") %>%
      dplyr::select(id_specimen, id_diconame_n)

    diconames_id <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::select(id_n, id_good_n)

    specimens_linked <-
      specimens_id_diconame %>%
      dplyr::left_join(diconames_id, by=c("id_diconame_n"="id_n")) %>%
      dplyr::rename(id_dico_name_specimen=id_good_n) %>%
      dplyr::select(id_specimen, id_dico_name_specimen)

    # test <-
    #   tbl(mydb, "data_individuals") %>%
    #   filter(id_table_liste_plots_n %in% res$id_liste_plots) %>%
    #   left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
    #   mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
    #   left_join(tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
    #   dplyr::select(id_n, dbh, full_name_used, id_diconame_n, id_dico_name_specimen, id_diconame_final, id_specimen) %>%
    #   arrange(id_n) %>%
    #   collect()

    selec_plot_tables <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::select(plot_name, team_leader, country, locality_name, id_liste_plots)

    res <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_table_liste_plots_n %in% res$id_liste_plots) %>%
      dplyr::left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
      dplyr::left_join(selec_plot_tables, by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
      dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
      dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
      dplyr::select(id_n, multi_tiges_id_good, plot_name,team_leader,country,locality_name, sous_plot_name, ind_num_sous_plot, code_individu, dbh, full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat,
                    id_diconame_final, dbh_height, tree_height, branch_height, branchlet_height, crown_spread, liane, strate_cat,
                    herbarium_code_char, id_specimen) %>%
      dplyr::arrange(id_n) %>%
      dplyr::collect()

  }

  # dbDisconnect(mydb)

  return(res)
}




#' Query taxa
#'
#' Provide information on selected taxa
#'
#' @return A tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param genus_searched string
#' @param tax_esp_searched string
#' @param tax_fam_searched string
#' @param id_search integer
#' @param show_synonymies logical
#' @param extract_individuals logical
#'
#' @return A tibble of taxa or individuals if extract_individuals is TRUE
#' @export
query_tax_all <- function(genus_searched=NULL,
                          tax_esp_searched=NULL,
                          tax_fam_searched=NULL,
                          id_search=NULL,
                          show_synonymies=TRUE,
                          extract_individuals=FALSE) {

  if(!exists("mydb")) .call.mydb()

  if(is.null(id_search)) {
    query <- 'SELECT * FROM diconame WHERE MMM'

    if(!is.null(genus_searched))
      query <- gsub(pattern = "MMM", replacement = paste0(" tax_gen ILIKE '%", genus_searched, "%' AND MMM"), x=query)

    if(!is.null(tax_esp_searched))
      query <- gsub(pattern = "MMM", replacement = paste0(" tax_esp ILIKE '", tax_esp_searched, "%' AND MMM"), x=query)

    if(!is.null(tax_fam_searched))
      query <- gsub(pattern = "MMM", replacement = paste0(" tax_fam ILIKE '%", tax_fam_searched, "%' AND MMM"), x=query)

    query <- gsub(pattern = "AND MMM", replacement = "", query)

    rs <- DBI::dbSendQuery(mydb, query)
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
  }else{
    if(!is.null(genus_searched) | !is.null(tax_esp_searched) | !is.null(tax_fam_searched)) cat("Query for tax based on ID, others entries ignored \n")
    res <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::filter(id_n %in% id_search) %>%
      dplyr::collect()
  }

  if(nrow(res)==1 & show_synonymies) {

    if(res$id_good_n!=res$id_n) {
      cat("\n This taxa is considered SYNONYM of")
      print(dplyr::tbl(mydb, "diconame") %>%
              dplyr::filter(id_n==res$id_good_n) %>%
              dplyr::select(full_name, full_name_no_auth, tax_fam, tax_gen, morphocat))
    }

    taxa_syn <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::filter(id_good_n==res$id_n, id_n!=res$id_n) %>%
      dplyr::collect()

    if(nrow(taxa_syn)>0) {
      cat("\n This taxa has the following SYNONYMS")
      print(taxa_syn %>%
              dplyr::select(full_name, full_name_no_auth, tax_fam, tax_gen, morphocat))
    }
  }

  # dbDisconnect(mydb)

  res <-
    res %>%
    dplyr::select(-id, -id_good)


  if(extract_individuals) {

    specimens_id_diconame <-
      dplyr::tbl(mydb, "specimens") %>%
      dplyr::select(id_specimen, id_diconame_n)

    diconames_id <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::select(id_n, id_good_n)

    specimens_linked <-
      specimens_id_diconame %>%
      dplyr::left_join(diconames_id, by=c("id_diconame_n"="id_n")) %>%
      dplyr::rename(id_dico_name_specimen=id_good_n) %>%
      dplyr::select(id_specimen, id_dico_name_specimen)

    selec_plot_tables <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::select(plot_name, team_leader, country, locality_name, id_liste_plots)

    res <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_diconame_n %in% res$id_n) %>%
      dplyr::left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
      dplyr::left_join(selec_plot_tables, by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
      dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
      dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
      dplyr::select(id_n, multi_tiges_id_good, plot_name,team_leader,country,locality_name, sous_plot_name, ind_num_sous_plot, code_individu, dbh, full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat,
                    id_diconame_final, dbh_height, tree_height, branch_height, branchlet_height, crown_spread, liane, strate_cat,
                    herbarium_code_char, id_specimen) %>%
      dplyr::arrange(id_n) %>%
      dplyr::collect()

  }


  return(dplyr::as_tibble(res))
}





#' Explore allometric relation
#'
#' Provide allometric data and graph dbh-height of selected taxa
#'
#' @return A tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param genus_searched string
#' @param tax_esp_searched string
#' @param tax_fam_searched string
#' @param id_search integer
#'
#' @return A tibble of taxa or individuals if extract_individuals is TRUE
#' @export
explore_allometric_taxa <- function(genus_searched=NULL,
                                    tax_esp_searched=NULL,
                                    tax_fam_searched=NULL,
                                    id_search=NULL) {

  if(!exists("mydb")) .call.mydb()

  tax_data <-
    query_tax_all(genus_searched = genus_searched, tax_esp_searched = tax_esp_searched, id_search = id_search)

  if(nrow(tax_data)>0) {
    cat(paste0("\n ", nrow(tax_data), " taxa selected"))
    print(tax_data$full_name_no_auth)

    specimens_id_diconame <-
      dplyr::tbl(mydb, "specimens") %>%
      dplyr::select(id_specimen, id_diconame_n)

    diconames_id <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::select(id_n, id_good_n)

    specimens_linked <-
      specimens_id_diconame %>%
      dplyr::left_join(diconames_id, by=c("id_diconame_n"="id_n")) %>%
      dplyr::rename(id_dico_name_specimen=id_good_n) %>%
      dplyr::select(id_specimen, id_dico_name_specimen)

    selec_plot_tables <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::select(plot_name, team_leader, country, locality_name, id_liste_plots)

    all_individuals_data <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
      dplyr::left_join(selec_plot_tables, by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
      dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
      dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
      dplyr::filter(id_diconame_final %in% tax_data$id_n) %>%
      dplyr::select(id_n, multi_tiges_id_good, plot_name,team_leader,country,locality_name, sous_plot_name, ind_num_sous_plot, code_individu, dbh, full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat,
                    id_diconame_final, dbh_height, tree_height, branch_height, branchlet_height, crown_spread, liane, strate_cat,
                    herbarium_code_char, id_specimen) %>%
      dplyr::arrange(id_n)

    data_allo1 <-
      all_individuals_data %>%
      dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, full_name_no_auth) %>%
      dplyr::filter(!is.na(tree_height), tree_height>0, dbh>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both tree height and dbh values is ", nrow(data_allo1)))

    if(nrow(data_allo1)>1) {
      gg_plot1 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo1, mapping = ggplot2::aes(x = dbh, y = tree_height))
    }else{
      gg_plot1 <- NA
    }

    data_allo2 <-
      all_individuals_data %>%
      dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, full_name_no_auth) %>%
      dplyr::filter(!is.na(crown_spread), crown_spread>0, dbh>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both crown_spread and dbh values is ", nrow(data_allo2)))

    if(nrow(data_allo2)>1) {
      gg_plot2 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo2, mapping = ggplot2::aes(x = dbh, y = crown_spread))
    }else{
      gg_plot2 <- NA
    }
  }else{
    cat("\n No taxa found. Select at least one taxa")
    # cat(paste0("\n You currently selected ", nrow(tax_data), "taxa"))
    print(tax_data)
  }

  # data_allo3 <-
  #   all_individuals_data %>%
  #   dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, locality_name) %>%
  #   filter(!is.na(crown_spread), !is.na(tree_height)) %>%
  #   collect()
  #
  # gg_plot3 <-
  #   ggplot() +
  #   geom_point(data = data_allo3, mapping = aes(x = tree_height, y = crown_spread))

  if(nrow(tax_data)>0) return(list(data_height_dbh=data_allo1, data_crow_dbh=data_allo2, taxa_data=tax_data, plot_height_dbh=gg_plot1, plot_crown_dbh=gg_plot2))
}


#' Herbarium labels
#'
#' Produce rtf file with herbarium specimens labels
#'
#' @return A tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dat data.frame
#' @param theme string
#' @param outfile string
#' @param font string
#' @param font_size integer
herbarium_label <-
  function (dat = NULL, theme = c("GILLES"), outfile = "herblabel.rtf", font = c("Roman", "Arial"),
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




#' Add new plot metadata
#'
#' Add metadata for new plots
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data string
#' @param col_names_select string
#' @param col_names_corresp string
#'
#' @return No return value, new plots are added
#' @export
add_plots <- function(new_data,
                      col_names_select,
                      col_names_corresp) {

  if(!exists("mydb")) .call.mydb()

  new_data_renamed <-
    new_data

  for (i in 1:length(col_names_select)) {
    if(any(colnames(new_data_renamed)==col_names_corresp[i])){
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    }else{
      stop(paste("Columne name provided not found in provided new dataset", col_names_corresp[i]))

    }
  }

  ## Checking dates
  if(any(colnames(new_data_renamed)=="date_y")) if(any(new_data_renamed$date_y>lubridate::year(Sys.Date())) | any(new_data_renamed$date_y<1900)) stop("ERREUR dans date_y, year provided impossible")
  if(any(colnames(new_data_renamed)=="date_m")) if(any(new_data_renamed$date_m>12) | any(new_data_renamed$date_m<1)) stop("ERREUR dans date_m, month provided impossible")
  if(any(colnames(new_data_renamed)=="data_d")) if(any(new_data_renamed$data_d>31) | any(new_data_renamed$data_d<1)) stop("ERREUR dans data_d, day provided impossible")
  if(any(colnames(new_data_renamed)=="ddlon")) if(any(new_data_renamed$ddlon>180) | any(new_data_renamed$ddlon< -180)) stop("ERREUR dans ddlon, longitude provided impossible")
  if(any(colnames(new_data_renamed)=="ddlat")) if(any(new_data_renamed$ddlat>90) | any(new_data_renamed$ddlon< -90)) stop("ERREUR dans ddlat, latitude provided impossible")

  ## Checking if names plot are already in the database
  if(any(colnames(new_data_renamed)=="plot_name")) {
    found_plot <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::filter(plot_name %in% new_data_renamed$plot_name) %>%
      dplyr::collect()

    if(nrow(found_plot)>0) {
      print(found_plot)
      stop("Some plot_name in new data already in the plot list table. No duplicate allowed.")
    }
  }

  ## Checking coordinates
  if(any(new_data_renamed$ddlat>90) | any(new_data_renamed$ddlon< -90)) stop("ERREUR dans ddlat, latitude provided impossible")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(col_names_corresp)

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
               data_modif_m=lubridate::month(Sys.Date()),
               data_modif_y=lubridate::year(Sys.Date()))

  DBI::dbWriteTable(mydb, "data_liste_plots", new_data_renamed, append = TRUE, row.names = FALSE)

}







#' Update plot metadata
#'
#' Update metadata plot _ only one plot at a time
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param team_lead string name of the team leader of the selected plot
#' @param plot_name string plot name of the selected plots
#' @param country string country of the selected plots
#' @param method string method of the selected plots
#' @param date_y integer year of collect of the selected plots
#' @param new_team_leader string new team leader
#' @param new_country string new country
#' @param new_ddlat double new latitude in decimal degrees
#' @param new_ddlon double new longitude in decimal degrees
#' @param new_elevation integer new elevation data
#' @param new_method integer new method data
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value, new plots are added
#' @export
update_plot_data <- function(team_lead=NULL,
                             plot_name=NULL,
                             country=NULL,
                             method=NULL,
                             date_y=NULL,
                             new_team_leader=NULL,
                             new_country=NULL,
                             new_ddlat=NULL,
                             new_ddlon=NULL,
                             new_elevation=NULL,
                             new_method=NULL,
                             add_backup=TRUE){
  if(!exists("mydb")) .call.mydb()

  quer_plots <-
    query_plots(team_lead = team_lead, plot_name = plot_name, country = country, method = method, date_y = date_y)

  if(nrow(quer_plots)==1) {
    new_values <- dplyr::tibble(plot_name=quer_plots$plot_name,
                         team_leader=ifelse(!is.null(new_team_leader), new_team_leader, quer_plots$team_leader),
                         country=ifelse(!is.null(new_country), new_country, quer_plots$country),
                         ddlat=ifelse(!is.null(new_ddlat), new_ddlat, quer_plots$ddlat),
                         ddlon=ifelse(!is.null(new_ddlon), new_ddlon, quer_plots$ddlon),
                         elevation=ifelse(!is.null(new_elevation), new_elevation, quer_plots$elevation),
                         method=ifelse(!is.null(new_method), new_method, quer_plots$method))

    ## trick to include NA values in comparison
    new_values <-
      new_values %>%
      tidyr::replace_na(list(ddlat = -1000, ddlon = -1000, elevation = -1000))

    quer_plots_sel <-
      quer_plots %>%
      dplyr::select(plot_name, method, team_leader, country, ddlat, ddlon, elevation) %>%
      tidyr::replace_na(list(ddlat = -1000, ddlon = -1000, elevation = -1000))

    if(new_values$ddlat==-1000 & quer_plots_sel$ddlat>-1000) new_values$ddlat=quer_plots_sel$ddlat
    if(new_values$ddlon==-1000 & quer_plots_sel$ddlon>-1000) new_values$ddlon=quer_plots_sel$ddlon
    if(new_values$elevation==-1000 & quer_plots_sel$elevation>-1000) new_values$elevation=quer_plots_sel$elevation

    if(new_values$ddlat==-1000 & quer_plots_sel$ddlat==-1000) new_values$ddlat=quer_plots_sel$ddlat=NA
    if(new_values$ddlon==-1000 & quer_plots_sel$ddlon==-1000) new_values$ddlon=quer_plots_sel$ddlon=NA
    if(new_values$elevation==-1000 & quer_plots_sel$elevation==-1000) new_values$elevation=quer_plots_sel$elevation=NA

    comp_values <- new_values != quer_plots_sel
    comp_values <- dplyr::as_tibble(comp_values)
    comp_values <- comp_values %>%
      dplyr::select_if(~sum(!is.na(.)) > 0)

    print(comp_values)

    if(any(unlist(comp_values))) {


      col_sel <- comp_values %>%
        dplyr::select_if(~sum(.) > 0) %>% colnames()
      print("Previous values")
      print(quer_plots %>%
              dplyr::select(!!col_sel))
      print("New values")
      print(new_values %>%
              dplyr::select(!!col_sel) )

      Q <- askYesNo("Confirm these modifications?")

      if(Q) {
        modif_types <-
          paste0(colnames(as.matrix(comp_values))[which(as.matrix(comp_values))], sep="__")


        if(add_backup) {

          colnames_plots <-
            dplyr::tbl(mydb, "followup_updates_liste_plots")  %>% dplyr::select(-date_modified, -modif_type, -id_fol_up_plots) %>%
            dplyr::top_n(1) %>%  dplyr::collect() %>% colnames()

          quer_plots <-
            quer_plots %>%
            dplyr::select(dplyr::one_of(colnames_plots))

          quer_plots <-
            quer_plots %>%
            tibble::add_column(date_modified=Sys.Date()) %>%
            tibble::add_column(modif_type=paste0(modif_types, collapse = ""))

          DBI::dbWriteTable(mydb, "followup_updates_liste_plots", quer_plots, append = TRUE, row.names = FALSE)
        }

        rs <-
          DBI::dbSendQuery(mydb, statement="UPDATE data_liste_plots SET plot_name = $2, method = $3, team_leader = $4, country = $5, ddlat = $6, ddlon = $7, elevation = $8, data_modif_d=$9, data_modif_m=$10, data_modif_y=$11 WHERE id_liste_plots = $1",
                      params=list(quer_plots$id_liste_plots, # $1
                                  new_values$plot_name, # $2
                                  new_values$method, # $3
                                  new_values$team_leader, # $4
                                  new_values$country, # $5
                                  new_values$ddlat, # $6
                                  new_values$ddlon, # $7
                                  new_values$elevation, # $8
                                  lubridate::day(Sys.Date()), # $9
                                  lubridate::month(Sys.Date()), # $10
                                  lubridate::year(Sys.Date()))) # $11

        # if(show_results) print(dbFetch(rs))
        DBI::dbClearResult(rs)

      }



    }else{

      cat("\n no update because no values differents from the entry")
    }
  }else{
    if(nrow(quer_plots)>1) cat("\n More than 1 plot selected. Select only one.")

    if(nrow(quer_plots)==0) cat("\n No plot to be update found.")
  }
}












#' Update individuals data
#'
#' Update individuals plot _ one or more individuals at a time
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data string name of the team leader of the selected plot
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param id_col integer indicate which name of col_names_select is the id for matching data
#' @param launch_update logical if TRUE updates are performed
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value individuals updated
#' @export
update_individuals <- function(new_data,
                               col_names_select,
                               col_names_corresp,
                               id_col,
                               launch_update=FALSE,
                               add_backup=TRUE) {

  if(!exists("mydb")) .call.mydb()

  new_data <-
    new_data %>%
    dplyr::rename_at(dplyr::vars(col_names_select[id_col]), ~ col_names_corresp[id_col])

  new_data_renamed <-
    new_data %>%
    dplyr::rename_at(dplyr::vars(col_names_select[-id_col]), ~ col_names_corresp[-id_col])

  corresponding_data <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::select(col_names_corresp) %>%
    dplyr::filter(id_old %in% new_data_renamed$id_old) %>%
    dplyr::collect()

  all_tb_update <- list()
  for (i in col_names_corresp[-id_col]) {
    cat(" ",i)
    # var <- enquo(col_names_corresp[-id_col][i])
    var <- rlang::enquo(i)

    var_new <- paste0(i, "_new")
    var_old <- paste0(i, "_old")
    id <- col_names_corresp[id_col]
    var_id <- rlang::enquo(id)

    quo_var <- rlang::quo_name(rlang::enquo(id))

    select_col_new <-
      dplyr::select(new_data_renamed, !!var_id, !!var) %>%
      dplyr::rename(!!var_new := !!var)

    select_col_old <-
      dplyr::select(corresponding_data, !!var_id, !!var) %>%
      dplyr::rename(!!var_old := !!var)

    matches <-
      dplyr::left_join(select_col_new, select_col_old)

    quo_var <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_new)))
    quo_var_old <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_old)))

    matches <-
      matches %>%
      dplyr::filter(!!quo_var != !!quo_var_old)

    all_tb_update[[length(all_tb_update)+1]] <- matches

    if(launch_update & nrow(matches)>0) {
      matches <-
        matches %>%
        dplyr::select(!!var_id, !!quo_var) %>%
        tibble::add_column(data_modif_d=rep(lubridate::day(Sys.Date()), nrow(matches)),
                   data_modif_m=rep(lubridate::month(Sys.Date()), nrow(matches)),
                   data_modif_y=rep(lubridate::year(Sys.Date()), nrow(matches)))

      # quo_var_id <- rlang::parse_expr(quo_name(enquo(id)))
      #
      # all_id_match <- pull(dplyr::select(matches, !!var_id))

      #
      # all_rows_to_be_updated <-
      #   all_rows_to_be_updated %>%
      #   add_column(date_modified=Sys.Date())
      #
      # query_p <-
      #   paste0("INSERT INTO followup_updates_individuals VALUES(", paste0(rep("$", ncol(all_rows_to_be_updated)),
      #                                                                     seq(1, ncol(all_rows_to_be_updated), 1), collapse = ", "), ")", collapse = "")
      #
      # cat("\n Starting updating for ",i," column")
      # for (j in 1:nrow(all_rows_to_be_updated)) {
      #   rs <-
      #     dbSendQuery(mydb, statement=query_p, params=as.list(all_rows_to_be_updated %>% slice(j) %>%  unlist(., use.names=FALSE)))
      #   dbClearResult(rs)
      # }


      if(add_backup) {

        all_rows_to_be_updated <-
          dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::filter(!!quo_var_id %in% all_id_match) %>%
          dplyr::collect()

        colnames_plots <-
          dplyr::tbl(mydb, "followup_updates_individuals")  %>% dplyr::select(-date_modified, -modif_type, -id_fol_up_ind) %>%
          dplyr::top_n(1) %>%  dplyr::collect() %>% colnames()

        all_rows_to_be_updated <-
          all_rows_to_be_updated %>%
          dplyr::select(dplyr::one_of(colnames_plots))

        all_rows_to_be_updated <-
          all_rows_to_be_updated %>%
          tibble::add_column(date_modified=Sys.Date()) %>%
          tibble::add_column(modif_type=i)

        print(all_rows_to_be_updated %>% dplyr::select(modif_type, date_modified))

        DBI::dbWriteTable(mydb, "followup_updates_individuals", all_rows_to_be_updated, append = TRUE, row.names = FALSE)
      }

      ## create a temporary table with new data
      DBI::dbWriteTable(mydb, "temp_table", matches, overwrite=T, fileEncoding = "UTF-8", row.names=F)

      query_up <-
        paste0("UPDATE data_individuals t1 SET (",i,", data_modif_d, data_modif_m, data_modif_y) = (t2.",var_new, ", t2.data_modif_d, t2.data_modif_m, t2.data_modif_y) FROM temp_table t2 WHERE t1.",id," = t2.",id)

      rs <-
        DBI::dbSendStatement(mydb, query_up)

      cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
      rs@sql
      DBI::dbClearResult(rs)

    }else{
      if(launch_update & nrow(matches)==0) cat("\n No new values found")
    }
  }

  return(all_tb_update)

}







#' Add new individuals data
#'
#' Add new individuals data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data string name of the team leader of the selected plot
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param id_col integer indicate which name of col_names_select is the id for matching data
#'
#'
#' @return No return value individuals updated
#' @export
add_individuals <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col) {

  if(!exists("mydb")) .call.mydb()

  if(length(col_names_select)!=length(col_names_corresp)) stop("Provide same numbers of corresponding and selected colnames")

  new_data_renamed <-
    new_data %>%
    dplyr::rename(plot_name= !!var)

  ids_plot <-
    new_data_renamed %>%
    dplyr::select(plot_name) %>%
    dplyr::distinct(plot_name) %>%
    dplyr::left_join(tbl(mydb, "data_liste_plots") %>%
                dplyr::select(plot_name, id_liste_plots) %>%
                  dplyr::collect())

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::left_join(ids_plot) %>%
    dplyr::rename(id_table_liste_plots_n=id_liste_plots)


  for (i in 1:length(col_names_select)) {
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
  }

  col_names_corresp <- c(col_names_corresp, "id_table_liste_plots_n")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(col_names_corresp)

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
               data_modif_m=lubridate::month(Sys.Date()),
               data_modif_y=lubridate::year(Sys.Date()))

  print(new_data_renamed)

  DBI::dbWriteTable(mydb, "data_individuals", new_data_renamed, append = TRUE, row.names = FALSE)

}






#' Update taxonomic data
#'
#' Update taxonomic data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param genus_searched string genus name searched
#' @param tax_esp_searched string species name searched
#' @param tax_fam_searched string family name searched
#' @param new_tax_gen string new genus name
#' @param new_tax_esp string new species name
#' @param new_full_name_auth string new full name with authors
#' @param new_tax_fam string new family name
#' @param new_taxook integer new tax code
#' @param new_detvalue integer new detvalue code
#' @param new_full_name_no_auth string new full name without authors - if not provided concatenate of new_esp and new_genus
#' @param new_full_name_used string new full_name_used
#' @param new_full_name_used2 string new full_name_used2
#' @param new_id_diconame_good integer if the selected name should be put in synonymy, id of the taxa
#' @param id_search integer id of the taxa searched
#' @param ask_before_update logical TRUE by default, ask for confirmation before updating
#' @param add_backup logical TRUE by default, add backup of modified data
#' @param show_results logical TRUE by default, show the data that has been modified
#' @param no_synonym_modif logical FALSE by default, if TRUE and if the selected taxa is considered as synonym, then this will be modified and the selected taxa will not longer be a synonym
#' @param synonym_of list if the selected taxa should be put in synonymy with an existing taxa, add in a list at least one values to identify to which taxa it will be put in synonymy: genus, species or id
#'
#'
#' @return No return value individuals updated
#' @export
update_dico_name <- function(genus_searched=NULL, tax_esp_searched=NULL, tax_fam_searched=NULL,
                             new_tax_gen=NULL,
                             new_tax_esp=NULL,
                             new_full_name_auth=NULL,
                             new_tax_fam=NULL,
                             new_taxook=NULL,
                             new_detvalue=NULL,
                             new_full_name_no_auth=NULL,
                             new_full_name_used=NULL,
                             new_full_name_used2=NULL,
                             new_id_diconame_good=NULL,
                             id_search=NULL,
                             ask_before_update=TRUE,
                             add_backup=TRUE,
                             show_results=TRUE,
                             no_synonym_modif=FALSE,
                             synonym_of = NULL) {

  if(!exists("mydb")) .call.mydb()

  if(all(is.null(c(genus_searched, tax_esp_searched, tax_fam_searched, synonym_of, id_search, new_full_name_no_auth, new_full_name_used, new_full_name_used2))) & !no_synonym_modif) stop("\n Provide the species to be updated or precise new synonymy")

  ### checking if at least one modification is asked
  new_vals <- c(new_tax_gen, new_tax_esp, new_full_name_auth, new_tax_fam, new_taxook,new_detvalue, new_id_diconame_good, no_synonym_modif, new_full_name_no_auth, new_full_name_used, new_full_name_used2)
  if(!any(!is.null(new_vals)) & is.null(synonym_of)) stop("\n No new values to be updated.")

  ## if the modif is a change in synonymy, show synonyms
  if(no_synonym_modif | !is.null(synonym_of)) {
    show_synonymies <- TRUE
  }else{
    show_synonymies <- FALSE
  }

  ### querying for entries to be modified
  if(is.null(id_search)) {
    query_tax <-
      query_tax_all(genus_searched = genus_searched, tax_esp_searched = tax_esp_searched, tax_fam_searched = tax_fam_searched, show_synonymies=show_synonymies)
  }else{
    query_tax <-
      query_tax_all(id_search = id_search, show_synonymies = FALSE)
  }

  if(nrow(query_tax)>0) {
    nrow_query=TRUE
  }else{
    nrow_query=FALSE
  }

  if(nrow_query) modif_types <-
    vector(mode = "character", length = nrow(query_tax))

  ## if the modification does not concern synonymies, check if provided values are different for those existing
  if(nrow_query & !no_synonym_modif & is.null(synonym_of)) {
    new_vals <- dplyr::tibble(tax_fam=ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam),
                       tax_gen=ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen),
                       tax_esp=ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp), # $4
                       taxook=ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), # $5
                       full_name=ifelse(!is.null(new_full_name_auth), new_full_name_auth, query_tax$full_name), # $6
                       detvalue=ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), # $7
                       id_good_n=ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n),
                       full_name_no_auth=ifelse(!is.null(new_full_name_no_auth), new_full_name_no_auth, query_tax$full_name_no_auth),
                       full_name_used=ifelse(!is.null(new_full_name_used), new_full_name_used, query_tax$full_name_used),
                       full_name_used2=ifelse(!is.null(new_full_name_used2), new_full_name_used2, query_tax$full_name_used2))

    # comp_vals <-
    #   query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, taxook, full_name, detvalue, id_good_n) != new_vals

    sel_query_tax <-
      dplyr::bind_rows(new_vals, query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, taxook, full_name, detvalue, id_good_n, full_name_no_auth, full_name_used, full_name_used2))

    comp_vals <-
      apply(sel_query_tax, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])

    if(!is.null(nrow(comp_vals))) {
      query_tax <-
        query_tax[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
      comp_vals <-
        apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
    }else{
      query_tax <- query_tax
    }

    if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]

    modif_types[1:length(modif_types)] <-
      paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")

  }else{
    comp_vals <- TRUE
  }

  if(nrow_query & no_synonym_modif) {
    if(query_tax$id_good_n==query_tax$id_n) {
      cat("\n This taxa is not considered as synonym. No modification is thus done on its synonymy")
      comp_vals <- FALSE
    }else{
      new_id_diconame_good <- query_tax$id_n

      modif_types[1:length(modif_types)] <-
        paste(modif_types, "cancel_synonymy", sep="__")

    }
  }

  if(nrow_query & !is.null(synonym_of)) {
    Q.syn <- TRUE
    if(query_tax$id_good_n!=query_tax$id_n) {
      query_tax_all(id_search = query_tax$id_good_n, show_synonymies = FALSE)
      Q.syn <-
        askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
    }

    if(Q.syn) {

      if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
      if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
      if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL

      new_syn <-
        query_tax_all(genus_searched = synonym_of$genus, tax_esp_searched = synonym_of$species, id_search = synonym_of$id, show_synonymies = FALSE)

      if(nrow(new_syn)==0) {
        cat("\n No taxa found for new synonymy. Select one.")
        Q.syn <- FALSE
      }

      if(nrow(new_syn)>1) {
        cat("\n More than one taxa found for new synonymy. Select only one.")
        Q.syn <- FALSE
      }

      if(nrow(new_syn)==1) {

        new_id_diconame_good <- new_syn$id_n

        modif_types[1:length(modif_types)] <-
          paste(modif_types, "new_synonymy", sep="__")

      }
    }
  }else{
    Q.syn <- TRUE
  }

  # if(!any(comp_vals)) stop("No update performed because no values are different.")

  if(any(comp_vals) & Q.syn & nrow_query) {

    cat(paste("\n Number of rows selected to be updated :", nrow(query_tax), "\n"))

    if(ask_before_update) {
      Q <-
        askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
    }else{
      Q <- TRUE
    }

    if(Q) {

      if(add_backup) {

        query_tax <-
          query_tax %>%
          tibble::add_column(date_modified=Sys.Date()) %>%
          tibble::add_column(modif_type=modif_types)


        DBI::dbWriteTable(mydb, "followup_updates_diconames", query_tax, append = TRUE, row.names = FALSE)

        # query_p <-
        #     paste0("INSERT INTO followup_updates_diconames VALUES(", paste0(rep("$", ncol(query_tax)), seq(1, ncol(query_tax), 1), collapse = ", "), ")",
        #            collapse = "")

        # rs <-
        #     dbSendQuery(mydb, statement=query_p, params=as.list(query_tax %>% slice(1) %>%  unlist(., use.names=FALSE)))
        #
        # dbClearResult(rs)
      }

      # tbl(mydb, "followup_updates_diconames")

      rs <-
        DBI::dbSendQuery(mydb, statement="UPDATE diconame SET tax_fam=$2, tax_gen=$3, tax_esp=$4, taxook=$5, full_name=$6, detvalue=$7, id_good_n=$8, full_name_no_auth=$9, full_name_used=$10, full_name_used2=$11 WHERE id_n = $1",
                    params=list(query_tax$id_n, # $1
                                rep(ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam), nrow(query_tax)), # $2
                                rep(ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen), nrow(query_tax)), # $3
                                rep(ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp), nrow(query_tax)), # $4
                                rep(ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), nrow(query_tax)), # $5
                                rep(ifelse(!is.null(new_full_name_auth), new_full_name_auth, query_tax$full_name), nrow(query_tax)), # $6
                                rep(ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), nrow(query_tax)), # $7
                                rep(ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n), nrow(query_tax)), # $8
                                rep(ifelse(!is.null(new_full_name_no_auth), new_full_name_no_auth, query_tax$full_name_no_auth), nrow(query_tax)), # $9
                                rep(ifelse(!is.null(new_full_name_used), new_full_name_used, query_tax$full_name_used), nrow(query_tax)), # $10
                                rep(ifelse(!is.null(new_full_name_used2), new_full_name_used2, query_tax$full_name_used2), nrow(query_tax)))) # $11
      DBI::dbClearResult(rs)

      rs <-
        DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
                    params=list(query_tax$id_n))
      if(show_results) print(dbFetch(rs))
      DBI::dbClearResult(rs)

    }
  }else{
    if(nrow(query_tax)==0) print("No update because no taxa found.")

    if(!any(comp_vals)) print("No update performed because no values are different.")

    if(!Q.syn) print("No update because new synonymy not correctly defined.")

    if(!nrow_query) print("No updates because none taxa were found based on query parameters (genus/species/family/id)")

  }

  # dbDisconnect(mydb)

}



#' Get backups of modified taxonomic data
#'
#' List taxonomic data that has been modified
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id look backups for a specific id (of taxonomic table)
#' @param last_months look backups performed this last month
#' @param last_10_entry look the last 10 backups performed
#' @param last look the last backup
#'
#' @return A tibble
#' @export
get_updates_diconame <- function(id=NULL,
                                 last_months=NULL,
                                 last_10_entry=TRUE,
                                 last=NULL) {

  if(!exists("mydb")) .call.mydb()

  tb <-
    dplyr::tbl(mydb, "followup_updates_diconames")

  if(!is.null(id) & !last_10_entry) {
    var <- rlang::enquo(id)
    entry <-
      dplyr::tbl %>%
      dplyr::filter(id_n==!!var) %>%
      dplyr::collect()
  }

  if(!is.null(last_months) & !last_10_entry) {

    one_month_earlier <-
      lubridate::month(Sys.Date() %m-% months(last_months))
    if(one_month_earlier<10) one_month_earlier <- paste0("0", one_month_earlier)
    one_month_earlier <-
      paste(lubridate::year(Sys.Date()), one_month_earlier, sep="-")

    this_month <-
      lubridate::month(Sys.Date())
    if(this_month<10) this_month <- paste0("0", this_month)
    this_month <-
      paste(lubridate::year(Sys.Date()), this_month, sep="-")

    query <-
      paste0("SELECT * FROM followup_updates_diconames WHERE date_modified ILIKE '%",one_month_earlier,"%' OR date_modified ILIKE '%", this_month, "%'")

    rs <- DBI::dbSendQuery(mydb, query)
    entry <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    entry <- dplyr::as_tibble(entry)
  }

  if(last_10_entry) {
    max_id<-
      dplyr::tbl %>%
      dplyr::arrange(dplyr::desc(id_fol_up_diconame)) %>%
      dplyr::select(id_fol_up_diconame) %>%
      dplyr::top_n(1) %>%
      dplyr::collect()

    if(is.null(last)) {
      last_10 <- (dplyr::pull(max_id)-10)
    }else{
      last_10 <- (dplyr::pull(max_id)-last)
    }

    entry <-
      dplyr::tbl %>%
      dplyr::filter(id_fol_up_diconame > last_10) %>%
      dplyr::collect()

  }

  return(entry)
}


#' Add new entry to taxonomic table
#'
#' Add new entry to taxonomic table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param tax_gen string genus name
#' @param tax_esp string species name
#' @param tax_fam string family name
#' @param detvalue integer detvalue code
#' @param morphocat integer morphocat code
#' @param full_name string full name : genus + species + authors
#' @param synonym_of list if the new entry should be put in synonymy with an existing taxa, add in a list at least one values to identify to which taxa it will be put in synonymy: genus, species or id
#'
#'
#' @return A tibble
#' @export
add_entry_dico_name <- function(tax_gen=NULL,
                                tax_esp = NULL,
                                tax_fam = NULL,
                                detvalue = NULL,
                                morphocat = NULL,
                                full_name = NULL,
                                synonym_of = NULL) {
  if(!exists("mydb")) .call.mydb()

  if(is.null(full_name) & !is.null(tax_esp)) stop("Provide full name with authors")

  if(is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam)) stop("Provide at least one genus/family new name to enter")

  if(!is.null(tax_fam) & is.null(full_name) & is.null(tax_gen)) {
    full_name <- tax_fam
  }

  if(is.null(detvalue)) {
    if(is.null(tax_esp) & !is.null(tax_gen)) detvalue <- 6
    if(is.null(tax_esp) & is.null(tax_gen)) detvalue <- 7

    if(!is.null(tax_esp) & !is.null(tax_gen)) detvalue <- 1

    cat(paste("\n No detvalue provided, by default, the following value is given:", detvalue))
  }

  if(is.null(morphocat)) {
    if(is.null(tax_esp) & !is.null(tax_gen)) morphocat <- 3
    if(is.null(tax_esp) & is.null(tax_gen)) morphocat <- 3

    if(!is.null(tax_esp) & !is.null(tax_gen)) morphocat <- 1

    cat(paste("\n No morphocat provided, by default, the following value is given:", morphocat))
  }

  check_taxo <- TRUE

  if(is.null(tax_fam)) {
    tax_fam <- query_tax_all(genus_searched = tax_gen) %>% dplyr::distinct(tax_fam) %>% dplyr::pull()
    tax_fam <- tax_fam[which(!is.na(tax_fam))]
    if(length(tax_fam)>1) cat(paste("\n No tax_fam provided, and two different family names for this genus", paste0(tax_fam, sep=", ")))
    if(length(tax_fam)>1) check_taxo <- FALSE
    if(length(tax_fam)==1) cat(paste("\n No tax_fam provided, based on genus, the following family is chosen:", tax_fam))
  }

  tax_fam_new <- TRUE
  if(!is.null(tax_fam)) {
    searched_tax_fam <-
      dplyr::tbl(mydb, "diconame") %>% dplyr::distinct(tax_fam) %>% dplyr::filter(tax_fam==!!tax_fam) %>% dplyr::collect()
    if(nrow(searched_tax_fam)==0) {
      tax_fam_new <-
        askYesNo(msg = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled?", default = FALSE)
    }
  }


  if(!is.null(full_name) & !is.null(tax_gen)) {
    if(!grepl(tax_gen, full_name)) stop("\n Genus and full name are provided, but genus is not found within full name, there must be an ERROR")
  }

  if(!is.null(full_name) & !is.null(tax_esp)) {
    if(!grepl(tax_esp, full_name)) stop("\n Species and full name are provided, but Species is not found within full name, there must be an ERROR")
  }

  if(is.null(tax_gen) & !is.null(tax_esp)) {
    stop("\n Genus provided but no species epithet (provide tax_gen)")
  }

  if(!is.null(tax_gen)) {
    family_check <-
      query_tax_all(tax_fam_searched = tax_fam)
    genus_check <-
      query_tax_all(genus_searched =  tax_gen)
    if(nrow(genus_check)>0 & !any(family_check$tax_gen==tax_gen)) {
      cat(paste("\n The provided genus is present in the dictionnary, but with different family name:", genus_check$tax_fam[1]))
      check_taxo <- FALSE
    }
  }

  # tbl(mydb, "diconame") %>% collect() %>% slice(n())

  if(check_taxo & tax_fam_new) {
    if(!is.null(tax_gen) & !is.null(tax_esp)) paste_taxa <- paste(tax_gen, tax_esp)
    if(!is.null(tax_gen) & is.null(tax_esp)) paste_taxa <- tax_gen
    if(!is.null(tax_fam) & is.null(tax_gen)) paste_taxa <- tax_fam
    if(is.null(full_name) & !is.null(tax_gen) & is.null(tax_esp)) full_name <- tax_gen

    if(is.null(tax_esp)) tax_esp <- NA
    if(is.null(tax_gen)) tax_gen <- NA

    new_rec <- dplyr::tibble(id=0, id_good=0, full_name=full_name, full_name_no_auth=paste_taxa, full_name_used=paste_taxa,
                      full_name_used2=paste_taxa, tax_fam=tax_fam, tax_gen=tax_gen, tax_esp=tax_esp, taxook=1, detvalue=detvalue,
                      morphocat=morphocat, id_good_n=0, data_modif_d=lubridate::day(Sys.Date()), data_modif_m=lubridate::month(Sys.Date()),
                      data_modif_y=lubridate::year(Sys.Date()))

    DBI::dbWriteTable(mydb, "diconame", new_rec, append = TRUE, row.names = FALSE)

    new_entry <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::filter(id_good_n==0, data_modif_d==lubridate::day(Sys.Date()), data_modif_m==lubridate::month(Sys.Date()), data_modif_y==lubridate::year(Sys.Date())) %>%
      dplyr::collect()

    if(!is.null(synonym_of)) {
      syn_searched <-
        query_tax_all(genus_searched = synonym_of[[1]], tax_esp_searched = synonym_of[[2]])

      print(syn_searched)
      if(nrow(syn_searched)>1) stop("More than 1 taxa as synonym. Select only one.")
      if(nrow(syn_searched)==0) stop("No taxa found in the dictionnary. Select one.")

      update_dico_name(new_id_diconame_good = syn_searched$id_good_n, id_search = new_entry$id_n,
                       ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)

    }else{
      update_dico_name(new_id_diconame_good = new_entry$id_n, id_search = new_entry$id_n,
                       ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)
    }
    print(dplyr::tbl(mydb, "diconame") %>% dplyr::collect() %>% dplyr::slice(n()))
  }else{

    cat("\n NO ADDED ENTRY")
  }

}


#' Delete an entry in taxonomic table
#'
#' Delete an entry in taxonomic table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
delete_entry_dico_name <- function(id) {

  if(!exists("mydb")) .call.mydb()

  DBI::dbExecute(mydb,
            "DELETE FROM diconame WHERE id_n=$1", params=list(id)
  )
}




#' Exploring specimens data
#'
#' Exploring specimens data and if necessary export labels
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param collector string collector name
#' @param number integer specimen number
#' @param number_min integer minimum specimen number
#' @param number_max integer maximum specimen number
#' @param genus_searched string specimens of genus searched
#' @param tax_esp_searched string specimens of species searched
#' @param tax_fam_searched string specimens of family searched
#' @param id_search integer id searched
#' @param subset_columns logical if only a subset of columns should be provided
#' @param show_previous_modif logical if you want to see previous modification of the entry - useful to see previous identification for example
#' @param generate_labels logical if labels should be produced
#' @param project_title string if labels are produced title of the label
#' @param file_labels string if labels are produced name of the rtf file
#'
#' @return A tibble
#' @export
query_specimens <- function(collector=NULL,
                            number=NULL,
                            number_min=NULL,
                            number_max=NULL,
                            genus_searched=NULL,
                            tax_esp_searched=NULL,
                            tax_fam_searched=NULL,
                            id_search=NULL,
                            subset_columns=TRUE,
                            show_previous_modif=TRUE,
                            generate_labels=FALSE,
                            project_title="Reference specimens collected in trees inventory",
                            file_labels="labels") {

  if(!exists("mydb")) .call.mydb()

  query_speci <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::left_join(tbl(mydb, "diconame") %>% dplyr::select(-detvalue, -data_modif_d, -data_modif_m, -data_modif_y), by=c("id_diconame_n"="id_n")) %>%
    dplyr::left_join(tbl(mydb, "table_colnam"), by=c("id_colnam"="id_table_colnam"))
  # %>%
  #   dplyr::select(-id_specimen_old, -id_diconame, -photo_tranche, -id_colnam, -id_good, -id, -id_good_n)


  if(subset_columns & !generate_labels)
    query_speci <-
    query_speci %>%
    dplyr::select(colnam, colnbr, suffix, full_name, tax_fam, tax_gen, tax_esp, ddlat, ddlon, country, detby, detd, detm, dety, add_col,
                  cold, colm, coly, detvalue, id_specimen, id_diconame_n)

  if(!is.null(collector) & is.null(id_search)) {

    var <- rlang::enquo(collector)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, colnam))
  }

  if(!is.null(number) & is.null(id_search)) {

    var <- rlang::enquo(number)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr==var)
  }

  if(!is.null(number_min) & is.null(id_search)) {

    var <- rlang::enquo(number_min)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr>=var)
  }

  if(!is.null(number_max) & is.null(id_search)) {

    var <- rlang::enquo(number_max)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr<=var)
  }

  if(!is.null(genus_searched) & is.null(id_search)) {

    var <- rlang::enquo(genus_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_gen))
  }

  if(!is.null(tax_fam_searched) & is.null(id_search)) {

    var <- rlang::enquo(tax_fam_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_fam))
  }

  if(!is.null(tax_esp_searched) & is.null(id_search)) {

    var <- rlang::enquo(tax_esp_searched)

    query_speci <-
      query_speci %>%
      dplyr::filter(grepl(!!var, tax_esp))
  }

  if(!is.null(id_search)) {

    var <- rlang::enquo(id_search)

    query_speci <-
      query_speci %>%
      dplyr::filter(id_specimen==var)
  }

  query <-
    query_speci %>%
    dplyr::collect()

  # print(query)


  if(nrow(query)==1 & show_previous_modif) {
    modif_backups <-
      dplyr::tbl(mydb, "followup_updates_specimens") %>%
      dplyr::filter(id_specimen==query$id_specimen) %>%
      dplyr::filter(grepl("id_good_diconame", modif_type)) %>%
      dplyr::collect()

    if(nrow(modif_backups)>0) {
      modif_backups <-
        dplyr::tbl(mydb, "followup_updates_specimens") %>%
        dplyr::filter(id_specimen==query$id_specimen) %>%
        dplyr::filter(grepl("id_good_diconame", modif_type)) %>%
        dplyr::left_join(tbl(mydb, "diconame"), by=c("id_diconame_n"="id_n")) %>%
        dplyr::select(colnam, colnbr, suffix, full_name, tax_fam, tax_gen, tax_esp, ddlat, ddlon, country, detby, detd, detm, dety, add_col,
                      cold, colm, coly, detvalue.x, id_specimen)
      cat("\n Previous modification in identification")
      print(modif_backups)
    }else{
      cat("\n No identification change in backups")
    }
  }

  if(generate_labels) {

    lat_convert <-
      measurements::conv_unit(query$ddlat, from = "dec_deg", to = "deg_min_sec")
    lat_convert_deg <-
      as.double(unlist(lapply(strsplit(lat_convert, " "), function(x) x[[1]])))
    lat_flag <-
      ifelse(lat_convert_deg>0, "N", "S")
    lat_convert_deg <-
      abs(lat_convert_deg)
    lat_convert_min <-
      unlist(lapply(strsplit(lat_convert, " "), function(x) ifelse(length(x)>1, x[[2]], NA)))
    lat_convert_sec <-
      unlist(lapply(strsplit(lat_convert, " "), function(x) ifelse(length(x)>1, x[[3]], NA)))

    long_convert <-
      measurements::conv_unit(query$ddlon, from = "dec_deg", to = "deg_min_sec")
    long_convert_deg <-
      as.double(unlist(lapply(strsplit(long_convert, " "), function(x) x[[1]])))
    long_flag <-
      ifelse(lat_convert_deg>0, "E", "W")
    long_convert_deg <-
      abs(long_convert_deg)
    long_convert_min <-
      unlist(lapply(strsplit(long_convert, " "), function(x) ifelse(length(x)>1, x[[2]], NA)))
    long_convert_sec <-
      unlist(lapply(strsplit(long_convert, " "), function(x) ifelse(length(x)>1, x[[3]], NA)))

    query_labels <-
      query %>%
      tibble::add_column(INSTITUTION_CODE=rep("BRLU", nrow(.)),
                 HERBARIUM=rep("BRLU", nrow(.)),
                 TITLE=rep(project_title, nrow(.)),
                 AUTHOR_OF_SPECIES=NA,
                 INFRASPECIFIC_RANK=NA,
                 INFRASPECIFIC_EPITHET=NA,
                 AUTHOR_OF_INFRASPECIFIC_RANK=NA,
                 COUNTY=NA,
                 IMAGE_URL=NA,
                 RELATED_INFORMATION=NA,
                 LAT_DEGREE=as.double(lat_convert_deg),
                 LAT_MINUTE=as.double(lat_convert_min),
                 LAT_SECOND=as.double(lat_convert_sec),
                 LON_DEGREE=as.double(long_convert_deg),
                 LON_MINUTE=as.double(long_convert_min),
                 LON_SECOND=as.double(long_convert_sec),
                 LAT_FLAG=lat_flag,
                 LON_FLAG=long_flag,
                 REMARKS=NA,
                 GEOREFERENCE_SOURCES=NA,
                 PROJECT=NA,
                 TYPE_STATUS=NA,
                 PROCESSED_BY=NA,
                 LOCAL_NAME=NA) %>%
      dplyr::rename(GLOBAL_UNIQUE_IDENTIFIER=id_specimen,
             COLLECTION_CODE=colnbr,
             COLLECTOR=colnam,
             ADDITIONAL_COLLECTOR=add_col,
             COLLECTOR_NUMBER=specimens_code,
             FAMILY=tax_fam,
             GENUS=tax_gen,
             SPECIES=tax_esp,
             COUNTRY=country,
             STATE_PROVINCE=majorarea,
             LOCALITY=locality,
             ELEVATION=elevation,
             ATTRIBUTES=description,
             IDENTIFIED_BY=detby,
             FULL_NAME=full_name) %>%
      dplyr::mutate(coly=ifelse(is.na(coly) | coly==0, "", coly),
             colm=ifelse(is.na(colm) | colm==0, "", colm),
             cold=ifelse(is.na(cold) | cold==0, "", cold)) %>%
      dplyr::mutate(DATE_COLLECTED=paste(coly, colm, cold, sep="-"),
             DATE_IDENTIFIED=paste(ifelse(is.na(dety) | dety==0, "", dety),
                                   ifelse(is.na(detm) | detm==0, "", detm),
                                   ifelse(is.na(detd) | detd==0, "", detd), sep="-"),
             DATE_LASTMODIFIED=paste(data_modif_y, data_modif_m, data_modif_d , sep="-")) %>%
      dplyr::select(INSTITUTION_CODE,
                    HERBARIUM,
                    TITLE,
                    AUTHOR_OF_SPECIES,
                    INFRASPECIFIC_RANK,
                    INFRASPECIFIC_EPITHET,
                    AUTHOR_OF_INFRASPECIFIC_RANK,
                    FULL_NAME,
                    COUNTY,
                    IMAGE_URL,
                    RELATED_INFORMATION,
                    LAT_DEGREE,
                    LAT_MINUTE,
                    LAT_SECOND,
                    LON_DEGREE,
                    LON_MINUTE,
                    LON_SECOND,
                    LAT_FLAG,
                    LON_FLAG,
                    REMARKS,
                    GEOREFERENCE_SOURCES,
                    PROJECT,
                    TYPE_STATUS,
                    PROCESSED_BY,
                    GLOBAL_UNIQUE_IDENTIFIER,
                    COLLECTION_CODE,
                    COLLECTOR,
                    ADDITIONAL_COLLECTOR,
                    COLLECTOR_NUMBER,
                    FAMILY,
                    GENUS,
                    SPECIES,
                    LOCAL_NAME,
                    COUNTRY,
                    STATE_PROVINCE,
                    LOCALITY,
                    ELEVATION,
                    ATTRIBUTES,
                    IDENTIFIED_BY,
                    DATE_COLLECTED,
                    DATE_IDENTIFIED,
                    DATE_LASTMODIFIED)

    herbarium_label(dat = query_labels,
                    theme="GILLES", outfile = paste0(file_labels, ".rtf"))

  }



  return(query)
}







#' Exploring specimens data
#'
#' Exploring specimens data and if necessary export labels
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param colnam string collector name
#' @param number integer specimen number
#' @param id_speci integer id of specimen
#' @param new_genus string new genus name
#' @param new_species string new species name
#' @param new_family string new family name
#' @param id_new_taxa integer id of the new taxa
#' @param new_detd integer day of identification
#' @param new_detm logical if you want to see previous modification of the entry - useful to see previous identification for example
#' @param new_dety logical if labels should be produced
#' @param new_detby string if labels are produced title of the label
#' @param new_detvalue string if labels are produced name of the rtf file
#' @param add_backup string if labels are produced name of the rtf file
#' @param show_results string if labels are produced name of the rtf file
#' @param only_new_ident string if labels are produced name of the rtf file
#'
#' @return A tibble
#' @export
update_ident_specimens <- function(colnam=NULL,
                                   number=NULL,
                                   id_speci=NULL,
                                   new_genus=NULL,
                                   new_species=NULL,
                                   new_family=NULL,
                                   id_new_taxa=NULL,
                                   new_detd=NULL,
                                   new_detm=NULL,
                                   new_dety=NULL,
                                   new_detby=NULL,
                                   new_detvalue=NULL,
                                   add_backup=TRUE,
                                   show_results=TRUE,
                                   only_new_ident=T) {
  if(!exists("mydb")) .call.mydb()

  if(is.null(id_speci)) {
    queried_speci <-
      query_specimens(collector = collector, number = number, subset_columns = FALSE)
  }else{
    queried_speci <-
      query_specimens(id_search = id_speci, subset_columns = FALSE)
  }


  print(queried_speci %>% dplyr::select(family_name, surname, colnbr, detd, detm, dety, detby, cold, colm, coly, country, id_specimen))

  if(nrow(queried_speci)==1) {
    nbr_queried_speci_ok <- TRUE
  }else{
    nbr_queried_speci_ok <- FALSE
  }

  if(nbr_queried_speci_ok)
    modif_types <- vector(mode = "character", length = nrow(queried_speci))

  if(is.null(id_new_taxa)) {
    query_new_taxa <- query_tax_all(genus_searched = new_genus, tax_esp_searched = new_species, tax_fam_searched = new_family, show_synonymies = F)
  }else{
    query_new_taxa <- query_tax_all(id_search = id_new_taxa, show_synonymies = F)
  }

  if(nrow(query_new_taxa)==1) {
    nbr_new_taxa_ok <- TRUE
  }else{
    nbr_new_taxa_ok <- FALSE
  }

  if(nbr_new_taxa_ok & nbr_queried_speci_ok) {
    new_values <-
      dplyr::tibble(id_good_diconame=ifelse(!is.null(new_genus) | !is.null(new_species) | !is.null(new_family | !is.null(id_new_taxa)), query_new_taxa$id_n, queried_speci$id_diconame_n),
             detd=ifelse(!is.null(new_detd), as.numeric(new_detd), queried_speci$detd),
             detm=ifelse(!is.null(new_detm), as.numeric(new_detm), queried_speci$detm),
             dety=ifelse(!is.null(new_dety), as.numeric(new_dety), queried_speci$dety),
             detby=ifelse(!is.null(new_detby), new_detby, queried_speci$detby),
             detvalue=ifelse(!is.null(new_detvalue), new_detvalue, queried_speci$detvalue)
      )

    if(new_values$detby=="NA") new_values$detby=NA

    new_values <-
      new_values %>%
      tidyr::replace_na(list(detvalue.x = 0, detd = 0, detm = 0, dety = 0, detby=0))

    query_select <-
      queried_speci %>%
      dplyr::select(id_diconame_n, detd, detm, dety, detby, detvalue) %>%
      tidyr::replace_na(list(detvalue.x =0, detd = 0, detm = 0, dety = 0, detby=0))

    if(new_values$detd==0 & query_select$detd>0) new_values$detd=query_select$detd
    if(new_values$detm==0 & query_select$detm>0) new_values$detm=query_select$detm
    if(new_values$dety==0 & query_select$dety>0) new_values$dety=query_select$dety
    if(new_values$detby==0 & query_select$detby>0) new_values$detby=query_select$detby

    if(new_values$detd==0 & query_select$detd==0) new_values$detd=query_select$detd=NA
    if(new_values$detm==0 & query_select$detm==0) new_values$detm=query_select$detm=NA
    if(new_values$dety==0 & query_select$dety==0) new_values$dety=query_select$dety=NA
    if(new_values$detby==0 & query_select$detby==0) new_values$detby=query_select$detby=NA

    comp_values <- new_values != query_select
    comp_values <- dplyr::as_tibble(comp_values)
    comp_values <- comp_values %>% dplyr::select_if(~sum(!is.na(.)) > 0)
  }else{
    comp_values <- TRUE
  }

  if(only_new_ident & nbr_new_taxa_ok) {
    if(any(comp_values %>% select_if(~sum(.) > 0) %>% colnames()=="id_good_diconame")) {
      new_ident <- TRUE
    }else{
      new_ident <- FALSE
    }

  }else{
    new_ident <- TRUE
  }

  if(nbr_new_taxa_ok & any(comp_values==TRUE) & nbr_queried_speci_ok & new_ident) {

    modif_types <-
      paste0(colnames(as.matrix(comp_values))[which(as.matrix(comp_values))], sep="__")

    # if(comp_values$id_good_diconame & any(new_values %>% dplyr::select(detd, detm, dety)==0)) {
    #   if(new_values$detd==0) new_values$detd <- lubridate::day(Sys.Date())
    #   if(new_values$detm==0) new_values$detm <- lubridate::month(Sys.Date())
    #   if(new_values$dety==0) new_values$dety <- lubridate::year(Sys.Date())
    # }

    # print(modif_types)
    col_sel <- comp_values %>% dplyr::select_if(~sum(.) > 0) %>% colnames()
    print(new_values %>% dplyr::select(!!col_sel) )
    print(query_tax_all(id_search = queried_speci$id_diconame_n, show_synonymies = F) %>% dplyr::select(-id, -id_good, -full_name_used, -full_name_used2))
    print(query_new_taxa %>% dplyr::select(-id, -id_good, -full_name_used, -full_name_used2))

    confirmed <- askYesNo("Confirm this update?")

    if(confirmed) {
      if(add_backup) {

        colnames_speci <-
          dplyr::tbl(mydb, "followup_updates_specimens")  %>% dplyr::select(-date_modified, -modif_type, -id_fol_up_specimens) %>%
          dplyr::top_n(1) %>%  dplyr::collect() %>% colnames()

        queried_speci <-
          queried_speci %>%
          dplyr::select(one_of(colnames_speci))

        queried_speci <-
          queried_speci %>%
          tibble::add_column(date_modified=Sys.Date()) %>%
          tibble::add_column(modif_type=paste0(modif_types, collapse = ""))

        DBI::dbWriteTable(mydb, "followup_updates_specimens", queried_speci, append = TRUE, row.names = FALSE)
      }

      rs <-
        DBI::dbSendQuery(mydb, statement="UPDATE specimens SET id_diconame_n=$2, detd=$3, detm=$4, dety=$5, detby=$6, detvalue=$7, data_modif_d=$8, data_modif_m=$9, data_modif_y=$10 WHERE id_specimen = $1",
                    params=list(queried_speci$id_specimen, # $1
                                new_values$id_good_diconame, # $2
                                as.numeric(new_values$detd), # $3
                                as.numeric(new_values$detm), # $4
                                as.numeric(new_values$dety), # $5
                                new_values$detby, # $6
                                new_values$detvalue, # $7
                                lubridate::day(Sys.Date()), # $8
                                lubridate::month(Sys.Date()), # $9
                                lubridate::year(Sys.Date()))) # $10

      # if(show_results) print(dbFetch(rs))
      DBI::dbClearResult(rs)

      if(show_results) query_specimens(id_search = queried_speci$id_specimen)
    }

  }else{
    if(!nbr_new_taxa_ok) cat("\n NO UPDATE. The number of taxa selected for new identification is ", nrow(query_new_taxa),". Select one taxa.")
    if(!any(comp_values==TRUE)) cat("\n NO UPDATE. No different values entered.")
    if(!nbr_queried_speci_ok) cat("\n NO UPDATE. The number of specimen selected is ",nrow(queried_speci),". Select ONE specimen. Not less, not more.")
    if(!new_ident) cat("\n No new identification")
  }


}



