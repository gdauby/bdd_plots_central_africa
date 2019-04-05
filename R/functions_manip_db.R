
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
launch_query_tax_app <- function() {

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


#' Internal function
#'
#' Looking for similar people name
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param input string
#' @param compared_table tibble
#' @param column_name string
#'
#' @import shiny
#' @export
.find_similar_string <- function(input, compared_table, column_name){
  dist. <-
    RecordLinkage::levenshteinSim(tolower(input),
                                  tolower(compared_table %>%
                                            dplyr::select(!!column_name) %>%
                                            dplyr::pull()))

  arranged_values <-
    compared_table %>%
    tibble::add_column(dist=dist.) %>%
    dplyr::arrange(dplyr::desc(dist))

  return(arranged_values)
}


#' Internal function
#'
#' Compute pairwise string similarity
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param string_vector string vector
#'
#' @export
.pairwise_string_similarity <- function(string_vector) {
  dist. <- c()
  for (h in 1:(length(string_vector)-1))
    for (d in (h+1):length(string_vector))
      dist. <- c(dist.,
                 RecordLinkage::levenshteinSim(str1 =  tolower(string_vector[h]),
                                               str2 = tolower(string_vector[d])))
  return(dist.)
}



#' Launch shiny app for standardizing people names
#'
#' Application for standardizing people names, especially specimens collectors
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @import shiny
#' @export
launch_stand_people_app <- function() {
  app <- list(ui = ui <- fluidPage(

    # Application title
    titlePanel("Standardisation des noms de collecteurs"),

    sidebarPanel(
      fileInput(inputId = "data1", label = "Ajoutez votre fichier de donnees - format excel"),

      uiOutput("Box4"),

      uiOutput("Box5")
    ),

    mainPanel(
      h4("Avancement et nombre de noms sans correspondance"),
      textOutput("summary"),

      uiOutput("Box1"),

      uiOutput("Box2"),

      uiOutput("Action1"),

      h4("   "),
      downloadButton("downloadData", "Exporter une table incorporant les resultats")
    )

  ),

  server = function(input, output) {

    if(!exists("mydb")) .call.mydb()
    all_colnames <-
      dplyr::tbl(mydb, "table_colnam") %>%
      dplyr::collect()

    all_colnames.reac <- reactiveValues(df = NULL) ### reactive value destine e stocker la table des noms
    all_colnames.reac$df <- all_colnames

    original.data <- reactiveValues(df = NULL) ### reactive value destine e stocker la table originale des noms (pas mise e jour)

    original.list.name <- reactiveValues(df = NULL) ### reactive value destine e stocker un vecteur contenant la liste des noms original

    stand.list.name <- reactiveValues(df = NULL) ### vecteur des noms devant être standardise

    data.to.standardize.reac <- reactiveValues(df = NULL) ### reactive value destine e stocker la table des individus qui sera mise e jour

    stand.list.name <- reactiveValues(df = NULL) ### vecteur des noms devant être standardise

    val.nom.chosen <- reactiveValues(n = 1) ### Pointeur pour le nom e standardiser

    test <- reactiveValues(df = NULL)

    ### Choix de la colonne de la table originale e standardiser
    output$Box4 = renderUI({
      req(input$data1)

      DATA.col <- original.data$df
      colnames(DATA.col) <- seq(1, ncol(original.data$df), 1)

      selectInput("champ.nom","Selectionner le nom de la colonne e standardiser",
                  c(colnames(original.data$df[,as.numeric(colnames(dplyr::select_if(DATA.col, is.character)))])))


    })

    observeEvent(input$data1, {
      dataset <- input$data1

      file.rename(input$data1$datapath,
                  paste(input$data1$datapath, ".xlsx", sep=""))

      DATA <- readxl::read_excel(paste(input$data1$datapath, ".xlsx", sep=""), 1)

      ### Save table des individus original
      original.data$df <- DATA

    })


    observeEvent(input$champ.nom, { #  input$data1

      file.rename(input$data1$datapath,
                  paste(input$data1$datapath, ".xlsx", sep=""))

      DATA <- readxl::read_excel(paste(input$data1$datapath, ".xlsx", sep=""), 1)

      original.list.name$df <- dplyr::distinct(DATA[,input$champ.nom])

      ### Ajout de colonnes destinees e la mise e jour
      if(!any(colnames(DATA)=="found.colname")) { ## Ajout de found.name si pas deja present
        DATA <- DATA %>%
          tibble::add_column(found.colname = "")
      }

      if(!any(colnames(DATA)=="ID.colnam")) { ## Ajout de ID.dico.name si pas deja present
        DATA <- DATA %>%
          tibble::add_column(ID.colnam = 0)
      }

      DATA.selected <- DATA[,input$champ.nom]
      colnames(DATA.selected) <- "Code"

      stand.list.name$df <-
        DATA.selected %>%
        dplyr::select(Code) %>%
        dplyr::distinct(Code) %>%
        dplyr::arrange(Code)

      data.to.standardize.reac$df <- DATA

    })



    ### Une fois une colonne choisi, proposer de commencer la standardisation
    observeEvent(input$champ.nom, {

      output$Box5 = renderUI({
        req(input$champ.nom)
        req(input$data1)

        actionButton("do", "Commencer la standardisation")
      })
    }
    )


    # observeEvent(input$champ.nom, {
    #
    #   output$Box5 = renderUI({
    #     req(input$champ.nom)
    #     req(input$data1)
    #
    #     actionButton("do", "Commencer la standardisation")
    #   })
    # }
    # )

    ### Une fois une colonne choisi, proposer de commencer la standardisation
    observeEvent(input$champ.nom, {

      output$Box5 = renderUI({
        req(input$champ.nom)
        req(input$data1)

        actionButton("do", "Commencer la standardisation")
      })
    }
    )

    ### Une fois la confirmation du debut de standardisation
    observeEvent(input$do, {

      output$Box1 = renderUI({
        req(input$data1)

        ### 'truc' pour eviter de mal afficher les noms avec caracteres speciaux

        id.names <- as.list(seq(1, nrow(stand.list.name$df), 1))
        names(id.names) <- enc2native(dplyr::pull(stand.list.name$df, Code))

        selectInput("sector","Selectionner un nom a standardiser",
                    choices=id.names, selected=val.nom.chosen$n)

      })

      output$Box2 = renderUI( {

        req(input$data1)

        # Name1 <- original.list.name$df[as.numeric(input$sector)]
        Name1 <-
          stand.list.name$df %>%
          dplyr::slice(as.numeric(input$sector)) %>%
          dplyr::pull(Code)

        test$df <- input$sector

        list.match <-
          .find_similar_string(input = Name1, compared_table = all_colnames, column_name = colnam)

        selectInput("stock",
                    "Choisir le nom correct si present",
                    list.match$colnam,
                    " ")

      })

      output$Action1 = renderUI({
        if (is.null(input$stock) || input$stock == "Choisir le nom correct si present") {
          return()
        }else{
          actionButton(inputId="confirm.name", label=paste("Choisir", input$stock))
        }
      })

      observeEvent(input$confirm.name, {

        req(input$data1)

        Name1 <- stand.list.name$df %>%
          dplyr::slice(as.numeric(input$sector)) %>%
          dplyr::pull(Code)

        Name2 <- input$stock ### nom corrige
        selected.field <- "colnam"

        data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"found.colname"] <-
          Name2
        data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"ID.colnam"] <-
          dplyr::filter(all_colnames.reac$df, colnam==Name2) %>%
          dplyr::select(id_table_colnam) %>%
          dplyr::pull()

      })

      observeEvent(input$confirm.name, {
        Name1 <- stand.list.name$df %>%
          dplyr::slice(as.numeric(input$sector)) %>%
          dplyr::pull(Code)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$Code==Name1)
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig + 1

      }
      )

      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(
        filename = function() {
          paste("standardized.colnam",".csv", sep="")
        },
        content = function(file) {
          # readr::write_excel_csv(data.to.standardize.reac$df, file)
          readr::write_excel_csv(data.to.standardize.reac$df, file)
        }
      )


    }
    )

    output$summary <- renderPrint({

      # req(input$champ.nom)
      # print(data.to.standardize.reac$df)

    })

  }

  )

  shiny::runApp(app, launch.browser = TRUE)
}




#' Launch shiny app for taxonomic standardization
#'
#' Tool for standardizing taxonomy according to the taxonomic backbone (step needed before importing data to databases)
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @import shiny
#' @export
launch_stand_tax_app <- function() {

  app <- list(ui = ui <- fluidPage(
    titlePanel("Standardisation de noms d'especes vegetales pour l'Afrique tropicale - correction des erreurs orthographiques et homogeneisation de la synonymie"),

    sidebarPanel(
      fileInput(inputId = "data1", label = "Ajoutez votre fichier de donnees - format excel"),

      uiOutput("Box4"),

      uiOutput("Box5"),

      h4("   "),

      radioButtons("dico.choix", "Utiliser le dictionnaire par defaut",
                   c("oui","choisir un autre dictionnaire"),
                   selected = "oui"),
      # textInput("champ.nom", "Quel est le nom du champs contenant les noms a standardiser?",
      #              value = "Code"),
      uiOutput("Dico")
      # fileInput(inputId = "data2", label = "Ajoutez votre dictionnaire - format csv")
    ),

    mainPanel(
      h4("Avancement et nombre de noms sans correspondance"),
      htmlOutput("summary"),

      verbatimTextOutput("test"),

      # h4("Observations"),
      # verbatimTextOutput("view"),

      h4("Propostion de correspondance par ressemblance orthographique"),
      verbatimTextOutput("list.match"),

      uiOutput("Box7"),

      uiOutput("Box9"),

      # uiOutput("BoxChoixNom"),

      uiOutput("Box1"),

      uiOutput("Box2"),

      uiOutput("Box3"),

      uiOutput("Box6"),

      uiOutput("Box8"),

      h4("Nom choisi"),

      tableOutput("list.sp.m"),


      uiOutput("Action1"),

      h4("   "),
      uiOutput("Action2"),

      # uiOutput("Action3"),

      h4("   "),
      uiOutput("go.back.one"),

      # Button
      h4("   "),
      downloadButton("downloadData", "Exporter une table incorporant les resultats")

    )
  ) ,

  server = function(input, output) {

    if(!exists("mydb")) .call.mydb()

    DicoNames1 <- dplyr::tbl(mydb, "diconame") %>%
      dplyr::collect()
    # %>%
    #   filter(taxook!=4)

    # values <- reactiveValues(n = 1) ### Pointeur

    val.nom.chosen <- reactiveValues(n = 1) ### Pointeur pour le nom a standardiser

    # val.kind.name <- reactiveValues(n = 1)

    data.to.standardize.reac <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus qui sera mise a jour
    original.data <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus originale (pas mise a jour)
    original.list.name <- reactiveValues(df = NULL) ### reactive value destine a stocker un vecteur contenant la liste des noms original
    stand.list.name <- reactiveValues(df = NULL) ### vecteur des noms devant etre standardise
    list.match.reac <- reactiveValues(df = NULL)

    test_dat <- reactiveValues(df = NULL)



    DicoNames <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus
    DicoNames$df <- DicoNames1

    ### Choix de la colonne de la table originale a standardiser
    output$Box4 = renderUI({
      req(input$data1)

      DATA.col <- original.data$df
      colnames(DATA.col) <- seq(1, ncol(original.data$df), 1)

      selectInput("champ.nom","Selectionner le nom de la colonne a standardiser",
                  c(colnames(original.data$df[,as.numeric(colnames(dplyr::select_if(DATA.col, is.character)))])))


    })

    observeEvent(input$data1, {
      dataset <- input$data1

      file.rename(input$data1$datapath,
                  paste(input$data1$datapath, ".xlsx", sep=""))

      DATA <- readxl::read_excel(paste(input$data1$datapath, ".xlsx", sep=""), 1)

      if(!any(colnames(DATA)=="id_data")) {
        DATA <-
          DATA %>%
          tibble::add_column(id_data=seq(1, nrow(DATA), 1))
      }

      ### Save table des individus original
      original.data$df <- DATA

    })


    observeEvent(input$champ.nom, { #  input$data1

      file.rename(input$data1$datapath,
                  paste(input$data1$datapath, ".xlsx", sep=""))

      DATA <-
        readxl::read_excel(paste(input$data1$datapath, ".xlsx", sep=""), 1)

      if(!any(colnames(DATA)=="id_data")) {
        DATA <-
          DATA %>%
          tibble::add_column(id_data=seq(1, nrow(DATA), 1))
      }

      # original.list.name$df <- dplyr::distinct(DATA[,input$champ.nom])

      champ.nom <-
        input$champ.nom

      original.list.name$df <-
        DATA %>%
        dplyr::distinct(!!rlang::sym(champ.nom))


      ### Ajout de colonnes destinees a la mise a jour
      if(!any(colnames(DATA)=="found.name")) { ## Ajout de found.name si pas deja present
        DATA <- DATA %>% tibble::add_column(found.name = "")
      }

      if(!any(colnames(DATA)=="ID.dico.name")) { ## Ajout de ID.dico.name si pas deja present
        DATA <- DATA %>% tibble::add_column(ID.dico.name = 0)
      }

      if(!any(colnames(DATA)=="ID.dico.name.good")) { ## Ajout de ID.dico.name.good si pas deja present
        DATA <- DATA %>% tibble::add_column(ID.dico.name.good = 0)
      }

      if(!any(colnames(DATA)=="corrected.name")) { ## Ajout de corrected.name si pas deja present
        DATA <- DATA %>% tibble::add_column(corrected.name = "")
      }

      if(any(colnames(DATA)=="found.name")) {
        if(length(which(is.na(DATA$found.name)))>0) DATA$found.name[which(is.na(DATA$found.name))] <- "" ### Si des observations de found.name sont en NA, les mettre en ""
      }

      if(any(colnames(DATA)=="detvalue")) {
        if(length(which(is.na(DATA$detvalue)))>0) DATA$found.name[which(is.na(DATA$detvalue))] <- 0 ### Si des observations de detvalue sont en NA, les mettre a 0
      }


      ### Procedure de matching des noms --> noms correspondant a des noms du referentiel

      # select code and id_data
      DATA.selected <-
        DATA %>%
        dplyr::select(!!rlang::sym(champ.nom), id_data) %>%
        dplyr::rename(Code=!!rlang::sym(champ.nom))

      # matching based on full_name_no_auth
      join.table <-
        dplyr::left_join(DATA.selected,
                  dplyr::select(DicoNames1, id_n, id_good_n, full_name, full_name_no_auth),
                  by=c("Code"="full_name_no_auth"))

      # if more than one macthing by name, take the first
      join.table <-
        join.table %>%
        dplyr::group_by(id_data) %>%
        dplyr::summarise(id_n=dplyr::first(id_n), id_good_n=dplyr::first(id_good_n), Code=dplyr::first(Code))

      # excluding names with no matching
      join.table_sel <-
        join.table %>%
        dplyr::filter(!is.na(id_n)) %>%
        dplyr::select(Code, id_data, id_n,id_good_n)

      # combining matched names with DATA, adding found.name
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, Code, id_data), by=c("id_data"="id_data")) %>%
        dplyr::mutate(found.name=Code) %>%
        dplyr::select(-Code)

      # adding ID.dico.name
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, id_n, id_data), by=c("id_data"="id_data")) %>%
        dplyr::mutate(ID.dico.name=ifelse(is.na(id_n), ID.dico.name, id_n)) %>%
        dplyr::select(-id_n)

      # adding ID.dico.name.good
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, id_good_n, id_data), by=c("id_data"="id_data")) %>%
        dplyr::mutate(ID.dico.name.good=ifelse(is.na(id_good_n), ID.dico.name.good, id_good_n)) %>%
        dplyr::select(-id_good_n)

      # finding good names
      join.corrected.names <-
        dplyr::left_join(DATA %>% dplyr::select(ID.dico.name.good),
                  DicoNames1 %>% dplyr::select(id_n, full_name_no_auth), by=c("ID.dico.name.good"="id_n"))

      # adding good names
      DATA$corrected.name <-
        join.corrected.names$full_name_no_auth

      # adding detvalue
      if(!any(colnames(DATA)=="detvalue")) {
        DATA <-
          DATA %>%
          tibble::add_column(detvalue=rep(0, nrow(.)))
      }

      # %>%
      #   dplyr::mutate(detvalue=as.integer(detvalue))

      test <-
        DATA %>%
        dplyr::filter(ID.dico.name==0) %>%
        dplyr::distinct(!!rlang::sym(champ.nom), found.name) %>%
        dplyr::filter(!is.na(!!rlang::sym(champ.nom))) %>%
        dplyr::arrange(!!rlang::sym(champ.nom)) %>%
        tibble::add_column(id_tax_search=seq(1, nrow(.), 1)) %>%
        dplyr::mutate(id_tax_search=as.character(id_tax_search))

      DATA <-
        DATA %>%
        dplyr::left_join(test %>% dplyr::select(!!rlang::sym(champ.nom), id_tax_search))

      test <-
        test %>%
        dplyr::rename(Code=1)

        stand.list.name$df <-
        test %>%
        dplyr::select(Code, id_tax_search) %>%
          dplyr::filter(!is.na(Code))

      data.to.standardize.reac$df <-
        DATA

      # test_dat$df <-
      #   test


    })

    ### Une fois une colonne choisi, proposer de commencer la standardisation
    observeEvent(input$champ.nom, {

      output$Box5 = renderUI({
        req(input$champ.nom)
        req(input$data1)

        actionButton("do", "Commencer la standardisation")
      })
    }
    )

    output$summary <- renderUI({

      req(input$champ.nom)

      TXT2 <- paste("Nombre total de noms : ", nrow(unique(original.data$df[,input$champ.nom])))

      TXT <- paste("Nombre total de noms a standardiser : ", nrow(stand.list.name$df))

      TXT3 <- paste("NOMBRE DE NOMS RESTANT : ", nrow(stand.list.name$df)-(val.nom.chosen$n-1))

      HTML(paste(TXT2, TXT, TXT3, sep = '<br/>'))
    })

    ### Une fois la confirmation du debut de standardisation
    observeEvent(input$do, {

      output$Box7 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{

          # numericInput(inputId = "nbe_choice",
          #              label = "Nombre de noms similaires propose",
          #              value = 10,
          #              min = 0,
          #              max = 300)
          radioButtons("nbe_choice", "Nombre de noms similaires propose",
                       c("Top 10","Tous"),
                       selected = "Top 10")



        }
      })

      output$Box9 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{

          radioButtons("sort", "Trier les propositions",
                       c("ressemblance","alphabetique"),
                       selected = "ressemblance")

        }
      })

      output$Box1 = renderUI({
        req(input$data1)
        if(input$dico.choix!="oui") req(input$data2)

        ### 'truc' pour eviter de mal afficher les noms avec caracteres speciaux

        id.names <- as.list(stand.list.name$df$id_tax_search)
        names(id.names) <- enc2utf8(dplyr::pull(stand.list.name$df, Code))

        # Encoding(names(id.names)) <-  "UTF-8"

        selectInput("sector","Selectionner un nom a standardiser",
                    choices=id.names, selected=val.nom.chosen$n)

      })
      #

      output$Box3 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{
          radioButtons("choice.kind", "Chercher une correspondance dans",
                       c("Noms sans auteurs","Noms avec auteurs"),
                       selected = "Noms sans auteurs")

        }
      })



      output$Box6 = renderUI( {
        if (is.null(input$sector) || input$sector == "Choisir") {
          return()
        }else{
          radioButtons("choice.kind2", "Chercher une correspondance dans",
                       c("les taxa","les genres"),
                       selected = "les taxa")

        }
      })

      output$Box8 = renderUI( {
        if (is.null(input$sector) || input$sector == "Choisir") {
          return()
        }else{
          radioButtons("cf", "Identification certaine ou pas (cf)",
                       c("ok","cf"),
                       selected = "ok")

        }
      })


      output$Box2 = renderUI( {

        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()
        }else{

          req(input$data1)
          if(input$dico.choix!="oui") req(input$data2)

          # Name1 <- original.list.name$df[as.numeric(input$sector)]
          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search==as.numeric(input$sector)) %>%
            dplyr::pull(Code)


          if(input$choice.kind=="Noms sans auteurs" & input$choice.kind2=="les taxa")
            dist. <-
            RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames1$full_name_no_auth))
          if(input$choice.kind=="Noms avec auteurs" & input$choice.kind2=="les taxa")
            dist. <-
            RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames1$full_name))
          if(input$choice.kind2=="les genres") {
            dico_genus <-
              DicoNames1 %>%
              dplyr::filter(detvalue > 5, is.na(tax_esp), taxook!=4, morphocat == 3)
            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_genus$tax_gen))
          }

          if(input$nbe_choice=="Top 10") nbe.match <- 10 ### Nombre de proposition pour la correspondance a afficher
          if(input$nbe_choice=="Tous") nbe.match <- length(dist.[!is.na(dist.)]) ### Nombre de proposition pour la correspondance a afficher

          # test_dat$df <- dico_genus[order(dist., decreasing = T),]

          if(input$choice.kind2=="les genres") {

            matches. <-
              dico_genus[order(dist., decreasing = T)[1:nbe.match],]

            # if(input$sort=="alphabetique")

          }else{
            matches. <-
              DicoNames1[order(dist., decreasing = T)[1:nbe.match],]
          }

          matches. <-
            dplyr::select(matches., full_name_no_auth, detvalue, taxook, id_n, id_good_n, full_name, tax_gen, tax_esp)

          if(input$choice.kind=="Noms sans auteurs" & input$choice.kind2=="les taxa") {
            selected.field <- "full_name_no_auth"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), id_n)
            # list.match <- c(matches.[,c(selected.field)])
          }

          if(input$choice.kind=="Noms avec auteurs" & input$choice.kind2=="les taxa") {
            selected.field <- "full_name"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), id_n)
            # list.match <- c(matches.[,c(selected.field)])
          }


          if(input$choice.kind2=="les genres") {
            selected.field <- "tax_gen"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), id_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$sort=="alphabetique")
            list.match <-
            list.match %>%
            arrange(!!rlang::sym(selected.field))


          list.match.reac$df <- list.match

          id.names <- as.list(seq(1, nrow(list.match.reac$df), 1))
          names(id.names) <- enc2utf8(dplyr::pull(list.match.reac$df, 1))

          # Encoding(names(id.names)) <-  "UTF-8"

          selectInput("stock","Choisir le nom correct si present",
                      choices=id.names, selected=1)



        }
      })


      output$Action1 = renderUI({
        if (is.null(input$stock) || input$stock == "Choisir le nom correct si present") {
          return()
        }else{
          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)

          actionButton(inputId="confirm.name", label=paste("Choisir", Name2))
        }
      })


      output$Action2 = renderUI({
        if (is.null(input$stock) || input$stock == "Choisir le nom correct si present") {
          return()
        }else{
          actionButton(inputId="no.match", label="Pas de correspondance, passer au nom suivant")
        }
      })

      output$list.sp.m <- renderTable({ # renderTable

        req(input$data1)
        if(input$dico.choix!="oui") req(input$data2)

        if (is.null(input$stock) || input$stock == " ") {
          return()
        }else{

          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search==as.numeric(input$sector)) %>%
            dplyr::pull(Code)

          id_Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(2)

          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)


          if(input$choice.kind=="Noms sans auteurs" & input$choice.kind2=="les taxa") selected.field <- "full_name_no_auth"
          if(input$choice.kind=="Noms avec auteurs" & input$choice.kind2=="les taxa") selected.field <- "full_name"
          if(input$choice.kind2=="les genres")  selected.field <- "tax_gen"

          test_syn <-
            DicoNames1 %>%
            dplyr::filter(id_n==id_Name2) %>%
            dplyr::select(id_n, id_good_n)

          if(test_syn$id_n!=test_syn$id_good_n) syn_check <- TRUE
          if(test_syn$id_n==test_syn$id_good_n) syn_check <- FALSE


          if(syn_check) {

            id_good <-
              DicoNames1 %>%
              # filter(!!rlang::sym(selected.field)==Name2) %>%
              dplyr::filter(id_n==id_Name2) %>%
              dplyr::select(id_good_n) %>%
              dplyr::pull()

            dplyr::tibble('Nom cherche'=Name1, "Nom propose"=Name2, "Est considere synonyme de"= DicoNames1 %>% dplyr::filter(id_n==!!id_good) %>% dplyr::select(full_name_no_auth) %>% dplyr::pull())

            # print("Considere comme synonyme de")
            # print(DicoNames$df$full_name_no_auth[which(DicoNames$df$id_n==DicoNames$df$id_good_n[which(DicoNames$df$full_name_no_auth %in% Name2)])])
          }else{
            dplyr::tibble('Nom cherche'=Name1, "Nom propose"=Name2)
          }

        }
      })

      observeEvent(input$confirm.name, {

        req(input$data1)
        if(input$dico.choix!="oui") req(input$data2)

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search==as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)

        id_Name2 <-
          list.match.reac$df %>%
          dplyr::slice(as.numeric(input$stock)) %>%
          dplyr::pull(2)


        if(input$choice.kind=="Noms sans auteurs" & input$choice.kind2=="les taxa") selected.field <- "full_name_no_auth"
        if(input$choice.kind=="Noms avec auteurs" & input$choice.kind2=="les taxa") selected.field <- "full_name"
        if(input$choice.kind2=="les genres")  selected.field <- "tax_gen"


        Name2 <-
          DicoNames1 %>%
          dplyr::filter(id_n==id_Name2) %>%
          dplyr::select(!!rlang::sym(selected.field)) %>%
          dplyr::pull()

        # complete found.name based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(found.name=replace(found.name, id_tax_search==Name1, Name2))

        Encoding(Name2) <- "UTF-8"

        found_taxa <-
          DicoNames1 %>%
          dplyr::filter(id_n==!!id_Name2)

        ### in case of duplicates in diconames
        if(nrow(found_taxa)>1) {
          found_taxa <-
            found_taxa %>%
            dplyr::group_by(!!rlang::sym(selected.field)) %>%
            dplyr::summarise(id_good_n=dplyr::first(id_good_n), full_name=dplyr::first(full_name), id_n=dplyr::first(id_n))
        }


        # complete ID.dico.name based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name=replace(ID.dico.name, id_tax_search==Name1, found_taxa$id_n))

        # complete ID.dico.name.good based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name.good=replace(ID.dico.name.good, id_tax_search==Name1, found_taxa$id_good_n))


        # complete detvalue based on buttons cf
        if(input$cf=="cf") {
          data.to.standardize.reac$df <-
            data.to.standardize.reac$df %>%
            dplyr::mutate(detvalue=replace(detvalue, id_tax_search==Name1, 3))
        }


        ### checking for synonymies
        if(found_taxa$id_n!=found_taxa$id_good_n) {
          found_correc_taxa <-
            DicoNames1 %>%
            dplyr::filter(id_n==found_taxa$id_good_n)

          data.to.standardize.reac$df <-
            data.to.standardize.reac$df %>%
            dplyr::mutate(corrected.name=replace(corrected.name, id_tax_search==Name1, found_correc_taxa$full_name_no_auth))
        }else{
          data.to.standardize.reac$df <-
            data.to.standardize.reac$df %>%
            dplyr::mutate(corrected.name=replace(corrected.name, id_tax_search==Name1, found_taxa$full_name_no_auth))
        }

      })

      observeEvent(input$confirm.name, {
        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search==as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$id_tax_search==Name1)
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig + 1

      }
      )

      # observeEvent(input$confirm.name, {
      #
      #   # values$n <- values$n + 1
      #   # if(values$n>nrow(stand.list.name$df))   values$n <- nrow(stand.list.name$df)
      # })

      observeEvent(input$no.match, {
        req(input$data1)
        if(input$dico.choix!="oui") req(input$data2)

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search==as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$id_tax_search==Name1)
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig + 1

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name=replace(ID.dico.name, id_tax_search==Name1, 0))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(found.name=replace(found.name, id_tax_search==Name1, ""))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name.good=replace(ID.dico.name.good, id_tax_search==Name1, 0))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(corrected.name=replace(corrected.name, id_tax_search==Name1, ""))

        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"found.name"] <- ""
        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"ID.dico.name"] <- 0
        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"ID.dico.name.good"] <- 0
        # data.to.standardize.reac$df[which(data.to.standardize.reac$df[,input$champ.nom][[1]]==Name1),"corrected.name"] <- ""
      })

      # Downloadable csv of selected dataset ----
      output$downloadData <- downloadHandler(


        filename = function() {
          paste("standardized.taxo",".csv", sep="")
        },
        content = function(file) {
          readr::write_excel_csv(data.to.standardize.reac$df %>% dplyr::select(-id_tax_search), file)
        }
      )

      output$go.back.one = renderUI({
        if (val.nom.chosen$n>1) {
          actionButton(inputId="go.back", label="Revenir au nom precedent")
        }
      })

      observeEvent(input$go.back, {
        # values$n <- values$n - 1
        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search==as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)

        id.orig <- which(stand.list.name$df[,"id_tax_search"]==Name1)
        #
        if(length(id.orig)==0) val.nom.chosen$n <- val.nom.chosen$n - 1
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig - 1

      })

    }
    )



    # output$view <- renderPrint({
    #
    #   print(test_dat$df)
    #
    # })

  }


  )

  shiny::runApp(app, launch.browser = TRUE)

}




#' Load the database
#'
#' Load the database and ask for password
#'
#' @param pass string
#'
#' @return The database is loaded
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
.call.mydb <- function(pass=NULL) {

  if(!exists("mydb")) {
    if(is.null(pass))
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
      dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>% #### selecting id_dico_name from specimens if any
      dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
      dplyr::select(id_n, multi_tiges_id_good, plot_name,team_leader,country,locality_name, sous_plot_name, ind_num_sous_plot, code_individu, dbh, full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat,
                    id_diconame_final, dbh_height, tree_height, branch_height, branchlet_height, crown_spread, liane, strate_cat,
                    herbarium_code_char, id_specimen, id_old) %>%
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
      query <- gsub(pattern = "MMM", replacement = paste0(" tax_gen ILIKE '", genus_searched, "%' AND MMM"), x=query)

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
    if(any(colnames(new_data_renamed)==col_names_select[i])){
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    }else{
      stop(paste("Column name provided not found in provided new dataset", col_names_corresp[i]))

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
  if(any(new_data_renamed$ddlat>90) | any(new_data_renamed$ddlon< -90))
    stop("ERREUR dans ddlat, latitude provided impossible")

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
#' @param new_plot_name string new plot name
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
                             new_plot_name=NULL,
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
    new_values <-
      dplyr::tibble(plot_name=ifelse(!is.null(new_plot_name), new_plot_name, quer_plots$plot_name),
                    method=ifelse(!is.null(new_method), new_method, quer_plots$method),
                    team_leader=ifelse(!is.null(new_team_leader), new_team_leader, quer_plots$team_leader),
                    country=ifelse(!is.null(new_country), new_country, quer_plots$country),
                    ddlat=ifelse(!is.null(new_ddlat), new_ddlat, quer_plots$ddlat),
                    ddlon=ifelse(!is.null(new_ddlon), new_ddlon, quer_plots$ddlon),
                    elevation=ifelse(!is.null(new_elevation), new_elevation, quer_plots$elevation)
                         )

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


      col_sel <-
        comp_values %>%
        dplyr::select_if(~sum(.) > 0) %>%
        colnames()
      print("Previous values")
      print(quer_plots %>%
              dplyr::select(!!col_sel))
      print("New values")
      print(new_values %>%
              dplyr::select(!!col_sel))

      Q <- utils::askYesNo("Confirm these modifications?")

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

      quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id)))

      all_id_match <- dplyr::pull(dplyr::select(matches, !!var_id))

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
          dplyr::top_n(1) %>%
          dplyr::collect() %>%
          colnames()

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
#' @param new_data tibble new data to be import
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param id_col integer indicate which name of col_names_select is the id for matching data
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_individuals <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col,
                            launch_adding_data=FALSE) {

  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )

  if(!exists("mydb")) .call.mydb()

  if(length(col_names_select)!=length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  new_data_renamed <-
    new_data %>%
    dplyr::rename_at(dplyr::vars(col_names_select[id_col]), ~ col_names_corresp[id_col])


  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename(plot_name= !!vars(col_names_select[id_col]))

  ids_plot <-
    new_data_renamed %>%
    dplyr::select(plot_name) %>%
    dplyr::distinct(plot_name) %>%
    dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                dplyr::select(plot_name, id_liste_plots) %>%
                  dplyr::collect())

  if(any(is.na(ids_plot$id_liste_plots))) {
    warning("some plot are not found in metadata")
    print(ids_plot %>%
            dplyr::filter(is.na(id_liste_plots)))
    ids_plot <-
      ids_plot %>%
      dplyr::filter(!is.na(id_liste_plots))

    logs <-
      dplyr::bind_rows(logs,
                     dplyr::tibble(
                       column = "id_liste_plots",
                       note = paste(nrow(ids_plot %>%
                                            dplyr::filter(is.na(id_liste_plots))),
                                     "some plot are not found in metadata")
                     ))
  }


  plots_already_in_db <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(id_table_liste_plots_n %in% ids_plot$id_liste_plots) %>%
    dplyr::distinct(id_table_liste_plots_n) %>%
    dplyr::collect()

  if(nrow(plots_already_in_db)>0) {
    print(plots_already_in_db %>%
            dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                               dplyr::select(plot_name, id_liste_plots) %>%
                               dplyr::collect(),
                             by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
            pull(plot_name)
          )
    stop("data for some plots already in database")

  }


  new_data_renamed <-
    new_data_renamed %>%
    dplyr::left_join(ids_plot) %>%
    dplyr::rename(id_table_liste_plots_n=id_liste_plots)


  col_names_select <-
    col_names_select[-id_col]
  col_names_corresp <-
    col_names_corresp[-id_col]

  for (i in 1:length(col_names_select)) {
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
  }

  col_names_corresp <-
    c(col_names_corresp, "id_table_liste_plots_n")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(col_names_corresp)

  ### CHECKS
  method <-
    ids_plot %>%
    dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                       dplyr::select(plot_name, id_liste_plots, method) %>%
                       dplyr::collect(), by=c("id_liste_plots"="id_liste_plots")) %>%
    dplyr::distinct(method)

  if(nrow(method)>1) {
    print(method)
    stop("More than one method selected, import plot of one method at a time")
  }

  if(!any(colnames(new_data_renamed)=="id_diconame_n")) stop("id_diconame_n column missing")

  if(any(new_data_renamed$id_diconame_n==0))
    stop(paste("id_diconame_n is NULL for", sum(new_data_renamed$id_diconame_n==0), "individuals"))

  if(any(is.na(new_data_renamed$id_diconame_n)))
    stop(paste("id_diconame_n is missing for", sum(new_data_renamed$id_diconame_n==0), "individuals"))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(id_diconame_n) %>%
    left_join(tbl(mydb, "diconame") %>%
                dplyr::select(id_n, id_good_n) %>%
                collect(), by=c("id_diconame_n"="id_n")) %>%
    filter(is.na(id_good_n)) %>%
    pull(id_diconame_n)

  if(length(unmatch_id_diconame)>0)
    stop(paste("id_diconame not found in diconame", unmatch_id_diconame))

  ## checking DBH
  if(!any(colnames(new_data_renamed)=="dbh")) stop("dbh column missing")
  if(any(is.na(new_data_renamed$dbh))) {
    warning(paste(sum(is.na(new_data_renamed$dbh)), "missing dbh values"))

    logs <-
      dplyr::bind_rows(logs,
                       dplyr::tibble(
                         column = "dbh",
                         note = paste(sum(is.na(new_data_renamed$dbh)), "missing dbh values")
                       ))

  }

  if(any(new_data_renamed$dbh[!is.na(new_data_renamed$dbh)]<0)) {
    warning(paste(sum(new_data_renamed$dbh[!is.na(new_data_renamed$dbh)]<0),
                  "negative dbh values"))

    logs <-
      dplyr::bind_rows(logs,
                       dplyr::tibble(
                         column = "dbh",
                         note = paste(sum(new_data_renamed$dbh[!is.na(new_data_renamed$dbh)]<0),
                                      "negative dbh values")
                       ))

  }

  if(any(new_data_renamed$dbh[!is.na(new_data_renamed$dbh)]>300)) {
    warning(paste(sum(new_data_renamed$dbh[!is.na(new_data_renamed$dbh)]>300),
                  "excessive (>300) dbh values"))

    print(new_data_renamed %>%
            dplyr::filter(dbh>300))

    logs <-
      dplyr::bind_rows(logs,
                       dplyr::tibble(
                         column = "dbh",
                         note = paste(sum(new_data_renamed$dbh[!is.na(new_data_renamed$dbh)]>300),
                                      "excessive (>300) dbh values")
                       ))

  }

  ## checking column given method
  if(dplyr::pull(method)=="Large") {
    if(!any(colnames(new_data_renamed)=="sous_plot_name")) stop("sous_plot_name column missing")
    if(!any(colnames(new_data_renamed)=="ind_num_sous_plot")) stop("ind_num_sous_plot column missing")
    if(!any(colnames(new_data_renamed)=="dbh")) stop("dbh column missing")
    if(!any(colnames(new_data_renamed)=="position_transect")) stop("position_transect column missing")
    if(!any(colnames(new_data_renamed)=="strate_cat")) stop("strate_cat column missing")

    ## checking strate info
    miss_strate <-
      new_data_renamed %>%
      filter(!strate_cat %in% c("Ad", "Ado"))

    if(nrow(miss_strate)>0) {
      warning(paste("strate_cat missing or not equal to Ad or Ado for", nrow(miss_strate), "individuals"))
      print(miss_strate)
    }

    ## checking sous_plot_name
    type_sousplot <-
      new_data_renamed %>%
      dplyr::distinct(sous_plot_name) %>%
      dplyr::pull()
    if(!any(type_sousplot == c("A", "B", "C", "D")))
      warning("sous_plot_name should include A B C and D")

    # check ind_num_sous_plot
    for (i in unique(new_data_renamed$id_table_liste_plots_n)) {
      for (j in c("A", "B", "C", "D")) {
        duplicates_ind_plot <-
          new_data_renamed %>%
          dplyr::filter(id_table_liste_plots_n==i, sous_plot_name==j) %>%
          dplyr::group_by(ind_num_sous_plot) %>%
          dplyr::count() %>%
          dplyr::filter(n>1)

      if(nrow(duplicates_ind_plot)>0) {
        plot_name <-
          dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(plot_name, id_liste_plots) %>%
          dplyr::filter(id_liste_plots==i) %>%
          dplyr::pull(plot_name)

        warning(paste(nrow(duplicates_ind_plot),
                      "duplicate in ind_num_sous_plot for ", plot_name, j))

        logs <-
          dplyr::bind_rows(logs,
                           dplyr::tibble(
                             column = "ind_num_sous_plot",
                             note = paste(nrow(duplicates_ind_plot),
                                          "duplicate in ind_num_sous_plot for ", plot_name, j)
                           ))
      }

      }

    }

  }

  # check herbarium specimen coherence

  if(!any(colnames(new_data_renamed)=="herbarium_nbe_type"))
    warning("herbarium_nbe_type column missing")
  if(!any(colnames(new_data_renamed)=="herbarium_nbe_char"))
    warning("herbarium_nbe_char column missing")

  all_herb_ref <-
    new_data_renamed %>%
    distinct(herbarium_nbe_char) %>%
    filter(!is.na(herbarium_nbe_char))

  if(any(colnames(new_data_renamed)=="herbarium_nbe_type")) {
    all_herb_type <-
      new_data_renamed %>%
      distinct(herbarium_nbe_type) %>%
      filter(!is.na(herbarium_nbe_type))

  if(nrow(all_herb_type) != nrow(all_herb_ref)) {
    stop("Number of herbarium specimen type and reference are not identical")
    print(all_herb_type)
    print(all_herb_ref)
  }

  herb_type_dups <-
    new_data_renamed %>%
    group_by(herbarium_nbe_type) %>%
    count() %>%
    filter(n>1, !is.na(herbarium_nbe_type))

    # herb_type_dups <-
    #   new_data_renamed %>%
    #   distinct(herbarium_nbe_type, id_diconame_n) %>%
    #   filter(!is.na(herbarium_nbe_type), !is.na(id_diconame_n)) %>%
    #   group_by(herbarium_nbe_type) %>%
    #   count() %>%
    #   filter(n>1)

    if(nrow(herb_type_dups)>0) {
      warning(paste("herbarium_nbe_type if duplicated for", nrow(herb_type_dups), "specimen"))
      new_data_renamed %>%
        filter(herbarium_nbe_type %in% pull(herb_type_dups, herbarium_nbe_type))

      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_type",
                           note = paste("herbarium_nbe_type is duplicated for",
                                        paste(pull(herb_type_dups, herbarium_nbe_type),
                                              collapse = ";"), "specimen")
                         ))
    }
  }

  ## check herbarium specimen reference coherence
  herb_ref_multiple_taxa <-
    new_data_renamed %>%
    distinct(herbarium_nbe_char, id_diconame_n) %>%
    filter(!is.na(herbarium_nbe_char)) %>%
    group_by(herbarium_nbe_char) %>%
    count() %>%
    filter(n>1)

  herb_ref_multiple_taxa <-
    new_data_renamed %>%
    filter(herbarium_nbe_char %in% pull(herb_ref_multiple_taxa, herbarium_nbe_char)) %>%
    dplyr::select(herbarium_nbe_char, original_tax_name, id_diconame_n) %>%
    distinct()

  if(nrow(herb_ref_multiple_taxa)>0) {
    logs <-
      dplyr::bind_rows(logs,
                       dplyr::tibble(
                         column = "herbarium_nbe_char",
                         note = paste("herbarium_nbe_char carry different identification for",
                                      paste(herb_ref_multiple_taxa %>%
                                              dplyr::distinct(herbarium_nbe_char) %>%
                                              dplyr::pull(),
                                            collapse = "; "),
                                      paste(herb_ref_multiple_taxa %>%
                                              dplyr::distinct(original_tax_name) %>%
                                              dplyr::pull(),
                                            collapse = ", "))
                       ))
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
               data_modif_m=lubridate::month(Sys.Date()),
               data_modif_y=lubridate::year(Sys.Date()))

  if(launch_adding_data) {

    print(list(new_data_renamed, logs))

    confirmed <- utils::askYesNo("Confirm adding?")

    if(confirmed) DBI::dbWriteTable(mydb, "data_individuals", new_data_renamed, append = TRUE, row.names = FALSE)

  }

  return(list(new_data_renamed, logs))

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
                             new_morphocat=NULL,
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

  if(all(is.null(c(genus_searched, tax_esp_searched, tax_fam_searched, synonym_of, id_search, new_full_name_no_auth, new_full_name_used, new_full_name_used2, new_morphocat))) & !no_synonym_modif) stop("\n Provide the species to be updated or precise new synonymy")

  ### checking if at least one modification is asked
  new_vals <- c(new_tax_gen, new_tax_esp, new_full_name_auth, new_tax_fam, new_taxook,new_detvalue, new_id_diconame_good, no_synonym_modif, new_full_name_no_auth, new_full_name_used, new_full_name_used2, new_morphocat)
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

  if(nrow_query)
    modif_types <-
    vector(mode = "character", length = nrow(query_tax))

  ## if the modification does not concern synonymies, check if provided values are different for those existing
  if(nrow_query & !no_synonym_modif & is.null(synonym_of)) {
    new_vals <-
      dplyr::tibble(tax_fam=ifelse(!is.null(new_tax_fam), as.character(new_tax_fam), query_tax$tax_fam),
                       tax_gen=ifelse(!is.null(new_tax_gen), as.character(new_tax_gen), query_tax$tax_gen),
                       tax_esp=ifelse(!is.null(new_tax_esp), as.character(new_tax_esp), query_tax$tax_esp), # $4
                       taxook=ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), # $5
                       full_name=ifelse(!is.null(new_full_name_auth), as.character(new_full_name_auth), query_tax$full_name), # $6
                       detvalue=ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), # $7
                       id_good_n=ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n),
                       full_name_no_auth=ifelse(!is.null(new_full_name_no_auth), as.character(new_full_name_no_auth), query_tax$full_name_no_auth),
                       full_name_used=ifelse(!is.null(new_full_name_used), as.character(new_full_name_used), query_tax$full_name_used),
                       full_name_used2=ifelse(!is.null(new_full_name_used2), as.character(new_full_name_used2), query_tax$full_name_used2),
                       morphocat=ifelse(!is.null(new_morphocat), new_morphocat, query_tax$morphocat))

    # comp_vals <-
    #   query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, taxook, full_name, detvalue, id_good_n) != new_vals

    new_vals <-
      new_vals %>%
      replace(., is.na(.), -9999)

    sel_query_tax <-
      dplyr::bind_rows(new_vals, query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, taxook, full_name, detvalue, id_good_n, full_name_no_auth, full_name_used, full_name_used2, morphocat))

    sel_query_tax <-
      sel_query_tax %>%
      replace(., is.na(.), -9999)

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
        utils::askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
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
        utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
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
        DBI::dbSendQuery(mydb, statement="UPDATE diconame SET tax_fam=$2, tax_gen=$3, tax_esp=$4, taxook=$5, full_name=$6, detvalue=$7, id_good_n=$8, full_name_no_auth=$9, full_name_used=$10, full_name_used2=$11, morphocat=$12 WHERE id_n = $1",
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
                                rep(ifelse(!is.null(new_full_name_used2), new_full_name_used2, query_tax$full_name_used2), nrow(query_tax)),
                                rep(ifelse(!is.null(new_morphocat), new_morphocat, query_tax$morphocat), nrow(query_tax)))) # $11
      DBI::dbClearResult(rs)

      rs <-
        DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
                    params=list(query_tax$id_n))
      if(show_results) print(DBI::dbFetch(rs))
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
        utils::askYesNo(msg = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled?", default = FALSE)
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

      if(!is.list(synonym_of)) {
        stop("synonym_of should be a list with the first element \nbeing the genus and the second element the species epiteth of the taxa of the correct name")
      }

      if(!any(names(synonym_of)=="genus") & !any(names(synonym_of)=="species") & !any(names(synonym_of)=="id"))
      stop("synonym_of should have at least of the thre following element : genus, species or id")

      if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
      if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
      if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL

      syn_searched <-
        query_tax_all(genus_searched = synonym_of$genus,
                      tax_esp_searched = synonym_of$species,
                      id_search = synonym_of$id)

      print(syn_searched)
      if(nrow(syn_searched)>1) stop("More than 1 taxa as synonym. Select only one.")
      if(nrow(syn_searched)==0) stop("No taxa found in the dictionnary. Select one.")

      update_dico_name(new_id_diconame_good = syn_searched$id_good_n, id_search = new_entry$id_n,
                       ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)

    }else{
      update_dico_name(new_id_diconame_good = new_entry$id_n, id_search = new_entry$id_n,
                       ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)
    }
    print(dplyr::tbl(mydb, "diconame") %>% dplyr::collect() %>% filter(id_n == max(id_n)))
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
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>% dplyr::select(-detvalue, -data_modif_d, -data_modif_m, -data_modif_y), by=c("id_diconame_n"="id_n")) %>%
    dplyr::left_join(dplyr::tbl(mydb, "table_colnam"), by=c("id_colnam"="id_table_colnam"))
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
        dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_n"="id_n")) %>%
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


#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param string_vector string vector
#'
#' @export
.link_colnam <- function(data_stand, collector_field) {
  col_name <- "col_name"

  data_stand <-
    data_stand %>%
    dplyr::rename_at(dplyr::vars(collector_field), ~ col_name)

  all_names_collector <-
    dplyr::distinct(data_stand, col_name)

  id_colname <-
    vector(mode = "integer", nrow(data_stand))

  for (i in 1:nrow(all_names_collector)) {

    all_colnames <-
      dplyr::tbl(mydb, "table_colnam") %>%
      dplyr::collect()

    print(dplyr::pull(all_names_collector)[i])
    sorted_matches <-
      .find_similar_string(input = pull(all_names_collector)[i],
                           compared_table = all_colnames, column_name = "colnam")
    print(sorted_matches)
    selected_name <-
      readline(prompt="Choose which name fit (if none enter 0): ")

    selected_name <- as.integer(selected_name)

    if(is.na(selected_name)) stop("Provide integer value for standardizing collector name")

    if(selected_name==0) {
      new_colname <-
        readline(prompt="Provide a new collector name following same format: ")

      new_family_name <-
        readline(prompt="Provide a new family_name name following same format: ")

      new_surname <-
        readline(prompt="Provide a new surname name following same format: ")

      new_nationality <-
        readline(prompt="Provide a nationality following same format: ")

      new_rec <- tibble::tibble(colnam=new_colname,
                                family_name=new_family_name,
                                surname=new_surname,
                                nationality=new_nationality)

      DBI::dbWriteTable(mydb, "table_colnam", new_rec, append = TRUE, row.names = FALSE)

      selected_name_id <-
        dplyr::tbl(mydb, "table_colnam") %>%
        dplyr::filter(colnam==new_colname) %>%
        dplyr::select(id_table_colnam) %>%
        dplyr::collect() %>%
        dplyr::pull()
    }

    if(selected_name>0) {
      selected_name_id <-
        sorted_matches %>%
        slice(selected_name) %>%
        dplyr::select(id_table_colnam) %>%
        dplyr::pull()
    }

    id_colname[data_stand$col_name==dplyr::pull(all_names_collector[i,1])] <-
      selected_name_id

  }

  data_stand <-
    data_stand %>%
    tibble::add_column(id_colnam=id_colname)

  return(data_stand)
}

#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param string_vector string vector
#'
#' @export
.query_unmatched_specimens <- function() {
  all_herbarium_individuals <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::select(herbarium_nbe_char, herbarium_code_char, herbarium_nbe_type, id_diconame_n, id_specimen, id_n) %>%
    dplyr::filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char) | !is.na(herbarium_nbe_type)) %>%
    collect()

  ### all specimens with more than one id_diconame in table individuals
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    distinct(herbarium_nbe_char, id_diconame_n) %>%
    group_by(herbarium_nbe_char) %>%
    count() %>%
    filter(n>1)

  #### all specimens with more than one id_diconame in table individuals with names
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    distinct(herbarium_nbe_char, id_diconame_n) %>%
    left_join(tbl(mydb, "diconame") %>%
                dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
                collect(),
              by=c("id_diconame_n"="id_n")) %>%
    filter(herbarium_nbe_char %in% pull(all_herbarium_individuals_not_linked_diff_tax,
                                        herbarium_nbe_char)) %>%
    arrange(herbarium_nbe_char)

  ### all specimens with more than one genus in table individuals
  herb_specimen_diff_gen <-
    all_herbarium_individuals_not_linked_diff_tax %>%
    dplyr::distinct(herbarium_nbe_char, tax_gen) %>%
    group_by(herbarium_nbe_char) %>%
    count() %>%
    filter(n>1)

  ### all individuals concerned by specimens with more than one genus in table individuals
  data_individuals_concerned <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(herbarium_nbe_char %in% herb_specimen_diff_gen$herbarium_nbe_char) %>%
    collect() %>%
    dplyr::select(dbh, code_individu, sous_plot_name, ind_num_sous_plot, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type, id_diconame_n) %>%
    dplyr::left_join(tbl(mydb, "diconame") %>%
                       dplyr::select(id_n, full_name_no_auth, tax_gen, tax_esp, tax_fam) %>%
                       collect(),
                     by=c("id_diconame_n"="id_n")) %>%
    arrange(herbarium_nbe_char) %>%
    collect()

  ### extraction of all specimens not linked to specimens table excluding problematic specimens
  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals %>%
    filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::filter(!herbarium_nbe_char %in%
                    unique(pull(all_herbarium_individuals_not_linked_diff_tax,
                                herbarium_nbe_char)))

  regexp <- "[[:digit:]]+"
  num_extracted <-
    str_extract(all_herbarium_individuals_not_linked$herbarium_nbe_char, regexp)

  df <-
    tibble(full = all_herbarium_individuals_not_linked$herbarium_nbe_char,
           num = num_extracted)

  coll_extracted <-
    apply(df, MARGIN = 1, FUN = function(x) gsub(x[2], "", x[1]))
  coll_extracted <-
    trimws(coll_extracted)

  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals_not_linked %>%
    tibble::add_column(col_name = coll_extracted) %>%
    tibble::add_column(colnbr = num_extracted)

  all_herbarium_individuals_not_linked <-
    .link_colnam(data_stand = all_herbarium_individuals_not_linked,
                 collector_field = 3)

  return(list(all_herbarium_individuals_not_linked_diff_tax = all_herbarium_individuals_not_linked_diff_tax,
              data_individuals_not_linked_diff_tax_concerned = data_individuals_concerned,
              all_herbarium_individuals_not_linked = all_herbarium_individuals_not_linked))
}


#' Add new specimens data
#'
#' Add new specimens data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be imported
#' @param col_names_select string plot name of the selected plots
#' @param col_names_corresp string country of the selected plots
#' @param id_col integer indicate which name of col_names_select is the id for colnam table
#' @param plot_name_field integer indicate which name of col_names_select is the id for matching liste plots table
#'
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_specimens <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col,
                          plot_name_field=NULL,
                            launch_adding_data=FALSE) {

  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )

  if(!exists("mydb")) .call.mydb()

  if(length(col_names_select)!=length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  new_data_renamed <-
    new_data %>%
    tibble::add_column(id_new_data=1:nrow(.))

  for (i in 1:length(col_names_select)) {
    if(any(colnames(new_data_renamed)==col_names_select[i])){
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    }else{
      stop(paste("Column name provided not found in provided new dataset", col_names_select[i]))
    }
  }

  col_names_corresp <-
    c(col_names_corresp, "id_new_data")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(col_names_corresp)


  ### check diconame id
  if(!any(colnames(new_data_renamed)=="id_diconame_n")) stop("id_diconame_n column missing")

  if(any(new_data_renamed$id_diconame_n==0))
    stop(paste("id_diconame_n is NULL for", sum(new_data_renamed$id_diconame_n==0), "individuals"))

  if(any(is.na(new_data_renamed$id_diconame_n)))
    stop(paste("id_diconame_n is missing for", sum(new_data_renamed$id_diconame_n==0), "individuals"))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(id_diconame_n) %>%
    left_join(tbl(mydb, "diconame") %>%
                dplyr::select(id_n, id_good_n) %>%
                collect(), by=c("id_diconame_n"="id_n")) %>%
    filter(is.na(id_good_n)) %>%
    pull(id_diconame_n)

  if(length(unmatch_id_diconame)>0)
    stop(paste("id_diconame not found in diconame", unmatch_id_diconame))


  ### check locality and adding it if link to plots
  if(!any(colnames(new_data_renamed)=="locality")) warning("locality column missing")

  ### Linking plot names
  if(!is.null(plot_name_field & !any(colnames(new_data_renamed)=="locality"))) {
    plot_name <- "plot_name"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(plot_name_field), ~ plot_name)

    all_plot_names <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::select(id_liste_plots, plot_name) %>%
      dplyr::collect()

    all_plot_name_new_dataset <-
      dplyr::distinct(new_data_renamed, plot_name)

    all_plot_name_new_dataset <-
      all_plot_name_new_dataset %>%
      left_join(all_plot_names)

    all_plot_name_new_dataset_no_match <-
      all_plot_name_new_dataset %>%
      filter(is.na(id_liste_plots))

    id_plotname <-
      vector(mode = "integer", nrow(new_data_renamed))
    for (i in 1:nrow(all_plot_name_new_dataset_no_match)) {
      print(all_plot_name_new_dataset$plot_name[i])
      sorted_matches <-
        .find_similar_string(input = all_plot_name_new_dataset$plot_name[i],
                             compared_table = all_plot_names, column_name = "plot_name")
      print(sorted_matches)
      selected_name <-
        readline(prompt="Choose which name fit (if none enter 0): ")

      selected_name <- as.integer(selected_name)

      if(is.na(selected_name)) stop("Provide integer value for standardizing collector name")

      if(selected_name==0) {
        print(paste(all_plot_name_new_dataset$plot_name[i]," not found"))
      }

      if(selected_name>0) {
        selected_name_id <-
          sorted_matches %>%
          slice(selected_name) %>%
          dplyr::select(id_liste_plots) %>%
          dplyr::pull()

        id_plotname[new_data_renamed$plot_name==all_plot_name_new_dataset_no_match$plot_name[i]] <-
          selected_name_id
      }
    }

    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_liste_plots=id_plotname)

    if(new_data_renamed %>%
       filter(is.na(id_liste_plots), !is.na(plot_name)) %>%
       nrow()>0) {
      print("Plot name not found !!")

    }

    ### importing plots information about specimens
    new_data_renamed <-
      new_data_renamed %>%
      left_join(tbl(mydb, "data_liste_plots") %>%
                  dplyr::select(id_liste_plots, country, ddlat, ddlon, elevation, locality_name, province) %>%
                  dplyr::collect(),
                by=c("id_liste_plots"="id_liste_plots"))

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-id_liste_plots, -plot_name) %>%
      dplyr::rename(locality = locality_name,
                    majorarea = province)

  }



  ### Linking collectors names
  if(!is.null(collector_field)) {
    # data_stand = new_data_renamed
    # collector_field = 1
    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed,
                collector_field = 1)
  }else{
    stop("indicate the field if of collector name for standardizing")
  }


  ## check if not duplicates in new specimens
  dup_imported_datasets <-
    new_data_renamed %>%
    dplyr::select(colnbr, id_colnam, id_new_data) %>%
    mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    dplyr::group_by(combined) %>%
    count() %>%
    filter(n>1)

  if(nrow(dup_imported_datasets)>0) {
    stop("Duplicates in imported dataset")
  }

  # link to table individuals

  unmatched_specimens_from_individuals <-
    .query_unmatched_specimens()

  all_herbarium_individuals_not_linked <-
    unmatched_specimens_from_individuals$all_herbarium_individuals_not_linked

  ## checking for duplicates in unmatched specimens in data individuals
  dup_stand_specimens <-
    all_herbarium_individuals_not_linked %>%
    group_by(colnbr, id_colnam) %>%
    count() %>%
    filter(n>1)

  if(nrow(dup_stand_specimens)) {
    print(dup_stand_specimens)
    warning("duplicates in specimens not linked in individuals table")
  }


  ### comparing id diconames from individuals and specimens
  linked_new_specimens_to_table_individuals <-
    new_data_renamed %>%
    # dplyr::select(colnbr, id_colnam, id_new_data, id_diconame_n) %>%
    mutate(colnbr = as.numeric(colnbr)) %>%
    mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    dplyr::select(combined, id_new_data, id_diconame_n) %>%
    left_join(all_herbarium_individuals_not_linked %>%
                dplyr::select(colnbr, id_colnam, id_diconame_n) %>%
                mutate(colnbr = as.numeric(colnbr)) %>%
                mutate(combined = paste(colnbr, id_colnam, sep="-")),
              by=c("combined"="combined"))

  cat(paste("Matchs from table individuals found for", nrow(linked_new_specimens_to_table_individuals), "specimens", "\n"))

  ## checking if taxo is different between table individuals and new specimens
  ## only select incongruent genus
  unmatch_taxo <-
    linked_new_specimens_to_table_individuals %>%
    dplyr::filter(id_diconame_n.x != id_diconame_n.y) %>%
    left_join(tbl(mydb, "diconame") %>%
                dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
                collect(),
              by=c("id_diconame_n.x"="id_n")) %>%
    left_join(tbl(mydb, "diconame") %>%
                dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
                collect(),
              by=c("id_diconame_n.y"="id_n")) %>%
    filter(tax_gen.x!=tax_gen.y)

  if(nrow(unmatch_taxo)>0) {
    warning("Different genus between new specimens and corresponding matched individuals")
    print(unmatch_taxo)
  }

  ## check if specimens are not already in database
  matched_specimens <-
    tbl(mydb, "specimens") %>%
    dplyr::select(colnbr, id_colnam, id_specimen) %>%
    dplyr::filter(!is.na(id_colnam)) %>%
    collect() %>%
    mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    left_join(new_data_renamed %>%
                dplyr::select(colnbr, id_colnam, id_new_data) %>%
                dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
                dplyr::select(combined, id_new_data)) %>%
    dplyr::filter(!is.na(id_new_data))

  if(nrow(matched_specimens)>0) {
    warning(paste("New specimens already in database", nrow(matched_specimens)))
    print(matched_specimens)
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
                       data_modif_m=lubridate::month(Sys.Date()),
                       data_modif_y=lubridate::year(Sys.Date()))

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-col_name, -id_new_data)

  if(launch_adding_data) {

    print(list(new_data_renamed, logs))

    confirmed <- utils::askYesNo("Confirm adding?")

    if(confirmed) DBI::dbWriteTable(mydb, "specimens", new_data_renamed, append = TRUE, row.names = FALSE)

  }

  return(list(new_data_renamed, logs))

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


  print(queried_speci %>%
          dplyr::select(family_name, surname, colnbr, detd, detm, dety, detby, cold, colm, coly, country, id_specimen))

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
    if(any(comp_values %>% dplyr::select_if(~sum(.) > 0) %>% colnames()=="id_good_diconame")) {
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

    confirmed <- utils::askYesNo("Confirm this update?")

    if(confirmed) {
      if(add_backup) {

        colnames_speci <-
          dplyr::tbl(mydb, "followup_updates_specimens")  %>% dplyr::select(-date_modified, -modif_type, -id_fol_up_specimens) %>%
          dplyr::top_n(1) %>%  dplyr::collect() %>% colnames()

        queried_speci <-
          queried_speci %>%
          dplyr::select(dplyr::one_of(colnames_speci))

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




#' Process trimble data
#'
#' Process raw trimble data and provide individuals data ready to import and log file with potential problems
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param path string path to trimble data
#' @param plot_name string plot name to give to plot
#'
#' @return A list with two tibbles
#' @export
process_trimble_data <- function(path =NULL, plot_name = NULL, heights_data=NULL) {
  all_dirs <-
    list.dirs(path = PATH,
              full.names = FALSE, recursive = FALSE)

  all_dirs <-
    list.dirs(path = PATH,
              full.names = FALSE, recursive = FALSE)


  regexp <- "[[:digit:]]+"

  files_prop <-
    tibble(dirs = all_dirs,
           number = as.numeric(str_extract(all_dirs, regexp)),
           gps = grepl("GPS", all_dirs))

  count_nbe_file <-
    files_prop %>%
    group_by(number) %>%
    count()
  if(any(count_nbe_file$n!=2)) stop("some plot do not have two directories!")

  ncol_list <- list()
  nbe_herb <- list()


  all_plot_list <- list()
  logs_process <- list()
  for (j in sort(unique(files_prop$number))) {
    logs_tb <-
      tibble(plot_name = as.character(), field = as.character(), issue = as.character(), id = as.character())
    # print(j)
    select_plot_dir <-
      files_prop %>%
      filter(number==j)

    occ_data_dir <- select_plot_dir %>%
      filter(gps==FALSE) %>%
      dplyr::select(dirs) %>%
      pull()

    gps_data_dir <-
      select_plot_dir %>%
      filter(gps==TRUE) %>%
      dplyr::select(dirs) %>%
      pull()

    print(occ_data_dir)

    data. <-
      list.files(path = paste0(PATH,
                               occ_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")
    data. <- data.[grep(".xlsx", data.)]

    if(length(data.)==0) stop("not xlsx file in directory")

    occ_data <-
      readxl::read_excel(data.)

    # check ID
    dupl_id <-
      occ_data %>%
      filter(duplicated(.[["ID"]]))

    if(nrow(dupl_id)>0) {
      stop(paste("id dupliqué", "plot", j))
      print(dupl_id)
    }

    if(occ_data$ID[1]!=1) {
      corrected_id <- vector(mode = "numeric", length = nrow(occ_data))
      for (m in 1:nrow(occ_data)) corrected_id[m] <- occ_data$ID[m]-occ_data$ID[1]+1

      logs_tb <- bind_rows(logs_tb,
                           tibble(plot_name = paste0(plot_name,ifelse(j<10, "00", "0") ,j),
                                  field = "ID",
                                  issue = "ID not starting at 1",
                                  id = "all"))
      occ_data$ID <- corrected_id

      message("replacing ID by continuous ID")
    }

    ## counting number of columns
    ncol_list[[j]] <-
      ncol(occ_data)

    ## counting of herbarium field different of No
    nbe_herb[[j]] <-
      sum(occ_data$Herbarium_!="No")

    ## counting pIRD different of empty
    nbe_herb[[j]] <-
      sum(!is.na(occ_data$pIRD))

    ## quadrat identification
    if(sum(occ_data$New_quadra[!is.na(occ_data$New_quadra)]=="yes")!=25) {
      warning(paste("25 quadrat expected but", sum(occ_data$New_quadra=="yes"),"found"))
    }

    all_expected_combination <-
      expand.grid(seq(0, 80, 20),seq(0, 80, 20)) %>%
      as_tibble() %>%
      mutate(quadrat = paste(Var1, Var2, sep="_"))

    all_quadrat <-
      occ_data %>%
      filter(New_quadra=="yes") %>%
      dplyr::select(ID, New_quadra, X_theo, Y_theo) %>%
      mutate(quadrat =paste(X_theo, Y_theo, sep="_"))

    missing_quadrats <-
      all_expected_combination %>%
      left_join(all_quadrat, by=c("quadrat"="quadrat")) %>%
      filter(is.na(ID)) %>%
      dplyr::select(quadrat) %>%
      pull()

    if(length(missing_quadrats)>0) {
      print("missing quadrat")
      print(missing_quadrats)
    }

    duplicates_quadrat <-
      all_quadrat %>%
      group_by(quadrat) %>%
      count() %>%
      filter(n>1)

    if(nrow(duplicates_quadrat)>0) {
      all_quadrat %>%
        filter(quadrat %in% duplicates_quadrat$quadrat) %>%
        print()

      ### ### ### ### ### ### ### ### ### ### ### ###
      ### correcting quadrat names ### ### ### ### ###
      ### ### ### ### ### ### ### ### ### ### ### ###
      path <- NULL
      quad1 <-
        all_quadrat %>%
        slice(1)

      quad2 <-
        all_quadrat %>%
        slice(2)

      if(quad1$X_theo==quad2$X_theo) path <- "vertical"
      if(quad1$Y_theo==quad2$Y_theo) path <- "horizontal"
      if(is.null(path)) stop("quadrat progression not logical")

      Y <-
        c(seq(0, 80, 20), rev(seq(0, 80, 20)), c(seq(0, 80, 20), rev(seq(0, 80, 20))),
          seq(0, 80, 20))
      X <-
        c(rep(0, 5), rep(20, 5), rep(40, 5), rep(60, 5), rep(80, 5))

      if(path=="horizontal") {
        expected_quadrats <-
          tibble(x = Y, y= X) %>%
          mutate(quadrat = paste(x, y, sep="_"))
      }
      if(path=="vertical") {
        expected_quadrats <-
          tibble(x = X, y= Y) %>%
          mutate(quadrat = paste(x, y, sep="_"))
      }

      quadrat_corrected <- vector(mode = "character", length = nrow(all_quadrat))
      for (k in 1:nrow(all_quadrat)) {
        quadrat_found <-
          all_quadrat %>%
          slice(k) %>%
          pull(quadrat)
        quadrat_expected <-
          expected_quadrats %>%
          slice(k) %>%
          pull(quadrat)
        quadrat_found
        quadrat_expected
        if(quadrat_found!=quadrat_expected) {
          message(paste("replacing observed quadrat", quadrat_found, "by", quadrat_expected))
          quadrat_corrected[k] <- quadrat_expected

          ids <-
            all_quadrat %>%
            slice(k) %>% pull(ID)
          ids <- paste(ids, collapse = ", ")

          logs_tb <- bind_rows(logs_tb,
                               tibble(plot_name = paste0(plot_name,ifelse(j<10, "00", "0") ,j),
                                      field = "New_quadra",
                                      issue = "error in quadrat automatically corrected",
                                      id = ids))

        }else(
          quadrat_corrected[k] <- quadrat_found
        )
      }

      if(length(unique(quadrat_corrected))!=25) stop("still not 25 unique quadrat name")

      all_quadrat <-
        all_quadrat %>%
        add_column(quadrat_corrected = quadrat_corrected)

      occ_data <-
        occ_data %>%
        left_join(all_quadrat %>%
                    dplyr::select(ID, quadrat_corrected), by=c("ID"="ID"))

    }

    if(any(colnames(all_quadrat)=="quadrat_corrected"))
      all_quadrat <-
      all_quadrat %>%
      rename(quadrat_good_format = quadrat_corrected)

    # add_column(quadrat_good_format = unlist(lapply(strsplit(x = all_quadrat$quadrat_corrected, split = "_"), FUN = function(x) paste("(", x[1], ";", x[2], ")",sep=""))))

    if(!any(colnames(all_quadrat)=="quadrat_corrected"))
      all_quadrat <-
      all_quadrat %>%
      rename(quadrat_good_format = quadrat)
    # add_column(quadrat_good_format = unlist(lapply(strsplit(x = all_quadrat$quadrat, split = "_"), FUN = function(x) paste("(", x[1], ";", x[2], ")",sep=""))))

    quadrats <-
      vector(mode = "character", length = nrow(occ_data))
    for (k in 1:(nrow(all_quadrat))) {
      if(k==25) {
        quadrats[all_quadrat$ID[k]:nrow(occ_data)] <-
          all_quadrat$quadrat_good_format[k]
      }else{
        quadrats[all_quadrat$ID[k]:all_quadrat$ID[k+1]] <-
          all_quadrat$quadrat_good_format[k]
        # print(all_quadrat$quadrat_good_format[k])
      }
    }

    occ_data <-
      occ_data %>%
      add_column(quadrat = quadrats)

    ### adding plot name
    occ_data <-
      occ_data %>%
      add_column(plot_name = rep(paste0(plot_name,ifelse(j<10, "00", "0") ,j),
                                 nrow(occ_data)))

    ### rename and check dbh
    occ_data <-
      occ_data %>%
      rename(dbh = DBH)
    if(any(occ_data$dbh>400)) stop("unrealistic dbh value")
    if(any(is.na(occ_data$dbh))) stop("missing dbh value")

    ### rename and check DBH_height
    if(!any(colnames(occ_data)=="dbh_height")) {
      occ_data <-
        occ_data %>%
        rename(dbh_height = DBH_height)
    }

    if(any(is.na(occ_data$dbh_height))) {
      print(occ_data %>%
              filter(is.na(dbh_height)))
      message("missing dbh_height value")

      ids <-
        occ_data %>%
        filter(is.na(dbh_height)) %>%
        pull(ID)
      ids <- paste(ids, collapse = ", ")

      logs_tb <-
        logs_tb %>%
        bind_rows(tibble(plot_name = paste0(plot_name,ifelse(j<10, "00", "0") ,j),
                         field = "dbh_height",
                         issue = "missing values",
                         id = ids))

    }
    if(any(occ_data$dbh_height[!is.na(occ_data$dbh_height)]>20)) stop("unrealistic dbh_height value")

    ### rename and check original taxa name
    occ_data <-
      occ_data %>%
      mutate(original_tax_name = ifelse(!is.na(Species), Species, ifelse(!is.na(Genus), Genus, Family))) %>%
      mutate(original_tax_name = ifelse(!is.na(original_tax_name), original_tax_name, Identif_co))

    ### multistem check
    multi_stem_ind <-
      occ_data %>%
      dplyr::select(ID, Rainfor_Tr, original_tax_name) %>%
      filter(grepl("multiple", Rainfor_Tr))

    ## identify and check coherence of multi stem
    list_multiple_stem <- list()
    if(nrow(multi_stem_ind)>0) {

      for (k in 1:nrow(multi_stem_ind)) {
        # print(k)
        if(length(multi_stem_chunk)>0) {ids <- unlist(lapply(list_multiple_stem, function(w) w$ID))
        }else{ids <- 0}

        if(!multi_stem_ind$ID[k] %in% ids) {

          ### screnning of multiple stem : check for ID tag continuity and taxa similarity. Stop when one is not true
          q <- 1
          found <- TRUE
          add_tag <- NULL
          while(found) {
            ## if ID tag continuity is true
            if(multi_stem_ind$ID[k]+q==multi_stem_ind$ID[ifelse((k+q)>nrow(multi_stem_ind), nrow(multi_stem_ind), k+q)]) {
              q <- q +1

              multi_stem_chunk <-
                multi_stem_ind %>%
                slice(k:(k+q-1))

              if(length(unique(multi_stem_chunk$original_tax_name))>1) {
                dist. <-
                  .pairwise_string_similarity(string_vector =
                                                multi_stem_chunk$original_tax_name[!is.na(multi_stem_chunk$original_tax_name)])
                dist.[is.na(dist.)] <- 1
              }else{
                dist. <- 1
              }

              if(any(dist.<0.8)) {
                found <- FALSE
                q <- q -1

                if(multi_stem_ind %>%
                   slice(k:(k+q-1)) %>%
                   nrow()==1) {

                  message("multiple stem chunk with very different identification")

                  ### putting into log the problems
                  ids <-
                    multi_stem_chunk %>%
                    pull(ID)

                  ids <- paste(ids, collapse = ", ")

                  logs_tb <-
                    logs_tb %>%
                    bind_rows(tibble(plot_name = paste0(plot_name,ifelse(j<10, "00", "0") ,j),
                                     field = "multistem",
                                     issue = "different identification",
                                     id = ids))


                }
              }

            }else{
              if(q==1) {

                ## check if following Tag is same identification and has not been put into multiple stem
                next_tag <-
                  occ_data %>%
                  filter(ID == multi_stem_ind$ID[k]+1) %>%
                  dplyr::select(ID, Rainfor_Tr, original_tax_name)

                dist. <-
                  .pairwise_string_similarity(string_vector = c(next_tag$original_tax_name, multi_stem_ind$original_tax_name[k]))
                dist.[is.na(dist.)] <- 1

                if(dist.<0.8) {
                  print(multi_stem_ind %>%
                          slice(k))
                  print(next_tag)
                  message("expected following tag in multiple stem and next individual of different taxa")

                  found <- FALSE

                  ### putting into log the problems
                  ids <-
                    multi_stem_ind %>%
                    slice(k) %>%
                    bind_rows(next_tag) %>%
                    pull(ID)

                  ids <- paste(ids, collapse = ", ")

                  logs_tb <-
                    logs_tb %>%
                    bind_rows(tibble(plot_name = paste0(plot_name,ifelse(j<10, "00", "0") ,j),
                                     field = "multistem",
                                     issue = "expected following tag in multiple stem and next individual of different taxa",
                                     id = as.character(ids)))

                }else{
                  add_tag <-
                    next_tag
                  found <- FALSE
                }

              }else{
                found <- FALSE
              }
            }
          }

          multi_stem_chunk <-
            multi_stem_ind %>%
            slice(k:(k+q-1))

          if(!is.null(add_tag)) multi_stem_chunk <-
            bind_rows(multi_stem_chunk, add_tag)

          if(nrow(multi_stem_chunk)>1) {
            list_multiple_stem[[length(list_multiple_stem)+1]] <-
              multi_stem_chunk
          }
        }
      }
    }

    multi_tiges_id <- vector(mode = "numeric", length = nrow(occ_data))
    if(length(list_multiple_stem)>0) {
      for (k in 1:length(list_multiple_stem)) {

        multi_stem_chunk <-
          list_multiple_stem[[k]]
        for (d in 2:nrow(multi_stem_chunk))
          multi_tiges_id[occ_data$ID==multi_stem_chunk$ID[d]] <-
            multi_stem_chunk$ID[1]
      }
    }

    multi_tiges_id <-
      replace(multi_tiges_id, multi_tiges_id==0, NA)

    occ_data <-
      occ_data %>%
      add_column(multi_tiges_id = multi_tiges_id)

    ### rename observations
    occ_data <-
      occ_data %>%
      rename(observation = Observatio)

    ### rename TAG number
    occ_data <-
      occ_data %>%
      rename(id = ID)

    if(any(occ_data$Total_heig>0)) stop("height values different of 0")
    if(any(occ_data$H_branch>0)) stop("branch values different of 0")

    all_plot_list[[j]] <-
      occ_data %>%
      dplyr::select(plot_name, id, dbh, original_tax_name, quadrat, multi_tiges_id, observation)

    logs_process[[j]] <- logs_tb

  }

  final_output <-
    bind_rows(all_plot_list)

  final_logs <-
    bind_rows(logs_process)

  if(!is.null(heights_data)) {




  }



  writexl::write_xlsx(final_output, paste("final_process_plot_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_logs, paste("final_process_plot_logs_", plot_name,".xlsx", sep = ""))


  return(list(final_output, final_logs))
}


