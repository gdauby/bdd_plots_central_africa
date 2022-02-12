

#' Launch shiny app for taxonomic standardization
#'
#' Tool for standardizing taxonomy according to the taxonomic backbone (step needed before importing data to databases)
#'
#' @return Launch in web browser the app
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @import shiny
#' @importFrom shinybusy use_busy_spinner show_spinner hide_spinner
#' @export
launch_stand_tax_app <- function() {

  app <- list(ui = ui <- fluidPage(
    titlePanel("Standardisation de noms d'especes vegetales pour l'Afrique tropicale - correction des erreurs orthographiques et homogeneisation de la synonymie"),

    # shinybusy::use_busy_bar(color = "#01DF01", height = "15px"),
    shinybusy::use_busy_spinner(spin = "fading-circle"),

    sidebarPanel(
      fileInput(inputId = "data1",
                label = "Ajoutez votre fichier de donnees - format excel (xlsx"),

      uiOutput("Box_match"),

      uiOutput("Box4"),

      uiOutput("Box5"),

      h4("   "),

      # radioButtons("dico.choix", "Utiliser le dictionnaire par defaut",
      #              c("oui","choisir un autre dictionnaire"),
      #              selected = "oui"),
      # textInput("champ.nom", "Quel est le nom du champs contenant les noms a standardiser?",
      #              value = "Code"),
      uiOutput("Dico")
      # fileInput(inputId = "data2", label = "Ajoutez votre dictionnaire - format csv")
    ),

    mainPanel(
      h4("Avancement et nombre de noms sans correspondance"),
      htmlOutput("summary"),

      verbatimTextOutput("test"),

      h4("Observations"),
      verbatimTextOutput("view"),

      h4("Propostion de correspondance par ressemblance orthographique"),
      verbatimTextOutput("list.match"),

      uiOutput("Box7"),

      uiOutput("Box9"),

      # uiOutput("BoxChoixNom"),

      uiOutput("Box1"),

      uiOutput("Box2"),

      uiOutput("Box3"),

      uiOutput("Box10"),

      uiOutput("Box6"),

      uiOutput("Box8"),

      h4("Nom choisi"),

      tableOutput("list.sp.m"),

      tableOutput("table_chosen_sp"),

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

  server = function(input, output, session) {

    options(shiny.maxRequestSize=30*1024^2)

    # stop the serveur in the end of the session
    session$onSessionEnded(function() {
      stopApp()
    })

    # options(shiny.maxRequestSize=30*1024^2)

    # if(!exists("mydb")) call.mydb()

    val.nom.chosen <- reactiveValues(n = 1) ### Pointeur pour le nom a standardiser
    data.to.standardize.reac <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus qui sera mise a jour
    original.data <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus originale (pas mise a jour)
    original.list.name <- reactiveValues(df = NULL) ### reactive value destine a stocker un vecteur contenant la liste des noms original
    stand.list.name <- reactiveValues(df = NULL) ### vecteur des noms devant etre standardise
    list.match.reac <- reactiveValues(df = NULL)

    test_dat <- reactiveValues(df = NULL)

    DicoNames <- reactiveValues(df = NULL) ### reactive value destine a stocker la table des individus


    observeEvent(input$data1, {

      test_dat$df <- input$data1$datapath

      # file.rename(input$data1$datapath,
      #             paste(input$data1$datapath, ".xlsx", sep=""))

      # DATA <- readxl::read_xlsx("D:/MonDossierR/database.transects/individuals_to_be_added/new_individuals_IDU.xlsx", sheet =  1)

      DATA <- readxl::read_xlsx(input$data1$datapath, sheet =  1)


      # DATA <- readxl::read_excel(paste(input$data1$datapath, ".xlsx", sep=""), 1)

      # DATA <- readxl::read_excel(input$data1$datapath, 1)


      if(!any(colnames(DATA) == "id_data")) {
        DATA <-
          DATA %>%
          tibble::add_column(id_data = seq(1, nrow(DATA), 1))
      }

      ### Save table des individus original
      original.data$df <- DATA

      shinybusy::show_spinner()

      data(table_taxa_tb)
      DicoNames1 <-
        table_taxa_tb %>%
        mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
        mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                        paste0(tax_gen,
                                               " ",
                                               tax_esp,
                                               ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                               ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                               ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                               ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                        NA)) %>%
        mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                             paste0(tax_gen,
                                                    " ",
                                                    tax_esp,
                                                    ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                    ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                    ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                    ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                    ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                    ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                    ifelse(!is.na(author3), paste0(" ", author3), "")),
                                             NA))

      # DicoNames1 <-
      #   dplyr::tbl(mydb, "table_taxa") %>%
      #   mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
      #   mutate(tax_infra_level = ifelse(!is.na(tax_esp),
      #                                   paste0(tax_gen,
      #                                          " ",
      #                                          tax_esp,
      #                                          ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
      #                                          ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
      #                                          ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
      #                                          ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
      #                                   NA)) %>%
      #   mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
      #                                        paste0(tax_gen,
      #                                               " ",
      #                                               tax_esp,
      #                                               ifelse(!is.na(author1), paste0(" ", author1), ""),
      #                                               ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
      #                                               ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
      #                                               ifelse(!is.na(author2), paste0(" ", author2), ""),
      #                                               ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
      #                                               ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
      #                                               ifelse(!is.na(author3), paste0(" ", author3), "")),
      #                                        NA)) %>%
      #   dplyr::collect()

      DicoNames$df <- DicoNames1

      # shinybusy::update_busy_bar(100)
      shinybusy::hide_spinner()


    })

    ### Choix de la colonne de la table originale a standardiser
    output$Box_match = renderUI({
      req(input$data1)

      checkboxInput("authors", "Matching with authors name ?", TRUE)

    })

    ### Choix de la colonne de la table originale a standardiser
    output$Box4 = renderUI({
      req(input$data1)

      DATA.col <- original.data$df
      colnames(DATA.col) <- seq(1, ncol(original.data$df), 1)

      selectInput("champ.nom",
                  "Selectionner le nom de la colonne a standardiser",
                  c(colnames(original.data$df[, as.numeric(colnames(dplyr::select_if(DATA.col, is.character)))])))


    })



    observeEvent(input$champ.nom, { #  input$data1

      DATA <- original.data$df

      champ.nom <-
        input$champ.nom

      original.list.name$df <-
        DATA %>%
        dplyr::distinct(!!rlang::sym(champ.nom))


      ### Ajout de colonnes destinees a la mise a jour
      if (!any(colnames(DATA) == "found.name")) {
        ## Ajout de found.name si pas deja present

        DATA <- DATA %>% dplyr::mutate(found.name = "")

      }

      if(!any(colnames(DATA)=="ID.dico.name")) { ## Ajout de ID.dico.name si pas deja present

        DATA <- DATA %>% dplyr::mutate(ID.dico.name = 0)

      }else{

        DATA <- DATA %>%
          dplyr::mutate(ID.dico.name = as.numeric(ID.dico.name))

      }

      if(!any(colnames(DATA)=="ID.dico.name.good")) { ## Ajout de ID.dico.name.good si pas deja present

        DATA <- DATA %>% dplyr::mutate(ID.dico.name.good = 0)

      } else{

        DATA <- DATA %>%
          dplyr::mutate(ID.dico.name.good = as.numeric(ID.dico.name.good))

      }

      if(!any(colnames(DATA) == "corrected.name")) { ## Ajout de corrected.name si pas deja present

        DATA <- DATA %>% dplyr::mutate(corrected.name = "")

      }

      if (any(colnames(DATA) == "found.name")) {
        if (length(which(is.na(DATA$found.name))) > 0)
          DATA$found.name[which(is.na(DATA$found.name))] <-
            "" ### Si des observations de found.name sont en NA, les mettre en ""
      }

      ### Procedure de matching des noms --> noms correspondant a des noms du referentiel

      # select code and id_data
      DATA.selected <-
        DATA %>%
        dplyr::select(!!rlang::sym(champ.nom), id_data) %>%
        dplyr::rename(Code =!!rlang::sym(champ.nom)) %>%
        dplyr::filter(!is.na(Code))

      # matching based on full_name_no_auth

      if(!input$authors)
        join.table <-
        DATA.selected %>%
        dplyr::left_join(DicoNames$df %>%
                           dplyr::select(idtax_n, tax_infra_level),
                         by=c("Code" = "tax_infra_level"))

      if(input$authors)
        join.table <-
        dplyr::left_join(DATA.selected,
                         dplyr::select(DicoNames$df, idtax_n, tax_infra_level_auth),
                         by=c("Code" = "tax_infra_level_auth"))


      ### Matching with genus for those missing
      join.table_not_match <-
        join.table %>%
        dplyr::filter(is.na(idtax_n)) %>%
        dplyr::select(-idtax_n)

      join.table_genus <-
        join.table_not_match %>%
        left_join(DicoNames$df %>%
                    dplyr::filter(is.na(tax_esp), !is.na(tax_gen)) %>%
                    dplyr::select(idtax_n,
                                  idtax_good_n,
                                  tax_gen),
                  by = c("Code" = "tax_gen")) %>%
        filter(!is.na(idtax_n))

      ## Matching with family for those missing
      join.table_not_match <-
        join.table %>%
        dplyr::filter(is.na(idtax_n)) %>%
        dplyr::select(-idtax_n)

      join.table_family <-
        join.table_not_match %>%
        left_join(DicoNames$df %>%
                    dplyr::filter(is.na(tax_esp), is.na(tax_gen), !is.na(tax_fam)) %>%
                    dplyr::select(idtax_n,
                                  idtax_good_n,
                                  tax_fam),
                  by = c("Code" = "tax_fam")) %>%
        filter(!is.na(idtax_n))



      join.table <-
        join.table %>%
        left_join(join.table_genus %>%
                    dplyr::select(idtax_n, id_data) %>%
                    dplyr::rename(idtax_n_genus = idtax_n),
                  by = c("id_data" = "id_data")) %>%
        dplyr::mutate(idtax_n = ifelse(is.na(idtax_n), idtax_n_genus, idtax_n)) %>%
        dplyr::select(-idtax_n_genus)

      join.table <-
        join.table %>%
        left_join(join.table_family %>%
                    dplyr::select(idtax_n, id_data) %>%
                    dplyr::rename(idtax_n_fam = idtax_n),
                  by = c("id_data" = "id_data")) %>%
        dplyr::mutate(idtax_n = ifelse(is.na(idtax_n), idtax_n_fam, idtax_n)) %>%
        dplyr::select(-idtax_n_fam)

      join.table <-
        join.table %>%
        left_join(DicoNames$df %>%
                    dplyr::select(idtax_n, idtax_good_n, tax_gen, tax_infra_level, tax_infra_level_auth),
                  by = c("idtax_n" = "idtax_n"))


      # if more than one matching by name, take the first
      join.table <-
        join.table %>%
        dplyr::group_by(id_data) %>%
        dplyr::summarise(idtax_n = dplyr::first(idtax_n),
                         idtax_good_n = dplyr::first(idtax_good_n),
                         Code = dplyr::first(Code))

      # excluding names with no matching
      join.table_sel <-
        join.table %>%
        dplyr::filter(!is.na(idtax_n)) %>%
        dplyr::select(Code, id_data, idtax_n, idtax_good_n)

      # combining matched names with DATA, adding found.name
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, Code, id_data),
                         by=c("id_data"="id_data")) %>%
        dplyr::mutate(found.name = Code) %>%
        dplyr::select(-Code)

      # adding ID.dico.name
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, idtax_n, id_data),
                         by = c("id_data" = "id_data")) %>%
        dplyr::mutate(ID.dico.name = ifelse(is.na(idtax_n), ID.dico.name, idtax_n)) %>%
        dplyr::select(-idtax_n)

      # adding ID.dico.name.good
      DATA <-
        DATA %>%
        dplyr::left_join(dplyr::select(join.table_sel, idtax_good_n, id_data),
                         by = c("id_data" = "id_data")) %>%
        dplyr::mutate(ID.dico.name.good = ifelse(is.na(idtax_good_n), ID.dico.name.good, idtax_good_n)) %>%
        dplyr::select(-idtax_good_n)

      # finding good names
      join.corrected.names <-
        dplyr::left_join(
          DATA %>%
            dplyr::select(ID.dico.name.good),
          DicoNames$df %>% dplyr::select(idtax_n, tax_infra_level),
          by = c("ID.dico.name.good" = "idtax_n")
        )

      # adding good names
      DATA$corrected.name <-
        join.corrected.names$tax_infra_level

      # adding detvalue
      # if (!any(colnames(DATA) == "detvalue")) {
      #   DATA <-
      #     DATA %>%
      #     tibble::add_column(detvalue = rep(0, nrow(.)))
      # }

      # %>%
      #   dplyr::mutate(detvalue=as.integer(detvalue))

      test <-
        DATA %>%
        dplyr::filter(ID.dico.name == 0) %>%
        dplyr::distinct(!!rlang::sym(champ.nom), found.name) %>%
        dplyr::filter(!is.na(!!rlang::sym(champ.nom))) %>%
        dplyr::arrange(!!rlang::sym(champ.nom))

      if (nrow(test) > 0) {

        test <- test %>%
          dplyr::mutate(id_tax_search = seq(1, nrow(.), 1)) %>%
          dplyr::mutate(id_tax_search = as.character(id_tax_search))

      } else {

        test <- test %>%
          dplyr::mutate(id_tax_search = "")
      }
#
#       test <- DATA %>%
#         dplyr::filter(ID.dico.name == 0) %>%
#         dplyr::distinct(`Code -T`, found.name) %>%
#         dplyr::filter(!is.na(`Code -T`)) %>%
#         dplyr::arrange(`Code -T`)

      DATA <-
        DATA %>%
        dplyr::left_join(test %>%
                           dplyr::select(!!rlang::sym(champ.nom), id_tax_search))

      # DATA <-
      #   DATA %>%
      #   dplyr::left_join(test %>%
      #                      dplyr::select(`Code -T`, id_tax_search))

      test <-
        test %>%
        dplyr::rename(Code = 1)

      stand.list.name$df <-
        test %>%
        dplyr::select(Code, id_tax_search) %>%
        dplyr::filter(!is.na(Code))

      data.to.standardize.reac$df <-
        DATA

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

          radioButtons("nbe_choice", "Nombre de noms similaires propose",
                       c("Top 10", "Top 20", "Top 30"),
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
        # if(input$dico.choix!="oui")
          # req(input$data2)

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
                       c("Noms sans auteurs", "Noms avec auteurs"),
                       selected = ifelse(input$authors, "Noms avec auteurs", "Noms sans auteurs") )
        }
      })

      output$Box10 = renderUI( {
        if (is.null(input$sector) || input$sector == "Selectionner un nom a standardiser") {
          return()

        }else{

          radioButtons("choice.kind3", "Afficher les taxas infra-specifiques",
                       c("oui", "non"),
                       selected = "oui")

        }
      })



      output$Box6 = renderUI( {
        if (is.null(input$sector) || input$sector == "Choisir") {
          return()
        }else{
          radioButtons("choice.kind2", "Chercher une correspondance dans",
                       c("les taxa", "les genres", "les familles", "les classes"), #
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
          # if(input$dico.choix!="oui")
            # req(input$data2)

          # Name1 <- original.list.name$df[as.numeric(input$sector)]
          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
            dplyr::pull(Code)


          if(input$choice.kind == "Noms sans auteurs" & input$choice.kind2 == "les taxa") {

            if(input$choice.kind3 == "oui") {

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames$df$tax_infra_level))

            }

            if(input$choice.kind3 == "non") {

              subset_dico <-
                DicoNames$df %>%
                dplyr::filter(is.na(tax_rank01) & is.na(tax_nam01))

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(subset_dico$tax_infra_level))
            }
          }


          if(input$choice.kind == "Noms avec auteurs" & input$choice.kind2 == "les taxa") {

            if(input$choice.kind3 == "oui") {

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames$df$tax_infra_level_auth))

            }

            if(input$choice.kind3 == "non") {

              subset_dico <-
                DicoNames$df %>%
                dplyr::filter(is.na(tax_rank01) & is.na(tax_nam01))

              dist. <-
                RecordLinkage::levenshteinSim(tolower(Name1), tolower(subset_dico$tax_infra_level_auth))
            }


          }

          # dist. <-
          # RecordLinkage::levenshteinSim(tolower(Name1), tolower(DicoNames1$tax_infra_level_auth))

          if(input$choice.kind2 == "les genres") {

            dico_genus <-
              DicoNames$df %>%
              dplyr::filter(is.na(tax_esp))

            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_genus$tax_gen))

          }

          if(input$choice.kind2 == "les familles") {

            dico_family <-
              DicoNames$df %>%
              dplyr::filter(is.na(tax_esp), is.na(tax_gen), !is.na(tax_fam))

            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_family$tax_fam))

          }

          if(input$choice.kind2 == "les classes") {

            dico_classes <-
              DicoNames$df %>%
              dplyr::filter(is.na(tax_esp), is.na(tax_fam), is.na(tax_order)) %>%
              dplyr::filter(!is.na(tax_famclass))

            dist. <-
              RecordLinkage::levenshteinSim(tolower(Name1), tolower(dico_classes$tax_famclass))

          }

          if (input$nbe_choice == "Top 10")
            nbe.match <-
            10 ### Nombre de proposition pour la correspondance a afficher
          if (input$nbe_choice == "Top 20")
            nbe.match <-
            20 ### Nombre de proposition pour la correspondance a afficher
          if (input$nbe_choice == "Top 30")
            nbe.match <-
            30 ### Nombre de proposition pour la correspondance a afficher
          # if (input$nbe_choice == "Tous")
          #   nbe.match <-
          #   length(dist.[!is.na(dist.)]) ### Nombre de proposition pour la correspondance a afficher

          # test_dat$df <- dico_genus[order(dist., decreasing = T),]

          if (input$choice.kind2 == "les genres") {

            matches. <-
              dico_genus[order(dist., decreasing = T)[1:nbe.match],]

            # if(input$sort=="alphabetique")

          } else {

            if (input$choice.kind2 == "les familles") {

              # if(nrow(dico_family) < nbe.match)
              #   nbe.match_class <- nrow(dico_family)

              matches. <-
                dico_family[order(dist., decreasing = T)[1:nbe.match],]

            } else {
              if (input$choice.kind2 == "les classes") {

              if(nrow(dico_classes) < nbe.match) {
                nbe.match_class <- nrow(dico_classes)

              } else {
                nbe.match_class <- nbe.match
              }

              matches. <-
                dico_classes[order(dist., decreasing = T)[1:nbe.match_class],]

            } else {

              if (input$choice.kind3 == "oui") {

                matches. <-
                  DicoNames$df[order(dist., decreasing = T)[1:nbe.match],]

              }

              if(input$choice.kind3 == "non") {

                subset_dico <-
                  DicoNames$df %>%
                  dplyr::filter(is.na(tax_rank01) & is.na(tax_nam01))

                matches. <-
                  subset_dico[order(dist., decreasing = T)[1:nbe.match],]

              }
            }
            }

          }

          matches. <-
            dplyr::select(matches., tax_infra_level,
                          tax_infra_level_auth,
                          idtax_n, idtax_good_n, tax_fam, tax_gen, tax_esp, tax_famclass)

          if (input$choice.kind == "Noms sans auteurs" &
              input$choice.kind2 == "les taxa") {

            selected.field <- "tax_infra_level"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n)
            # list.match <- c(matches.[,c(selected.field)])

          }

          if (input$choice.kind == "Noms avec auteurs" &
              input$choice.kind2 == "les taxa") {
            selected.field <- "tax_infra_level_auth"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n)
            # list.match <- c(matches.[,c(selected.field)])
          }


          if(input$choice.kind2 == "les genres") {
            selected.field <- "tax_gen"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$choice.kind2 == "les familles") {
            selected.field <- "tax_fam"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$choice.kind2 == "les classes") {
            selected.field <- "tax_famclass"
            list.match <-
              matches. %>%
              dplyr::select(!!rlang::sym(selected.field), idtax_n, tax_esp)
            # list.match <- c(unique(matches.[,c(selected.field)]))
          }

          if(input$sort == "alphabetique")
            list.match <-
            list.match %>%
            arrange(!!rlang::sym(selected.field))

          list.match.reac$df <- list.match

          id.names <- as.list(seq(1, nrow(list.match.reac$df), 1))
          names(id.names) <- enc2utf8(dplyr::pull(list.match.reac$df, 1))

          # Encoding(names(id.names)) <-  "UTF-8"

          selectInput("stock",
                      "Choisir le nom correct si present",
                      choices = id.names,
                      selected = 1)



        }
      })


      output$Action1 = renderUI({
        if (is.null(input$stock) ||
            input$stock == "Choisir le nom correct si present") {
          return()

        } else {
          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)

          actionButton(inputId = "confirm.name", label = paste("Choisir", Name2))

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
        # if(input$dico.choix!="oui") req(input$data2)

        if (is.null(input$stock) || input$stock == " ") {
          return()
        }else{

          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
            dplyr::pull(Code)

          id_Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(2)

          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)


          if (input$choice.kind == "Noms sans auteurs" &
              input$choice.kind2 == "les taxa")
            selected.field <- "tax_infra_level"
          if (input$choice.kind == "Noms avec auteurs" &
              input$choice.kind2 == "les taxa")
            selected.field <- "tax_infra_level_auth"
          if (input$choice.kind2 == "les genres")
            selected.field <- "tax_gen"
          if (input$choice.kind2 == "les familles")
            selected.field <- "tax_fam"
          if (input$choice.kind2 == "les classes")
            selected.field <- "tax_famclass"

          test_syn <-
            DicoNames$df %>%
            dplyr::filter(idtax_n == id_Name2) %>%
            dplyr::select(idtax_n, idtax_good_n)

          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n != test_syn$idtax_good_n)
            syn_check <- TRUE
          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n == test_syn$idtax_good_n)
            syn_check <- FALSE
          if(is.na(test_syn$idtax_good_n)) syn_check <- FALSE

          if(syn_check) {

            id_good <-
              DicoNames$df %>%
              # filter(!!rlang::sym(selected.field)==Name2) %>%
              dplyr::filter(idtax_n == !!id_Name2) %>%
              dplyr::select(idtax_good_n) %>%
              dplyr::pull()

            good_name <-
              DicoNames$df %>%
              dplyr::filter(idtax_n == !!id_good) %>%
              dplyr::select(tax_infra_level_auth) %>%
              dplyr::pull()

            data_to_print <-
              dplyr::tibble('Nom cherche' = Name1,
                            "Nom propose" = Name2,
                            "Est considere synonyme de" = good_name
              )


            # test_dat$df <-
            #   good_name

            data_to_print

            # print("Considere comme synonyme de")
            # print(DicoNames$df$full_name_no_auth[which(DicoNames$df$id_n==DicoNames$df$id_good_n[which(DicoNames$df$full_name_no_auth %in% Name2)])])
          }else{

            dplyr::tibble('Nom cherche'=Name1, "Nom propose" = Name2)

          }

        }
      })


      output$table_chosen_sp <- renderTable({ # renderTable

        req(input$data1)
        # if(input$dico.choix!="oui") req(input$data2)

        if (is.null(input$stock) || input$stock == " ") {
          return()
        }else{

          Name1 <-
            stand.list.name$df %>%
            dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
            dplyr::pull(Code)

          id_Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(2)

          Name2 <-
            list.match.reac$df %>%
            dplyr::slice(as.numeric(input$stock)) %>%
            dplyr::pull(1)


          # if(input$choice.kind=="Noms sans auteurs" & input$choice.kind2=="les taxa")
          #   selected.field <- "tax_infra_level"
          # if(input$choice.kind=="Noms avec auteurs" & input$choice.kind2=="les taxa")
          #   selected.field <- "tax_infra_level_auth"
          # if(input$choice.kind2=="les genres")  selected.field <- "tax_gen"

          test_syn <-
            DicoNames$df %>%
            dplyr::filter(idtax_n == !!id_Name2) %>%
            dplyr::select(idtax_n, idtax_good_n)

          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n != test_syn$idtax_good_n)
            syn_check <- TRUE
          if(!is.na(test_syn$idtax_good_n)) if(test_syn$idtax_n == test_syn$idtax_good_n)
            syn_check <- FALSE
          if(is.na(test_syn$idtax_good_n)) syn_check <- FALSE

          if(syn_check) {

            id_good <-
              DicoNames$df %>%
              dplyr::filter(idtax_n == !!id_Name2) %>%
              dplyr::select(idtax_good_n) %>%
              dplyr::pull()

            DicoNames$df %>%
              dplyr::filter(idtax_n == !!id_good) %>%
              # mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
              #                                      paste0(tax_gen,
              #                                             " ",
              #                                             tax_esp,
              #                                             ifelse(!is.na(author1), paste0(" ", author1), ""),
              #                                             ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
              #                                             ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
              #                                             ifelse(!is.na(author2), paste0(" ", author2), ""),
              #                                             ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
              #                                             ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
              #                                             ifelse(!is.na(author3), paste0(" ", author3), "")),
              #                                      NA)) %>%
              dplyr::select(tax_fam,
                            tax_gen,
                            tax_esp,
                            tax_rank01,
                            tax_nam01,
                            tax_rank02,
                            author1,
                            author2)

          }else{

            DicoNames$df %>%
              dplyr::filter(idtax_n == id_Name2) %>%
              # mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
              #                                      paste0(tax_gen,
              #                                             " ",
              #                                             tax_esp,
              #                                             ifelse(!is.na(author1), paste0(" ", author1), ""),
              #                                             ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
              #                                             ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
              #                                             ifelse(!is.na(author2), paste0(" ", author2), ""),
              #                                             ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
              #                                             ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
              #                                             ifelse(!is.na(author3), paste0(" ", author3), "")),
              #                                      NA)) %>%
              dplyr::select(tax_fam,
                            tax_gen,
                            tax_esp,
                            tax_rank01,
                            tax_nam01,
                            tax_rank02,
                            author1,
                            author2)

          }
        }
      })

      observeEvent(input$confirm.name, {

        req(input$data1)
        # if(input$dico.choix!="oui") req(input$data2)

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)

        id_Name2 <-
          list.match.reac$df %>%
          dplyr::slice(as.numeric(input$stock)) %>%
          dplyr::pull(2)


        if (input$choice.kind == "Noms sans auteurs" &
            input$choice.kind2 == "les taxa")
          selected.field <- "tax_infra_level"
        if (input$choice.kind == "Noms avec auteurs" &
            input$choice.kind2 == "les taxa")
          selected.field <- "tax_infra_level_auth"
        if (input$choice.kind2 == "les genres")
          selected.field <- "tax_gen"
        if (input$choice.kind2 == "les familles")
          selected.field <- "tax_fam"
        if (input$choice.kind2 == "les classes")
          selected.field <- "tax_famclass"


        Name2 <-
          DicoNames$df %>%
          dplyr::filter(idtax_n == id_Name2) %>%
          dplyr::select(!!rlang::sym(selected.field)) %>%
          dplyr::pull()

        # complete found.name based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(found.name = replace(found.name, id_tax_search == Name1, Name2))

        Encoding(Name2) <- "UTF-8"

        found_taxa <-
          DicoNames$df %>%
          dplyr::filter(idtax_n == !!id_Name2)

        ### in case of duplicates in diconames
        if(nrow(found_taxa)>1) {
          found_taxa <-
            found_taxa %>%
            dplyr::group_by(!!rlang::sym(selected.field)) %>%
            dplyr::summarise(
              idtax_good_n = dplyr::first(idtax_good_n),
              tax_infra_level = dplyr::first(tax_infra_level),
              idtax_n = dplyr::first(idtax_n)
            )
        }


        # complete ID.dico.name based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name = replace(ID.dico.name, id_tax_search == Name1, found_taxa$idtax_n))

        # complete ID.dico.name.good based on found_taxa
        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name.good = replace(ID.dico.name.good, id_tax_search == Name1, found_taxa$idtax_good_n))


        # complete detvalue based on buttons cf
        # if(input$cf=="cf") {
        #   data.to.standardize.reac$df <-
        #     data.to.standardize.reac$df %>%
        #     dplyr::mutate(detvalue=replace(detvalue, id_tax_search==Name1, 3))
        # }


        ### checking for synonymies
        if(!is.na(found_taxa$idtax_good_n)) {
          if(found_taxa$idtax_n != found_taxa$idtax_good_n) {
            found_correc_taxa <-
              DicoNames$df %>%
              dplyr::filter(idtax_n == found_taxa$idtax_good_n)

            data.to.standardize.reac$df <-
              data.to.standardize.reac$df %>%
              dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, found_correc_taxa$tax_infra_level))
          }else{
            data.to.standardize.reac$df <-
              data.to.standardize.reac$df %>%
              dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, found_taxa$tax_infra_level))
          }
        }else{
          data.to.standardize.reac$df <-
            data.to.standardize.reac$df %>%
            dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, found_taxa$tax_infra_level))
        }

      })

      observeEvent(input$confirm.name, {
        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$id_tax_search == Name1)
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
        # if(input$dico.choix!="oui") req(input$data2)

        Name1 <-
          stand.list.name$df %>%
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
          dplyr::pull(id_tax_search)
        # Encoding(Name1) <- "UTF-8"

        id.orig <- which(stand.list.name$df$id_tax_search == Name1)
        if(length(id.orig)>0) val.nom.chosen$n <- id.orig + 1

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name = replace(ID.dico.name, id_tax_search == Name1, 0))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(found.name = replace(found.name, id_tax_search == Name1, ""))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(ID.dico.name.good = replace(ID.dico.name.good, id_tax_search == Name1, 0))

        data.to.standardize.reac$df <-
          data.to.standardize.reac$df %>%
          dplyr::mutate(corrected.name = replace(corrected.name, id_tax_search == Name1, ""))

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

          readr::write_excel_csv(data.to.standardize.reac$df %>%
                                   dplyr::select(-id_tax_search,
                                                 -id_data),
                                 file, na = "")

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
          dplyr::filter(id_tax_search == as.numeric(input$sector)) %>%
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
    #   print(input$authors)
    #
    #
    # })



  }
  )

  shiny::runApp(app, launch.browser = TRUE)

}


#' Internal function
#'
#' Looking for similar name
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param input string vector of one value containing the string to compare
#' @param compared_table tibble including one column containing the different strings to which input should be compared
#' @param column_name string the column name of compared_table containing the compared values
.find_similar_string <- function(input, compared_table, column_name){
  dist. <-
    RecordLinkage::levenshteinSim(tolower(input),
                                  tolower(compared_table %>%
                                            dplyr::select(!!column_name) %>%
                                            dplyr::pull()))

  arranged_values <-
    compared_table %>%
    tibble::add_column(dist = dist.) %>%
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
.pairwise_string_similarity <- function(string_vector) {
  dist. <- c()
  for (h in 1:(length(string_vector)-1))
    for (d in (h+1):length(string_vector))
      dist. <- c(dist.,
                 RecordLinkage::levenshteinSim(str1 =  tolower(string_vector[h]),
                                               str2 = tolower(string_vector[d])))
  return(dist.)
}



#' Load the database
#'
#' Load the database and ask for password
#'
#' @param pass string
#' @param user string
#'
#' @importFrom rstudioapi askForPassword
#' @importFrom RPostgres Postgres
#' @return The database is loaded
#' #'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
call.mydb <- function(pass=NULL, user=NULL, offline = FALSE) {

  if(!exists("mydb")) {

    if(!offline) {

      if(is.null(pass))
        pass <- rstudioapi::askForPassword("Please enter your password")

      if(is.null(user))
        user <- rstudioapi::askForPassword("Please enter your user name")


      # mydb <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
      #                   host = 'localhost',
      #                   port = 5432, # or any other port specified by your DBA
      #                   user = 'postgres',
      #                   password = pass)

      mydb <- DBI::dbConnect(RPostgres::Postgres(),dbname = 'plots_transects',
                             host = 'dg474899-001.dbaas.ovh.net',
                             port = 35699, # or any other port specified by your DBA
                             user = user,
                             password = pass)

    } else {

      # mydb <-
      #   list(data_liste_plots = dplyr::tbl(mydb, "data_liste_plots"),
      #        data_liste_sub_plots = dplyr::tbl(mydb, "data_liste_sub_plots"),
      #        table_colnam = dplyr::tbl(mydb, "table_colnam"),
      #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
      #        specimens = dplyr::tbl(mydb, "specimens"),
      #        diconame = dplyr::tbl(mydb, "diconame"),
      #        data_individuals = dplyr::tbl(mydb, "data_individuals"),
      #        data_link_specimens = dplyr::tbl(mydb, "data_link_specimens"),
      #        subplotype_list = dplyr::tbl(mydb, "subplotype_list"),
      #        traitlist = dplyr::tbl(mydb, "traitlist"),
      #        data_traits_measures = dplyr::tbl(mydb, "data_traits_measures"))

      mydb <-
        list(data_liste_plots = data_liste_plots,
             data_liste_sub_plots = data_liste_sub_plots,
             table_colnam = table_colnam,
             subplotype_list = subplotype_list,
             specimens = specimens,
             diconame = diconame,
             data_individuals = data_individuals,
             data_link_specimens = data_link_specimens,
             subplotype_list = subplotype_list,
             traitlist = traitlist,
             data_traits_measures = data_traits_measures)


    }

    assign("mydb", mydb, envir = .GlobalEnv)

    # return(mydb)
  }
}

#' List of countries
#'
#' Provide list of countries where plots occur
#'
#' @return A tibble of all countries
#' @import dplyr
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
country_list <- function() {

  if(!exists("mydb")) call.mydb() # mydb <-

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")
  nn <-
    dplyr::group_by(nn, country) %>%
    summarise(n = n())
  nn <-
    nn %>%
    collect() %>%
    dplyr::mutate(n = as.integer(n)) %>%
    arrange(country)

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
#' @importFrom rlang enquo
#' @export
province_list <- function(country=NULL) {
  if(!exists("mydb")) call.mydb()

  nn <-
    dplyr::tbl(mydb, "data_liste_plots")

  if(!is.null(country)) {

    var <- rlang::enquo(country)

    nn <-
      nn %>%
      dplyr::filter(country == !!var)
  }

  nn <-
    nn %>%
    dplyr::group_by(province) %>%
    dplyr::collect() %>%
    dplyr::count() %>%
    dplyr::mutate(n = as.integer(n))

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

  if(!exists("mydb")) call.mydb()

  nn <-
    dplyr::tbl(mydb, "data_liste_plots") %>%
    dplyr::select(id_method) %>%
    dplyr::left_join(dplyr::tbl(mydb, "methodslist")) %>%
    dplyr::group_by(method) %>%
    dplyr::summarise(n = n()) %>%
    dplyr::mutate(n = as.integer(n)) %>%
    dplyr::collect()

  # dbDisconnect(mydb)
  return(nn)
}


#' List of subplot feature
#'
#' Provide list of subplot features of plot
#'
#' @return A tibble of all subplot features
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
subplot_list <- function() {
  if(!exists("mydb")) call.mydb()

  nn <-
    dplyr::tbl(mydb, "subplotype_list") %>%
    dplyr::collect()

  # dbDisconnect(mydb)
  return(nn)
}

#' List of trait
#'
#' Provide list of traits available
#'
#' @return A tibble of all traits
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @export
traits_list <- function() {

  all_colnames_ind <-
    dplyr::tbl(mydb, "traitlist") %>%
    dplyr::select(trait, id_trait, traitdescription) %>%
    dplyr::collect()

  return(all_colnames_ind)
}



#' List, extract map selected plots
#'
#' Provide and map list of selected plots, including associated traits for individuals
#'
#' @return A tibble of all plots or individuals
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param team_lead string fuzzy person name to look for
#' @param plot_name string fuzzy plot name to look for
#' @param tag numeric exact tag number of the plot
#' @param country string fuzzy country name to look for
#' @param province string fuzzy province name to look for
#' @param locality_name string fuzzy locality_name name to look for
#' @param method stringfuzzy method name to look for
#' @param date_y integer year of collect
#' @param extract_individuals logical whether individuals instead of plot list as output
#' @param map logical whether map whould be produced
#' @param zoom integer positive values indicating zoom level for ggplot style map
#' @param label logical for ggplot map, whether adding label or not
#' @param try_optimal_label logical if map is ggplot
#' @param map_type string mapview or ggplot, mapview by default
#' @param id_individual numeric id of individual to be extracted
#' @param id_plot numeric id of plot to be extracted
#' @param show_multiple_census logical whether multiple census should be shown, by default FALSE
#' @param remove_ids logical remove all ids columns, by default TRUE
#' @param collapse_multiple_val logical whether multiple traits measures should be collapsed (resulting values as character, separated by dash)
#' @param extract_traits whether species level traits should be extracted as well
#'
#' @importFrom DBI dbSendQuery dbFetch dbClearResult dbWriteTable
#' @importFrom stringr str_flatten str_trim str_extract
#' @importFrom date as.date
#' @importFrom tidyselect vars_select_helpers
#' @importFrom BIOMASS correctCoordGPS
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
#'
#' @export
query_plots <- function(team_lead = NULL,
                        plot_name = NULL,
                        tag = NULL,
                        country = NULL,
                        province = NULL,
                        locality_name = NULL,
                        method = NULL,
                        date_y = NULL,
                        extract_individuals = FALSE,
                        map = FALSE,
                        zoom = 0,
                        label = T,
                        try_optimal_label = F,
                        map_type = "mapview",
                        id_individual = NULL,
                        id_plot = NULL,
                        id_diconame = NULL,
                        show_multiple_census = FALSE,
                        show_all_coordinates = FALSE,
                        remove_ids = TRUE,
                        collapse_multiple_val = FALSE,
                        extract_traits = TRUE) {

  if (!exists("mydb")) call.mydb()

  if (!is.null(id_individual))
  {

    cli::cli_rule(left = "Extracting from queried individuals - id_individual")
    extract_individuals <- TRUE

    if(!is.null(id_plot))
      cli::cli_alert_warning("id_plot not null replaced by id_plot of the id_individuals")

    id_plot <-
      tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_n %in% id_individual) %>%
      dplyr::select(id_table_liste_plots_n) %>%
      dplyr::collect() %>%
      pull()

  }

  if (!is.null(id_diconame))
  {

    cli::cli_rule(left = "Extracting from queried taxa - idtax_n")
    extract_individuals <- TRUE

    if(!is.null(id_plot))
      cli::cli_alert_warning("id_plot not null replaced by id_plot where idtax_n are found")

    id_plot <-
      merge_individuals_taxa() %>%
      filter(idtax_individual_f %in% !!id_diconame) %>%
      pull(id_table_liste_plots_n)

    # id_plot <-
    #   tbl(mydb, "data_individuals") %>%
    #   query_tax_all(id_search = id_diconame, extract_individuals = T, verbose = FALSE, simple_ind_extract = T) %>%
    #   pull(id_table_liste_plots_n)

  }

  if (is.null(id_plot)) {

    query <- 'SELECT * FROM data_liste_plots WHERE MMM'

    if(!is.null(team_lead)) {

      id_liste_plots_match <- vector('list', length(team_lead))
      for (i in 1:length(team_lead)) {

        query_colnam <-
          paste0("SELECT * FROM table_colnam WHERE colnam ILIKE '%", team_lead[i], "%'")

        rs_col <- DBI::dbSendQuery(mydb, query_colnam)
        rs_colnam <- DBI::dbFetch(rs_col)
        DBI::dbClearResult(rs_col)
        rs_colnam <- dplyr::as_tibble(rs_colnam)
        id_liste_plots_match[[i]] <- rs_colnam$id_table_colnam
      }

      query <-
        gsub(
          pattern = "MMM",
          replacement = paste0("id_colnam IN ('", paste(unlist(id_liste_plots_match), collapse = "', '"), "') AND MMM"),
          x = query
        )

    }
      # query <- gsub(pattern = "MMM", replacement = paste0(" team_leader ILIKE '%", team_lead, "%' AND MMM"), x=query)

    # query <- "SELECT * FROM data_liste_plots WHERE  team_leader ILIKE '%Dauby%' AND country IN ('Gabon', 'Cameroun')"

    if (!is.null(country)) {
      if (length(country) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" country ILIKE '%", country, "%' AND MMM"),
            x = query
          )
      } else{
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0("country IN ('", paste(country, collapse = "', '"), "') AND MMM"),
            x = query
          )
      }
    }

    if (!is.null(locality_name)) {
      if (length(locality_name) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" locality_name ILIKE '%", locality_name, "%' AND MMM"),
            x = query
          )
      } else {

        id_liste_plots_match <- vector('list', length(locality_name))
        for (i in 1:length(locality_name)) {
          query_loc <-
            paste0("SELECT * FROM data_liste_plots WHERE locality_name ILIKE '%", locality_name[i], "%'")

          rs_loc <- DBI::dbSendQuery(mydb, query_loc)
          res_loc <- DBI::dbFetch(rs_loc)
          DBI::dbClearResult(rs_loc)
          res_loc <- dplyr::as_tibble(res_loc)
          id_liste_plots_match[[i]] <- res_loc$id_liste_plots
        }

        query <-
          gsub(
          pattern = "MMM",
          replacement = paste0(
            "id_liste_plots IN ('",
            paste(unlist(id_liste_plots_match), collapse = "', '"),
            "') AND MMM"
          ),
          x = query
        )
      }
    }

    if (!is.null(province)) {
      if (length(province) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" province ILIKE '%", province, "%' AND MMM"),
            x = query
          )
      } else{
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(
              "province IN ('",
              paste(province, collapse = "', '"),
              "') AND MMM"
            ),
            x = query
          )
      }
    }

    if (!is.null(method)) {
      query_method <- 'SELECT * FROM methodslist WHERE MMM'
      if (length(method) == 1) {
        query_method <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" method ILIKE '%", method, "%' AND MMM"),
            x = query_method
          )

      } else {
        query_method <-
          gsub(
            pattern = "MMM",
            replacement = paste0("method IN ('", paste(method, collapse = "', '"), "') AND MMM"),
            x = query_method
          )
      }

      query_method <-
        gsub(pattern = "AND MMM", replacement = "", query_method)

      rs_meth <- DBI::dbSendQuery(mydb, query_method)
      res_meth <- DBI::dbFetch(rs_meth)
      DBI::dbClearResult(rs_meth)
      res_meth <- dplyr::as_tibble(res_meth)

      if (nrow(res_meth) == 0) {
        warning("no method selected!")

      } else{
        message("\n method(s) selected")
        print(res_meth)

        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(
              "id_method IN ('",
              paste(res_meth$id_method, collapse = "', '"),
              "') AND MMM"
            ),
            x = query
          )
      }
    }

    if (!is.null(plot_name)) {
      if (length(plot_name) == 1) {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(" plot_name ILIKE '%", plot_name, "%' AND MMM"),
            x = query
          )
      } else {
        query <-
          gsub(
            pattern = "MMM",
            replacement = paste0(
              "plot_name IN ('",
              paste(plot_name, collapse = "', '"),
              "') AND MMM"
            ),
            x = query
          )
      }
    }

    query <- gsub(pattern = "AND MMM", replacement = "", query)

    if(grepl("WHERE MMM", query)) query <- gsub(pattern = " WHERE MMM", replacement = "", query)

    rs <- DBI::dbSendQuery(mydb, query)
    res <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)
    res <- dplyr::as_tibble(res)
    res <-
      res %>%
      dplyr::select(-id_old)


  } else {

    cli::cli_rule(left = "Extracting from queried plot - id_plot")

    res <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::filter(id_liste_plots %in% id_plot) %>%
      dplyr::collect() %>%
      dplyr::select(-id_old)

  }

  ## link method
  res <-
    res %>%
    dplyr::select(-method) %>% # remove old method
    dplyr::left_join(dplyr::tbl(mydb, "methodslist") %>%
                       dplyr::collect(),
                     by = c("id_method" = "id_method")) %>%
    dplyr::relocate(method, .after = data_provider)

  ## link colnam
  res <-
    res %>%
    dplyr::select(-team_leader) %>% # remove old method
    dplyr::left_join(dplyr::tbl(mydb, "table_colnam") %>%
                       dplyr::collect(),
                     by = c("id_colnam" = "id_table_colnam")) %>%
    dplyr::rename(team_leader = colnam) %>%
    dplyr::relocate(team_leader, .after = plot_name)

  if (nrow(res) == 0)
    stop("No plot are found based on inputs")

  ### checking for subplots data
  ids_plots <- res$id_liste_plots
  sub_plot_data <-
    dplyr::tbl(mydb, "data_liste_sub_plots") %>%
    dplyr::filter(id_table_liste_plots %in% ids_plots)

  ## max census for each plot
  census_plots <-
    sub_plot_data %>%
    dplyr::filter(id_type_sub_plot == 27) %>% ### id of census information
    dplyr::group_by(id_table_liste_plots) %>%
    dplyr::summarise(number_of_census = max(typevalue, na.rm = T)) %>%
    dplyr::collect()

  census_features <-
    sub_plot_data %>%
    dplyr::filter(id_type_sub_plot == 27) %>% ### id of census information
    dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
                     by = c("id_colnam" = "id_table_colnam")) %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "data_liste_plots") %>%
        dplyr::select(plot_name, id_liste_plots),
      by = c("id_table_liste_plots" = "id_liste_plots")
    ) %>%
    dplyr::select(
      year,
      month,
      day,
      typevalue,
      plot_name,
      colnam,
      additional_people,
      id_sub_plots,
      id_table_liste_plots
    ) %>%
    dplyr::collect()

  all_sub_type <-
    sub_plot_data %>%
    dplyr::distinct(id_type_sub_plot) %>%
    dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                       dplyr::select(type, valuetype, typedescription, id_subplotype),
                     by=c("id_type_sub_plot"= "id_subplotype")) %>%
    dplyr::collect()

  all_sub_type_to_summarise <-
    all_sub_type %>%
    dplyr::filter(type != "census",
                  !grepl("ddlat", type),
                  !grepl("ddlon", type))

  ## if any plots features others than census, adding them to metadata
  if(nrow(all_sub_type_to_summarise) > 0) {

    for (i in 1:nrow(all_sub_type_to_summarise)) {

      id_sub_plot <-
        all_sub_type_to_summarise %>%
        dplyr::slice(i) %>%
        dplyr::pull(id_type_sub_plot)

      type_sub_plot <-
        all_sub_type_to_summarise %>%
        dplyr::slice(i) %>%
        dplyr::pull(valuetype)

      if(type_sub_plot == "numeric") {

         summarized_subplot_data <-
          sub_plot_data %>%
          dplyr::filter(id_type_sub_plot == id_sub_plot) %>%
          dplyr::group_by(id_table_liste_plots) %>%
          dplyr::summarise(sum_sub_plot = mean(typevalue, na.rm=T)) %>%
          dplyr::collect()

         subplot_name <-
           all_sub_type_to_summarise %>%
           dplyr::slice(i) %>%
           dplyr::pull(type)

         subplot_name <-
           paste0(subplot_name, "_averaged")

         summarized_subplot_data <-
           summarized_subplot_data %>%
           dplyr::rename(!!subplot_name := sum_sub_plot)

      }

      if(type_sub_plot == "character") {

        summarized_subplot_data <-
          sub_plot_data %>%
          dplyr::filter(id_type_sub_plot == id_sub_plot) %>%
          dplyr::group_by(id_table_liste_plots) %>%
          dplyr::collect() %>%
          # mutate(sum_sub_plot = first(typevalue_char)) %>%
          # select(id_table_liste_plots, sum_sub_plot)
          dplyr::summarise(sum_sub_plot =
                             ifelse(
                               length(unique(typevalue_char)) > 1,
                               stringr::str_flatten(typevalue_char, collapse = " | "),
                               typevalue_char
                             ))
        subplot_name <-
          all_sub_type_to_summarise %>%
          dplyr::slice(i) %>%
          dplyr::pull(type)

        summarized_subplot_data <-
          summarized_subplot_data %>%
          dplyr::rename(!!subplot_name := sum_sub_plot)

      }

      res <-
        res %>%
        dplyr::left_join(summarized_subplot_data, by = c("id_liste_plots"="id_table_liste_plots"))

    }

  } else {

    cli::cli_alert_info("No sub_type (other than census) for selected plot(s)")

  }

  if(show_all_coordinates) {
    ### getting id subplot features for coordinates types

    all_ids_subplot_coordinates <- c(all_sub_type %>%
                                       filter(grepl("ddlon", type)) %>%
                                       pull(id_type_sub_plot),
                                     all_sub_type %>%
                                       filter(grepl("ddlat", type)) %>%
                                       pull(id_type_sub_plot))

    if(length(all_ids_subplot_coordinates) > 0) {

      cli::cli_alert_info('Extracting coordinates')

      all_coordinates_subplots <-
        sub_plot_data %>%
        dplyr::filter(id_type_sub_plot %in% !!all_ids_subplot_coordinates) %>% ### id of census information
        dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
                         by = c("id_colnam" = "id_table_colnam")) %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "data_liste_plots") %>%
            dplyr::select(plot_name, id_liste_plots),
          by = c("id_table_liste_plots" = "id_liste_plots")
        ) %>%
        dplyr::select(
          year,
          typevalue,
          plot_name,
          colnam,
          id_sub_plots,
          id_table_liste_plots,
          id_sub_plots,
          id_type_sub_plot
        ) %>%
        left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                    dplyr::select(type, valuetype, typedescription, id_subplotype),
                  by=c("id_type_sub_plot"= "id_subplotype")) %>%
        dplyr::collect() %>%
        dplyr::relocate(c("typevalue", "type", "plot_name", "typedescription", "valuetype"),
                        .before = year)

      all_coordinates_subplots_rf <-
        all_coordinates_subplots %>%
        mutate(coord2 = unlist(lapply(strsplit(type, "_"), function(x) x[length(x)])),
               coord1 = unlist(lapply(strsplit(type, "_"), function(x) x[length(x)-1])),
               coord3 = unlist(lapply(strsplit(type, "_"), function(x) x[1])),
               coord4 = unlist(lapply(strsplit(type, "_"), function(x) x[2]))) %>%
        dplyr::select(coord1, coord2, coord3, coord4, type, typevalue, plot_name, id_sub_plots, id_table_liste_plots) %>%
        arrange(coord2)


      all_plots_coord <- unique(all_coordinates_subplots_rf$plot_name)

      coordinates_subplots_plot <- vector('list', length(all_plots_coord))
      coordinates_subplots_plot_sf <- vector('list', length(all_plots_coord))
      for (j in 1:length(all_plots_coord)) {

        coordinates_subplots <-
          tidyr::pivot_wider(all_coordinates_subplots_rf %>%
                               dplyr::select(-type, -id_table_liste_plots) %>%
                               dplyr::filter(plot_name == all_plots_coord[j]),
                             names_from = coord3,
                             values_from = c(typevalue, id_sub_plots))

        if (nrow(coordinates_subplots) > 0) {

          coordinates_subplots <-
            coordinates_subplots %>%
            mutate(Xrel = as.numeric(coord1) - min(as.numeric(coord1)),
                   Yrel = as.numeric(coord2) - min(as.numeric(coord2)))

          if(all(coordinates_subplots$coord4 == 'plot')) {
            cor_coord <-
              BIOMASS::correctCoordGPS(
                longlat = coordinates_subplots[, c("typevalue_ddlon", "typevalue_ddlat")],
                rangeX = c(0, 100),
                rangeY = c(0, 100),
                coordRel = coordinates_subplots %>%
                  dplyr::select(Xrel, Yrel),
                drawPlot = F,
                rmOutliers = T
              )

            poly_plot <- as(cor_coord$polygon, 'sf')
            sf::st_crs(poly_plot) <- cor_coord$codeUTM

            poly_plot <- sf::st_transform(poly_plot, 4326)

            poly_plot <-
              poly_plot %>%
              dplyr::mutate(plot_name = all_plots_coord[j]) %>%
              left_join(res %>%
                          dplyr::select(plot_name, id_liste_plots),
                        by = c("plot_name" = "plot_name"))

            coordinates_subplots_plot_sf[[j]] <-
              poly_plot
          }

        } else {

          cli::cli_alert_danger("No coordinates for {all_plots_coord[j]} available")


        }

        coordinates_subplots_plot[[j]] <-
          coordinates_subplots

      }

      coordinates_subplots <-
        bind_rows(coordinates_subplots_plot)

      coordinates_subplots_plot_sf <-
        do.call('rbind', coordinates_subplots_plot_sf)

    } else {

      show_all_coordinates <- FALSE
      cli::cli_alert_danger("No coordinates for quadrat available")

    }

  }

  res <-
    res %>%
    dplyr::select(-date)

  if(nrow(census_features) > 1) {
    cli::cli_rule(left = "Attaching census informations")
    cli::cli_alert_info("Number of census found : {unique(pull(census_features, typevalue))}")

    for (i in 1:nrow(dplyr::distinct(census_features, typevalue))) {


      census_features_selected <-
        census_features %>%
        dplyr::filter(typevalue == i)

      census_features_selected <-
        census_features_selected %>%
        dplyr::mutate(date =
                        paste(ifelse(!is.na(month),
                                     month, 1), # if day is missing, by default 1
                              ifelse(!is.na(day),
                                     day, 1), # if month is missing, by default 1
                              ifelse(!is.na(year),
                                     year, ""),
                              sep = "/")) %>%
        dplyr::mutate(date_julian = date::as.date(date))


      res <-
        res %>%
        dplyr::left_join(
          census_features_selected %>%
            dplyr::select(id_table_liste_plots, date, date_julian),
          by = c("id_liste_plots" = "id_table_liste_plots")
        )

      date_name <- paste0("date_census_", i)
      date_name_enquo <-
        rlang::parse_expr(rlang::quo_name(rlang::enquo(date_name)))

      res <-
        res %>%
        dplyr::rename(!!date_name_enquo := date)

      date_name <- paste0("date_census_julian_", i)
      date_name_enquo <-
        rlang::parse_expr(rlang::quo_name(rlang::enquo(date_name)))

      res <-
        res %>%
        dplyr::rename(!!date_name_enquo := date_julian)

    }

    res <-
      res %>%
      dplyr::left_join(census_plots, by = c("id_liste_plots"="id_table_liste_plots"))
    }

  if(map) {

    cli::cli_rule(left = "Mapping")

    if (requireNamespace("spData", quietly = TRUE)) {
      library(spData)
      data(world)
    }

    if(any(is.na(res$ddlat)) | any(is.na(res$ddlon))) {
      not_georef_plot <-
        dplyr::filter(res, is.na(ddlat), is.na(ddlon)) %>%
        dplyr::pull(plot_name)

      cli::cli_alert_warning("removing following plots because missing coordinates: {not_georef_plot}")

    }

    res <-
      res %>%
      dplyr::filter(!is.na(ddlat), !is.na(ddlon)) %>%
      dplyr::select(-id_senterre_db)

    data_sf <- sf::st_as_sf(res, coords = c("ddlon", "ddlat"), crs = 4326)
    bbox_data <- sf::st_bbox(data_sf)

    if(map_type == "ggplot") {

      outputmap <-
        ggplot2::ggplot() +
        ggplot2::geom_sf(data = world, alpha=0.8)

      if(label) {
        if(try_optimal_label) {

          if (!any(rownames(utils::installed.packages()) == "ggrepel"))
            stop("ggrepel package needed, please install it")

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

    if(map_type == "mapview") {
      map_types <- c("Esri.NatGeoWorldMap", "Esri.WorldImagery", "OpenStreetMap.DE",
                     "Esri.WorldPhysical")

      print(map_type)
      outputmap <-  mapview::mapview(data_sf, map.types = map_types)

      if(show_all_coordinates) {
        if(!is.null(unlist(coordinates_subplots_plot_sf)))
          outputmap <- outputmap +
            mapview::mapview(coordinates_subplots_plot_sf, map.types = map_types)
      }

    }

    print(outputmap)

  }

  if(extract_individuals) {
    # res <-
    #   tbl(mydb, "data_individuals") %>%
    #   filter(id_table_liste_plots_n %in% res$id_liste_plots) %>%
    #
    #   collect()



    #   dplyr::rename(id_dico_name_specimen = id_good_n) %>%
    #   dplyr::select(id_specimen, id_dico_name_specimen)

    # test <-
    #   tbl(mydb, "data_individuals") %>%
    #   filter(id_table_liste_plots_n %in% res$id_liste_plots) %>%
    #   left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
    #   mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
    #   left_join(tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
    #   dplyr::select(id_n, dbh, full_name_used, id_diconame_n, id_dico_name_specimen, id_diconame_final, id_specimen) %>%
    #   arrange(id_n) %>%
    #   collect()

    # # getting traits
    # traits_measures <-
    #   dplyr::tbl(mydb, "data_traits_measures") %>%
    #   dplyr::left_join(dplyr::tbl(mydb, "traitlist"), by=c("traitid"="id_trait")) %>%
    #   dplyr::select(-id_specimen, -id_diconame)

    ## getting all metadata
    selec_plot_tables <-
      # dplyr::tbl(mydb, "data_liste_plots") %>%
      res %>%
      dplyr::select(plot_name, team_leader, country, locality_name,
                    data_provider, id_liste_plots,
                    dplyr::contains("date_census"))

    res_individuals_full <-
      merge_individuals_taxa(id_individual = id_individual,
                             id_plot = selec_plot_tables$id_liste_plots)

    if (!is.null(id_diconame))
      res_individuals_full <-
      res_individuals_full %>%
      dplyr::filter(idtax_f %in% !!id_diconame)


    ### Removing old fields not used anymore
    res_individuals_full <-
      res_individuals_full %>%
      dplyr::select(
        -photo_tranche,
        -liane,
        -dbh,
        -dbh_height,
        -tree_height,
        -branch_height,
        -branchlet_height,-crown_spread,
        -observations,
        -observations_census_2,
        -id_census2,
        -dbh_census2,
        -id_specimen_old,
        -tax_tax,
        -a_habit,
        -a_habit_secondary,
        -id_korup_ctfs,
        -tag_korup_ctfs,
        -id_table_data_senterre,
        -id_diconame,
        -code_individu
      )

    if(!is.null(tag)) {

      res_individuals_full <-
        res_individuals_full %>%
        dplyr::filter(ind_num_sous_plot == tag)

    }

    res_individuals_full <-
      res_individuals_full %>%
      dplyr::collect() %>%
      dplyr::mutate(original_tax_name = stringr::str_trim(original_tax_name))

    #adding metada information
    res_individuals_full <-
      res_individuals_full %>%
      dplyr::left_join(selec_plot_tables,
                       by = c("id_table_liste_plots_n" = "id_liste_plots"))


    #### Add traits at individual levels
    all_traits <- traits_list()
    all_traits_list <-
      .get_trait_individuals_values(
        traits = all_traits$trait,
        id_individuals = res_individuals_full$id_n,
        show_multiple_measures = show_multiple_census,
        skip_dates = T,
        collapse_multiple_val = collapse_multiple_val
      )

    if (length(all_traits_list) > 0) {
      for (i in 1:length(all_traits_list)) {
        res_individuals_full <-
          res_individuals_full %>%
          dplyr::left_join(all_traits_list[[i]] %>%
                             dplyr::select(-id_old),
                           by = c("id_n" = "id_n"))

      }
    }

    if (extract_traits) {

      cli::cli_alert_info("Extracting taxa-level traits")

      queried_traits_tax <-
        query_traits_measures(idtax = unique(res_individuals_full$idtax_individual_f))

      if (nrow(queried_traits_tax$traits_found) > 0) {

        if (any(class(queried_traits_tax$traits_idtax_num) == "data.frame"))
          res_individuals_full <-
            res_individuals_full %>%
            left_join(queried_traits_tax$traits_idtax_num,
                      by = c("idtax_individual_f" = "idtax"))

        if (any(class(queried_traits_tax$traits_idtax_char) == "data.frame"))
          res_individuals_full <-
            res_individuals_full %>%
            left_join(queried_traits_tax$traits_idtax_char,
                      by = c("idtax_individual_f" = "idtax"))

      } else {

          cli::cli_alert_info("No taxa-level traits found for extracted taxa")

      }

      ### complete traits at genus level

      list_genera <- res_individuals_full %>%
        # dplyr::filter(is.na(tax_sp_level)) %>%
        dplyr::select(id_n, tax_gen)

      all_sp_genera <- query_taxa(
        genus = list_genera %>%
          dplyr::filter(!is.na(tax_gen)) %>%
          dplyr::distinct(tax_gen) %>%
          dplyr::pull(tax_gen),
        class = NULL,
        extract_traits = FALSE,
        verbose = FALSE,
        exact_match = TRUE
      )

      all_sp_genera <-
        all_sp_genera %>%
        filter(tax_gen %in% unique(list_genera$tax_gen),
               !is.na(tax_infra_level))

      all_val_sp <- query_traits_measures(idtax = all_sp_genera %>%
                                            filter(!is.na(tax_esp)) %>%
                                            pull(idtax_n),
                                          add_taxa_info = T)

      # level_trait <- rep("species", nrow(res))

      if (any(class(all_val_sp$traits_idtax_char) == "data.frame")) {

        traits_idtax_char <-
          all_val_sp$traits_found %>%
          dplyr::filter(valuetype == "categorical") %>%
          dplyr::select(idtax,
                        trait,
                        traitvalue_char,
                        basisofrecord,
                        id_trait_measures) %>%
          dplyr::mutate(rn = data.table::rowid(trait)) %>%
          tidyr::pivot_wider(
            names_from = trait,
            values_from = c(traitvalue_char, basisofrecord, id_trait_measures)
          ) %>%
          dplyr::select(-rn) %>%
          left_join(all_val_sp$traits_idtax_char %>%
                      dplyr::select(idtax, tax_gen),
                    by = c("idtax" = "idtax"))

        names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))

        traits_idtax_concat <-
          traits_idtax_char %>%
          dplyr::select(tax_gen, starts_with("id_trait_")) %>%
          dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
          dplyr::group_by(tax_gen) %>%
          dplyr::mutate(dplyr::across(where(is.character),
                                      ~ stringr::str_c(.[!is.na(.)],
                                                       collapse = ", "))) %>%
          dplyr::ungroup() %>%
          dplyr::distinct()

        cli::cli_alert_info("Extracting most frequent value for categorical traits at genus level")

        traits_idtax_char <-
          traits_idtax_char %>%
          dplyr::select(-starts_with("id_trait_")) %>%
          group_by(tax_gen, across(where(is.character))) %>%
          count() %>%
          arrange(tax_gen, desc(n)) %>%
          ungroup() %>%
          group_by(tax_gen) %>%
          dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))

        traits_idtax_char <-
          left_join(traits_idtax_char,
                    traits_idtax_concat, by = c("tax_gen" = "tax_gen"))

        colnames_traits <- names(traits_idtax_char %>%
                                   dplyr::select(
                                     -tax_gen,
                                     -starts_with("id_trait_"),
                                     -starts_with("basisofrecord_")
                                   ))

        for (j in 1:length(colnames_traits)) {

          if (colnames_traits[j] %in% names(res_individuals_full)) {

            var1 <- paste0(colnames_traits[j], ".y")
            var2 <- paste0(colnames_traits[j], ".x")

            res_individuals_full <-
              res_individuals_full %>%
              left_join(
                traits_idtax_char %>%
                  dplyr::select(tax_gen, colnames_traits[j]),
                by = c("tax_gen" = "tax_gen")
              ) %>%
              # dplyr::select(
              #   tax_sp_level,
              #   id_n,
              #   tax_gen,
              #   paste0(colnames_traits[j], ".x"),
              #   paste0(colnames_traits[j], ".y")
              # ) %>%
              mutate("{colnames_traits[j]}" :=
                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                              ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                     NA,
                                     !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                              !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
              mutate("source_{colnames_traits[j]}" :=
                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                              ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                     NA,
                                     "genus"),
                              "species")) %>%
              dplyr::select(-paste0(colnames_traits[j], ".x"),
                            -paste0(colnames_traits[j], ".y"))

          } else {

            var1 <- colnames_traits[j]

            res_individuals_full <-
              res_individuals_full %>%
              left_join(
                traits_idtax_char %>%
                  dplyr::select(tax_gen, colnames_traits[j]),
                by = c("tax_gen" = "tax_gen")
              ) %>%
              mutate("source_{colnames_traits[j]}" :=
                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                              NA,
                              "genus"))

          }
        }



        # res_subset <- res_individuals_full %>%
        #   filter(!id_n %in% list_genera$id_n)
        #
        # col_traits_char <- names(traits_idtax_char %>%
        #         dplyr::select(-tax_gen))
        #
        # res_completed <-
        #   res_individuals_full %>%
        #   filter(id_n %in% list_genera$id_n) %>%
        #   dplyr::select(-col_traits_char[col_traits_char %in% names(res_individuals_full)]) %>%
        #   left_join(traits_idtax_char,
        #             by = c("tax_gen" = "tax_gen"))
        #
        # res_individuals_full <- bind_rows(res_subset, res_completed)
        #
        # level_trait[which(res_individuals_full$id_n %in% list_genera$id_n)] <- "genus"

        # res_individuals_full %>%
        #   filter(id_n %in% list_genera$id_n) %>%
        #   group_by(phenology) %>%
        #   count()

      }

      if (any(class(all_val_sp$traits_idtax_num) == "data.frame")) {

        traits_idtax_num <-
          all_val_sp$traits_found %>%
          dplyr::filter(valuetype == "numeric") %>%
          dplyr::select(idtax,
                        trait,
                        traitvalue,
                        basisofrecord,
                        id_trait_measures) %>%
          dplyr::mutate(rn = data.table::rowid(trait)) %>%
          tidyr::pivot_wider(
            names_from = trait,
            values_from = c(traitvalue, basisofrecord, id_trait_measures)
          ) %>%
          dplyr::select(-rn) %>%
          dplyr::left_join(all_val_sp$traits_idtax_num %>%
                      dplyr::select(idtax, tax_gen),
                    by = c("idtax" = "idtax"))

        names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))

        traits_idtax_concat <-
          traits_idtax_num %>%
          dplyr::select(tax_gen, starts_with("id_trait_")) %>%
          dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
          dplyr::group_by(tax_gen) %>%
          dplyr::mutate(dplyr::across(where(is.character),
                                      ~ stringr::str_c(.[!is.na(.)],
                                                       collapse = ", "))) %>%
          dplyr::ungroup() %>%
          dplyr::distinct()

        traits_idtax_num <-
          traits_idtax_num %>%
          dplyr::select(-starts_with("id_trait_"), -idtax) %>%
          dplyr::group_by(tax_gen) %>%
          dplyr::summarise(dplyr::across(where(is.numeric),
                                  .fns= list(mean = mean,
                                             sd = sd,
                                             n = length),
                                  .names = "{.col}_{.fn}"))


        colnames_traits <- names(traits_idtax_num %>%
                                   dplyr::select(
                                     -tax_gen,
                                     -starts_with("id_trait_"),
                                     -starts_with("basisofrecord_")
                                   ))

        for (j in 1:length(colnames_traits)) {

          if (colnames_traits[j] %in% names(res_individuals_full)) {

            var1 <- paste0(colnames_traits[j], ".y")
            var2 <- paste0(colnames_traits[j], ".x")

            res_individuals_full <-
              res_individuals_full %>%
              left_join(
                traits_idtax_num %>%
                  dplyr::select(tax_gen, colnames_traits[j]),
                by = c("tax_gen" = "tax_gen")
              ) %>%
              # dplyr::select(
              #   tax_sp_level,
              #   id_n,
              #   tax_gen,
              #   paste0(colnames_traits[j], ".x"),
              #   paste0(colnames_traits[j], ".y")
              # ) %>%
              mutate("{colnames_traits[j]}" :=
                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                              ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                     NA,
                                     !!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                              !!rlang::parse_expr(quo_name(rlang::enquo(var2))))) %>%
              mutate("source_{colnames_traits[j]}" :=
                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var2)))),
                              ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                                     NA,
                                     "genus"),
                              "species")) %>%
              dplyr::select(-paste0(colnames_traits[j], ".x"),
                            -paste0(colnames_traits[j], ".y"))


          } else {

            var1 <- colnames_traits[j]

            res_individuals_full <-
              res_individuals_full %>%
              left_join(
                traits_idtax_num %>%
                  dplyr::select(tax_gen, colnames_traits[j]),
                by = c("tax_gen" = "tax_gen")
              ) %>%
              mutate("source_{colnames_traits[j]}" :=
                       ifelse(is.na(!!rlang::parse_expr(quo_name(rlang::enquo(var1)))),
                              NA,
                              "genus"))



          }




        }

        # res_subset <- res_individuals_full %>%
        #   filter(!id_n %in% list_genera$id_n)
        #
        # res_completed <- res_individuals_full %>%
        #   filter(id_n %in% list_genera$id_n) %>%
        #   dplyr::select(-names(traits_idtax_num %>% dplyr::select(-tax_gen))) %>%
        #   left_join(traits_idtax_num,
        #             by = c("tax_gen" = "tax_gen"))
        #
        # res_individuals_full <-
        #   bind_rows(res_subset, res_completed)
        #
        # # res_individuals_full %>%
        # #   filter(!is.na(wood_density_mean))
        #
        # level_trait[which(res_individuals_full$id_n %in% list_genera$id_n)] <- "genus"

        # res_individuals_full %>%
        #   filter(id_n %in% list_genera$id_n) %>%
        #   group_by(wood_density_mean) %>%
        #   count()

      }


      #
      #
      # all_genera <- query_taxa(genus = unique(list_genera$tax_gen),
      #                          only_genus = T,
      #                             class = NULL, check_synonymy = F,
      #                             extract_traits = F)
      #
      # tbl(mydb, "table_taxa") %>%
      #   dplyr::filter(tax_gen %in% !!unique(list_genera$tax_gen)) %>%
      #   dplyr::filter(is.na(tax_esp)) %>%
      #   dplyr::select(idtax_n) %>%
      #   dplyr::collect()
      #
      # all_genera_traits <- query_traits_measures(idtax = all_sp_genera %>%
      #                                       filter(!is.na(tax_esp)) %>%
      #                                       pull(idtax_n))

    }

    res <-
      res_individuals_full %>%
      # dplyr::select(id_n, multi_tiges_id_good, plot_name, team_leader, country, locality_name, sous_plot_name, ind_num_sous_plot,
      #               full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat, id_diconame_final,
      #               liane, strate_cat,
      #               id_specimen, id_old,
      #               all_traits$trait, paste0(all_traits$trait, "_issue")) %>%
      dplyr::arrange(id_n) %>%
      dplyr::relocate(plot_name, .before = id_old) %>%
      dplyr::relocate(locality_name, .before = id_old) %>%
      dplyr::relocate(sous_plot_name, .before = id_old) %>%
      dplyr::relocate(ind_num_sous_plot, .before = id_old) %>%
      dplyr::relocate(tax_sp_level, .before = id_old) %>%
      dplyr::relocate(tax_infra_level, .before = id_old) %>%
      dplyr::relocate(tax_gen, .before = id_old) %>%
      dplyr::relocate(tax_fam, .before = id_old) %>%
      dplyr::relocate(colnam, .before = id_old) %>%
      dplyr::relocate(colnbr, .before = id_old) %>%
      dplyr::relocate(suffix, .before = id_old) %>%
      dplyr::relocate(position_x, .before = strate_cat) %>%
      dplyr::relocate(position_y, .before = strate_cat)

    if (any(names(res_individuals_full) == "stem_diameter"))
      res <-
      res %>%
      dplyr::relocate(stem_diameter, .before = ind_num_sous_plot)

    if (any(names(res_individuals_full) == "tree_height"))
      res <-
      res %>%
      dplyr::relocate(tree_height, .before = ind_num_sous_plot)

  }

  if(remove_ids & extract_individuals) {

    cli::cli_alert_warning("ids removed - remove_ids = {remove_ids} ")

    # res <-
    #   res %>%
    #   dplyr::select_at(which(!grepl("id_", colnames(res))))

    res <-
      res %>%
      dplyr::rename(idDB = id_n) %>%
      dplyr::select(-tidyselect::starts_with("id_")) %>%
      dplyr::rename(id_n = idDB)

  }

  if(remove_ids & !extract_individuals) {

    cli::cli_alert_warning("ids removed - remove_ids = {remove_ids} ")

    # res <-
    #   res %>%
    #   dplyr::select_at(which(!grepl("id_", colnames(res))))

    res <-
      res %>%
      dplyr::rename(idDB = id_liste_plots) %>%
      dplyr::select(-tidyselect::starts_with("id_")) %>%
      dplyr::rename(id_liste_plots = idDB)

  }

  # dbDisconnect(mydb)

  res_list <-
    list(
      extract = NA,
      census_features = NA,
      coordinates = NA,
      coordinates_sf = NA
    )

  res_list$extract <- res

  if(show_multiple_census)
    res_list$census_features <- census_features

  if (show_all_coordinates)
    res_list$coordinates <- coordinates_subplots

  if (show_all_coordinates)
    res_list$coordinates_sf <- coordinates_subplots_plot_sf

  res_list <- res_list[!is.na(res_list)]

  if (length(res_list) == 1)
    res_list <- res_list[[1]]

  return(res_list)

}



#' List, selected subplots
#'
#' Table of subplot for selected plots
#'
#' @return A tibble of all subplots
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param team_lead string fuzzy person name to look for
#' @param plot_name string fuzzy plot name to look for
#' @param tag numeric exact tag number of the plot
#' @param country string fuzzy country name to look for
#' @param province string fuzzy province name to look for
#' @param locality_name string fuzzy locality_name name to look for
#' @param method stringfuzzy method name to look for
#' @param subtype string subtype to select
query_subplots <- function(team_lead = NULL,
                        plot_name = NULL,
                        tag = NULL,
                        country = NULL,
                        province = NULL,
                        locality_name = NULL,
                        method = NULL,
                        subtype = NULL) {

  queried_plots <-
    query_plots(
      team_lead = team_lead,
      plot_name = plot_name,
      tag = tag,
      country = country,
      province = province,
      locality_name = locality_name,
      method = method,
      remove_ids = FALSE
    )

  cli::cli_alert_info("{nrow(queried_plots)} plots selected")

  sub_plot_data <-
    dplyr::tbl(mydb, "data_liste_sub_plots") %>%
    dplyr::filter(id_table_liste_plots %in% !!queried_plots$id_liste_plots)

  cli::cli_alert_info("subplot_features found for {nrow(dplyr::distinct(sub_plot_data %>%
                                                         dplyr::collect(),
                                                       id_table_liste_plots))} plots")

  all_sub_type <-
    sub_plot_data %>%
    dplyr::distinct(id_type_sub_plot) %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "subplotype_list") %>%
        dplyr::select(type, valuetype, typedescription, id_subplotype),
      by = c("id_type_sub_plot" = "id_subplotype")
    )
  # %>%
  #   dplyr::filter(type != "census")

  if (!is.null(subtype)) {
    all_sub_type <-
      all_sub_type %>%
      dplyr::filter(grepl(subtype, type))
    cli::cli_alert_info("Selected subplot features:")
    print(all_sub_type)
  }

  extracted_data <-
    sub_plot_data %>%
    dplyr::filter(id_type_sub_plot %in% !!(all_sub_type %>%
                                             dplyr::pull(id_type_sub_plot))) %>%
    dplyr::left_join(all_sub_type,
                     by = c("id_type_sub_plot" = "id_type_sub_plot")) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      queried_plots %>%
        dplyr::select(plot_name, id_liste_plots),
      by = c("id_table_liste_plots" = "id_liste_plots")
    ) %>%
    dplyr::select(plot_name,
                  id_table_liste_plots,
                  year,
                  month,
                  year,
                  type,
                  valuetype,
                  typevalue,
                  typevalue_char,
                  original_subplot_name,
                  id_sub_plots)

  return(extracted_data)

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
explore_allometric_taxa <- function(genus_searched = NULL,
                                    tax_esp_searched = NULL,
                                    tax_fam_searched = NULL,
                                    id_search = NULL) {

  if(!exists("mydb")) call.mydb()

  res_taxa <- query_taxa(
    genus = genus_searched,
    species = tax_esp_searched,
    ids =  id_search, verbose = F)

  tax_data <-
    query_plots(id_diconame = res_taxa$idtax_n)

  if(nrow(tax_data)>0) {
    # cat(paste0("\n ", nrow(tax_data), " taxa selected"))
    # print(tax_data$full_name_no_auth)
    #
    # specimens_id_diconame <-
    #   dplyr::tbl(mydb, "specimens") %>%
    #   dplyr::select(id_specimen, id_diconame_n)
    #
    # diconames_id <-
    #   dplyr::tbl(mydb, "diconame") %>%
    #   dplyr::select(id_n, id_good_n)
    #
    # specimens_linked <-
    #   specimens_id_diconame %>%
    #   dplyr::left_join(diconames_id, by=c("id_diconame_n"="id_n")) %>%
    #   dplyr::rename(id_dico_name_specimen=id_good_n) %>%
    #   dplyr::select(id_specimen, id_dico_name_specimen)
    #
    # selec_plot_tables <-
    #   dplyr::tbl(mydb, "data_liste_plots") %>%
    #   dplyr::select(plot_name, team_leader, country, locality_name, id_liste_plots)
    #
    # all_individuals_data <-
    #   dplyr::tbl(mydb, "data_individuals") %>%
    #   dplyr::left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>%
    #   dplyr::left_join(selec_plot_tables, by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
    #   dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>%
    #   dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) %>%
    #   dplyr::filter(id_diconame_final %in% tax_data$id_n) %>%
    #   dplyr::select(id_n, multi_tiges_id_good, plot_name,team_leader,country,locality_name, sous_plot_name, ind_num_sous_plot, code_individu, dbh, full_name_no_auth, full_name_used, tax_fam, tax_gen, tax_esp, morphocat,
    #                 id_diconame_final, dbh_height, tree_height, branch_height, branchlet_height, crown_spread, liane, strate_cat,
    #                 herbarium_code_char, id_specimen) %>%
    #   dplyr::arrange(id_n)

    data_allo1 <-
      tax_data %>%
      # dplyr::select(tree_height, stem_diameter) %>%
      dplyr::filter(!is.na(tree_height), tree_height>0, stem_diameter>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both tree height and stem_diameter values is ", nrow(data_allo1)))

    if(nrow(data_allo1)>1) {
      gg_plot1 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo1,
                            mapping = ggplot2::aes(x = stem_diameter, y = tree_height)) +
        ggplot2::xlab("Stem diameter (cm)") +
        ggplot2::ylab("Tree height (m)")

    }else{
      gg_plot1 <- NA
    }

    data_allo2 <-
      tax_data %>%
      # dplyr::select(tree_height, dbh, crown_spread, id_n, plot_name, country, full_name_no_auth) %>%
      dplyr::filter(!is.na(crown_width), crown_width>0, stem_diameter>0) %>%
      dplyr::collect()

    cat(paste0("\n The number of individuals with both crown_width and stem_diameter values is ", nrow(data_allo2)))
    cat("\n")

    if(nrow(data_allo2)>1) {
      gg_plot2 <-
        ggplot2::ggplot() +
        ggplot2::geom_point(data = data_allo2, mapping =
                              ggplot2::aes(x = stem_diameter, y = crown_width)) +
        ggplot2::xlab("Stem diameter (cm)") +
        ggplot2::ylab("Crown width (m)")
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

  if (nrow(tax_data) > 0)
    return(
      list(
        data_height_dbh = data_allo1,
        data_crow_dbh = data_allo2,
        taxa_data = tax_data,
        plot_height_dbh = gg_plot1,
        plot_crown_dbh = gg_plot2
      )
    )
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
  function (dat = NULL,
            theme = c("GILLES"),
            outfile = "herblabel.rtf",
            font = c("Roman", "Arial"),
            font_size = 1
  )
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
      stop(paste(
        "\"HERBARIUM\" not provided for row: ",
        paste(which(is.na(
          herbdat000$HERBARIUM
        )) +
          1, collapse = ", ")
      ))
    }

    if (any(is.na(herbdat000$COLLECTOR))) {
      stop(paste(
        "\"COLLECTOR\" not provided for row: ",
        paste(which(is.na(
          herbdat000$COLLECTOR
        )) +
          1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$COLLECTOR_NUMBER))) {
      stop(paste(
        "\"COLLECTOR_NUMBER\" not provided for row: ",
        paste(which(is.na(
          herbdat000$COLLECTOR_NUMBER
        )) +
          1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$DATE_COLLECTED))) {
      stop(paste(
        "\"DATE_COLLECTED\" not provided for row: ",
        paste(which(is.na(
          herbdat000$DATE_COLLECTED
        )) +
          1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$FAMILY))) {
      warning(paste(
        "\"FAMILY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$FAMILY
        )) +
          1, collapse = ", ")
      ))
    }
    # if (any(is.na(herbdat000$GENUS))) {
    #   warning(paste("\"GENUS\" must be provided for row: ",
    #                 paste(which(is.na(herbdat000$GENUS)) + 1, collapse = ", ")))
    # }
    if (any(is.na(herbdat000$COUNTRY))) {
      stop(paste(
        "\"COUNTRY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$COUNTRY
        )) +
          1, collapse = ", ")
      ))
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
      warning(paste(
        "\"LOCALITY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$LOCALITY
        )) + 1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$IDENTIFIED_BY))) {
      warning(paste(
        "\"IDENTIFIED_BY\" not provided for row: ",
        paste(which(is.na(
          herbdat000$IDENTIFIED_BY
        )) + 1, collapse = ", ")
      ))
    }
    if (any(is.na(herbdat000$DATE_IDENTIFIED))) {
      warning(paste(
        "\"DATE_IDENTIFIED\" not provided for row: ",
        paste(which(is.na(
          herbdat000$DATE_IDENTIFIED
        )) +
          1, collapse = ", ")
      ))
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

    # same_families <- c("Palmae", "Arecaceae", "Gramineae", "Poaceae",
    #                    "Leguminosae", "Fabaceae", "Guttiferae", "Clusiaceae",
    #                    "Cruciferae", "Brassicaceae", "Labiatae", "Lamiaceae",
    #                    "Compositae", "Asteraceae", "Umbelliferae", "Apiaceae")


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
      herbdat_temp <- herbdat000[i,]
      if (any(
        !is.na(herbdat_temp$LAT_DEGREE),
        !is.na(herbdat_temp$LAT_MINUTE),!is.na(herbdat_temp$LAT_SECOND)
      ) & is.na(herbdat_temp$LAT_FLAG)) {
        lat_check_ind_1[i] <- TRUE
      }
      if (all(
        !is.na(herbdat_temp$LAT_DEGREE),
        !is.na(herbdat_temp$LAT_MINUTE),!is.na(herbdat_temp$LAT_SECOND)
      ) & is.na(herbdat_temp$LAT_FLAG)) {
        lat_check_ind_2[i] <- TRUE
      }
      if (!herbdat_temp$LAT_FLAG %in% c("N", "S", NA)) {
        lat_check_ind_3[i] <- TRUE
      }
      if (any(
        !is.na(herbdat_temp$LON_DEGREE),
        !is.na(herbdat_temp$LON_MINUTE),!is.na(herbdat_temp$LON_SECOND)
      ) & is.na(herbdat_temp$LON_FLAG)) {
        lon_check_ind_1[i] <- TRUE
      }
      if (all(
        !is.na(herbdat_temp$LON_DEGREE),
        !is.na(herbdat_temp$LON_MINUTE),!is.na(herbdat_temp$LON_SECOND)
      ) & is.na(herbdat_temp$LON_FLAG)) {
        lon_check_ind_2[i] <- TRUE
      }
      if (!herbdat_temp$LON_FLAG %in% c("E", "W", NA)) {
        lon_check_ind_3[i] <- TRUE
      }
    }

    if (any(lat_check_ind_1)) {
      lat_check_ind_1_msg <-
        paste(
          "Degree, Minutes and Seconds for Latitude not completed in row:",
          paste(which(lat_check_ind_1), collapse = ", ")
        )
      stop(lat_check_ind_1_msg)
    }
    if (any(lat_check_ind_2)) {
      lat_check_ind_2_msg <- paste("LAT_FLAG not specified in row:",
                                   paste(which(lat_check_ind_2), collapse = ", "))
      stop(lat_check_ind_2_msg)
    }
    if (any(lat_check_ind_3)) {
      lat_check_ind_3_msg <-
        paste("Only N or S is allowed for the LAT_FLAG in row:",
              paste(which(lat_check_ind_3), collapse = ", "))
      stop(lat_check_ind_3_msg)
    }
    if (any(lon_check_ind_1)) {
      lon_check_ind_1_msg <-
        paste(
          "Degree, Minutes and Seconds for Longitude not completed in row:",
          paste(which(lon_check_ind_1), collapse = ", ")
        )
      stop(lon_check_ind_1_msg)
    }
    if (any(lon_check_ind_2)) {
      lon_check_ind_2_msg <- paste("LON_FLAG must be specified in row:",
                                   paste(which(lon_check_ind_2), collapse = ", "))
      stop(lon_check_ind_2_msg)
    }
    if (any(lon_check_ind_3)) {
      lon_check_ind_3_msg <-
        paste("Only N or S is allowed for the LON_FLAG in row:",
              paste(which(lon_check_ind_3), collapse = ", "))
      stop(lon_check_ind_3_msg)
    }

    if (font == "Roman") {
      fonttab <-
        "{\\fonttbl{\\f0\\froman\\fcharset134 SimSun;}{\\f1\\froman\\fcharset134 Times New Roman;}}"
    }

    if (font == "Arial") {
      fonttab <-
        "{\\fonttbl{\\f0\\fswiss\\fcharset134 SimSun;}{\\f1\\fswiss\\fcharset134 Arial;}}"
    }

    temp1 <-
      paste(
        "{\\rtf1\\ansi\\ansicpg936\\deflangfe2052\\fcharset134\\deff1",
        fonttab,
        "{\\stylesheet{\\*\\cs3 Default Paragraph Font;}}{\\colortbl\\red0\\green0\\blue0;\\red255\\green0\\blue0;\\red0\\green255\\blue0;\\red0\\green0\\blue255;}\\paperw12240\\paperh15840\\margl1800\\margr1800\\margt1440\\margb1440\\gutter0\\ftnbj\\aenddoc\\jcompress1\\viewkind4\\viewscale100\\asianbrkrule\\allowfieldendsel\\snaptogridincell\\viewkind4\\sectd\\sbkpage\\pgwsxn11906\\pghsxn16838\\marglsxn600\\margrsxn600\\margtsxn720\\margbsxn10\\guttersxn0\\headery720\\footery720\\pgbrdropt0\\sectdefaultcl\\cols2\\colsx1080\\linebetcol1\\endnhere",
        sep = ""

      )

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
        res <-
          c(
            paste(
              "{\\pard\\keep\\keepn\\fi0\\li0\\brsp20\\qc\\sb350\\sa80\\fs",
              trunc(font_size * 20),
              "",
              herbdat$HERBARIUM,
              "\\b0\\par }",
              sep = ""
            ),
            ifelse(
              is.na(herbdat$TITLE),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\fs",
                trunc(font_size * 18),
                "\\qc\\sb10\\sa100\\b ",
                herbdat$TITLE,
                "\\b0 \\par }",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$FAMILY),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100\\fs",
                trunc(font_size * 18),
                "\\b ",
                "\\b0\\qc0 \\par }",
                sep = ""
              ),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\qc\\sb10\\sa100\\fs",
                trunc(font_size * 18),
                "\\b ",
                herbdat$FAMILY,
                "\\b0\\qc0 \\par }",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$FULL_NAME),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi-288\\li288\\sb100\\sa200\\fs",
                trunc(font_size * 20),
                "\\b\\i ",
                herbdat$FULL_NAME,
                "\\b0\\par}",
                sep = ""
              )
            ),
            paste(
              "{\\pard\\keep\\keepn\\fi0\\li0\\sb120\\sa20\\fs",
              trunc(font_size * 18),
              " ",
              REPLACE(
                paste(
                  toupper(ifelse(
                    is.na(herbdat$COUNTRY), "", herbdat$COUNTRY
                  )),
                  ", ",
                  ifelse(is.na(herbdat$STATE_PROVINCE), "", herbdat$STATE_PROVINCE),
                  ", ",
                  ifelse(is.na(herbdat$COUNTY), "", herbdat$COUNTY),
                  ", ",
                  ifelse(
                    is.na(herbdat$LOCALITY),
                    "",
                    as.character(herbdat$LOCALITY)
                  ),
                  sep = ""
                )
              ),
              "\\par}",
              sep = ""
            ),
            REPLACE(ifelse(
              is.na(herbdat$LAT_DEGREE),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\sb20\\sa150\\fs",
                trunc(font_size * 18),
                "\\qj ",
                herbdat$LAT_DEGREE,
                "\\u176;",
                herbdat$LAT_MINUTE,
                "\\u39;",
                herbdat$LAT_SECOND,
                "\\u34;",
                herbdat$LAT_FLAG,
                ", ",
                herbdat$LON_DEGREE,
                "\\u176;",
                herbdat$LON_MINUTE,
                "\\u39;",
                herbdat$LON_SECOND,
                "\\u34;",
                herbdat$LON_FLAG,
                ifelse(
                  is.na(herbdat$ELEVATION),
                  "",
                  paste("; ", herbdat$ELEVATION, "m", sep = "")
                ),
                "\\par }",
                sep = ""
              )
            )),
            ifelse((is.na(
              herbdat$ATTRIBUTES
            )) &
              (is.na(herbdat$REMARKS)), "", italic_latin(gsub(
                "\\.  ", "\\. ", gsub(" \\.", "\\.", gsub(
                  "\\. \\.", "\\. ", gsub("\\. +", "\\. ",
                                          REPLACE(
                                            paste(
                                              "{\\pard\\keep\\keepn\\fi0\\li0",
                                              paste("\\fs", trunc(font_size * 18), sep = ""),
                                              "\\sb60",
                                              ifelse(is.na(herbdat$ATTRIBUTES), "", Cap2(as.character(
                                                herbdat$ATTRIBUTES
                                              ))),
                                              ifelse(is.na(herbdat$ATTRIBUTES), "", ". "),
                                              ifelse(is.na(herbdat$REMARKS), "", Cap2(as.character(herbdat$REMARKS))),
                                              "\\sa80\\par}",
                                              sep = " "
                                            )
                                          ))
                ))
              ))),
            ifelse(
              is.na(herbdat$ADDITIONAL_COLLECTOR),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\sb200\\sa100\\fs",
                trunc(font_size * 18),
                "\\tqr\\tx4850\\b ",
                herbdat$COLLECTOR,
                ", #",
                herbdat$COLLECTOR_NUMBER,
                "\\b0",
                "  ",
                ifelse(nchar(
                  paste(
                    herbdat$COLLECTOR,
                    herbdat$ADDITIONAL_COLLECTOR,
                    ", #",
                    herbdat$COLLECTOR_NUMBER
                  )
                ) > 40, "\\line", "  "),
                " \\tab ",
                tryCatch(
                  formatdate(herbdat$DATE_COLLECTED),
                  error = function(e) {
                    cat(" ")
                    herbdat$DATE_COLLECTED
                  }
                ),
                "\\par}",
                sep = ""
              ),
              paste(
                "{\\pard\\keep\\keepn\\fi0\\sb200\\sa100",
                paste("\\fs", trunc(font_size * 18), sep = ""),
                "\\tqr\\tx4850\\b ",
                herbdat$COLLECTOR,
                ", ",
                herbdat$ADDITIONAL_COLLECTOR,
                "  #",
                herbdat$COLLECTOR_NUMBER,
                "\\b0",
                "  ",
                ifelse(nchar(
                  paste(
                    herbdat$COLLECTOR,
                    herbdat$ADDITIONAL_COLLECTOR,
                    ", #",
                    herbdat$COLLECTOR_NUMBER
                  )
                ) > 40, "\\line", "  "),
                " \\tab ",
                tryCatch(
                  formatdate(herbdat$DATE_COLLECTED),
                  error = function(e) {
                    cat(" ")
                    herbdat$DATE_COLLECTED
                  }
                ),
                "\\par}",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$PROJECT),
              "",
              paste(
                "{\\pard\\keep\\keepn\\fi0\\li0\\sa160\\fs",
                trunc(font_size * 18),
                "\\ql\\b ",
                as.character(herbdat$PROJECT),
                "\\ql0\\b0\\par }",
                sep = ""
              )
            ),
            ifelse(
              is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER) &
                is.na(herbdat$TYPE_STATUS) &
                is.na(herbdat$IDENTIFIED_BY) & is.na(herbdat$DATE_IDENTIFIED),
              "",
              paste(
                "{\\pard\\keep\\sa40\\keepn\\fi0\\li0\\fs",
                trunc(font_size * 18),
                "\\tqr\\tx4850 ",
                # gsub("_","", ifelse(is.na(herbdat$GLOBAL_UNIQUE_IDENTIFIER), "", as.character(herbdat$GLOBAL_UNIQUE_IDENTIFIER))),
                " \\tab ",
                ifelse(is.na(herbdat$TYPE_STATUS), "", herbdat$TYPE_STATUS),
                ifelse(
                  is.na(herbdat$IDENTIFIED_BY),
                  "",
                  paste(" Det.: ", herbdat$IDENTIFIED_BY)
                ),
                ifelse(is.na(herbdat$DATE_IDENTIFIED), "", ", "),
                ifelse(is.na(herbdat$DATE_IDENTIFIED), "",
                       tryCatch(
                         formatdate(herbdat$DATE_IDENTIFIED),
                         error = function(e) {
                           cat(" ")
                           herbdat$DATE_IDENTIFIED
                         }
                       )),
                "\\par}",
                sep = ""
              )
            ),
            paste(
              "{\\pard\\keep\\keepn\\sa100\\fs",
              trunc(font_size * 18),
              sep = ""
            ),
            " \\par }",
            paste("{\\pard\\keep\\qc\\fs", trunc(font_size * 18), sep = ""),
            "  .                  .                   .\\par}"
          )
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
#' @param new_data tibble
#' @param col_names_select string a vector of string indicating columns names of new_data
#' @param col_names_corresp string a vector of string indicating to which columns selected columns of new_data corresponds
#'
#' @importFrom methods new
#' @importFrom stats dist sd
#' @importFrom utils askYesNo data
#' @importFrom kableExtra cell_spec kable_styling
#'
#' @return No return value, new plots are added
#' @export
add_plots <- function(new_data,
                      col_names_select,
                      col_names_corresp) {

  if(!exists("mydb")) call.mydb()

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
  if (any(colnames(new_data_renamed) == "date_y"))
    if (any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] > lubridate::year(Sys.Date())) |
        any(new_data_renamed$date_y[!is.na(new_data_renamed$date_y)] < 1900))
      stop("ERREUR dans date_y, year provided impossible")
  if (any(colnames(new_data_renamed) == "date_m"))
    if (any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] > 12) |
        any(new_data_renamed$date_m[!is.na(new_data_renamed$date_m)] < 1))
      stop("ERREUR dans date_m, month provided impossible")
  if (any(colnames(new_data_renamed) == "data_d"))
    if (any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] > 31) |
        any(new_data_renamed$data_d[!is.na(new_data_renamed$data_d)] < 1))
      stop("ERREUR dans data_d, day provided impossible")
  if (any(colnames(new_data_renamed) == "ddlon"))
    if (any(new_data_renamed$ddlon > 180) |
        any(new_data_renamed$ddlon < -180))
      stop("ERREUR dans ddlon, longitude provided impossible")
  if (any(colnames(new_data_renamed) == "ddlat"))
    if (any(new_data_renamed$ddlat > 90) |
        any(new_data_renamed$ddlon < -90))
      stop("ERREUR dans ddlat, latitude provided impossible")

  ## Checking if names plot are already in the database
  if(any(colnames(new_data_renamed) == "plot_name")) {
    found_plot <-
      dplyr::tbl(mydb, "data_liste_plots") %>%
      dplyr::filter(plot_name %in% !!new_data_renamed$plot_name) %>%
      dplyr::collect()

    if (nrow(found_plot) > 0) {
      print(found_plot)
      stop("Some plot_name in new data already in the plot list table. No duplicate allowed.")
    }
  }

  ## Checking method
  if(!any(names(new_data_renamed) == "method")) {

    stop("missing method information")

  } else {

    new_data_renamed <-
      new_data_renamed %>%
      mutate(id_method = .link_method(method = unique(new_data_renamed$method)))

    new_data_renamed <-
      new_data_renamed %>%
     dplyr::select(-method)

    col_names_corresp[which(col_names_corresp == "method")] <-
      "id_method"

  }


  ## Checking team_leader
  if(!any(names(new_data_renamed) == "team_leader")) {

    stop("missing team_leader information")

  } else {

    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed,
                   collector_field = "team_leader")

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-col_name)

    col_names_corresp[which(col_names_corresp == "team_leader")] <-
      "id_colnam"

  }


  ## Checking coordinates
  if (any(new_data_renamed$ddlat > 90) |
      any(new_data_renamed$ddlon < -90))
    stop("ERREUR dans ddlat, latitude provided impossible")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(col_names_corresp)

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
               data_modif_m=lubridate::month(Sys.Date()),
               data_modif_y=lubridate::year(Sys.Date()))

  DT::datatable(new_data_renamed)

  add <- utils::askYesNo(msg = "Add these data to the table of plot data?")

  if(add) {
    message(paste("\nMeta data of plots added:", nrow(new_data_renamed)))
    DBI::dbWriteTable(mydb, "data_liste_plots", new_data_renamed, append = TRUE, row.names = FALSE)
  }

  if(!add)
     message("no data added")

  return(new_data_renamed)

}


#' Add an observation in subplot_features table
#'
#' Add a trait measure in subplot_features table
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param collector_field string column name which contain the collector name
#' @param plot_name_field string column name which contain the plot_name for linking
#' @param id_plot_name string id of plot_name
#' @param subplottype_field string vector listing trait columns names in new_data
#' @param add_data logical whether or not data should be added - by default FALSE
#' @param ask_before_update logical ask before adding
#'
#' @export
add_subplot_features <- function(new_data,
                                 col_names_select,
                                 col_names_corresp,
                                 collector_field = NULL,
                                 plot_name_field = NULL,
                                 id_plot_name = NULL,
                                 id_plot_name_corresp = "id_table_liste_plots_n",
                                 subplottype_field,
                                 add_data = FALSE,
                                 ask_before_update = TRUE) {

  if(!exists("mydb")) call.mydb()

  for (i in 1:length(subplottype_field)) if(!any(colnames(new_data)==subplottype_field[i]))
    stop(paste("subplottype_field provide not found in new_data", subplottype_field[i]))

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  if(is.null(plot_name_field) & is.null(id_plot_name)) stop("no plot links provided, provide either plot_name_field or id_plot_name")

  if(!any(col_names_corresp=="day")) {
    warning("no information collection day provided")
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(day = NA)
  }

  if(!any(col_names_corresp=="year")) {
    warning("no information collection year provided")
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(year = NA)
  }

  if(!any(col_names_corresp=="month")) {
    warning("no information collection month provided")
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(month = NA)
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(id_new_data=1:nrow(.))

  ### Linking collectors names
  if(!is.null(collector_field)) {
    if(!any(colnames(new_data_renamed) == collector_field))
      stop("no collector_field found in new dataset")
    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed, collector_field = collector_field)
  }else{
    warning("no information on collector provided")
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_colnam = NA)
  }

  ### Linking plot names
  if(!is.null(plot_name_field)) {
    if(!any(colnames(new_data_renamed)==plot_name_field))
      stop("plot_name_field not found in colnames")

    new_data_renamed <-
      .link_plot_name(data_stand = new_data_renamed, plot_name_field = plot_name_field)

  }

  if(!is.null(id_plot_name)) {

    # if(id_plot_name == "id_table_liste_plots_n") id_plot_name <- "id_table_liste_plots_n"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::all_of(dplyr::vars(id_plot_name)), ~ id_plot_name_corresp)

    if(any(colnames(new_data_renamed) == "plot_name"))
      new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-plot_name)

    if(id_plot_name_corresp == "id_table_liste_plots_n")
    link_plot <-
      new_data_renamed %>%
      dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                         dplyr::select(plot_name, id_liste_plots) %>% dplyr::collect(),
                       by=c("id_table_liste_plots_n" = "id_liste_plots"))


    if(id_plot_name_corresp == "id_old")
    link_plot <-
      new_data_renamed %>%
      dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                         dplyr::select(plot_name, id_old) %>% dplyr::collect(),
                       by=c("id_old" = "id_old"))

    if(dplyr::filter(link_plot, is.na(plot_name)) %>%
       nrow() > 0) {
      print(dplyr::filter(link_plot, is.na(plot_name)))
      warning("provided id plot not found in plot metadata")
    }

    if(id_plot_name_corresp == "id_table_liste_plots_n")
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename(id_liste_plots = id_table_liste_plots_n)

    if(id_plot_name_corresp == "id_old")
      new_data_renamed <-
        new_data_renamed %>%
        left_join(tbl(mydb, "data_liste_plots") %>%
                    dplyr::select(id_old, id_liste_plots) %>%
                    collect(),
                  c("id_old"="id_old"))

  }

  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(subplottype_field))
  for (i in 1:length(subplottype_field)) {

    subplottype <- subplottype_field[i]

    if(!any(colnames(new_data_renamed)==subplottype))
      stop(paste("subplottype field not found", subplottype))

    data_subplottype <-
      new_data_renamed

    subplottype_name <-
      "subplottype"

    data_subplottype <-
      data_subplottype %>%
      dplyr::rename_at((dplyr::vars(dplyr::all_of(subplottype))), ~ subplottype_name)

    data_subplottype <-
      data_subplottype %>%
      dplyr::filter(!is.na(subplottype))

    ### adding subplot id and adding potential issues based on subplot
    data_subplottype <-
      .link_subplotype(data_stand = data_subplottype,
                       subplotype = subplottype)

    print(".add_modif_field")
    data_subplottype <-
      .add_modif_field(dataset = data_subplottype)


    ## see what type of value numeric of character
    valuetype <-
      data_subplottype %>%
      dplyr::distinct(id_subplottype) %>%
      dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                         dplyr::select(valuetype, id_subplotype) %>%
                         dplyr::collect(),
                       by=c("id_subplottype"="id_subplotype"))

    print("data_to_add")
    data_to_add <-
      dplyr::tibble(id_table_liste_plots = data_subplottype$id_liste_plots,
                    id_colnam = data_subplottype$id_colnam,
                    year = data_subplottype$year,
                    month = data_subplottype$month,
                    day = data_subplottype$day,
                    id_type_sub_plot = data_subplottype$id_subplottype,
                    # typevalue = data_subplottype$subplottype,
                    typevalue = ifelse(rep(valuetype$valuetype == "numeric",
                                           nrow(data_subplottype)), data_subplottype$subplottype, NA),
                    typevalue_char = ifelse(rep(valuetype$valuetype == "character",
                                                 nrow(data_subplottype)), data_subplottype$subplottype, NA),
                    original_subplot_name = ifelse(rep(any(colnames(data_subplottype)=="original_subplot_name"), nrow(data_subplottype)), data_subplottype$original_subplot_name, NA),
                    issue = data_subplottype$issue,
                    comment = ifelse(rep(any(colnames(data_subplottype)=="comment"),
                                         nrow(data_subplottype)), data_subplottype$comment, NA),
                    date_modif_d = data_subplottype$date_modif_d,
                    date_modif_m = data_subplottype$date_modif_m,
                    date_modif_y = data_subplottype$date_modif_y)

    if(any(is.na(data_to_add$id_table_liste_plots))) {
      rm_na <- utils::askYesNo(msg = "Remove features not linked to plot?")

      if(rm_na) data_to_add <-
          data_to_add %>%
          filter(!is.na(id_table_liste_plots))

    }

    list_add_data[[i]] <-
      data_to_add

    ## check if new data already exist in database
    selected_new_data <-
      data_to_add %>%
      dplyr::select(id_table_liste_plots, id_type_sub_plot, typevalue) %>%
      tibble::add_column(new = "new")

    all_existing_data <-
      dplyr::tbl(mydb, "data_liste_sub_plots") %>%
      dplyr::select(id_table_liste_plots, id_type_sub_plot, typevalue) %>%
      dplyr::collect() %>%
      tibble::add_column(old = "old")

    crossing_data <-
      selected_new_data %>%
      dplyr::left_join(all_existing_data,
                by = c("id_table_liste_plots"="id_table_liste_plots",
                       "id_type_sub_plot"="id_type_sub_plot")) %>%
      dplyr::filter(new == "new", old == "old")

    continue <- TRUE
    if(nrow(crossing_data)>0) {
      message("Data to be imported already exist in the database")
      print(crossing_data)
      continue <- utils::askYesNo(msg = "Continue importing?")
    }

    print(data_to_add)

  if(continue) {

    if (ask_before_update) {
      response <-
        utils::askYesNo("Confirm add these data to data_liste_sub_plots table?")
    } else {
      response <- TRUE
    }
  } else {
    response <- FALSE
  }

    if(add_data & response) {
      message(paste("adding data:", nrow(data_subplottype), "rows"))
      DBI::dbWriteTable(mydb, "data_liste_sub_plots",
                        data_to_add, append = TRUE, row.names = FALSE)
    }
  }

  # linked_problems_individuals_list <-
  #   linked_problems_individuals_list %>%
  #   dplyr::select(plot_name,
  #                 ind_num_sous_plot,
  #                 country,
  #                 leaf_area,
  #                 specific_leaf_area,
  #                 dbh.x,
  #                 dbh.y,
  #                 original_tax_name,
  #                 corrected.name,
  #                 full_name_no_auth,
  #                 id_table_liste_plots_n,
  #                 ddlon,
  #                 ddlat) %>%
  #   left_join(tbl(mydb, "data_liste_plots") %>%
  #               dplyr::select(plot_name, id_liste_plots) %>%
  #               collect(), by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
  #   rename(dbh_provided = dbh.x,
  #          dbh_database = dbh.y,
  #          name_provided = original_tax_name,
  #          name_provided_corrected = corrected.name,
  #          name_database = full_name_no_auth,
  #          plot_name_provided = plot_name.x,
  #          plot_name_corrected = plot_name.y)


  return(list_add_data)

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
#' @param id_table_plot integer id of plot to be updated
#' @param new_team_leader string new team leader
#' @param new_plot_name string new plot name
#' @param new_country string new country
#' @param new_ddlat double new latitude in decimal degrees
#' @param new_ddlon double new longitude in decimal degrees
#' @param new_elevation integer new elevation data
#' @param new_method string new method data
#' @param new_province string new province data
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value, new plots are added
#' @export
update_plot_data <- function(team_lead = NULL,
                             plot_name = NULL,
                             country = NULL,
                             method = NULL,
                             date_y = NULL,
                             id_table_plot = NULL,
                             new_plot_name = NULL,
                             new_team_leader = NULL,
                             new_country = NULL,
                             new_ddlat = NULL,
                             new_ddlon = NULL,
                             new_elevation = NULL,
                             new_method = NULL,
                             new_province = NULL,
                             new_data_provider = NULL,
                             new_locality_name = NULL,
                             add_backup = TRUE,
                             ask_before_update = TRUE) {

  if(!exists("mydb")) call.mydb()

  if (is.null(id_table_plot)) {
    quer_plots <-
      query_plots(
        team_lead = team_lead,
        plot_name = plot_name,
        country = country,
        method = method,
        date_y = date_y,
        remove_ids = FALSE
      )
  } else{

    quer_plots <-
      query_plots(id_plot = id_table_plot, remove_ids = FALSE)

  }

  if (nrow(quer_plots) == 1) {

    if(!is.null(new_method)) {

      id_new_method <- .link_method(method = new_method)

    }

    new_values <-
      dplyr::tibble(
        plot_name = ifelse(!is.null(new_plot_name), new_plot_name, quer_plots$plot_name),
        id_method  = ifelse(!is.null(new_method), id_new_method, quer_plots$id_method),
        team_leader = ifelse(
          !is.null(new_team_leader),
          new_team_leader,
          quer_plots$team_leader
        ),
        country = ifelse(!is.null(new_country), new_country, quer_plots$country),
        ddlat = ifelse(!is.null(new_ddlat), new_ddlat, quer_plots$ddlat),
        ddlon = ifelse(!is.null(new_ddlon), new_ddlon, quer_plots$ddlon),
        elevation = ifelse(!is.null(new_elevation), new_elevation, quer_plots$elevation),
        province = ifelse(!is.null(new_province), new_province, quer_plots$province),
        data_provider = ifelse(!is.null(new_data_provider), new_data_provider, quer_plots$data_provider),
        locality_name = ifelse(!is.null(new_locality_name), new_locality_name, quer_plots$locality_name)
      )

    # quer_plots_sel <-
    #   quer_plots

    comp_res <- .comp_print_vec(vec_1 = quer_plots  %>%
                                  dplyr::select(!!colnames(new_values)),
                                vec_2 = new_values)

    print(comp_res$comp_html)

    comp_values <- comp_res$comp_tb
#
#     if (new_values$ddlat == -1000 &
#         quer_plots_sel$ddlat > -1000)
#       new_values$ddlat = quer_plots_sel$ddlat
#
#     if (new_values$ddlon == -1000 &
#         quer_plots_sel$ddlon > -1000)
#       new_values$ddlon = quer_plots_sel$ddlon
#
#     if (new_values$elevation == -1000 &
#         quer_plots_sel$elevation > -1000)
#       new_values$elevation = quer_plots_sel$elevation
#
#     if (new_values$ddlat == -1000 &
#         quer_plots_sel$ddlat == -1000)
#       new_values$ddlat = quer_plots_sel$ddlat = NA
#
#     if (new_values$ddlon == -1000 &
#         quer_plots_sel$ddlon == -1000)
#       new_values$ddlon = quer_plots_sel$ddlon = NA
#
#     if (new_values$elevation == -1000 &
#         quer_plots_sel$elevation == -1000)
#       new_values$elevation = quer_plots_sel$elevation = NA
#
#     comp_values <- new_values != quer_plots_sel
#     comp_values <- dplyr::as_tibble(comp_values)
#     comp_values <- comp_values %>%
#       dplyr::select_if( ~ sum(!is.na(.)) > 0)
#
#     print(comp_values)

    if(any(comp_values %>% pull())) {

      # col_sel <-
      #   comp_values %>%
      #   dplyr::select_if( ~ sum(.) > 0) %>%
      #   colnames()
      # cli::cli_h1("Previous values")
      # print(quer_plots %>%
      #         dplyr::select(!!col_sel))
      # cli::cli_h1("New values")
      # print(new_values %>%
      #         dplyr::select(!!col_sel))

      if(ask_before_update) {

        Q <- utils::askYesNo("Confirm these modifications?")

      } else {

        Q <- TRUE

      }


      if(Q) {

        modif_types <-
          paste0(names(comp_values), sep="__")

        if(add_backup) {

          colnames_plots <-
            dplyr::tbl(mydb, "followup_updates_liste_plots")  %>%
            dplyr::select(-date_modified, -modif_type, -id_fol_up_plots) %>%
            # dplyr::top_n(1) %>%
            dplyr::collect(n=1) %>%
            colnames()

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
          DBI::dbSendQuery(mydb, statement="UPDATE data_liste_plots SET plot_name = $2, id_method = $3, team_leader = $4, country = $5, ddlat = $6, ddlon = $7, elevation = $8, province = $9, data_provider = $10, locality_name = $11, data_modif_d=$12, data_modif_m=$13, data_modif_y=$14 WHERE id_liste_plots = $1",
                      params=list(quer_plots$id_liste_plots, # $1
                                  new_values$plot_name, # $2
                                  new_values$id_method, # $3
                                  new_values$team_leader, # $4
                                  new_values$country, # $5
                                  new_values$ddlat, # $6
                                  new_values$ddlon, # $7
                                  new_values$elevation, # $8
                                  new_values$province, # $9
                                  new_values$data_provider, # $10,
                                  new_values$locality_name, # $11
                                  lubridate::day(Sys.Date()), # $12
                                  lubridate::month(Sys.Date()), # $13
                                  lubridate::year(Sys.Date()))) # $14

        # if(show_results) print(dbFetch(rs))
        DBI::dbClearResult(rs)

      }

    }else{

      cli::cli_alert_info("no update because no values differents from the entry")

    }

  } else {

    if (nrow(quer_plots) > 1)
      cli::cli_alert_info("More than 1 plot selected. Select only one.")

    if (nrow(quer_plots) == 0)
      cli::cli_alert_info("No plot to be update found.")
  }
}


#' Update individuals data
#'
#' Update individuals plot _ one or more individuals at a time
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data data frame data containing id and new values
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
                               launch_update = FALSE,
                               add_backup = TRUE) {

  if(!exists("mydb")) call.mydb()

  all_colnames_ind <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::select(-dbh, -liane, -tree_height, -branch_height, -branchlet_height, -crown_spread, -dbh_height) %>%
    colnames()

  if(length(col_names_select) != length(col_names_corresp))
    stop("col_names_select and col_names_corresp should have same length")

  for (i in 1:length(col_names_select))
    if(!any(col_names_select[i] == colnames(new_data)))
      stop(paste(col_names_select[i], "not found in new_data"))

  for (i in 1:length(col_names_corresp))
    if(!any(col_names_corresp[i] == all_colnames_ind))
      stop(paste(col_names_corresp[i], "not found in data_individuals, check others tables, observation/traits should be updated in traits_measurements table"))

  id_db <- col_names_corresp[id_col]

  if(!any(id_db == c("id_old", "id_n"))) stop("id for matching should be one of id_old or id_n")

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename_at(dplyr::vars(col_names_select[-id_col]), ~ col_names_corresp[-id_col])

  # dataset = new_data_renamed
  # col_new = col_names_corresp
  # id_col_nbr = id_col
  # type_data = "individuals"

  output_matches <- .find_ids(dataset = new_data_renamed,
                                  col_new = col_names_corresp,
                                  id_col_nbr = id_col,
                                  type_data = "individuals")

  matches_all <-
    output_matches[[2]]

  for (i in 1:length(matches_all)) {

    field <- names(matches_all)[i]
    var_new <- paste0(field, "_new")
    matches <- matches_all[[i]]

    if(launch_update & nrow(matches)>0) {

      matches <-
        matches %>%
        dplyr::select(id, dplyr::contains("_new"))
      matches <-
        .add_modif_field(matches)

      all_id_match <- dplyr::pull(dplyr::select(matches, id))

      if(add_backup) {

        quo_var_id <- rlang::parse_expr(quo_name(rlang::enquo(id_db)))

        all_rows_to_be_updated <-
          dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::filter(!!quo_var_id %in% all_id_match) %>%
          dplyr::collect()

        colnames_plots <-
          dplyr::tbl(mydb, "followup_updates_individuals")  %>%
          dplyr::select(-date_modified, -modif_type, -id_fol_up_ind) %>%
          dplyr::collect() %>%
          dplyr::top_n(1) %>%
          colnames()

        all_rows_to_be_updated <-
          all_rows_to_be_updated %>%
          dplyr::select(dplyr::one_of(colnames_plots))

        all_rows_to_be_updated <-
          all_rows_to_be_updated %>%
          tibble::add_column(date_modified=Sys.Date()) %>%
          tibble::add_column(modif_type=field)

        print(all_rows_to_be_updated %>%
                dplyr::select(modif_type, date_modified))

        DBI::dbWriteTable(mydb, "followup_updates_individuals",
                          all_rows_to_be_updated, append = TRUE, row.names = FALSE)
      }

      if(any(names(matches) == "idtax_n_new"))
        matches <-
        matches %>%
        dplyr::mutate(idtax_n_new == as.integer(idtax_n_new))

      ## create a temporary table with new data
      DBI::dbWriteTable(mydb, "temp_table", matches,
                        overwrite=T, fileEncoding = "UTF-8", row.names=F)

      query_up <-
        paste0("UPDATE data_individuals t1 SET (",field,", data_modif_d, data_modif_m, data_modif_y) = (t2.",var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.", id_db," = t2.id")

      rs <-
        DBI::dbSendStatement(mydb, query_up)

      cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
      rs@sql
      DBI::dbClearResult(rs)

    } else{

      if (launch_update & nrow(matches) == 0)
        cat("\n No new values found")

    }
  }
  return(matches_all)
}


#' Update specimens table
#'
#' Update specimens table
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
update_ident_specimens <- function(colnam = NULL,
                                   number = NULL,
                                   id_speci = NULL,
                                   new_genus = NULL,
                                   new_species = NULL,
                                   new_family = NULL,
                                   id_new_taxa = NULL,
                                   new_detd = NULL,
                                   new_detm = NULL,
                                   new_dety = NULL,
                                   new_detby = NULL,
                                   new_detvalue = NULL,
                                   new_colnbr = NULL,
                                   new_suffix = NULL,
                                   add_backup = TRUE,
                                   show_results = TRUE,
                                   only_new_ident = TRUE,
                                   ask_before_update = TRUE) {

  if(!exists("mydb")) call.mydb()

  if(is.null(id_speci)) {

    if(!is.numeric(number)) stop("number specimen is not a numeric, it must be numeric")

    new_data_renamed <-
      .link_colnam(data_stand = tibble(collector = colnam), collector_field = "collector")

    queried_speci <-
      query_specimens(id_colnam = new_data_renamed$id_colnam,
                      number = number, subset_columns = FALSE)

  } else {

    queried_speci <-
      query_specimens(id_search = id_speci, subset_columns = FALSE)

  }

  if (nrow(queried_speci) > 0) {

    print(
      queried_speci %>%
        dplyr::select(
          family_name,
          surname,
          colnbr,
          suffix,
          detd,
          detm,
          dety,
          detby,
          cold,
          colm,
          coly,
          country,
          id_specimen
        )
    )


    if (nrow(queried_speci) == 1) {
      nbr_queried_speci_ok <- TRUE
    } else{
      nbr_queried_speci_ok <- FALSE
    }

    if(nbr_queried_speci_ok)
      modif_types <- vector(mode = "character", length = nrow(queried_speci))

    if (is.null(id_new_taxa)) {

      if (!is.null(new_genus) | !is.null(new_family) | !is.null(new_species)) {

        query_new_taxa <-
          query_taxa(genus = new_genus,
                     species = new_species,
                     family = new_family,
                     check_synonymy = F,
                     extract_traits = F,
                     class = NULL)

      } else {

        query_new_taxa <- tibble(1)
      }

    } else {

      query_new_taxa <-
        query_taxa(
          ids = id_new_taxa,
          check_synonymy = F,
          extract_traits = F,
          class = NULL
        )

    }

    if (nrow(query_new_taxa) == 1) {

      nbr_new_taxa_ok <- TRUE

    } else{

      nbr_new_taxa_ok <- FALSE

    }

    if(nbr_new_taxa_ok & nbr_queried_speci_ok) {

      new_values <-
        dplyr::tibble(
          idtax_n =
            ifelse(
              !is.null(new_genus) |
                !is.null(new_species) |
                !is.null(new_family) |
                !is.null(id_new_taxa),
              query_new_taxa$idtax_n,
              queried_speci$idtax_n
            ),
          detd = ifelse(!is.null(new_detd), as.numeric(new_detd), queried_speci$detd),
          detm = ifelse(!is.null(new_detm), as.numeric(new_detm), queried_speci$detm),
          dety = ifelse(!is.null(new_dety), as.numeric(new_dety), queried_speci$dety),
          detby = ifelse(!is.null(new_detby), new_detby, queried_speci$detby),
          detvalue = ifelse(!is.null(new_detvalue), new_detvalue, queried_speci$detvalue),
          colnbr = ifelse(!is.null(new_colnbr), new_colnbr, queried_speci$colnbr),
          suffix = ifelse(!is.null(new_suffix), new_suffix, queried_speci$suffix)
        )

      ## correcting if NA is not well coded and null
      if(!is.na(new_values$detby))
        if(new_values$detby == "NA")
          new_values$detby <- NA


        comp_res <- .comp_print_vec(vec_1 = queried_speci  %>%
                         dplyr::select(!!colnames(new_values)),
                       vec_2 = new_values)

        if(!is.na(comp_res$comp_html))
          print(comp_res$comp_html)


        print(queried_speci %>%
                dplyr::select(family_name, surname, colnbr, suffix, detd, detm, dety, detby, cold, colm, coly, country, id_specimen))

        # htmlTable::htmlTable(comp_res$comp_html)

        comp_values <- comp_res$comp_tb

      # new_values <-
      #   new_values %>%
      #   tidyr::replace_na(list(detvalue.x = 0,
      #                          detd = 0,
      #                          detm = 0,
      #                          dety = 0,
      #                          detby = 0))
      #
      # query_select <-
      #   queried_speci %>%
      #   dplyr::select(id_diconame_n, detd, detm, dety, detby, detvalue) %>%
      #   tidyr::replace_na(list(detvalue.x =0, detd = 0, detm = 0, dety = 0, detby=0))
      #
      # if(new_values$detd==0 & query_select$detd>0) new_values$detd <- query_select$detd
      # if(new_values$detm==0 & query_select$detm>0) new_values$detm <- query_select$detm
      # if(new_values$dety==0 & query_select$dety>0) new_values$dety <- query_select$dety
      # if(new_values$detby==0 & query_select$detby>0) new_values$detby <- query_select$detby
      #
      # if(new_values$detd==0 & query_select$detd==0) new_values$detd <- query_select$detd <- NA
      # if(new_values$detm==0 & query_select$detm==0) new_values$detm <- query_select$detm <- NA
      # if(new_values$dety==0 & query_select$dety==0) new_values$dety <- query_select$dety <- NA
      # if(new_values$detby==0 & query_select$detby==0) new_values$detby <- query_select$detby <- NA
      #
      # comp_values <- new_values != query_select
      # comp_values <- dplyr::as_tibble(comp_values)
      # comp_values <- comp_values %>%
      #   dplyr::select_if(~sum(!is.na(.)) > 0)

    } else{
      comp_values <- TRUE
    }

    if (only_new_ident & nbr_new_taxa_ok & any(comp_values == TRUE)) {
      if (any(comp_values %>%
              dplyr::select_if( ~ sum(.) > 0) %>%
              colnames() == "idtax_n")) {

        new_ident <- TRUE

      } else{

        new_ident <- FALSE

      }

    } else {

      new_ident <- TRUE

    }

    if (nbr_new_taxa_ok & any(comp_values == TRUE) &
        nbr_queried_speci_ok & new_ident) {

      modif_types <-
        paste0(colnames(as.matrix(comp_values))[which(as.matrix(comp_values))], sep="__")

      # if(comp_values$id_good_diconame & any(new_values %>% dplyr::select(detd, detm, dety)==0)) {
      #   if(new_values$detd==0) new_values$detd <- lubridate::day(Sys.Date())
      #   if(new_values$detm==0) new_values$detm <- lubridate::month(Sys.Date())
      #   if(new_values$dety==0) new_values$dety <- lubridate::year(Sys.Date())
      # }

      # print(modif_types)
      # col_sel <-
      #   comp_values %>%
      #   rename(id_diconame_n = id_good_diconame) %>%
      #   dplyr::select_if(~sum(.) > 0) %>%
      #   colnames()
      #
      # sel_new_values <-
      #   new_values %>%
      #   rename(id_diconame_n = id_good_diconame) %>%
      #   dplyr::select(!!col_sel)
      #
      # query_select <-
      #   query_select %>%
      #   dplyr::select(!!col_sel)
      #
      #
      #
      # comp_tb <-
      #   tibble(cols = colnames(query_select),
      #        current = unlist(query_select),
      #        new = unlist(sel_new_values)) %>%
      #   mutate(comp = ifelse(current == new, FALSE, TRUE)) %>%
      #   mutate(col = ifelse(comp,
      #                       kableExtra::cell_spec(comp, color = "red", bold = T))) %>%
      #   knitr::kable(escape = F)
      #
      # htmlTable::htmlTable(comp_tb)
      #
      # print(new_values %>%
      #         dplyr::select(!!col_sel) )
      # print(query_tax_all(id_search = queried_speci$id_diconame_n,
      #                     show_synonymies = F) %>%
      #         dplyr::select(-full_name_used, -full_name_used2) %>%
      #         as.data.frame())
      # print(query_new_taxa %>%
      #         dplyr::select(-full_name_used, -full_name_used2) %>%
      #         as.data.frame())

      if (ask_before_update) {

        confirmed <- utils::askYesNo("Confirm this update?")

      } else
      {
        confirmed <- TRUE
      }

      if(confirmed) {
        if(add_backup) {

          colnames_speci <-
            dplyr::tbl(mydb, "followup_updates_specimens") %>%
            dplyr::select(-date_modified, -modif_type, -id_fol_up_specimens) %>%
            dplyr::collect() %>%
            dplyr::top_n(1) %>%
            colnames()

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
          DBI::dbSendQuery(mydb, statement="UPDATE specimens SET idtax_n=$2, detd=$3, detm=$4, dety=$5, detby=$6, detvalue=$7, colnbr=$8, suffix=$9, data_modif_d=$10, data_modif_m=$11, data_modif_y=$12 WHERE id_specimen = $1",
                           params=list(queried_speci$id_specimen, # $1
                                       new_values$idtax_n, # $2
                                       as.numeric(new_values$detd), # $3
                                       as.numeric(new_values$detm), # $4
                                       as.numeric(new_values$dety), # $5
                                       new_values$detby, # $6
                                       new_values$detvalue, # $7
                                       new_values$colnbr, # $8
                                       new_values$suffix, # $9
                                       lubridate::day(Sys.Date()), # $10
                                       lubridate::month(Sys.Date()), # $11
                                       lubridate::year(Sys.Date()))) # $12

        # if(show_results) print(dbFetch(rs))
        DBI::dbClearResult(rs)

        if(show_results) query_specimens(id_search = queried_speci$id_specimen)
      }

    } else{
      if (!nbr_new_taxa_ok)
        cat(
          "\n NO UPDATE. The number of taxa selected for new identification is ",
          nrow(query_new_taxa),
          ". Select one taxa."
        )
      if (!any(comp_values == TRUE))
        cat("\n NO UPDATE. No different values entered.")
      if (!nbr_queried_speci_ok)
        cat(
          "\n NO UPDATE. The number of specimen selected is ",
          nrow(queried_speci),
          ". Select ONE specimen. Not less, not more."
        )
      if (!new_ident)
        cat("\n No new identification")
    }

    return(NA)
  } else {

    cat("\n SPECIMEN NOT FOUND")
    return(dplyr::tibble(collector = dplyr::tbl(mydb, "table_colnam") %>%
                           dplyr::filter(id_table_colnam == !!new_data_renamed$id_colnam) %>%
                           dplyr::select(colnam) %>%
                           dplyr::collect() %>%
                           dplyr::pull(),
                  number = number))

  }
}





#' Internal function
#'
#' Compare two rows and provide cols that differ and html with coloring different values
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param vec_1 tibble one row and same col numbers of vec_2
#' @param vec_2 tibble one row and same col numbers of vec_1
#'
#' @importFrom kableExtra cell_spec kable_styling
#'
#' @return A list with one tibble of logical and a html table
#' @export
.comp_print_vec <- function(vec_1, vec_2) {

  # vec_2 <- vec_2 %>%
  #   replace(., is.na(.), -9999)

  vars_2_num <- names(vec_2)[unlist(lapply(vec_2, is.numeric))]
  vars_2_char <- names(vec_2)[unlist(lapply(vec_2, is.character))]

  vec_2 <- vec_2 %>%
    mutate_at(vars(all_of(c(
      vars_2_num
    ))),
    ~ tidyr::replace_na(. , -9999))

  vec_2 <- vec_2 %>%
    mutate_at(vars(all_of(c(
      vars_2_char
    ))),
    ~ tidyr::replace_na(. , "-9999"))

  vars_1_num <- names(vec_2)[unlist(lapply(vec_1, is.numeric))]
  vars_1_char <- names(vec_2)[unlist(lapply(vec_1, is.character))]

  vec_1 <- vec_1 %>%
    mutate_at(vars(all_of(c(
      vars_1_num
    ))),
    ~ tidyr::replace_na(. , -9999))
  vec_1 <- vec_1 %>%
    mutate_at(vars(all_of(c(
      vars_1_char
    ))),
    ~ tidyr::replace_na(. , "-9999"))

  vec_2_miss <- vec_2
  vec_2_miss <- vec_2_miss %>%
    mutate_if(is.character, list(~ if_else(. == "-9999", TRUE, FALSE)))
  vec_2_miss <- vec_2_miss %>%
    mutate_if(is.numeric, list(~ if_else(. == -9999, TRUE, FALSE)))

  vec_1_miss <- vec_1
  vec_1_miss <- vec_1_miss %>%
    mutate_if(is.character, list(~ if_else(. == "-9999", TRUE, FALSE)))
  vec_1_miss <- vec_1_miss %>%
    mutate_if(is.numeric, list(~ if_else(. == -9999, TRUE, FALSE)))

  comp_val <-
    vec_1 != vec_2

  comp_val <-
    as_tibble(comp_val)

  comp_val <-
    comp_val %>%
    dplyr::select_if(~isTRUE(.))

  if (ncol(comp_val) > 0) {

    if (any(colnames(vec_1) == "idtax_n")) {
      old_tax <-
        query_taxa(ids = vec_2$idtax_n, check_synonymy = F, class = NULL, extract_traits = F)

      new_tax <-
        query_taxa(ids = vec_1$idtax_n, check_synonymy = F, class = NULL, extract_traits = F)

      vec_1 <-
        vec_1 %>%
        dplyr::left_join(
          new_tax %>%
            dplyr::select(tax_fam, tax_gen, tax_esp, idtax_n),
          by = c("idtax_n" = "idtax_n")
        )

      vec_2 <-
        vec_2 %>%
        dplyr::left_join(
          old_tax %>%
            dplyr::select(tax_fam, tax_gen, tax_esp, idtax_n),
          by = c("idtax_n" = "idtax_n")
        )

    }

    comp_tb <-
      tibble(
        cols = colnames(vec_1),
        current = unlist(vec_1),
        new = unlist(vec_2)
      )

    comp_tb_html <- comp_tb

    comp_tb_html <-
      comp_tb_html %>%
      replace(., is.na(.),"-9999") %>%
      mutate(new :=
               kableExtra::cell_spec(new, "html",
                                     color = ifelse(new != current, "red", "blue"))) %>%
      replace(., . == "-9999", NA)

    comp_tb_html <-
      comp_tb_html %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F)

    return(list(comp_tb = comp_val, comp_html = comp_tb_html))

  } else{

    return(list(comp_tb = FALSE, comp_html = NA))

  }

  # print(comp_tb_html)

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
#' @param new_tax_rank1 string new rank
#' @param new_tax_name1 string new name of rank1
#' @param new_taxook integer new tax code
#' @param new_morphocat integer new morphocat code
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
# update_dico_name <- function(genus_searched = NULL,
#                              tax_esp_searched = NULL,
#                              tax_fam_searched = NULL,
#                              # tax_order_searched = NULL,
#                              id_searched = NULL,
#                              new_tax_gen = NULL,
#                              new_tax_esp = NULL,
#                              new_tax_fam = NULL,
#                              # new_tax_order = NULL,
#                              new_tax_rank1 = NULL,
#                              new_tax_name1 = NULL,
#                              # new_introduced_status = NULL,
#                              ask_before_update = TRUE,
#                              add_backup = TRUE,
#                              show_results = TRUE,
#                              cancel_synonymy= FALSE,
#                              synonym_of = NULL,
#                              exact_match = FALSE) {
#
#   if(!exists("mydb")) call.mydb()
#
#   if(all(is.null(c(genus_searched, tax_esp_searched,
#                    tax_fam_searched, synonym_of,
#                    id_searched)) & !cancel_synonymy))
#     stop("\n Provide the species to be updated or precise new synonymy")
#
#   ### checking if at least one modification is asked
#   new_vals <- c(new_tax_gen = new_tax_gen,
#                 new_tax_esp = new_tax_esp,
#                 new_tax_fam = new_tax_fam)
#   if (!any(!is.null(new_vals)) &
#       is.null(synonym_of) &
#       !cancel_synonymy)
#     stop("\n No new values to be updated.")
#
#   ### querying for entries to be modified
#   if(is.null(id_searched)) {
#     cat(paste("\n", genus_searched, " - ", tax_esp_searched, "-", tax_fam_searched))
#     query_tax <-
#       query_tax_all(genus_searched = genus_searched, tax_esp_searched = tax_esp_searched,
#                     tax_fam_searched = tax_fam_searched)
#     #    , check_synonymy = FALSE,
#     # exact_match = exact_match
#   }else{
#     query_tax <-
#       query_tax_all(id_search = id_searched)
#     # , check_synonymy = FALSE
#   }
#
#   if(is.null(query_tax)) query_tax <- tibble()
#
#   if(nrow(query_tax)>0) {
#     message(paste("\n ", nrow(query_tax), "taxa selected"))
#     print(query_tax %>% as.data.frame())
#     nrow_query=TRUE
#   }else{
#     nrow_query=FALSE
#   }
#
#   if(nrow_query)
#     modif_types <-
#     vector(mode = "character", length = nrow(query_tax))
#
#   ## if the modification does not concern synonymies, check if provided values are different for those existing
#   if(nrow_query & !cancel_synonymy & is.null(synonym_of)) {
#
#     query_tax_n <- query_tax
#     col_new <- c()
#     for (i in c("tax_esp", "tax_fam",
#                 "tax_gen", "tax_rank01", "tax_nam01")) {
#       if(any(grepl(pattern = i, x = names(new_vals)))) {
#         col_new <- c(col_new, i)
#         var <- enquo(i)
#         query_tax_n <-
#           query_tax_n %>%
#           dplyr::mutate(!!var := new_vals[grep(i, names(new_vals))])
#       }
#     }
#
#     query_tax_n <-
#       query_tax_n %>%
#       dplyr::select(col_new)
#
#     # new_vals <-
#     #   dplyr::tibble(
#     #     tax_order = ifelse(!is.null(new_tax_order), new_tax_order, query_tax$tax_order),
#     #     tax_fam = ifelse(!is.null(new_tax_fam), as.character(new_tax_fam), query_tax$tax_fam),
#     #     tax_gen = ifelse(!is.null(new_tax_gen), as.character(new_tax_gen), query_tax$tax_gen),
#     #     tax_esp = ifelse(!is.null(new_tax_esp), as.character(new_tax_esp), query_tax$tax_esp),
#     #     tax_rank1 = ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank01),
#     #     tax_name1 = ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_nam01),
#     #     introduced_status = ifelse(!is.null(new_introduced_status), new_introduced_status, query_tax$introduced_status)
#     #                 )
#
#     query_tax_n <-
#       query_tax_n %>%
#       replace(., is.na(.), -9999)
#
#     sel_query_tax <-
#       dplyr::bind_rows(query_tax_n, query_tax %>%
#                          dplyr::select(col_new))
#
#     sel_query_tax <-
#       sel_query_tax %>%
#       replace(., is.na(.), -9999)
#
#     print(sel_query_tax)
#
#     comp_vals <-
#       apply(
#         sel_query_tax,
#         MARGIN = 2,
#         FUN = function(x)
#           unique(x[nrow(query_tax_n)]) != x[(nrow(query_tax_n) + 1):length(x)]
#       )
#
#     # comp_vals <-
#     #   apply(sel_query_tax, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
#
#     if(!is.null(nrow(comp_vals))) {
#       query_tax <-
#         query_tax[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
#       modif_types <- modif_types[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x))]
#       comp_vals <-
#         apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
#     }else{
#       query_tax <- query_tax
#     }
#
#     if(any(is.na(comp_vals)))
#       comp_vals <-
#       comp_vals[!is.na(comp_vals)]
#
#     modif_types[1:length(modif_types)] <-
#       paste0(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")
#
#   }else{
#     comp_vals <- TRUE
#   }
#
#   new_id_diconame_good <- NULL
#   if(nrow_query & cancel_synonymy) {
#     if(query_tax$id_good_n == query_tax$id_n) {
#
#       cat("\n This taxa is not considered as synonym. No modification is thus done on its synonymy")
#       comp_vals <- FALSE
#
#     }else{
#
#       new_id_diconame_good <- NA
#
#       modif_types[1:length(modif_types)] <-
#         paste(modif_types, "cancel_synonymy", sep="__")
#
#     }
#
#   }
#
#
#   Q.syn2 <- FALSE
#   if(nrow_query & !is.null(synonym_of)) {
#     Q.syn <- TRUE
#
#     ## checking if taxa selected is already a synonym of another taxa
#     if(query_tax$id_good_n != query_tax$id_n) {
#
#       if(query_tax$id_good_n != query_tax$id_n) {
#
#         query_tax_all(id_search = query_tax$id_good_n)
#
#         Q.syn <-
#           utils::askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
#
#       }
#     }
#
#     if(Q.syn) {
#
#       ## checking if others names are pointing to the selected taxa as synonyms
#       syn_of_new_syn <-
#         tbl(mydb, "diconame") %>%
#         filter(id_good_n == !!query_tax$id_n) %>%
#         collect()
#
#       if(nrow(syn_of_new_syn) > 0) {
#
#         message("\n Some names are considered synonyms of the selected taxa:")
#
#         print(
#           syn_of_new_syn %>%
#             dplyr::select(
#               tax_fam,
#               tax_gen,
#               tax_esp,
#               tax_rank1,
#               tax_name1,
#               id_n,
#               id_good_n
#             )
#         )
#
#         Q.syn2 <-
#           utils::askYesNo("Do you confirm to also modify the synonymies of these selected names?",
#                           default = FALSE)
#
#         if(Q.syn2)
#           ids_others_names_synonyms <-
#           syn_of_new_syn$id_n
#
#       }
#
#       # if(Q.syn2) {
#
#       if (!any(names(synonym_of) == "genus"))
#         synonym_of$genus <- NULL
#       if (!any(names(synonym_of) == "species"))
#         synonym_of$species <- NULL
#       if (!any(names(synonym_of) == "id"))
#         synonym_of$id <- NULL
#
#       new_syn <-
#         query_tax_all(genus_searched = synonym_of$genus,
#                       tax_esp_searched = synonym_of$species,
#                       id_search = synonym_of$id,
#                       show_synonymies = FALSE)
#
#       if(nrow(new_syn) == 0) {
#
#         cat("\n No taxa found for new synonymy. Select one.")
#         Q.syn <- FALSE
#
#       }
#
#       if(nrow(new_syn) > 1) {
#
#         cat("\n More than one taxa found for new synonymy. Select only one.")
#         Q.syn <- FALSE
#
#       }
#
#       if(nrow(new_syn) == 1) {
#
#         # message("\n synonym of:")
#         # print(new_syn %>% as.data.frame())
#
#         new_id_diconame_good <- new_syn$id_n
#
#         modif_types[1:length(modif_types)] <-
#           paste(modif_types, "new_synonymy", sep="__")
#
#       }
#
#       # }
#     }
#
#   }else{
#     Q.syn <- TRUE
#   }
#
#   # if(!any(comp_vals)) stop("No update performed because no values are different.")
#
#   if(any(comp_vals) & Q.syn & nrow_query) {
#
#     cli::cli_alert_info(" Number of rows selected to be updated {nrow(query_tax)} ")
#
#     if(ask_before_update) {
#       Q <-
#         utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
#     }else{
#       Q <- TRUE
#     }
#
#     if(Q) {
#
#       if(add_backup) {
#
#         query_tax <-
#           query_tax %>%
#           tibble::add_column(date_modified = Sys.Date()) %>%
#           tibble::add_column(modif_type = modif_types)
#
#         DBI::dbWriteTable(mydb, "followup_updates_diconames",
#                           query_tax, append = TRUE, row.names = FALSE)
#
#         if(Q.syn2) {
#           syn_of_new_syn <-
#             syn_of_new_syn %>%
#             tibble::add_column(date_modified = Sys.Date()) %>%
#             tibble::add_column(modif_type = modif_types)
#
#           DBI::dbWriteTable(mydb, "followup_updates_diconames",
#                             syn_of_new_syn, append = TRUE, row.names = FALSE)
#
#
#         }
#       }
#
#       rs <-
#         DBI::dbSendQuery(
#           mydb,
#           statement = "UPDATE diconame SET tax_fam=$2, tax_gen=$3, tax_esp=$4, id_good_n=$5, tax_rank1=$6, tax_name1=$7 WHERE id_n = $1",
#           params = list(
#             query_tax$id_n,
#             # $1
#             rep(
#               ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam),
#               nrow(query_tax)
#             ),
#             # $2
#             rep(
#               ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen),
#               nrow(query_tax)
#             ),
#             # $3
#             rep(
#               ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp),
#               nrow(query_tax)
#             ),
#             # $4
#             # rep(ifelse(!is.null(new_tax_order), new_tax_order, query_tax$tax_order), nrow(query_tax)), # $5
#             rep(
#               ifelse(
#                 !is.null(new_id_diconame_good),
#                 new_id_diconame_good,
#                 query_tax$id_good_n
#               ),
#               nrow(query_tax)
#             ),
#             # $5
#             rep(
#               ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank1),
#               nrow(query_tax)
#             ),
#             # $6
#             rep(
#               ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_name1),
#               nrow(query_tax)
#             )
#           )
#         ) # $7
#
#       DBI::dbClearResult(rs)
#
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                          params=list(query_tax$id_n))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
#
#       if(Q.syn2) {
#         cli::cli_alert_info("Updating synonymies for others taxa")
#         rs <-
#           DBI::dbSendQuery(mydb, statement="UPDATE diconame SET id_good_n=$2 WHERE id_n = $1",
#                            params= list(ids_others_names_synonyms, # $1
#                                         rep(ifelse(!is.null(new_id_diconame_good),
#                                                    new_id_diconame_good, syn_of_new_syn$id_good_n),
#                                             nrow(syn_of_new_syn)) # $2
#                            ))
#
#         DBI::dbClearResult(rs)
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                            params=list(ids_others_names_synonyms))
#         if(show_results) print(DBI::dbFetch(rs))
#         DBI::dbClearResult(rs)
#
#
#       }
#
#     }
#   }else{
#     if(nrow(query_tax)==0) print("No update because no taxa found.")
#
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#
#     if(!Q.syn) print("No update because new synonymy not correctly defined.")
#
#     if(!nrow_query) print("No updates because none taxa were found based on query parameters (genus/species/family/id)")
#
#   }
# }


# update_dico_name <- function(genus_searched = NULL,
#                              tax_esp_searched = NULL,
#                              tax_fam_searched = NULL,
#                              new_tax_gen = NULL,
#                              new_tax_esp = NULL,
#                              new_full_name_auth = NULL,
#                              new_tax_fam = NULL,
#                              new_tax_rank1 = NULL,
#                              new_tax_name1 = NULL,
#                              new_taxook = NULL,
#                              new_morphocat = NULL,
#                              new_detvalue = NULL,
#                              new_full_name_no_auth = NULL,
#                              new_full_name_used = NULL,
#                              new_full_name_used2 = NULL,
#                              new_id_diconame_good = NULL,
#                              id_search = NULL,
#                              ask_before_update = TRUE,
#                              add_backup = TRUE,
#                              show_results = TRUE,
#                              no_synonym_modif = FALSE,
#                              synonym_of = NULL
# ) {
#
#   if(!exists("mydb")) call.mydb()
#
#   if(all(is.null(c(genus_searched, tax_esp_searched, tax_fam_searched, synonym_of, id_search, new_full_name_no_auth, new_full_name_used, new_full_name_used2, new_morphocat))) & !no_synonym_modif) stop("\n Provide the species to be updated or precise new synonymy")
#
#   ### checking if at least one modification is asked
#   new_vals <- c(new_tax_gen, new_tax_esp, new_full_name_auth, new_tax_fam, new_taxook,new_detvalue, new_id_diconame_good, no_synonym_modif, new_full_name_no_auth, new_full_name_used, new_full_name_used2, new_morphocat)
#   if(!any(!is.null(new_vals)) & is.null(synonym_of)) stop("\n No new values to be updated.")
#
#   ## if the modif is a change in synonymy, show synonyms
#   if(no_synonym_modif | !is.null(synonym_of)) {
#     show_synonymies <- TRUE
#   }else{
#     show_synonymies <- FALSE
#   }
#
#   ### querying for entries to be modified
#   if(is.null(id_search)) {
#     query_tax <-
#       query_tax_all(genus_searched = genus_searched, tax_esp_searched = tax_esp_searched, tax_fam_searched = tax_fam_searched, show_synonymies=show_synonymies)
#   }else{
#     query_tax <-
#       query_tax_all(id_search = id_search, show_synonymies = FALSE)
#   }
#
#   if(nrow(query_tax)>0) {
#     nrow_query=TRUE
#   }else{
#     nrow_query=FALSE
#   }
#
#   if(nrow_query)
#     modif_types <-
#     vector(mode = "character", length = nrow(query_tax))
#
#   ## if the modification does not concern synonymies, check if provided values are different for those existing
#   if(nrow_query & !no_synonym_modif & is.null(synonym_of)) {
#     new_vals <-
#       dplyr::tibble(tax_fam=ifelse(!is.null(new_tax_fam), as.character(new_tax_fam), query_tax$tax_fam),
#                     tax_gen=ifelse(!is.null(new_tax_gen), as.character(new_tax_gen), query_tax$tax_gen),
#                     tax_esp=ifelse(!is.null(new_tax_esp), as.character(new_tax_esp), query_tax$tax_esp),
#                     tax_rank=ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank1),
#                     new_tax_name1=ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_name1), # $4
#                     taxook=ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), # $5
#                     full_name=ifelse(!is.null(new_full_name_auth), as.character(new_full_name_auth), query_tax$full_name), # $6
#                     detvalue=ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), # $7
#                     id_good_n=ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n),
#                     full_name_no_auth=ifelse(!is.null(new_full_name_no_auth), as.character(new_full_name_no_auth), query_tax$full_name_no_auth),
#                     full_name_used=ifelse(!is.null(new_full_name_used), as.character(new_full_name_used), query_tax$full_name_used),
#                     full_name_used2=ifelse(!is.null(new_full_name_used2), as.character(new_full_name_used2), query_tax$full_name_used2),
#                     morphocat=ifelse(!is.null(new_morphocat), new_morphocat, query_tax$morphocat))
#
#     # comp_vals <-
#     #   query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, taxook, full_name, detvalue, id_good_n) != new_vals
#
#     new_vals <-
#       new_vals %>%
#       replace(., is.na(.), -9999)
#
#     sel_query_tax <-
#       dplyr::bind_rows(new_vals, query_tax %>% dplyr::select(tax_fam, tax_gen, tax_esp, tax_rank1, tax_name1, taxook, full_name, detvalue, id_good_n, full_name_no_auth, full_name_used, full_name_used2, morphocat))
#
#     sel_query_tax <-
#       sel_query_tax %>%
#       replace(., is.na(.), -9999)
#
#     comp_vals <-
#       apply(sel_query_tax, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])
#
#     if(!is.null(nrow(comp_vals))) {
#       query_tax <-
#         query_tax[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
#       comp_vals <-
#         apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
#     }else{
#       query_tax <- query_tax
#     }
#
#     if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]
#
#     modif_types[1:length(modif_types)] <-
#       paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")
#
#   }else{
#     comp_vals <- TRUE
#   }
#
#   if(nrow_query & no_synonym_modif) {
#     if(query_tax$id_good_n==query_tax$id_n) {
#       cat("\n This taxa is not considered as synonym. No modification is thus done on its synonymy")
#       comp_vals <- FALSE
#     }else{
#       new_id_diconame_good <- query_tax$id_n
#
#       modif_types[1:length(modif_types)] <-
#         paste(modif_types, "cancel_synonymy", sep="__")
#
#     }
#   }
#
#   ## checking if taxa selected is already a synonym of another taxa
#   if(nrow_query & !is.null(synonym_of)) {
#     Q.syn <- TRUE
#     if(query_tax$id_good_n != query_tax$id_n) {
#       query_tax_all(id_search = query_tax$id_good_n, show_synonymies = FALSE)
#       Q.syn <-
#         utils::askYesNo("Taxa selected is already a synonym of this taxa. Are you sure you want to modify this?", default = FALSE)
#     }
#
#     if(Q.syn) {
#
#       ## checking if others names are pointing to the selected taxa as synonyms
#       syn_of_new_syn <-
#         tbl(mydb, "diconame") %>%
#         filter(id_good_n == !!query_tax$id_n) %>%
#         collect()
#
#       if(nrow(syn_of_new_syn)>0) {
#
#         message("\n Some names are considered synonyms of the selected taxa:")
#
#         print(
#           syn_of_new_syn %>%
#             dplyr::select(
#               full_name_no_auth,
#               tax_fam,
#               tax_gen,
#               tax_esp,
#               tax_rank1,
#               tax_name1,
#               id_n,
#               id_good_n
#             )
#         )
#
#         Q.syn2 <-
#           utils::askYesNo("Do you confirm to also modify the synonymies of these selected names?",
#                           default = FALSE)
#
#         if(Q.syn2)
#
#           ids_others_names_synonyms <-
#           syn_of_new_syn$id_n
#
#       }
#
#
#
#       if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
#       if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
#       if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL
#
#       new_syn <-
#         query_tax_all(genus_searched = synonym_of$genus, tax_esp_searched = synonym_of$species, id_search = synonym_of$id, show_synonymies = FALSE)
#
#
#       if(nrow(new_syn)==0) {
#         cat("\n No taxa found for new synonymy. Select one.")
#         Q.syn <- FALSE
#       }
#
#       if(nrow(new_syn)>1) {
#         cat("\n More than one taxa found for new synonymy. Select only one.")
#         Q.syn <- FALSE
#       }
#
#       if(nrow(new_syn) == 1) {
#
#
#         new_id_diconame_good <- new_syn$id_n
#
#         modif_types[1:length(modif_types)] <-
#           paste(modif_types, "new_synonymy", sep="__")
#
#       }
#     }
#   }else{
#     Q.syn <- TRUE
#   }
#
#   # if(!any(comp_vals)) stop("No update performed because no values are different.")
#
#   if(any(comp_vals) & Q.syn & nrow_query) {
#
#     cat(paste("\n Number of rows selected to be updated :", nrow(query_tax), "\n"))
#
#     if(ask_before_update) {
#       Q <-
#         utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
#     }else{
#       Q <- TRUE
#     }
#
#     if(Q) {
#
#       if(add_backup) {
#
#         query_tax <-
#           query_tax %>%
#           tibble::add_column(date_modified = Sys.Date()) %>%
#           tibble::add_column(modif_type = modif_types)
#
#         DBI::dbWriteTable(
#           mydb,
#           "followup_updates_diconames",
#           query_tax,
#           append = TRUE,
#           row.names = FALSE
#         )
#
#         # query_p <-
#         #     paste0("INSERT INTO followup_updates_diconames VALUES(", paste0(rep("$", ncol(query_tax)), seq(1, ncol(query_tax), 1), collapse = ", "), ")",
#         #            collapse = "")
#
#         # rs <-
#         #     dbSendQuery(mydb, statement=query_p, params=as.list(query_tax %>% slice(1) %>%  unlist(., use.names=FALSE)))
#         #
#         # dbClearResult(rs)
#
#       }
#
#       # tbl(mydb, "followup_updates_diconames")
#
#       rs <-
#         DBI::dbSendQuery(mydb, statement="UPDATE diconame SET tax_fam=$2, tax_gen=$3, tax_esp=$4, taxook=$5, full_name=$6, detvalue=$7, id_good_n=$8, full_name_no_auth=$9, full_name_used=$10, full_name_used2=$11, morphocat=$12, tax_rank1=$13, tax_name1=$14 WHERE id_n = $1",
#                          params=list(query_tax$id_n, # $1
#                                      rep(ifelse(!is.null(new_tax_fam), new_tax_fam, query_tax$tax_fam), nrow(query_tax)), # $2
#                                      rep(ifelse(!is.null(new_tax_gen), new_tax_gen, query_tax$tax_gen), nrow(query_tax)), # $3
#                                      rep(ifelse(!is.null(new_tax_esp), new_tax_esp, query_tax$tax_esp), nrow(query_tax)), # $4
#                                      rep(ifelse(!is.null(new_taxook), new_taxook, query_tax$taxook), nrow(query_tax)), # $5
#                                      rep(ifelse(!is.null(new_full_name_auth), new_full_name_auth, query_tax$full_name), nrow(query_tax)), # $6
#                                      rep(ifelse(!is.null(new_detvalue), new_detvalue, query_tax$detvalue), nrow(query_tax)), # $7
#                                      rep(ifelse(!is.null(new_id_diconame_good), new_id_diconame_good, query_tax$id_good_n), nrow(query_tax)), # $8
#                                      rep(ifelse(!is.null(new_full_name_no_auth), new_full_name_no_auth, query_tax$full_name_no_auth), nrow(query_tax)), # $9
#                                      rep(ifelse(!is.null(new_full_name_used), new_full_name_used, query_tax$full_name_used), nrow(query_tax)), # $10
#                                      rep(ifelse(!is.null(new_full_name_used2), new_full_name_used2, query_tax$full_name_used2), nrow(query_tax)), # $11
#                                      rep(ifelse(!is.null(new_morphocat), new_morphocat, query_tax$morphocat), nrow(query_tax)), # $12
#                                      rep(ifelse(!is.null(new_tax_rank1), new_tax_rank1, query_tax$tax_rank1), nrow(query_tax)), # $13
#                                      rep(ifelse(!is.null(new_tax_name1), new_tax_name1, query_tax$tax_name1), nrow(query_tax)))) # $14
#
#       DBI::dbClearResult(rs)
#
#       rs <-
#         DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                          params=list(query_tax$id_n))
#       if(show_results) print(DBI::dbFetch(rs))
#       DBI::dbClearResult(rs)
#
#       if(Q.syn2) {
#
#         message("\n updating synonymies for others taxa")
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="UPDATE diconame SET id_good_n=$2 WHERE id_n = $1",
#                            params = list(ids_others_names_synonyms, # $1
#                                         rep(ifelse(!is.null(new_id_diconame_good),
#                                                    new_id_diconame_good, syn_of_new_syn$id_good_n),
#                                             nrow(syn_of_new_syn)) # $2
#                            ))
#
#         DBI::dbClearResult(rs)
#
#         rs <-
#           DBI::dbSendQuery(mydb, statement="SELECT *FROM diconame WHERE id_n = $1",
#                            params=list(ids_others_names_synonyms))
#         if(show_results) print(DBI::dbFetch(rs))
#         DBI::dbClearResult(rs)
#
#
#       }
#     }
#
#   }else{
#     if(nrow(query_tax)==0) print("No update because no taxa found.")
#
#     if(!any(comp_vals)) print("No update performed because no values are different.")
#
#     if(!Q.syn) print("No update because new synonymy not correctly defined.")
#
#     if(!nrow_query) print("No updates because none taxa were found based on query parameters (genus/species/family/id)")
#
#   }
#
#   # dbDisconnect(mydb)
#
# }








#' Update trait_list_table
#'
#' Update trait_list_table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param trait_searched string genus name searched
#' @param trait_id string genus name searched
#' @param new_trait_name string new trait name
#' @param new_relatedterm string new relatedterm name
#' @param new_maxallowedvalue numeric new maxallowedvalue
#' @param new_minallowedvalue numeric new minallowedvalue
#' @param new_traitdescription string new traitdescription
#' @param new_expectedunit string new expectedunit
#' @param ask_before_update logical TRUE by default, ask for confirmation before updating
#' @param add_backup logical TRUE by default, add backup of modified data
#' @param show_results logical TRUE by default, show the data that has been modified
#'
#' @return No return value individuals updated
#' @export
update_trait_list_table <- function(trait_searched = NULL,
                                    trait_id = NULL,
                                    new_trait_name = NULL,
                                    new_relatedterm = NULL,
                                    new_maxallowedvalue = NULL,
                                    new_minallowedvalue = NULL,
                                    new_traitdescription = NULL,
                                    new_expectedunit = NULL,
                                    ask_before_update = TRUE,
                                    add_backup = TRUE,
                                    show_results=TRUE) {

  if(!exists("mydb")) call.mydb()

  if(all(is.null(c(trait_searched, trait_id))))
    stop("\n Provide trait_searched or trait_id to update")

  ### checking if at least one modification is asked
  new_vals <- c(new_trait_name, new_relatedterm, new_maxallowedvalue,
                new_minallowedvalue, new_traitdescription, new_expectedunit)
  if(!any(!is.null(new_vals))) stop("\n No new values to be updated.")

  ### querying for entries to be modified
  if(!is.null(trait_searched)) {
    query <- 'SELECT * FROM traitlist WHERE MMM'
    query <- gsub(pattern = "MMM", replacement = paste0(" trait ILIKE '%",
                                                        trait_searched, "%'"), x=query)

    rs <- DBI::dbSendQuery(mydb, query)
    query_trait <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)

    }else{
      query_trait <-
        dplyr::tbl(mydb, "traitlist") %>%
        dplyr::filter(id_trait == !!trait_id) %>%
        dplyr::collect()
    }
  print(query_trait %>% as.data.frame())
  if(nrow(query_trait)>1) stop("more than one trait selected, select one")
  if(nrow(query_trait)==0) stop("no trait selected, select one")

  modif_types <-
    vector(mode = "character", length = nrow(query_trait))

  new_vals <-
    dplyr::tibble(trait = ifelse(!is.null(new_trait_name), as.character(new_trait_name),
                                 query_trait$trait),
                  relatedterm = ifelse(!is.null(new_relatedterm), as.character(new_relatedterm),
                                       query_trait$relatedterm),
                  maxallowedvalue = ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
                                           query_trait$maxallowedvalue),
                  minallowedvalue = ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
                                           query_trait$minallowedvalue),
                  traitdescription = ifelse(!is.null(new_traitdescription), as.character(new_traitdescription),
                                            query_trait$traitdescription),
                  expectedunit = ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
                                        query_trait$expectedunit))

  new_vals <-
      new_vals %>%
      replace(., is.na(.), -9999)

    sel_query_trait <-
      dplyr::bind_rows(new_vals, query_trait %>%
                         dplyr::select(-valuetype, -id_trait, -date_modif_d, -date_modif_m, -date_modif_y))

    sel_query_trait <-
      sel_query_trait %>%
      replace(., is.na(.), -9999)

    comp_vals <-
      apply(sel_query_trait, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])

    # if(!is.null(nrow(comp_vals))) {
    #   query_trait <-
    #     query_trait[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
    #   comp_vals <-
    #     apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
    # }else{
    #   query_trait <- query_trait
    # }

    if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]

    modif_types[1:length(modif_types)] <-
      paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")

  # if(!any(comp_vals)) stop("No update performed because no values are different.")

  if(any(comp_vals)) {

    cat(paste("\n Number of rows selected to be updated :", nrow(query_trait), "\n"))

    if(ask_before_update) {

      sel_query_trait %>%
        dplyr::select(!!names(comp_vals)) %>%
        dplyr::select(which(comp_vals)) %>%
        print()

      Q <-
        utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
    }else{
      Q <- TRUE
    }

    if(Q) {

      if(add_backup) {
        message("no back up for this table yet")
        # query_trait <-
        #   query_trait %>%
        #   tibble::add_column(date_modified=Sys.Date()) %>%
        #   tibble::add_column(modif_type=modif_types)
        #
        #
        # DBI::dbWriteTable(mydb, "followup_updates_diconames", query_tax, append = TRUE, row.names = FALSE)

      }

      query_trait <-
        query_trait %>%
        dplyr::select(-date_modif_d, -date_modif_m, -date_modif_y)

      query_trait <-
        .add_modif_field(query_trait)

      rs <-
        DBI::dbSendQuery(mydb,
                         statement="UPDATE traitlist SET trait=$2, relatedterm=$3, valuetype=$4, maxallowedvalue=$5, minallowedvalue=$6, traitdescription=$7, factorlevels=$8, expectedunit=$9, date_modif_d=$10, date_modif_m=$11, date_modif_y=$12  WHERE id_trait = $1",
                         params=list(query_trait$id_trait, # $1
                                     rep(ifelse(!is.null(new_trait_name), as.character(new_trait_name),
                                                query_trait$trait), nrow(query_trait)), # $2
                                     rep(ifelse(!is.null(new_relatedterm), as.character(new_relatedterm),
                                                query_trait$relatedterm), nrow(query_trait)), # $3
                                     rep(query_trait$valuetype, nrow(query_trait)), # $4
                                     rep(ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
                                                query_trait$maxallowedvalue), nrow(query_trait)), # $5
                                     rep(ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
                                                query_trait$minallowedvalue), nrow(query_trait)), # $6
                                     rep(ifelse(!is.null(new_traitdescription), as.character(new_traitdescription),
                                                query_trait$traitdescription), nrow(query_trait)), # $7
                                     rep(query_trait$factorlevels, nrow(query_trait)), # $8
                                     rep(ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
                                                query_trait$expectedunit), nrow(query_trait)), # $9
                                     rep(query_trait$date_modif_d, nrow(query_trait)), # $10
                                     rep(query_trait$date_modif_m, nrow(query_trait)), # $11
                                     rep(query_trait$date_modif_y, nrow(query_trait))) # $12
        )

      DBI::dbClearResult(rs)

      rs <-
        DBI::dbSendQuery(mydb, statement="SELECT *FROM traitlist WHERE id_trait = $1",
                         params=list(query_trait$id_trait))
      if(show_results) print(DBI::dbFetch(rs))
      DBI::dbClearResult(rs)

    }
  }else{

    if(!any(comp_vals)) print("No update performed because no values are different.")
  }

  # dbDisconnect(mydb)

}










#' Update subplot_table
#'
#' Update subplot_table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param subplots_id integer id of subplot to update
#' @param new_id_type_sub_plot id of new type subplot
#' @param new_typevalue numeric new value of type subplot
#' @param new_year integer new year of subplot
#' @param new_month integer new month of subplot
#' @param new_day integer new day of subplot
#' @param new_colnam character new colnam of subplot
#' @param ask_before_update logical TRUE by default, ask for confirmation before updating
#' @param add_backup logical TRUE by default, add backup
#' @param show_results logical TRUE by default, show the data that has been modified
#'
#'
#' @return No return value individuals updated
#' @export
update_subplots_table <- function(subplots_id = NULL,
                                  new_id_type_sub_plot = NULL,
                                  new_typevalue = NULL,
                                  new_year = NULL,
                                  new_month = NULL,
                                  new_day = NULL,
                                  new_colnam = NULL,
                                  new_add_people = NULL,
                                  ask_before_update = TRUE,
                                  add_backup = TRUE,
                                  show_results = TRUE) {

  if(!exists("mydb")) call.mydb()

  if(all(is.null(c(subplots_id))))
    stop("\n Provide subplots_id to update")

  ### checking if at least one modification is asked
  new_vals <- c(new_id_type_sub_plot,
                new_typevalue,
                new_year,
                new_month,
                new_day,
                new_colnam,
                new_add_people)

  if (!any(!is.null(new_vals)))
    stop("\n No new values to be updated.")

  ### querying for entries to be modified
  query_subplots <-
      dplyr::tbl(mydb, "data_liste_sub_plots") %>%
      dplyr::filter(id_sub_plots == subplots_id) %>%
      dplyr::collect()

  print(query_subplots %>% as.data.frame())
  if (nrow(query_subplots) > 1)
    stop("more than one subplots selected, select one")
  if (nrow(query_subplots) == 0)
    stop("no subplots selected, select one")

  if(!is.null(new_colnam)) {
    new_id_colnam <-
      .link_colnam(data_stand = tibble(colnam = new_colnam),
                   collector_field = 1)

    new_id_colnam <-
      new_id_colnam$id_colnam

  }else{
    new_id_colnam <- NULL
  }

  modif_types <-
    vector(mode = "character", length = nrow(query_subplots))

  new_vals <-
    dplyr::tibble(id_type_sub_plot = ifelse(!is.null(new_id_type_sub_plot), as.numeric(new_id_type_sub_plot),
                                            query_subplots$id_type_sub_plot),
                  year = ifelse(!is.null(new_year), as.numeric(new_year),
                                query_subplots$year),
                  month = ifelse(!is.null(new_month), as.numeric(new_month),
                                 query_subplots$month),
                  day = ifelse(!is.null(new_day), as.numeric(new_day),
                               query_subplots$day),
                  typevalue = ifelse(!is.null(new_typevalue), as.numeric(new_typevalue),
                                     query_subplots$typevalue),
                  id_colnam = ifelse(!is.null(new_id_colnam), as.numeric(new_id_colnam),
                                     query_subplots$id_colnam),
                  additional_people = ifelse(!is.null(new_add_people), new_add_people,
                                             query_subplots$additional_people))



  # new_vals <-
  #   new_vals %>%
  #   replace(., is.na(.), -9999)

  sel_query_subplots <-
    dplyr::bind_rows(
      new_vals,
      query_subplots %>%
        dplyr::select(id_type_sub_plot, year, month, day, typevalue, id_colnam, additional_people)
    )


  sel_query_subplots <- replace_NA(vec = sel_query_subplots)

  # sel_query_subplots <-
  #   sel_query_subplots %>%
  #   mutate_if(is.character, ~ tidyr::replace_na(., "-9999")) %>%
  #   mutate_if(is.numeric, ~ tidyr::replace_na(., -9999))

  comp_vals <-
    apply(
      sel_query_subplots,
      MARGIN = 2,
      FUN = function(x)
        x[1] != x[2:length(x)]
    )

  # if(!is.null(nrow(comp_vals))) {
  #   query_trait <-
  #     query_trait[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
  #   comp_vals <-
  #     apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
  # }else{
  #   query_trait <- query_trait
  # }

  if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]

  modif_types[1:length(modif_types)] <-
    paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "), length(modif_types)), collapse ="__")

  # if(!any(comp_vals)) stop("No update performed because no values are different.")

  if(any(comp_vals)) {

    cat(paste("\n Number of rows selected to be updated :", nrow(query_subplots), "\n"))

    if(ask_before_update) {

      sel_query_subplots %>%
        dplyr::select(!!names(comp_vals)) %>%
        dplyr::select(which(comp_vals)) %>%
        print()

      Q <-
        utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?", default = FALSE)
    }else{
      Q <- TRUE
    }

    if(Q) {

      if(add_backup) {
        message("no back up for this table yet")
        # query_trait <-
        #   query_trait %>%
        #   tibble::add_column(date_modified=Sys.Date()) %>%
        #   tibble::add_column(modif_type=modif_types)
        #
        #
        # DBI::dbWriteTable(mydb, "followup_updates_diconames", query_tax, append = TRUE, row.names = FALSE)

      }

      query_subplots <-
        query_subplots %>%
        dplyr::select(-date_modif_d, -date_modif_m, -date_modif_y)

      query_subplots <-
        .add_modif_field(query_subplots)

      rs <-
        DBI::dbSendQuery(mydb,
                         statement = "UPDATE data_liste_sub_plots SET id_type_sub_plot=$2, year=$3, month=$4, day=$5, typevalue=$6, date_modif_d=$7, date_modif_m=$8, date_modif_y=$9, id_colnam=$10, additional_people=$11 WHERE id_sub_plots = $1",
                         params = list(query_subplots$id_sub_plots, # $1
                                     rep(ifelse(!is.null(new_id_type_sub_plot), as.numeric(new_id_type_sub_plot),
                                                query_subplots$id_type_sub_plot), nrow(query_subplots)), # $2
                                     rep(ifelse(!is.null(new_year), as.numeric(new_year),
                                                query_subplots$year), nrow(query_subplots)), # $3
                                     rep(ifelse(!is.null(new_month), as.numeric(new_month),
                                                query_subplots$month), nrow(query_subplots)), # $4
                                     rep(ifelse(!is.null(new_day), as.numeric(new_day),
                                                query_subplots$day), nrow(query_subplots)), # $5
                                     rep(ifelse(!is.null(new_typevalue), as.numeric(new_typevalue),
                                                query_subplots$typevalue), nrow(query_subplots)), # $6
                                     rep(query_subplots$date_modif_d, nrow(query_subplots)), # $7
                                     rep(query_subplots$date_modif_m, nrow(query_subplots)), # $8
                                     rep(query_subplots$date_modif_y, nrow(query_subplots)), # $9
                                     rep(ifelse(!is.null(new_id_colnam), as.numeric(new_id_colnam),
                                                query_subplots$id_colnam), nrow(query_subplots)), # $10
                                     rep(ifelse(!is.null(new_add_people), as.character(new_add_people),
                                                query_subplots$additional_people), nrow(query_subplots))) # 11

                         )

      DBI::dbClearResult(rs)

      rs <-
        DBI::dbSendQuery(mydb, statement="SELECT *FROM data_liste_sub_plots WHERE id_sub_plots = $1",
                         params=list(query_subplots$id_sub_plots))
      if(show_results) print(DBI::dbFetch(rs))
      DBI::dbClearResult(rs)

    }
  }else{

    if(!any(comp_vals)) print("No update performed because no values are different.")
  }


}



#' Update traits_measurements table
#'
#' Update traits_measurements table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data string name of the team leader of the selected plot
#' @param trait_values_new_data string vector with columns names of new_data that contain traits measures
#' @param col_names_trait_corresp string vector with trait names corresponding to trait_values_new_data
#' @param measures_property_new_data string vector with columns names new_data others than traits measures values
#' @param col_names_property_corresp string vector with corresponding columns names for measures_property_new_data
#' @param id_new_data integer which column of new_data contain the trait measure id to match dataset
#' @param col_name_id_corresp integer which column of new_data contain the id to match dataset
#' @param launch_update logical if TRUE updates are performed
#' @param add_backup logical whether backup of modified data should be recorded
#'
#'
#' @return No return value individuals updated
#' @export
update_traits_measures <- function(new_data,
                                   trait_values_new_data = NULL,
                                   col_names_trait_corresp = NULL,
                                   measures_property_new_data = NULL,
                                   col_names_property_corresp = NULL,
                                   id_new_data,
                                   col_name_id_corresp,
                                   launch_update = FALSE,
                                   add_backup = TRUE) {

  if(!exists("mydb")) call.mydb()

  if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp)) {

    if(length(trait_values_new_data)!=length(col_names_trait_corresp))
      stop("trait_values_new_data and col_names_trait_corresp should have same length")

    all_colnames_ind <- traits_list()

    for (i in 1:length(col_names_trait_corresp))
      if(!any(col_names_trait_corresp[i] == all_colnames_ind$trait)) {
        stop(paste(col_names_trait_corresp[i], "not found in trait list"))
        print("check")
        print(all_colnames_ind$trait)
      }
  }

  if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp)) {

    if(length(measures_property_new_data)!=length(col_names_property_corresp))
      stop("measures_property_new_data and col_names_property_corresp should have same length")

    colnames_property <-
      dplyr::tbl(mydb, "data_traits_measures") %>%
      dplyr::select(country, decimallatitude, decimallongitude, elevation,
                    verbatimlocality,
                    basisofrecord, year, month, day,
                    issue, measurementmethod) %>%
      colnames()

    colnames_property <- c(colnames_property, "collector")

    for (i in 1:length(col_names_property_corresp))
      if(!any(col_names_property_corresp[i] == colnames_property)) {
        stop(paste(col_names_property_corresp[i], "not found in property measureament"))
        print("check")
        print(colnames_property)
      }
  }

  # id_db <- col_id

  if(!any(col_name_id_corresp == c("id_trait_measures", "id_n", "id_old")))
    stop("id for matching should be one of id_trait_measures, id_n, id_old")

  if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp)) {
    new_data <-
      .rename_data(dataset = new_data,
                   col_old = c(trait_values_new_data, id_new_data),
                   col_new = c(col_names_trait_corresp, col_name_id_corresp))
  }

  if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp)) {
    new_data <-
      .rename_data(dataset = new_data,
                   col_old = c(measures_property_new_data, id_new_data),
                   col_new = c(col_names_property_corresp, col_name_id_corresp))
  }

  all_corresponding_matches <- list()
  if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp))
    nbe_col_cor <- length(col_names_trait_corresp)
  if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp))
    nbe_col_cor <- length(col_names_property_corresp)

  for (k in 1:nbe_col_cor) {

    if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp))
      output_matches <-
        .find_ids(dataset = new_data,
                                col_new = c(col_names_trait_corresp[k], col_name_id_corresp),
                                id_col_nbr = 2,
                                type_data = "trait")

    if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp))
      output_matches <-
        .find_ids(dataset = new_data,
                col_new = c(col_names_property_corresp[k], col_name_id_corresp),
                id_col_nbr = 2,
                type_data = "trait")

    matches <-
      output_matches[[2]][[1]]

    if(launch_update & nrow(matches) > 0) {
      matches <-
        matches %>%
        dplyr::select(id, dplyr::contains("_new"))
      matches <-
        .add_modif_field(matches)

      all_id_match <- dplyr::pull(dplyr::select(matches, id))

      if(col_name_id_corresp %in% c("id_n", "id_old")) {
        ids_traits_measures <-
          output_matches[[1]] %>%
          dplyr::filter(id %in% all_id_match) %>%
          dplyr::select(dplyr::contains("id_trait_measures"))

        matches <-
          matches %>%
          dplyr::mutate(id = dplyr::pull(ids_traits_measures))
      }

      if(dplyr::tbl(mydb, "data_traits_measures") %>%
        dplyr::filter(id_trait_measures %in% !!matches$id) %>%
        dplyr::distinct(traitid) %>%
        dplyr::collect() %>%
        nrow()>2) stop("more than one trait to be updated whereas only one expected")

      if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp))
        field <- "traitvalue"
      if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp))
        field <- col_names_property_corresp[k]

      ## create a temporary table with new data
      DBI::dbWriteTable(mydb, "temp_table", matches,
                        overwrite=T, fileEncoding = "UTF-8", row.names=F)

      var_new <- matches %>%
        dplyr::select(dplyr::contains("_new")) %>%
        colnames()

      query_up <-
        paste0("UPDATE data_traits_measures t1 SET (", field ,", date_modif_d, date_modif_m, date_modif_y) = (t2.", var_new, ", t2.date_modif_d, t2.date_modif_m, t2.date_modif_y) FROM temp_table t2 WHERE t1.id_trait_measures = t2.id")

      rs <-
        DBI::dbSendStatement(mydb, query_up)

      cat("Rows updated", RPostgres::dbGetRowsAffected(rs))
      rs@sql
      DBI::dbClearResult(rs)

      if(add_backup) {
        field <- col_names_trait_corresp[k]

        ids_measures <- matches$id

        all_rows_to_be_updated <-
          dplyr::tbl(mydb, "data_traits_measures") %>%
          dplyr::filter(id_trait_measures %in% ids_measures) %>%
          dplyr::collect()

        colnames_measures <-
          dplyr::tbl(mydb, "followup_updates_traits_measures") %>%
          dplyr::select(-date_modified, -modif_type, -id_fol_up_traits_measures) %>%
          dplyr::collect() %>%
          dplyr::top_n(1) %>%
          colnames()

        all_rows_to_be_updated <-
          all_rows_to_be_updated %>%
          dplyr::select(dplyr::one_of(colnames_measures))

        all_rows_to_be_updated <-
          all_rows_to_be_updated %>%
          tibble::add_column(date_modified=Sys.Date()) %>%
          tibble::add_column(modif_type=field)

        print(all_rows_to_be_updated %>% dplyr::select(modif_type, date_modified))

        DBI::dbWriteTable(mydb, "followup_updates_traits_measures",
                          all_rows_to_be_updated, append = TRUE, row.names = FALSE)
      }
    }else{
      if(launch_update & nrow(matches)==0) cat("\n No new values found")
    }

    all_corresponding_matches[[k]] <- output_matches[[2]][[1]]
    if(!is.null(trait_values_new_data) & !is.null(col_names_trait_corresp)) names(all_corresponding_matches)[k] <- col_names_trait_corresp[k]
    if(!is.null(measures_property_new_data) & !is.null(col_names_property_corresp)) names(all_corresponding_matches)[k] <- col_names_property_corresp[k]
  }

  return(all_corresponding_matches)
}




#' Add new individuals data
#'
#' Add new individuals data
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble new data to be import
#' @param col_names_select string
#' @param col_names_corresp string
#' @param id_col integer indicate which name of col_names_select is the id for matching plot in metadata
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_individuals <- function(new_data ,
                            col_names_select,
                            col_names_corresp,
                            id_col,
                            launch_adding_data = FALSE) {

  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )

  if(!exists("mydb")) call.mydb()

  if(length(col_names_select) != length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename_at(dplyr::vars(col_names_select[id_col]), ~ col_names_corresp[id_col])

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  # new_data_renamed <-
  #   new_data %>%
  #   dplyr::rename(plot_name= !!vars(col_names_select[id_col]))

  ### Linking plot names

  new_data_renamed <-
    .link_plot_name(data_stand = new_data_renamed, plot_name_field = "plot_name")

  ids_plot <-
    new_data_renamed %>%
    dplyr::select(plot_name, id_liste_plots) %>%
    dplyr::distinct(plot_name, id_liste_plots)

  if(any(is.na(ids_plot$id_liste_plots))) {
    warning("some plot are not found in metadata")
    print(ids_plot %>%
            dplyr::filter(is.na(id_liste_plots)))
    ids_plot <-
      ids_plot %>%
      dplyr::filter(is.na(id_liste_plots))

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
    dplyr::filter(id_table_liste_plots_n %in% !!ids_plot$id_liste_plots) %>%
    dplyr::distinct(id_table_liste_plots_n) %>%
    dplyr::collect()

  if (nrow(plots_already_in_db) > 0) {
    print(
      plots_already_in_db %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "data_liste_plots") %>%
            dplyr::select(plot_name, id_liste_plots) %>%
            dplyr::collect(),
          by = c("id_table_liste_plots_n" = "id_liste_plots")
        ) %>%
        dplyr::pull(plot_name)
    )
    warning("data for some plots already in database")

  }


  new_data_renamed <-
    new_data_renamed %>%
    dplyr::left_join(ids_plot) %>%
    dplyr::rename(id_table_liste_plots_n = id_liste_plots)


  col_names_select <-
    col_names_select[-id_col]
  col_names_corresp <-
    col_names_corresp[-id_col]



  col_names_corresp <-
    c(col_names_corresp, "id_table_liste_plots_n")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  ### CHECKS
  method <-
    ids_plot %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "data_liste_plots") %>%
        dplyr::select(plot_name, id_liste_plots, id_method) %>%
        dplyr::left_join(dplyr::tbl(mydb, "methodslist")) %>%
        dplyr::collect(),
      by = c("id_liste_plots" = "id_liste_plots")
    ) %>%
    dplyr::distinct(method)

  if (nrow(method) > 1) {
    print(method)
    stop("More than one method selected, import plot of one method at a time")
  }

  if (!any(colnames(new_data_renamed) == "idtax_n"))
    stop("idtax_n column missing")

  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "table_taxa") %>%
        dplyr::select(idtax_n, tax_tax) %>%
        filter(idtax_n %in% !!new_data_renamed$idtax_n) %>%
        dplyr::collect(),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    dplyr::filter(is.na(tax_tax)) %>%
    dplyr::pull(idtax_n)

  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in diconame", unmatch_id_diconame))

  ## checking DBH
  # if(!any(colnames(new_data_renamed)=="dbh")) stop("dbh column missing")
  if(any(is.na(names(new_data_renamed) == "dbh"))) {

    message("\n dbh and others traits measure should be added independantly using add_traits_measures function")

  }



  ## checking column given method
  if(dplyr::pull(method) == "Large") {

    if (!any(colnames(new_data_renamed) == "sous_plot_name"))
      stop("sous_plot_name column missing")
    if (!any(colnames(new_data_renamed) == "ind_num_sous_plot"))
      stop("ind_num_sous_plot column missing")
    # if (!any(colnames(new_data_renamed) == "dbh"))
    #   stop("dbh column missing")
    if (!any(colnames(new_data_renamed) == "position_transect"))
      stop("position_transect column missing")
    if (!any(colnames(new_data_renamed) == "strate_cat"))
      stop("strate_cat column missing")

    ## checking strate info
    miss_strate <-
      new_data_renamed %>%
      dplyr::filter(!strate_cat %in% c("Ad", "Ado"))

    if (nrow(miss_strate) > 0) {
      warning(paste(
        "strate_cat missing or not equal to Ad or Ado for",
        nrow(miss_strate),
        "individuals"
      ))
      print(miss_strate)
    }

    ## checking sous_plot_name
    type_sousplot <-
      new_data_renamed %>%
      dplyr::distinct(sous_plot_name) %>%
      dplyr::pull()

    if (!any(type_sousplot == c("A", "B", "C", "D")))
      warning("sous_plot_name should include A B C and D")

    # check ind_num_sous_plot
    for (i in unique(new_data_renamed$id_table_liste_plots_n)) {
      for (j in c("A", "B", "C", "D")) {
        duplicates_ind_plot <-
          new_data_renamed %>%
          dplyr::filter(id_table_liste_plots_n == i, sous_plot_name == j) %>%
          dplyr::group_by(ind_num_sous_plot) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1)

      if(nrow(duplicates_ind_plot)>0) {
        plot_name <-
          dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(plot_name, id_liste_plots) %>%
          dplyr::filter(id_liste_plots == i) %>%
          dplyr::pull(plot_name)

        warning(paste(nrow(duplicates_ind_plot),
                      "duplicate in ind_num_sous_plot for ", plot_name, j))

        logs <-
          dplyr::bind_rows(logs,
                           dplyr::tibble(
                             column = "ind_num_sous_plot",
                             note = paste(
                               nrow(duplicates_ind_plot),
                               "duplicate in ind_num_sous_plot for ",
                               plot_name,
                               j
                             )
                           ))
      }

      }

    }

  }

  if (dplyr::pull(method) == "1ha-IRD" | dplyr::pull(method) == " ") {
    if (!any(colnames(new_data_renamed) == "ind_num_sous_plot"))
      stop("ind_num_sous_plot column missing - Tag individual")


    ### checking duplicated tags within plots
    duplicated_tags <-
      new_data_renamed %>%
      group_by(id_table_liste_plots_n, ind_num_sous_plot) %>%
      count() %>%
      filter(n > 1)

    duplicated_tags <-
      new_data_renamed %>%
      dplyr::left_join(
        duplicated_tags ,
        by = c(
          "id_table_liste_plots_n" = "id_table_liste_plots_n",
          "ind_num_sous_plot" = "ind_num_sous_plot"
        )
      ) %>%
      dplyr::filter(!is.na(n)) %>%
      dplyr::left_join(
        tbl(mydb, "data_liste_plots") %>%
          dplyr::select(id_liste_plots, plot_name) %>%
          dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )

    if(nrow(duplicated_tags) > 0) {
      warning("\n Duplicated tags in some plots")
      print(duplicated_tags)

      readr::write_excel_csv(duplicated_tags, "duplicated_tags.csv")
    }

    if(any(names(new_data_renamed) == "multi_tiges_id")) {
      cli::cli_alert_info("Checking multi tiges")

      # all_multi_tiges <- new_data_renamed %>%
      #   filter(!is.na(multi_tiges_id))

      # all_multi_tiges %>%
      #   dplyr::select(idtax_n, id_table_liste_plots_n) %>%
      #   left_join(new_data_renamed %>%
      #               dplyr::select(idtax_n, id_table_liste_plots_n))
      #
      # for (j in 1:length(unique(all_multi_tiges$id_table_liste_plots_n))) {
      #
      #   new_data_subset <-
      #     new_data_renamed %>%
      #     filter(id_table_liste_plots_n == unique(all_multi_tiges$id_table_liste_plots_n)[j])
      #
      #   all_multi_tiges_subset <-
      #     all_multi_tiges %>%
      #     filter(id_table_liste_plots_n == unique(all_multi_tiges$id_table_liste_plots_n)[j])
      #
      #   for (i in 1:nrow(all_multi_tiges_subset)) {
      #
      #     new_data_subset %>%
      #       dplyr::select(ind_num_sous_plot, idtax_n) %>%
      #       filter()
      #
      #
      #
      #   }
      #
      #
      # }


    }

  }

  ## checking ind_num_sous_plot

  if(!is.numeric(new_data_renamed$ind_num_sous_plot)) {

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(ind_num_sous_plot = as.numeric(ind_num_sous_plot))

    if(any(is.na(new_data_renamed$ind_num_sous_plot)))
      new_data_renamed %>%
      filter(is.na(ind_num_sous_plot)) %>%
      print()
      stop("ind_num_sous_plot missing after converting to numeric")
  }

  # check herbarium specimen coherence

  if (!any(colnames(new_data_renamed) == "herbarium_nbe_type"))
    cli::cli_alert_danger("herbarium_nbe_type column missing")
  if (!any(colnames(new_data_renamed) == "herbarium_nbe_char"))
    cli::cli_alert_danger("herbarium_nbe_char column missing")

  if (any(colnames(new_data_renamed) == "herbarium_nbe_char")) {
    all_herb_ref <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char) %>%
      dplyr::filter(!is.na(herbarium_nbe_char))
  }


  if (any(colnames(new_data_renamed) == "herbarium_nbe_type")) {

    all_herb_type <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_type) %>%
      dplyr::filter(!is.na(herbarium_nbe_type))

    if (nrow(all_herb_type) != nrow(all_herb_ref)) {

      print(all_herb_type)
      print(all_herb_ref)
      cli::cli_alert_warning("Number of herbarium specimen type and reference are not identical")

      missing_herb_ref <-
        all_herb_type %>%
        filter(!herbarium_nbe_type %in% all_herb_ref$herbarium_nbe_char)

      if(nrow(missing_herb_ref) > 0) {
        print(missing_herb_ref)
        stop("Specimen in type not found in reference specimens")
      }

      missing_herb_type <- all_herb_ref %>%
        filter(!herbarium_nbe_char %in% all_herb_type$herbarium_nbe_type)


      if(nrow(missing_herb_type) > 0) {

        cli::cli_alert_danger("Some specimens type not represented in specimens links")

        print(missing_herb_type)

        complete_type_specimen <-
          askYesNo(msg = "Complete automatically type specimen by taking the first individual?")

        if(complete_type_specimen) {

          new_data_renamed <-
            new_data_renamed %>%
            mutate(id_temp = 1:nrow(.))

          for (i in 1:nrow(missing_herb_type)) {

            id_selected <-
              new_data_renamed %>%
              filter(herbarium_nbe_char == missing_herb_type$herbarium_nbe_char[i]) %>%
              arrange(ind_num_sous_plot, id_table_liste_plots_n) %>%
              dplyr::slice(1) %>%
              dplyr::select(id_temp)

            new_data_renamed <-
              new_data_renamed %>%
              mutate(herbarium_nbe_type = replace(herbarium_nbe_type,
                                                  id_temp == id_selected$id_temp,
                                                  missing_herb_type$herbarium_nbe_char[i]))
            # %>%
            #   filter(herbarium_nbe_char == missing_herb_type$herbarium_nbe_char[i]) %>%
            #   dplyr::select(herbarium_nbe_type)




          }

          new_data_renamed <-
            new_data_renamed %>%
            dplyr::select(-id_temp)

        }
      }
    }

    herb_type_dups <-
      new_data_renamed %>%
      dplyr::group_by(herbarium_nbe_type) %>%
      dplyr::count() %>%
      dplyr::filter(n > 1,!is.na(herbarium_nbe_type))

    # herb_type_dups <-
    #   new_data_renamed %>%
    #   distinct(herbarium_nbe_type, id_diconame_n) %>%
    #   filter(!is.na(herbarium_nbe_type), !is.na(id_diconame_n)) %>%
    #   group_by(herbarium_nbe_type) %>%
    #   count() %>%
    #   filter(n>1)

    if (nrow(herb_type_dups) > 0) {

      warning(paste(
        "herbarium_nbe_type is duplicated for",
        nrow(herb_type_dups),
        "specimen"
      ))

      new_data_renamed %>%
        dplyr::filter(herbarium_nbe_type %in% dplyr::pull(herb_type_dups, herbarium_nbe_type))

      logs <-
        dplyr::bind_rows(logs,
                         dplyr::tibble(
                           column = "herbarium_nbe_type",
                           note = paste(
                             "herbarium_nbe_type is duplicated for",
                             paste(
                               dplyr::pull(herb_type_dups, herbarium_nbe_type),
                               collapse = ";"
                             ),
                             "specimen"
                           )
                         ))
    }
  }

  ## check herbarium specimen reference coherence
  if(any(colnames(new_data_renamed)=="herbarium_nbe_char")) {
    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::distinct(herbarium_nbe_char, idtax_n) %>%
      dplyr::filter(!is.na(herbarium_nbe_char)) %>%
      dplyr::group_by(herbarium_nbe_char) %>%
      dplyr::count() %>%
      dplyr::filter(n>1)

    herb_ref_multiple_taxa <-
      new_data_renamed %>%
      dplyr::filter(herbarium_nbe_char %in% dplyr::pull(herb_ref_multiple_taxa, herbarium_nbe_char)) %>%
      dplyr::select(herbarium_nbe_char, original_tax_name, idtax_n) %>%
      dplyr::distinct()

    if(nrow(herb_ref_multiple_taxa) > 0) {
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

  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(
      data_modif_d = lubridate::day(Sys.Date()),
      data_modif_m = lubridate::month(Sys.Date()),
      data_modif_y = lubridate::year(Sys.Date())
    )

  if(launch_adding_data) {

    print(list(new_data_renamed, logs))

    confirmed <- utils::askYesNo("Confirm adding?")

    if(confirmed)
      DBI::dbWriteTable(mydb, "data_individuals", new_data_renamed, append = TRUE, row.names = FALSE)

  }

  return(list(new_data_renamed, logs))

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
get_updates_diconame <- function(id = NULL,
                                 last_months = NULL,
                                 last_10_entry = TRUE,
                                 last = NULL) {


  if(!exists("mydb")) call.mydb()

  tb <-
    dplyr::tbl(mydb, "followup_updates_diconames")

  if (!is.null(id) & !last_10_entry) {
    var <- rlang::enquo(id)
    entry <-
      tb %>%
      dplyr::filter(id_n == !!var) %>%
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
    max_id <-
      tb %>%
      dplyr::arrange(dplyr::desc(id_fol_up_diconame)) %>%
      dplyr::select(id_fol_up_diconame) %>%
      dplyr::slice_head() %>%
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
#' @param tax_rank1 string tax_rank1 name
#' @param tax_name1 string tax_name1 name
#' @param detvalue integer detvalue code
#' @param morphocat integer morphocat code
#' @param full_name string full name : genus + species + authors
#' @param synonym_of list if the new entry should be put in synonymy with an existing taxa, add in a list at least one values to identify to which taxa it will be put in synonymy: genus, species or id
#'
#'
#' @return A tibble
#' @export
add_entry_dico_name <- function(tax_gen = NULL,
                                tax_esp = NULL,
                                tax_fam = NULL,
                                full_name = NULL,
                                tax_rank1 = NULL,
                                tax_name1 = NULL,
                                synonym_of = NULL,
                                detvalue = NULL,
                                morphocat = NULL) {

  if(!exists("mydb")) call.mydb()

  if(is.null(full_name) & !is.null(tax_esp)) stop("Provide full name with authors")

  if(is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam)) stop("Provide at least one genus/family/ new name to enter")

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

  if(is.null(tax_fam) & !is.null(tax_gen)) {
    tax_fam <-
      query_tax_all(genus_searched = tax_gen)  %>%
      dplyr::distinct(tax_fam) %>%
      dplyr::pull()
    tax_fam <- tax_fam[which(!is.na(tax_fam))]
    if(length(tax_fam)>1) cat(paste("\n No tax_fam provided, and two different family names for this genus", paste0(tax_fam, sep=", ")))
    if(length(tax_fam)>1) check_taxo <- FALSE
    if(length(tax_fam)==1) cat(paste("\n No tax_fam provided, based on genus, the following family is chosen:", tax_fam))
  }

  # if(is.null(tax_order) & !is.null(tax_fam)) {
  #   tax_order <-
  #     query_tax_all(tax_fam_searched = tax_fam) %>%
  #   # , verbose = F, exact_match = T,
  #   #              class = NULL, check_synonymy = FALSE)
  #     dplyr::distinct(tax_order) %>%
  #     dplyr::pull()
  #   tax_order <- tax_order[which(!is.na(tax_order))]
  #   if(length(tax_order)>1)
  #     cat(paste("\n No tax_order provided, and two different order names for this family", paste0(tax_order, sep=", ")))
  #   if(length(tax_order)>1) check_taxo <- FALSE
  #   if(length(tax_order)==1)
  #     cat(paste("\n No tax_order provided, based on family, the following order is chosen:", tax_order))
  # }

  # if(is.null(tax_famclass) & !is.null(tax_order)) {
  #   tax_famclass <-
  #     query_taxa(order = tax_order, verbose = F, exact_match = T,
  #                class = NULL, check_synonymy = FALSE) %>%
  #     dplyr::distinct(tax_famclass) %>%
  #     dplyr::pull()
  #   tax_famclass <- tax_famclass[which(!is.na(tax_famclass))]
  #   if(length(tax_famclass)>1)
  #     cat(paste("\n No tax_famclass provided, and two different class names for this order",
  #               paste0(tax_famclass, sep=", ")))
  #   if(length(tax_famclass)>1) check_taxo <- FALSE
  #   if(length(tax_famclass)==1)
  #     cat(paste("\n No tax_famclass provided, based on order, the following class is chosen:", tax_famclass))
  # }

  tax_fam_new <- TRUE
  if(!is.null(tax_fam) & check_taxo) {
    searched_tax_fam <-
      dplyr::tbl(mydb, "diconame") %>%
      dplyr::distinct(tax_fam) %>%
      dplyr::filter(tax_fam == !!tax_fam) %>%
      dplyr::collect()
    if(nrow(searched_tax_fam)==0) {
      tax_fam_new <-
        utils::askYesNo(msg = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled?", default = FALSE)
    }
  }


  if(!is.null(full_name) & !is.null(tax_gen)) {
    if(!grepl(tax_gen, full_name)) stop("\n Genus and full_name are provided, but genus is not found within full name, there must be an ERROR")
  }

  if(!is.null(full_name) & !is.null(tax_esp)) {
    if(!grepl(tax_esp, full_name)) stop("\n Species and full_name are provided, but tax_esp is not found within full_name, there must be an ERROR")
  }

  if(is.null(tax_gen) & !is.null(tax_esp)) {
    stop("\n species epithet provided but no genus (provide tax_gen)")
  }

  if(!is.null(tax_gen)) {

    family_check <-
      query_tax_all(tax_fam_searched = tax_fam)
    # exact_match = T, verbose = F, class = NULL, check_synonymy = FALSE)

    genus_check <-
      query_tax_all(genus_searched = tax_gen)
    # exact_match = T,
    # verbose = F,
    # class = NULL,
    # check_synonymy = FALSE)

    if(!is.null(genus_check)) {
      if(nrow(genus_check) > 0 & !any(family_check$tax_gen %in% tax_gen)) {
        cat(paste("\n The provided genus is present in the taxonomic backbone, but with different family name:", genus_check$tax_fam[1]))
        check_taxo <- FALSE
      }
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
    if(is.null(tax_fam)) tax_fam <- NA
    if(is.null(tax_rank1)) tax_rank1 <- NA
    if(is.null(tax_name1)) tax_name1 <- NA
    # if(is.null(tax_rank2)) tax_rank2 <- NA
    # if(is.null(tax_name2)) tax_name2 <- NA

    # tax_rank <- NA
    # if(!is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name1))
    #   tax_rank <- "ESP"
    # if(is.na(tax_esp) & is.na(tax_name1) & is.na(tax_name2))
    #   tax_rank <- NA
    # if(!is.na(tax_esp) & !is.na(tax_rank1)) {
    #   if(tax_rank1=="subsp.") tax_rank <- "SUBSP"
    #   if(tax_rank1=="var.") tax_rank <- "VAR"
    #   if(tax_rank1=="f.") tax_rank <- "F"
    # }
    #
    # if(!is.na(tax_rank)) {
    #   if(tax_rank=="VAR") tax_rankinf <- "VAR"
    #   if(tax_rank=="SUBSP") tax_rankinf <- "SUBSP"
    # }
    # if(!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "FAM"
    # if(!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankinf <- "GEN"
    # if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp) & is.na(tax_rank))
    #   tax_rankinf <- "ESP"
    # if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp) & tax_rank=="ESP")
    #   tax_rankinf <- "ESP"
    #
    # if(!is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "ORDER"
    # if(!is.null(tax_famclass) & is.na(tax_order) & is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "CLASS"
    # if(!is.na(tax_fam) & is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "FAM"
    # if(!is.na(tax_fam) & !is.na(tax_gen) & is.na(tax_esp)) tax_rankesp <- "GEN"
    # if(!is.na(tax_fam) & !is.na(tax_gen) & !is.na(tax_esp))
    #   tax_rankesp <- "ESP"

    if (!is.na(tax_gen) &
        !is.na(tax_esp))
      paste_taxa <- paste(tax_gen, tax_esp)
    if (!is.na(tax_gen) &
        is.na(tax_esp))
      paste_taxa <- tax_gen
    if (!is.na(tax_fam) &
        is.na(tax_gen))
      paste_taxa <- tax_fam

    new_rec <-
      dplyr::tibble(
        tax_fam = tax_fam,
        tax_gen = tax_gen,
        tax_esp = tax_esp,
        tax_rank1 = tax_rank1,
        tax_name1 = tax_name1,
        detvalue = detvalue,
        morphocat = morphocat,
        full_name_no_auth = paste_taxa,
        full_name_used = paste_taxa,
        full_name_used2 = paste_taxa,
        # tax_rank02 = tax_rank2,
        # tax_nam02 = tax_name2,
        full_name = full_name,
        id_good = NA,
        id = NA
      ) %>%
      dplyr::mutate(id_good = as.numeric(id_good))

    seek_dup <-
      tbl(mydb, "diconame")


    # if(!is.na(new_rec$tax_order)) {
    #   seek_dup <- seek_dup %>%
    #     filter(tax_order == !!new_rec$tax_order)
    # }else{
    #   seek_dup <- seek_dup %>%
    #     filter(is.na(tax_order))
    # }

    if(!is.na(new_rec$tax_fam)) {
      seek_dup <- seek_dup %>%
        filter(tax_fam == !!new_rec$tax_fam)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_fam))
    }

    if(!is.na(new_rec$tax_gen)) {
      seek_dup <- seek_dup %>%
        filter(tax_gen == !!new_rec$tax_gen)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_gen))
    }

    if(!is.na(new_rec$tax_esp)) {
      seek_dup <- seek_dup %>%
        filter(tax_esp == !!new_rec$tax_esp)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_esp))
    }

    if(!is.na(new_rec$tax_rank1)) {
      seek_dup <- seek_dup %>%
        filter(tax_rank1 == !!new_rec$tax_rank1)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_rank1))
    }

    if(!is.na(new_rec$tax_name1)) {
      seek_dup <- seek_dup %>%
        filter(tax_name1 == !!new_rec$tax_name1)
    }else{
      seek_dup <- seek_dup %>%
        filter(is.na(tax_name1))
    }

    # if(!is.na(new_rec$tax_nam02)) {
    #   seek_dup <- seek_dup %>%
    #     filter(tax_nam02 == !!new_rec$tax_nam02)
    # }else{
    #   seek_dup <- seek_dup %>%
    #     filter(is.na(tax_nam02))
    # }

    seek_dup <-
      seek_dup %>%
      collect()

    launch_adding_data <- TRUE

    if (nrow(seek_dup) > 0) {

      cli::cli_alert_info("New entry fit to one entry already in table_taxa")
      print(as.data.frame(seek_dup))
      launch_adding_data <- FALSE

    }

    if(launch_adding_data) {

      new_rec <-
        new_rec %>%
        tibble::add_column(data_modif_d = lubridate::day(Sys.Date()),
                           data_modif_m = lubridate::month(Sys.Date()),
                           data_modif_y = lubridate::year(Sys.Date()))

      cli::cli_alert_info("Adding new entry")
      DBI::dbWriteTable(mydb, "diconame", new_rec, append = TRUE, row.names = FALSE)

      new_entry <-
        dplyr::tbl(mydb, "diconame") %>%
        dplyr::filter(full_name == !!new_rec$full_name,
                      data_modif_d == !!lubridate::day(Sys.Date()),
                      data_modif_m == !!lubridate::month(Sys.Date()),
                      data_modif_y == !!lubridate::year(Sys.Date())) %>%
        dplyr::collect()

      if(!is.null(synonym_of)) {

        if (!is.list(synonym_of)) {
          stop(
            "synonym_of should be a list with the first element \nbeing the genus and the second element the species epiteth of the taxa of the correct name \nOR the idtax_n"
          )
        }

        if (!any(names(synonym_of) == "genus") &
            !any(names(synonym_of) == "species") &
            !any(names(synonym_of) == "id"))
          stop("synonym_of should have at least of the thre following element : genus, species or idtax_n")

        if (!any(names(synonym_of) == "genus"))
          synonym_of$genus <- NULL
        if (!any(names(synonym_of) == "species"))
          synonym_of$species <- NULL
        if (!any(names(synonym_of) == "id"))
          synonym_of$id <- NULL

        syn_searched <-
          query_tax_all(genus_searched = synonym_of$genus,
                        tax_esp_searched = synonym_of$species,
                        id_search = synonym_of$id)

        print(syn_searched)
        if (nrow(syn_searched) > 1)
          stop("More than 1 taxa as synonym. Select only one.")
        if (nrow(syn_searched) == 0)
          stop("No taxa found in the dictionnary. Select one.")


        update_dico_name(new_id_diconame_good = syn_searched$id_good_n,
                         id_search = new_entry$id_n,
                         ask_before_update = FALSE,
                         add_backup = FALSE)
        # update_dico_name(
        #   new_id_diconame_good = syn_searched$idtax_good_n,
        #   id_search = new_entry$idtax_n,
        #   ask_before_update = FALSE,
        #   add_backup = FALSE,
        #   show_results = FALSE
        # )

      }else{

        rs <-
          DBI::dbSendQuery(mydb, statement="UPDATE diconame SET id_good_n=$2 WHERE id_n = $1",
                           params=list(new_entry$id_n, new_entry$id_n)) # $10

        DBI::dbClearResult(rs)

      }

      # print(dplyr::tbl(mydb, "table_taxa") %>%
      #         dplyr::collect() %>%
      #         dplyr::filter(idtax_n == max(idtax_n)))

      res_selected <- dplyr::tbl(mydb, "diconame") %>%
        dplyr::filter(id_n == !!new_entry$id_n) %>%
        collect()

      res_selected <- as_tibble(cbind(
        columns = names(res_selected),
        record = t(res_selected)
      )) %>%
        kableExtra::kable(format = "html", escape = F) %>%
        kableExtra::kable_styling("striped", full_width = F)

      print(res_selected)

    }

  }else{

    cat("\n NO ADDED ENTRY")

  }
}



# add_entry_dico_name <- function(tax_gen=NULL,
#                                 tax_esp = NULL,
#                                 tax_fam = NULL,
#                                 tax_rank1 = NULL,
#                                 tax_name1 = NULL,
#                                 detvalue = NULL,
#                                 morphocat = NULL,
#                                 full_name = NULL,
#                                 synonym_of = NULL) {
#
#   if(!exists("mydb")) call.mydb()
#
#   if(is.null(full_name) & !is.null(tax_esp)) stop("Provide full name with authors")
#
#   if(is.null(tax_gen) & is.null(tax_esp) & is.null(tax_fam)) stop("Provide at least one genus/family new name to enter")
#
#   if(!is.null(tax_fam) & is.null(full_name) & is.null(tax_gen)) {
#     full_name <- tax_fam
#   }
#
#   if(is.null(detvalue)) {
#     if(is.null(tax_esp) & !is.null(tax_gen)) detvalue <- 6
#     if(is.null(tax_esp) & is.null(tax_gen)) detvalue <- 7
#
#     if(!is.null(tax_esp) & !is.null(tax_gen)) detvalue <- 1
#
#     cat(paste("\n No detvalue provided, by default, the following value is given:", detvalue))
#   }
#
#   if(is.null(morphocat)) {
#     if(is.null(tax_esp) & !is.null(tax_gen)) morphocat <- 3
#     if(is.null(tax_esp) & is.null(tax_gen)) morphocat <- 3
#
#     if(!is.null(tax_esp) & !is.null(tax_gen)) morphocat <- 1
#
#     cat(paste("\n No morphocat provided, by default, the following value is given:", morphocat))
#   }
#
#   check_taxo <- TRUE
#
#   if(is.null(tax_fam)) {
#     tax_fam <- query_tax_all(genus_searched = tax_gen) %>% dplyr::distinct(tax_fam) %>% dplyr::pull()
#     tax_fam <- tax_fam[which(!is.na(tax_fam))]
#     if(length(tax_fam)>1) cat(paste("\n No tax_fam provided, and two different family names for this genus", paste0(tax_fam, sep=", ")))
#     if(length(tax_fam)>1) check_taxo <- FALSE
#     if(length(tax_fam)==1) cat(paste("\n No tax_fam provided, based on genus, the following family is chosen:", tax_fam))
#   }
#
#   tax_fam_new <- TRUE
#   if(!is.null(tax_fam)) {
#     searched_tax_fam <-
#       dplyr::tbl(mydb, "diconame") %>% dplyr::distinct(tax_fam) %>% dplyr::filter(tax_fam==!!tax_fam) %>% dplyr::collect()
#     if(nrow(searched_tax_fam)==0) {
#       tax_fam_new <-
#         utils::askYesNo(msg = "The provided family name is currently not present in the dictionnary. Are you sure it is correctly spelled?", default = FALSE)
#     }
#   }
#
#   if(!is.null(full_name) & !is.null(tax_gen)) {
#     if(!grepl(tax_gen, full_name)) stop("\n Genus and full name are provided, but genus is not found within full name, there must be an ERROR")
#   }
#
#   if(!is.null(full_name) & !is.null(tax_esp)) {
#     if(!grepl(tax_esp, full_name)) stop("\n Species and full name are provided, but Species is not found within full name, there must be an ERROR")
#   }
#
#   if(is.null(tax_gen) & !is.null(tax_esp)) {
#     stop("\n Genus provided but no species epithet (provide tax_gen)")
#   }
#
#   if(!is.null(tax_gen)) {
#
#     family_check <-
#       query_tax_all(tax_fam_searched = tax_fam)
#
#     genus_check <-
#       query_tax_all(genus_searched =  tax_gen)
#
#     if (nrow(genus_check) > 0 & !any(family_check$tax_gen %in% tax_gen)) {
#
#       cat(paste("\n The provided genus is present in the dictionnary, but with different family name:", genus_check$tax_fam[1]))
#       check_taxo <- FALSE
#
#     }
#   }
#
#   # tbl(mydb, "diconame") %>% collect() %>% slice(n())
#
#   if(check_taxo & tax_fam_new) {
#     if(!is.null(tax_gen) & !is.null(tax_esp)) paste_taxa <- paste(tax_gen, tax_esp)
#     if(!is.null(tax_gen) & is.null(tax_esp)) paste_taxa <- tax_gen
#     if(!is.null(tax_fam) & is.null(tax_gen)) paste_taxa <- tax_fam
#     if(is.null(full_name) & !is.null(tax_gen) & is.null(tax_esp)) full_name <- tax_gen
#
#     if(is.null(tax_esp)) tax_esp <- NA
#     if(is.null(tax_gen)) tax_gen <- NA
#     if(is.null(tax_rank1)) tax_rank1 <- NA
#     if(is.null(tax_name1)) tax_name1 <- NA
#
#     new_rec <- dplyr::tibble(id=0, id_good=0, full_name=full_name, full_name_no_auth=paste_taxa, full_name_used=paste_taxa,
#                       full_name_used2=paste_taxa, tax_fam=tax_fam, tax_gen=tax_gen, tax_esp=tax_esp, taxook=1, detvalue=detvalue,
#                       morphocat=morphocat, id_good_n=0, data_modif_d=lubridate::day(Sys.Date()), data_modif_m=lubridate::month(Sys.Date()),
#                       data_modif_y=lubridate::year(Sys.Date()),
#                       tax_rank1=tax_rank1, tax_name1=tax_name1)
#
#     DBI::dbWriteTable(mydb, "diconame", new_rec, append = TRUE, row.names = FALSE)
#
#     new_entry <-
#       dplyr::tbl(mydb, "diconame") %>%
#       dplyr::filter(id_good_n==0, data_modif_d==lubridate::day(Sys.Date()), data_modif_m==lubridate::month(Sys.Date()), data_modif_y==lubridate::year(Sys.Date())) %>%
#       dplyr::collect()
#
#     if(!is.null(synonym_of)) {
#
#       if(!is.list(synonym_of)) {
#         stop("synonym_of should be a list with the first element \nbeing the genus and the second element the species epiteth of the taxa of the correct name")
#       }
#
#       if(!any(names(synonym_of)=="genus") & !any(names(synonym_of)=="species") & !any(names(synonym_of)=="id"))
#       stop("synonym_of should have at least of the thre following element : genus, species or id")
#
#       if(!any(names(synonym_of)=="genus")) synonym_of$genus <- NULL
#       if(!any(names(synonym_of)=="species")) synonym_of$species <- NULL
#       if(!any(names(synonym_of)=="id")) synonym_of$id <- NULL
#
#       syn_searched <-
#         query_tax_all(genus_searched = synonym_of$genus,
#                       tax_esp_searched = synonym_of$species,
#                       id_search = synonym_of$id)
#
#       print(syn_searched)
#       if(nrow(syn_searched)>1) stop("More than 1 taxa as synonym. Select only one.")
#       if(nrow(syn_searched)==0) stop("No taxa found in the dictionnary. Select one.")
#
#       update_dico_name(new_id_diconame_good = syn_searched$id_good_n, id_search = new_entry$id_n,
#                        ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)
#
#     }else{
#       update_dico_name(new_id_diconame_good = new_entry$id_n, id_search = new_entry$id_n,
#                        ask_before_update = FALSE, add_backup = FALSE, show_results = FALSE)
#     }
#
#     print(dplyr::tbl(mydb, "diconame") %>% dplyr::collect() %>% dplyr::filter(id_n == max(id_n)) %>%
#             as.data.frame())
#   }else{
#
#     cat("\n NO ADDED ENTRY")
#   }
# }









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
.delete_taxa <- function(id) {

  if(!exists("mydb")) call.mydb()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM table_taxa WHERE idtax_n=$1", params=list(id)
  # )

  query <- "DELETE FROM table_taxa WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("idtax_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)
}

#' Delete an entry in subplotype_list table
#'
#' Delete an entry in subplotype_list table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_subplottype <- function(id) {

  if(!exists("mydb")) call.mydb()



  DBI::dbExecute(mydb,
                 "DELETE FROM subplotype_list WHERE id_subplotype=$1", params=list(id)
  )
}

#' Delete an entry in subplotype_list table
#'
#' Delete an entry in subplotype_list table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
.delete_subplotfeature <- function(id) {

  if(!exists("mydb")) call.mydb()

  query <- "DELETE FROM data_liste_sub_plots WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_sub_plots IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_liste_sub_plots WHERE id_sub_plots=$1", params=list(id)
  # )
}

#' Delete an entry in trait measurement table
#'
#' Delete an entry in trait measurement table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_entry_trait_measure <- function(id) {

  if(!exists("mydb")) call.mydb()

  DBI::dbExecute(mydb,
                 "DELETE FROM data_traits_measures WHERE id_trait_measures=$1", params=list(id)
  )
}


#' Delete an entry in trait measurement table
#'
#' Delete an entry in diconame table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
# .delete_entry_diconame <- function(id) {
#
#   if(!exists("mydb")) call.mydb()
#
#   DBI::dbExecute(mydb,
#                  "DELETE FROM diconame WHERE id_n=$1", params=list(id)
#   )
# }


#' Delete an entry in trait measurement table
#'
#' Delete an entry in trait measurement table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_ind integer
#' @param id_specimen integer
#'
#' @return No values
#' @export
.delete_link_individual_specimen <- function(id_ind = NULL,
                                             id_specimen = NULL) {

  if(!exists("mydb")) call.mydb()

  if(!is.null(id_ind)) {
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_n %in% !!id_ind) %>%
      dplyr::collect() %>%
      as.data.frame()

    print(selected_link)
    confirm <-
      utils::askYesNo(msg = "Confirm removing these links?")

    if(confirm)
      for (i in 1:nrow(selected_link))
        DBI::dbExecute(mydb,
                     "DELETE FROM data_link_specimens WHERE id_n=$1",
                     params=list(selected_link$id_n[i]))
  }

  if(!is.null(id_specimen)) {
    selected_link <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen %in% !!id_specimen) %>%
      dplyr::collect() %>%
      as.data.frame()

    print(selected_link)

    confirm <-
      utils::askYesNo(msg = "Confirm removing these links?")

    if(confirm) {
      query <- "DELETE FROM data_link_specimens WHERE MMM"
      query <-
        gsub(
          pattern = "MMM",
          replacement = paste0("id_specimen IN ('",
                               paste(unique(selected_link$id_specimen), collapse = "', '"), "')"),
          x = query
        )

      rs <- DBI::dbSendQuery(mydb, query)
      DBI::dbClearResult(rs)
    }


    # if(confirm)
    #   for (i in 1:nrow(selected_link))
    #     DBI::dbExecute(mydb,
    #                    "DELETE FROM data_link_specimens WHERE id_specimen=$1",
    #                    params=list(selected_link$id_specimen[i]))
  }

}

#' Delete an entry in individuals table
#'
#' Delete an entry in individuals table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_individuals <- function(id) {

  if(!exists("mydb")) call.mydb()

  # DBI::dbExecute(mydb,
  #                "DELETE FROM data_individuals WHERE id_n=$1", params=list(id)
  # )

  query <- "DELETE FROM data_individuals WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_n IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

}


#' Delete an entry in specimen table
#'
#' Delete an entry in specimen table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_specimens <- function(id) {

  if(!exists("mydb")) call.mydb()

  query <- "DELETE FROM specimens WHERE MMM"
  query <-
    gsub(
      pattern = "MMM",
      replacement = paste0("id_specimen IN ('",
                           paste(unique(id), collapse = "', '"), "')"),
      x = query
    )

  rs <- DBI::dbSendQuery(mydb, query)
  DBI::dbClearResult(rs)

}


#' Delete an entry in plot meta-data
#'
#' Delete an entry in plot meta-data table using id for selection
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id integer
#'
#' @return No values
#' @export
.delete_plot <- function(id) {

  if(!exists("mydb")) call.mydb()

  DBI::dbExecute(mydb,
                 "DELETE FROM data_liste_plots WHERE id_liste_plots=$1", params=list(id)
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
#' @param id_colnam integer id of collector
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
#' @param extract_linked_individuals logical extract individuals linked to selected specimens
#'
#' @return A tibble
#' @export
query_specimens <- function(collector = NULL,
                            id_colnam = NULL,
                            number = NULL,
                            number_min = NULL,
                            number_max = NULL,
                            genus_searched = NULL,
                            tax_esp_searched = NULL,
                            tax_fam_searched = NULL,
                            id_search = NULL,
                            subset_columns = TRUE,
                            show_previous_modif = TRUE,
                            generate_labels = FALSE,
                            project_title = "Reference specimens collected in trees inventory",
                            file_labels = "labels",
                            extract_linked_individuals = FALSE) {

  if(!exists("mydb")) call.mydb()

  query_speci <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::left_join(
      dplyr::tbl(mydb, "table_taxa") %>%
        dplyr::mutate(idtax_f = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n)) %>%
        dplyr::select(idtax_n, idtax_f),
      by = c("idtax_n" = "idtax_n")
    ) %>%
    left_join(add_taxa_table_taxa(),
              by = c("idtax_f" = "idtax_n")) %>%
    dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
                     by = c("id_colnam" = "id_table_colnam"))

  # %>%
  #   dplyr::select(-id_specimen_old, -id_diconame, -photo_tranche, -id_colnam, -id_good, -id, -id_good_n)

  if (subset_columns & !generate_labels)
    query_speci <-
    query_speci %>%
    dplyr::select(
      colnam,
      colnbr,
      suffix,
      tax_sp_level,
      tax_infra_level_auth,
      tax_fam,
      tax_gen,
      tax_esp,
      ddlat,
      ddlon,
      country,
      detby,
      detd,
      detm,
      dety,
      add_col,
      cold,
      colm,
      coly,
      detvalue,
      morpho_species,
      id_specimen,
      idtax_f,
      id_tropicos
    )

  ## filter by collector or id_colnam (id of people table)
  if ((!is.null(collector) |
       !is.null(id_colnam)) & is.null(id_search)) {
    if (is.null(id_colnam)) {
      var <- rlang::enquo(collector)

      query_speci <-
        query_speci %>%
        dplyr::filter(grepl(!!var, colnam))
    } else{
      query_speci <-
        query_speci %>%
        dplyr::filter(id_colnam == !!id_colnam)
    }
  }

  if(!is.null(number) & is.null(id_search)) {

    var <- rlang::enquo(number)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr %in% var)
  }

  if(!is.null(number_min) & is.null(id_search)) {

    var <- rlang::enquo(number_min)

    query_speci <-
      query_speci %>%
      dplyr::filter(colnbr >= var)
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
      dplyr::filter(id_specimen == var)
  }

  query <-
    query_speci %>%
    dplyr::collect()

  # print(query)

  if(extract_linked_individuals) {

    linked_ind <-
      dplyr::tbl(mydb, "data_link_specimens") %>%
      dplyr::filter(id_specimen == !!query$id_specimen) %>%
      dplyr::select(id_n, id_specimen) %>%
      dplyr::collect()

    linked_ind <-
      query_plots(
        id_individual = linked_ind$id_n,
        extract_individuals = TRUE,
        remove_ids = FALSE
      )

    cli::cli_alert_info(
      "This specimen is linked to {nrow(linked_ind)} individuals from {length(unique(linked_ind$plot_name))} plot(s)"
    )

  }

  if (nrow(query) == 1 & show_previous_modif) {

    ## get previous modifications of queried entries
    # modif_backups <-
    #   dplyr::tbl(mydb, "followup_updates_specimens") %>%
    #   dplyr::filter(id_specimen == !!query$id_specimen) %>%
    #   dplyr::filter(grepl("idtax_n", modif_type)) %>%
    #   dplyr::left_join(
    #     dplyr::tbl(mydb, "table_taxa") %>%
    #       dplyr::select(-data_modif_d,-data_modif_m,-data_modif_y),
    #     by = c("id_diconame_n" = "idtax_n")
    #   ) %>%
    #   dplyr::left_join(dplyr::tbl(mydb, "table_colnam"),
    #                    by = c("id_colnam" = "id_table_colnam")) %>%
    #   dplyr::collect()

    # if(nrow(modif_backups) > 0) {
    #
    #         # cat("\n Previous modification in identification")
    #   #
    #   # print(modif_backups)
    #
    #   modif_backups <-
    #     modif_backups %>%
    #     dplyr::select(all_of(names(query)))
    #
    # }else{
    #
    #   cli::cli_alert_info("No identification change in backups")
    #
    # }
  }

  if(generate_labels) {

    if (!any(rownames(utils::installed.packages()) == "measurements"))
      stop("measurements package needed, please install it")

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
      dplyr::mutate(specimen_code_up =
                      paste0(colnam,' ' , ifelse(!is.na(specimen_nbe_char), specimen_nbe_char, colnbr))) %>%
      tibble::add_column(
        INSTITUTION_CODE = rep("BRLU", nrow(.)),
        HERBARIUM = rep("BRLU", nrow(.)),
        TITLE = rep(project_title, nrow(.)),
        AUTHOR_OF_SPECIES = NA,
        INFRASPECIFIC_RANK = NA,
        INFRASPECIFIC_EPITHET = NA,
        AUTHOR_OF_INFRASPECIFIC_RANK = NA,
        COUNTY = NA,
        IMAGE_URL = NA,
        RELATED_INFORMATION = NA,
        LAT_DEGREE = as.double(lat_convert_deg),
        LAT_MINUTE = as.double(lat_convert_min),
        LAT_SECOND = as.double(lat_convert_sec),
        LON_DEGREE = as.double(long_convert_deg),
        LON_MINUTE = as.double(long_convert_min),
        LON_SECOND = as.double(long_convert_sec),
        LAT_FLAG = lat_flag,
        LON_FLAG = long_flag,
        REMARKS = NA,
        GEOREFERENCE_SOURCES = NA,
        PROJECT = NA,
        TYPE_STATUS = NA,
        PROCESSED_BY = NA,
        LOCAL_NAME = NA
      ) %>%
      dplyr::rename(
        GLOBAL_UNIQUE_IDENTIFIER = id_specimen,
        COLLECTION_CODE = colnbr,
        COLLECTOR = colnam,
        ADDITIONAL_COLLECTOR = add_col,
        COLLECTOR_NUMBER = specimen_code_up,
        FAMILY = tax_fam,
        GENUS = tax_gen,
        SPECIES = tax_esp,
        COUNTRY = country,
        STATE_PROVINCE = majorarea,
        LOCALITY = locality,
        ELEVATION = elevation,
        ATTRIBUTES = description,
        IDENTIFIED_BY = detby,
        FULL_NAME = full_name
      ) %>%
      dplyr::mutate(
        coly = ifelse(is.na(coly) | coly == 0, "", coly),
        colm = ifelse(is.na(colm) | colm == 0, "", colm),
        cold = ifelse(is.na(cold) | cold == 0, "", cold)
      ) %>%
      dplyr::mutate(
        DATE_COLLECTED = paste(coly, colm, cold, sep = "-"),
        DATE_IDENTIFIED = paste(
          ifelse(is.na(dety) | dety == 0, "", dety),
          ifelse(is.na(detm) |
                   detm == 0, "", detm),
          ifelse(is.na(detd) |
                   detd == 0, "", detd),
          sep = "-"
        ),
        DATE_LASTMODIFIED = paste(data_modif_y, data_modif_m, data_modif_d , sep =
                                    "-")
      ) %>%
      dplyr::select(
        INSTITUTION_CODE,
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
        DATE_LASTMODIFIED
      )

    herbarium_label(dat = query_labels,
                    theme="GILLES",
                    outfile = paste0(file_labels, ".rtf"))

  }

  nrow_query <-
    nrow(query)

  # if (nrow(query) == 1 & show_previous_modif) {
  #
  #   if(nrow(modif_backups) > 0)
  #     query <-
  #     bind_rows(query,
  #               modif_backups)
  #
  # }

  if(nrow(query) < 20)
  {
    res_html <-
      tibble(columns = names(query), data.frame(t(query),
                                                fix.empty.names = T)) %>%
      mutate_all(~ tidyr::replace_na(., ""))

    # if(nrow(query) == 1 & show_previous_modif) {
    #   if(nrow(modif_backups) > 0) {
    #     for (i in ((nrow_query  + 2):(nrow(query)  + 1))) {
    #
    #       col_ <- colnames(res_html)[i]
    #       var_new <-
    #         rlang::parse_expr(rlang::quo_name(rlang::enquo(col_)))
    #
    #       res_html <-
    #         res_html %>%
    #         mutate(!!var_new :=
    #                  kableExtra::cell_spec(!!var_new,
    #                                        "html",
    #                                        background = "grey",
    #                                        color = "white", italic = T))
    #     }
    #   }
    # }

    res_html %>%
      kableExtra::kable(format = "html", escape = F) %>%
      kableExtra::kable_styling("striped", full_width = F) %>%
      print()
  }


  if(!extract_linked_individuals) return(query)

  if(extract_linked_individuals) return(list(linked_ind = linked_ind,
                                             query = query))

}



#' Add a trait in trait list
#'
#' Add trait and associated descriptors in trait list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_trait string value with new trait descritors - try to avoid space
#' @param new_relatedterm string related trait to new trait
#' @param new_valuetype string one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character'
#' @param new_maxallowedvalue numeric if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue numeric if valuetype is numeric, indicate the minimum allowed value
#' @param new_traitdescription string full description of trait
#' @param new_factorlevels string a vector of all possible value if valuetype is categorical or ordinal
#' @param new_expectedunit string expected unit (unitless if none)
#' @param new_comments string any comments
#'
#' @export
add_trait <- function(new_trait = NULL,
                      new_relatedterm = NULL,
                      new_valuetype = NULL,
                      new_maxallowedvalue = NULL,
                      new_minallowedvalue = NULL,
                      new_traitdescription = NULL,
                      new_factorlevels = NULL,
                      new_expectedunit = NULL,
                      new_comments = NULL) {

  if(is.null(new_trait)) stop("define new trait")
  if(is.null(new_valuetype)) stop("define new_valuetype")

  if(!any(new_valuetype==c('numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character'))) stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', or 'character'")

  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_maxallowedvalue) & !is.integer(new_maxallowedvalue)) stop("valuetype numeric of integer and max value not of this type")
  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_minallowedvalue) & !is.integer(new_minallowedvalue)) stop("valuetype numeric of integer and min value not of this type")

  if(!exists("mydb")) call.mydb()

  new_data_renamed <- tibble(trait = new_trait,
                             relatedterm = ifelse(is.null(new_relatedterm), NA, new_relatedterm),
                             valuetype = new_valuetype,
                             maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
                             minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
                             traitdescription = ifelse(is.null(new_traitdescription), NA, new_traitdescription),
                             factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
                             expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
                             comments = ifelse(is.null(new_comments), NA, new_comments))

  print(new_data_renamed)

  Q <- utils::askYesNo("confirm adding this trait?")

  if(Q) DBI::dbWriteTable(mydb, "traitlist", new_data_renamed, append = TRUE, row.names = FALSE)

}



#' Add a type in subplot table
#'
#' Add feature and associated descriptors in subplot list table
#'
#' @return nothing
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_type string value with new type descritors - try to avoid space
#' @param new_valuetype string one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', 'character'
#' @param new_maxallowedvalue numeric if valuetype is numeric, indicate the maximum allowed value
#' @param new_minallowedvalue numeric if valuetype is numeric, indicate the minimum allowed value
#' @param new_typedescription string full description of trait
#' @param new_factorlevels string a vector of all possible value if valuetype is categorical or ordinal
#' @param new_expectedunit string expected unit (unitless if none)
#' @param new_comments string any comments
#'
#' @export
add_subplottype <- function(new_type = NULL,
                      new_valuetype = NULL,
                      new_maxallowedvalue = NULL,
                      new_minallowedvalue = NULL,
                      new_typedescription = NULL,
                      new_factorlevels = NULL,
                      new_expectedunit = NULL,
                      new_comments = NULL) {

  if(is.null(new_type)) stop("define new type")

  if(dplyr::tbl(mydb, "subplotype_list") %>%
    dplyr::distinct(type) %>%
    dplyr::filter(type == !!new_type) %>%
    dplyr::collect() %>%
    nrow()>0)  stop("new type already in table")


  if(is.null(new_valuetype)) stop("define new_valuetype")

  if(!any(new_valuetype==c('numeric',
                           'integer',
                           'categorical',
                           'ordinal',
                           'logical',
                           'character'))) stop("valuetype should one of following 'numeric', 'integer', 'categorical', 'ordinal', 'logical', or 'character'")

  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_maxallowedvalue) & !is.integer(new_maxallowedvalue)) stop("valuetype numeric of integer and max value not of this type")
  if(new_valuetype=="numeric" | new_valuetype=="integer")
    if(!is.numeric(new_minallowedvalue) & !is.integer(new_minallowedvalue)) stop("valuetype numeric of integer and min value not of this type")

  if(!exists("mydb")) call.mydb()

  new_data_renamed <- tibble(type = new_type,
                             valuetype = new_valuetype,
                             maxallowedvalue = ifelse(is.null(new_maxallowedvalue), NA, new_maxallowedvalue),
                             minallowedvalue = ifelse(is.null(new_minallowedvalue), NA, new_minallowedvalue),
                             typedescription = ifelse(is.null(new_typedescription), NA, new_typedescription),
                             factorlevels = ifelse(is.null(new_factorlevels), NA, new_factorlevels),
                             expectedunit = ifelse(is.null(new_expectedunit), NA, new_expectedunit),
                             comments = ifelse(is.null(new_comments), NA, new_comments))

  print(new_data_renamed)

  Q <- utils::askYesNo("confirm adding this type?")

  if(Q)
    DBI::dbWriteTable(mydb, "subplotype_list", new_data_renamed, append = TRUE, row.names = FALSE)


}







#' Add an observation in trait measurement table
#'
#' Add a trait measure in trait measurement table
#'
#' @return list of tibbles that should be/have been added
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param new_data tibble
#' @param col_names_select string vector
#' @param col_names_corresp string vector
#' @param collector_field string column name which contain the collector name
#' @param plot_name_field string column name which contain the plot_name for linking
#' @param individual_plot_field string column name which contain the individual tag for linking
#' @param id_plot_name string column name which contain the ID of plot_name
#' @param id_tag_plot string column name which contain the ID of individuals table
#' @param id_specimen string column name which contain the ID of specimen
#' @param traits_field string vector listing trait columns names in new_data
#' @param add_data logical whether or not data should be added - by default FALSE
#'
#' @export
add_traits_measures <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = NULL,
                                collector_field = NULL,
                                plot_name_field = NULL,
                                individual_plot_field = NULL,
                                id_plot_name = NULL,
                                id_tag_plot = NULL,
                                id_specimen = NULL,
                                traits_field,
                                add_data = FALSE) {

  for (i in 1:length(traits_field))
    if (!any(colnames(new_data) == traits_field[i]))
      stop(paste("traits_field provide not found in new_data", traits_field[i]))

  if (!is.null(col_names_select) & !is.null(col_names_corresp)) {
    new_data_renamed <-
      .rename_data(dataset = new_data,
                   col_old = col_names_select,
                   col_new = col_names_corresp)
  } else{

    new_data_renamed <- new_data

  }

  ## removing entries with NA values for traits
  new_data_renamed <-
    new_data_renamed %>%
    dplyr::filter_at(dplyr::vars(!!traits_field), dplyr::any_vars(!is.na(.)))

  if (nrow(new_data_renamed) == 0)
    stop("no values for selected trait(s)")

  if (!any(col_names_corresp == "day")) {
    warning("no information collection day provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(day = NA) %>%
      mutate(day = as.numeric(day))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }

  if (!any(col_names_corresp == "year")) {
    warning("no information collection year provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(year = NA) %>%
      mutate(year = as.numeric(year))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }

  if (!any(col_names_corresp == "month")) {
    warning("no information collection month provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(month = NA) %>%
      mutate(month = as.numeric(month))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus date is mandatory")
  }

  if(!any(col_names_corresp == "country")) {
    warning("no country provided")
    new_data_renamed <-
      new_data_renamed %>%
      mutate(country = NA) %>%
      mutate(country = as.character(country))

    if(is.null(plot_name_field) & is.null(individual_plot_field) &
       is.null(id_specimen) & is.null(id_plot_name) &
       is.null(id_tag_plot)) stop("no links provided (either plot, specimen or tag), thus country is mandatory")

  }

  if (!any(col_names_corresp == "decimallatitude")) {
    warning("no decimallatitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(decimallatitude = NA) %>%
      dplyr::mutate(decimallatitude = as.double(decimallatitude))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus decimallatitude is mandatory")
  }

  if (!any(col_names_corresp == "decimallongitude")) {
    warning("no decimallongitude provided")
    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(decimallongitude = NA) %>%
      dplyr::mutate(decimallongitude = as.double(decimallongitude))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus decimallongitude is mandatory")
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(id_new_data = 1:nrow(.))

  ### Linking collectors names
  if(!is.null(collector_field)) {
    if(!any(colnames(new_data_renamed) == collector_field))
      stop("no collector_field found in new dataset")
    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed, collector_field = collector_field)
  } else{
    new_data_renamed <-
      new_data_renamed %>%
      mutate(id_colnam = NA) %>%
      mutate(id_colnam = as.numeric(id_colnam))

    if (is.null(plot_name_field) & is.null(individual_plot_field) &
        is.null(id_specimen) & is.null(id_plot_name) &
        is.null(id_tag_plot))
      stop("no links provided (either plot, specimen or tag), thus collector_field is mandatory")
  }

  ### Linking plot names
  if(!is.null(plot_name_field)) {
    if (!any(colnames(new_data_renamed) == plot_name_field))
      stop("plot_name_field not found in colnames")

    new_data_renamed <-
      .link_plot_name(data_stand = new_data_renamed, plot_name_field = plot_name_field)

  }

  if(!is.null(id_plot_name)) {
    id_plot_name <- "id_table_liste_plots_n"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(id_plot_name), ~ id_plot_name)

    if (any(colnames(new_data_renamed) == "plot_name"))
      new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-plot_name)

    link_plot <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(plot_name, id_liste_plots) %>% dplyr::collect(),
        by = c("id_table_liste_plots_n" = "id_liste_plots")
      )

    if (dplyr::filter(link_plot, is.na(plot_name)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_plot, is.na(plot_name)))
      cli::cli_alert_danger("provided id plot not found in plot metadata")
    }

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename(id_liste_plots = id_table_liste_plots_n)
  }

  ### linking individuals by id
  if(!is.null(id_tag_plot) & is.null(individual_plot_field)) {

    id_tag <-
      "id_n"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(all_of(id_tag_plot)), ~ id_tag)


    link_individuals <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::select(idtax_n, id_n) %>% dplyr::collect(),
        by = c("id_n" = "id_n")
      )

    if (dplyr::filter(link_individuals, is.na(idtax_n)) %>%
        nrow() > 0) {
      print(dplyr::filter(link_individuals, is.na(idtax_n)))
      stop("provided id individuals not found in data_individuals")
    }

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename(id_data_individuals = id_n)
  } else{

    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_data_individuals = NA) %>%
      dplyr::mutate(id_data_individuals = as.integer(id_data_individuals))

  }


  if (is.null(id_plot_name) & is.null(plot_name_field)) {

    if (!is.null(id_tag_plot) & is.null(individual_plot_field)) {

      queried_individuals <-
        query_plots(id_individual = new_data_renamed$id_data_individuals, remove_ids = F)

      new_data_renamed <-
        new_data_renamed %>%
        left_join(queried_individuals %>%
                    dplyr::select(id_n, id_table_liste_plots_n),
                  by = c("id_data_individuals" = "id_n")) %>%
        rename(id_liste_plots = id_table_liste_plots_n)

    } else {

      new_data_renamed <-
        new_data_renamed %>%
        tibble::add_column(id_liste_plots = NA) %>%
        dplyr::mutate(id_liste_plots = as.integer(id_liste_plots))

    }
  }


  ### check for different census for concerned plots
  multiple_census <- FALSE
  census_check <- utils::askYesNo(msg = "Link trait measures to census ?")

  if(census_check) {
    unique_ids_plots <- unique(new_data_renamed$id_liste_plots)
    censuses <-
      dplyr::tbl(mydb, "data_liste_sub_plots") %>%
      dplyr::filter(id_table_liste_plots %in% unique_ids_plots, id_type_sub_plot==27) %>%
      dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                         dplyr::select(plot_name, id_liste_plots), by=c("id_table_liste_plots"="id_liste_plots")) %>%
      dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                         dplyr::select(type, id_subplotype), by=c("id_type_sub_plot"="id_subplotype")) %>%
      dplyr::left_join(dplyr::tbl(mydb, "table_colnam") %>%
                         dplyr::select(id_table_colnam, colnam), by=c("id_colnam"="id_table_colnam")) %>%
      dplyr::collect()

    if(nrow(censuses) > 0) { # & length(unique(censuses$typevalue))>1

      message("\n multiple census for concerned plots")
      censuses %>%
        dplyr::select(plot_name, id_table_liste_plots, year, month, day, typevalue, type, colnam, additional_people) %>%
        as.data.frame() %>%
        print()
      census_chosen <- readline(prompt="Choose census ")

      chosen_ids_subplots <-
        censuses %>%
        dplyr::filter(typevalue == as.numeric(census_chosen)) %>%
        dplyr::select(id_table_liste_plots, id_sub_plots)

      if(nrow(chosen_ids_subplots) == 0) stop("chosen census not available")

      missing_census <-
        new_data_renamed %>%
        dplyr::distinct(id_liste_plots) %>%
        dplyr::filter(!id_liste_plots %in% chosen_ids_subplots$id_table_liste_plots) %>%
        dplyr::filter(!is.na(id_liste_plots))

      if(nrow(missing_census)) {
        print(missing_census %>%
                dplyr::left_join(dplyr::tbl(mydb, "data_liste_plots") %>%
                                   dplyr::select(id_liste_plots, plot_name) %>%
                                   dplyr::collect(),
                                 by=c("id_liste_plots"="id_liste_plots")) %>%
                as.data.frame())
        warning(paste("Missing census for", nrow(missing_census),"plots, census chosen :", census_chosen))
      }

      new_data_renamed <-
        new_data_renamed %>%
        dplyr::left_join(chosen_ids_subplots,
                         by = c("id_liste_plots" = "id_table_liste_plots"))
      # %>%
      #   filter(id_liste_plots==824) %>%
      #   select(id_sub_plots)

      if (as.numeric(census_chosen) > 1)
        multiple_census <- TRUE

    } else {

      new_data_renamed <-
        new_data_renamed %>%
        tibble::add_column(id_sub_plots = NA) %>%
        dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))
      multiple_census <- FALSE
    }
  }else{

    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_sub_plots = NA) %>%
      dplyr::mutate(id_sub_plots = as.integer(id_sub_plots))

  }

  ### Linking specimens
  if(!is.null(id_specimen)) {

    id_tag <-
      "id_specimen"

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::rename_at(dplyr::vars(id_specimen), ~ id_tag)

    link_specimen <-
      new_data_renamed %>%
      dplyr::filter(!is.na(id_specimen)) %>%
      dplyr::left_join(dplyr::tbl(mydb, "specimens") %>%
                  dplyr::select(id_diconame_n, id_specimen) %>% dplyr::collect(),
                by=c("id_specimen"="id_specimen"))

    if(dplyr::filter(link_specimen, is.na(id_diconame_n)) %>%
       nrow()>0) {
      print(dplyr::filter(link_specimen, is.na(id_diconame_n)))
      stop("provided id specimens not found in specimens table")
    }
  }else{

    if (!any(colnames(new_data_renamed) == "id_specimen")) {

      new_data_renamed <-
        new_data_renamed %>%
        tibble::add_column(id_specimen = NA) %>%
        dplyr::mutate(id_specimen = as.integer(id_specimen))

    } else{

      warning("id_specimen column already in new_data, check if content is correct")

    }

  }

  ### preparing dataset to add for each trait
  list_add_data <- vector('list', length(traits_field))
  for (i in 1:length(traits_field)) {

    trait <- traits_field[i]
    if(!any(colnames(new_data_renamed) == trait))
      stop(paste("trait field not found", trait))

    data_trait <-
      new_data_renamed

    trait_name <-
      "trait"
    data_trait <-
      data_trait %>%
      dplyr::rename_at(dplyr::vars(all_of(trait)), ~ trait_name)

    data_trait <-
      data_trait %>%
      dplyr::filter(!is.na(trait))

    if(any(data_trait$trait == 0)) {

      add_0 <- utils::askYesNo("Some value are equal to 0. Do you want to add these values anyway ??")

      if(!add_0)
        data_trait <-
          data_trait %>%
          dplyr::filter(trait != 0)

    }

    if(nrow(data_trait) > 0) {
      ### adding trait id and adding potential issues based on trait
      data_trait <-
        .link_trait(data_stand = data_trait, trait = trait)

      ## see what type of value numeric of character
      valuetype <-
        data_trait %>%
        dplyr::distinct(id_trait) %>%
        dplyr::left_join(
          dplyr::tbl(mydb, "traitlist") %>%
            dplyr::select(valuetype, id_trait) %>%
            dplyr::collect(),
          by = c("id_trait" = "id_trait")
        )



      ### Linking individuals
      if (!is.null(individual_plot_field)) {

        individual_plot <-
          "ind_num_sous_plot"

        data_trait <-
          data_trait %>%
          dplyr::rename_at(dplyr::vars(all_of(individual_plot_field)), ~ individual_plot)


        ## not numeric or missing individuals tag
        nbe_not_numeric <-
          suppressWarnings(which(is.na(as.numeric(data_trait$ind_num_sous_plot))))

        data_trait <-
          data_trait %>%
          dplyr::mutate(ind_num_sous_plot = as.numeric(ind_num_sous_plot))

        if(length(nbe_not_numeric) > 0) {
          cli::cli_alert_warning(
            "Number of non numeric (or missing) value in column indicating invividual number in plot : {length(nbe_not_numeric)}"
          )
          print(nbe_not_numeric)

          data_trait <-
            data_trait %>%
            filter(!is.na(ind_num_sous_plot))

          cli::cli_alert_warning("Number of non numeric (or missing) value REMOVED")
        }

        ## vector of id of all plots
        ids_plots_represented <-
          data_trait %>%
          dplyr::distinct(id_liste_plots) %>%
          dplyr::filter(!is.na(id_liste_plots)) %>%
          dplyr::pull()

        ## query of all individuals of these plots
        all_individual_selected_plot <-
          dplyr::tbl(mydb, "data_individuals") %>%
          dplyr::select(ind_num_sous_plot, id_table_liste_plots_n,
                        id_n, id_diconame_n, id_specimen) %>%
          dplyr::filter(id_table_liste_plots_n %in% ids_plots_represented) %>%
          dplyr::collect()


        cli::cli_h3("Checking plot by plot if individuals already linked to selected trait")
        cli::cli_alert_info("Expected for some individuals if added traits measures are new census")

        linked_individuals_list <- vector('list', length(ids_plots_represented))
        linked_individuals_likely_dup <- vector('list', length(ids_plots_represented))
        for (j in 1:length(ids_plots_represented)) {

          ### getting all individuals of selected plot
          all_individual_selected_plot_subset <-
            all_individual_selected_plot %>%
            dplyr::filter(id_table_liste_plots_n == ids_plots_represented[j])

          new_data_renamed_subset <-
            data_trait %>%
            dplyr::filter(id_liste_plots == ids_plots_represented[j])

          ## individuals in new observations linked to data_individuals
          linked_individuals <-
            dplyr::left_join(new_data_renamed_subset,
                             all_individual_selected_plot_subset,
                             by=c("ind_num_sous_plot" = "ind_num_sous_plot"))

          ## getting individuals that have already observations traits_measures table
          individuals_already_traits <-
            dplyr::tbl(mydb, "data_traits_measures") %>%
            dplyr::filter(id_data_individuals %in% !!linked_individuals$id_n) %>%
            dplyr::collect()

          if(nrow(individuals_already_traits) > 0 &
             any(unique(data_trait$id_trait) %in%
                 unique(individuals_already_traits$traitid))) {

            cli::cli_alert_warning("Individuals of plot id {ids_plots_represented[j]} already linked to this trait - consistency should be checked")

            linked_individuals %>%
              dplyr::select(id_new_data,
                            id_trait,
                            id_table_liste_plots_n,
                            id_sub_plots,
                            ind_num_sous_plot,
                            id_n,
                            trait)

            ## traits measures linked to same individuals from same subplot and same trait
            possible_duplicates_measures <-
              individuals_already_traits %>%
              filter(
                traitid == unique(data_trait$id_trait),
                id_sub_plots %in% unique(data_trait$id_sub_plots)
              ) %>%
              dplyr::select(
                            id_data_individuals,
                            id_trait_measures,
                            traitvalue) %>%
              dplyr::rename(traitvalue_exist = traitvalue)


            linked_individuals_already_db <-
              linked_individuals %>%
              dplyr::left_join(possible_duplicates_measures,
                               by = c("id_n" = "id_data_individuals")) %>%
              filter(!is.na(id_trait_measures)) %>%
              dplyr::select(id_new_data, trait, traitvalue_exist)

            linked_individuals_likely_dup[[j]] <-
              linked_individuals_already_db

            # linked_problems_individuals_list[[j]] <-
            #   individuals_already_traits

            # if(any(colnames(new_data_renamed)=="id_diconame_n") &
            #    any(traits_field=="id_diconame_n"))

            ### Which individuals show different information between what is provided and what is in the db
            # problems_individuals <-
            #   linked_individuals %>%
            #   dplyr::filter(id_diconame!=id_diconame_n | dbh.x!=dbh.y)
            #
            # if(nrow(problems_individuals)>0) {
            #   cat(paste(nrow(problems_individuals), "individuals with problematic matches\n",
            #             nrow(linked_individuals), "individuals in total"))
            #
            #   selected_tax <-
            #     dplyr::tbl(mydb,"diconame") %>%
            #     dplyr::filter(id_n %in% problems_individuals$id_diconame_n) %>%
            #     dplyr::collect()
            #
            #   problems_individuals <-
            #     problems_individuals %>%
            #     dplyr::left_join(selected_tax %>%
            #                        dplyr::select(id_n, full_name_no_auth),
            #                      by=c("id_diconame_n"="id_n"))
            #
            #   for (j in 1:nrow(problems_individuals)) {
            #     problems_individuals_selected <-
            #       problems_individuals %>%
            #       dplyr::slice(j)
            #
            #     print(problems_individuals_selected %>%
            #             dplyr::select(plot_name,
            #                           ind_num_sous_plot, dbh.x, dbh.y,
            #                           corrected.name, full_name_no_auth))
            #     response <-
            #       askYesNo("Do you want to still link these individuals?")
            #
            #     if(!response) {
            #       linked_individuals <-
            #         linked_individuals %>%
            #         dplyr::filter(!id_new_data %in% problems_individuals_selected$id_new_data)
            #
            #

            #     }
            #   }
            # }
          }

          linked_individuals_list[[j]] <-
            linked_individuals %>%
            dplyr::select(id_new_data, id_n, id_specimen)

        }

        linked_individuals_list <-
          dplyr::bind_rows(linked_individuals_list)

        linked_individuals_likely_dup <-
          dplyr::bind_rows(linked_individuals_likely_dup)

        if(nrow(linked_individuals_likely_dup) > 0) {

          cli::cli_alert_info("Found {nrow(linked_individuals_likely_dup)} measures likely already in db")

          remove_dup <- askYesNo(msg = "Remove these measures?")
          if(remove_dup)
            data_trait <-
            data_trait %>%
            filter(!id_new_data %in% linked_individuals_likely_dup$id_new_data)

        }



        # linked_problems_individuals_list <-
        #   dplyr::bind_rows(linked_problems_individuals_list)

        ## Adding link to individuals in plots
        data_trait <-
          data_trait %>%
          dplyr::left_join(linked_individuals_list)

        if (!any(colnames(data_trait) == "id_data_individuals")) {

          data_trait <-
            data_trait %>%
            dplyr::rename(id_data_individuals = id_n)

        } else {

          data_trait <-
            data_trait %>%
            dplyr::mutate(id_data_individuals = id_n)

        }

        not_linked_ind <-
          data_trait %>%
          dplyr::filter(is.na(id_data_individuals))

        if (nrow(not_linked_ind) > 0) {
          message("Measures not linked to individuals")
          print(paste(nrow(not_linked_ind), "measures"))
          print(not_linked_ind %>%
                  as.data.frame())
          remove_not_link <-
            utils::askYesNo(msg = "Remove these measures ?")

          unlinked_individuals <-
            not_linked_ind

          if (remove_not_link)
            data_trait <-
            data_trait %>%
            dplyr::filter(!is.na(id_data_individuals))


        }else{
          unlinked_individuals <- NA
        }

        ## identify duplicated individuals i.e. observations linked to same individual
        ids_dup <-
          data_trait %>%
          dplyr::group_by(id_data_individuals) %>%
          dplyr::count() %>%
          dplyr::filter(n > 1, !is.na(id_data_individuals))

        if (nrow(ids_dup) > 0) {
          cli::cli_alert_warning("More than one observation of selected trait for a given individual - {nrow(ids_dup)} individuals concerned - adding issue")

          obs_dup <-
            data_trait %>%
            dplyr::filter(id_data_individuals %in% dplyr::pull(ids_dup, id_data_individuals)) %>%
            dplyr::select(trait, plot_name, ind_num_sous_plot, id_data_individuals, id_new_data)

          issue_2 <- vector(mode = "character", length = nrow(data_trait))
          for (k in 1:nrow(ids_dup)) {
            obs_dup_sel <- obs_dup %>%
              dplyr::filter(id_data_individuals %in% ids_dup$id_data_individuals[k])
            if(length(unique(obs_dup_sel$trait))>1) {
              issue_2[data_trait$id_new_data %in% obs_dup_sel$id_new_data] <-
                rep("more than one observation for a single individual carrying different value", nrow(obs_dup_sel))
            }else{
              issue_2[data_trait$id_new_data %in% obs_dup_sel$id_new_data] <-
                rep("more than one observation for a single individual carrying identical value", nrow(obs_dup_sel))
            }
          }

          issue_2[issue_2 == ""] <- NA

          ## merging issue
          data_trait <-
            data_trait %>%
            tibble::add_column(issue_2 = issue_2) %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue), ifelse(is.na(issue_2), "", issue_2), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue ==", ", NA, issue)) %>%
            dplyr::select(-issue_2)
        }
      } # end Linking individuals

      ## adding id_diconame_n ONLY if no individuals or specimen linked
      # otherwise, identification retrieved from individual or specimen
      if (!any(colnames(data_trait) == "id_diconame")) {
        data_no_specimen_no_individual <-
          data_trait

        if (any(colnames(data_trait) == "id_data_individuals")) {
          data_no_specimen_no_individual <-
            data_no_specimen_no_individual %>%
            dplyr::filter(is.na(id_data_individuals))
        }

        if (any(colnames(data_trait) == "id_specimen")) {
          data_no_specimen_no_individual <-
            data_no_specimen_no_individual %>%
            dplyr::filter(is.na(id_specimen))
        }

        data_trait <-
          data_trait %>%
          tibble::add_column(id_diconame = NA) %>%
          dplyr::mutate(id_diconame = as.integer(id_diconame))

      } else {
        data_no_specimen_no_individual <-
          data_trait %>%
          dplyr::filter(is.na(id_data_individuals) & is.na(id_specimen) & is.na(id_diconame))

        ids_ind <- data_trait$id_data_individuals

        ## retrieving taxonomic information for linked individuals
        founded_ind <-
          query_plots(extract_individuals = T, id_individual = ids_ind, remove_ids = FALSE)

        ids_diconames <- data_trait$id_diconame

        data_trait_compa_taxo <-
          data_trait %>%
          dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                      dplyr::filter(id_n %in% ids_diconames) %>%
                      dplyr::select(tax_fam, tax_gen, full_name_no_auth, id_n) %>%
                      dplyr::collect(),
                    by=c("id_diconame"="id_n"))

        data_trait_compa_taxo <-
          data_trait_compa_taxo %>%
          dplyr::left_join(founded_ind %>%
                      dplyr::select(id_n, tax_fam, tax_gen, full_name_no_auth) %>%
                      dplyr::rename(tax_fam_linked = tax_fam, tax_gen_linked = tax_gen, full_name_no_auth_linked = full_name_no_auth),
                    by=c("id_data_individuals"="id_n")) %>%
          dplyr::select(id_new_data, tax_fam, tax_fam_linked, tax_gen,
                        tax_gen_linked, full_name_no_auth, full_name_no_auth_linked)

        diff_fam <-
          data_trait_compa_taxo %>%
          dplyr::filter(tax_fam != tax_fam_linked)
        if (nrow(diff_fam) > 0) {
          message("Some measures linked to individuals carry different family")
          print(diff_fam)
          diff_fam <-
            diff_fam %>%
            dplyr::mutate(
              issue = paste(
                "ident. when measured and in DB)",
                full_name_no_auth,
                full_name_no_auth_linked
              )
            )
          ## merging issue
          data_trait <-
            data_trait %>%
            dplyr::left_join(
              diff_fam %>%
                dplyr::select(id_new_data, issue) %>%
                dplyr::rename(issue_tax = issue),
              by = c("id_new_data" = "id_new_data")
            )

          data_trait <-
            data_trait %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue),
                                        ifelse(is.na(issue_tax), "", issue_tax), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue == ", ", NA, issue)) %>%
            dplyr::select(-issue_tax)
        }

        diff_gen <-
          data_trait_compa_taxo %>%
          dplyr::filter(tax_gen != tax_gen_linked, !id_new_data %in% diff_fam$id_new_data)

        if(nrow(diff_gen)>0) {
          message("Some measures linked to individuals carry different genus")
          print(diff_gen)
          diff_gen <-
            diff_gen %>%
            dplyr::mutate(issue = paste("ident. when measured and in DB)",
                                 full_name_no_auth, full_name_no_auth_linked))

          ## merging issue
          data_trait <-
            data_trait %>%
            dplyr::left_join(diff_gen %>%
                        dplyr::select(id_new_data, issue) %>%
                        dplyr::rename(issue_tax = issue),
                      by=c("id_new_data"="id_new_data"))

          data_trait <-
            data_trait %>%
            dplyr::mutate(issue = paste(ifelse(is.na(issue), "", issue),
                                        ifelse(is.na(issue_tax), "", issue_tax), sep = ", ")) %>%
            dplyr::mutate(issue = ifelse(issue ==", ", NA, issue)) %>%
            dplyr::select(-issue_tax)
        }

      }

      no_linked_measures <- FALSE
      if (nrow(data_no_specimen_no_individual) > 0) {
        print(data_no_specimen_no_individual)
        cli::cli_alert_danger(
          "no taxa identification, no link to specimen, no link to individuals for measures/observations"
        )
        no_linked_measures <- TRUE
      }

      ### choosing kind of measures
      cli::cli_h3("basis")
      if (!any(colnames(data_trait) == "basisofrecord")) {
        choices <-
          dplyr::tibble(
            basis =
              c(
                'LivingSpecimen',
                'PreservedSpecimen',
                'FossilSpecimen',
                'literatureData',
                'traitDatabase',
                'expertKnowledge'
              )
          )

        print(choices)
        selected_basisofrecord <-
          readline(prompt = "Choose basisofrecord : ")

        data_trait <-
          data_trait %>%
          tibble::add_column(basisofrecord = rep(choices$basis[as.numeric(selected_basisofrecord)], nrow(.)))
      }


      ### comparing measures from previous census
      if(multiple_census) {
        message("\n comparing measures from previous censuses")

        comparisons <-
          data_trait %>%
          dplyr::select(id_data_individuals, trait) %>%
          dplyr::left_join(dplyr::tbl(mydb, "data_traits_measures") %>%
                             dplyr::filter(traitid == !!unique(data_trait$id_trait)) %>%
                             dplyr::select(id_data_individuals, traitvalue) %>%
                             dplyr::collect(),
                           by=c("id_data_individuals"="id_data_individuals")) %>%
          dplyr::group_by(id_data_individuals) %>%
          dplyr::summarise(traitvalue = max(traitvalue, na.rm = TRUE),
                           trait = dplyr::first(trait)) %>%
          dplyr::mutate(traitvalue = replace(traitvalue, traitvalue == -Inf, NA))

        ## comparison with previous census if new values is lower than previous --> issue annotated
        if (any(!is.na(comparisons$traitvalue)) &
            valuetype$valuetype == "numeric") {
          # message("\n multiple data")
          finding_incoherent_values <-
            comparisons %>%
            dplyr::mutate(diff = trait - traitvalue) %>%
            dplyr::filter(diff < 0)

          if(any( finding_incoherent_values$diff < 0)) {
            message("\n incoherent new values compared to previous censuses")
            finding_incoherent_values <-
              finding_incoherent_values %>%
              dplyr::mutate(issue_new =
                              ifelse(diff < 0, "value lower than previous census", NA))

            ### merging issues
            data_trait <-
              data_trait %>%
              dplyr::left_join(finding_incoherent_values %>%
                                 dplyr::select(id_data_individuals, issue_new),  by = c("id_data_individuals"="id_data_individuals")) %>%
              dplyr::mutate(issue = ifelse(!is.na(issue), paste(issue, issue_new, sep="|"), issue_new)) %>%
              dplyr::select(-issue_new)

          }
        }
      }





      ### identify if measures are already within DB
      message("\n Identifying if imported values are already in DB")
      trait_id <- unique(data_trait$id_trait)
      selected_data_traits <-
        data_trait %>%
        dplyr::select(id_data_individuals,
                      id_trait,
                      id_liste_plots,
                      id_sub_plots,
                      trait,
                      issue)

      all_vals <-
        dplyr::tbl(mydb, "data_traits_measures") %>%
        dplyr::select(id_data_individuals, traitid, id_table_liste_plots, id_sub_plots,
                      traitvalue, traitvalue_char, issue) %>%
        dplyr::filter(traitid == trait_id) %>% #, !is.na(id_sub_plots)
        dplyr::collect()

      if (valuetype$valuetype == "numeric")
        all_vals <-
        all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue) %>%
        dplyr::select(-traitvalue_char)

      if (valuetype$valuetype == "character")
        all_vals <- all_vals %>%
        dplyr::rename(id_trait = traitid,
                      id_liste_plots = id_table_liste_plots,
                      trait = traitvalue_char) %>%
        dplyr::select(-traitvalue) %>%
        dplyr::mutate(trait = stringr::str_trim(trait))

      # all_vals %>%
      #   dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots, issue) %>%
      #   dplyr::count() %>%
      #   dplyr::filter(n>1)

      # duplicated_rows_same_values <-
      #   bind_rows(selected_data_traits,
      #             all_vals) %>%
      #   group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots, issue, trait) %>%
      #   count() %>%
      #   filter(n>1)


      duplicated_rows <-
        dplyr::bind_rows(selected_data_traits,
                         all_vals) %>%
        dplyr::filter(is.na(issue)) %>%
        dplyr::group_by(id_data_individuals,
                        id_trait,
                        id_liste_plots,
                        id_sub_plots,
                        issue) %>%
        dplyr::count() %>%
        dplyr::filter(n > 1)
      # %>% # , id_data_individuals==73764
      # dplyr::filter(!grepl("more than one observation", issue))

      duplicated_rows_with_issue_no_double <-
        dplyr::bind_rows(selected_data_traits,
                         all_vals) %>%
        dplyr::filter(!is.na(issue),!grepl("more than one observation", issue)) %>%
        dplyr::select(-issue) %>%
        dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots) %>%
        dplyr::count() %>%
        dplyr::filter(n > 1)

      duplicated_rows_with_issue_double <-
        dplyr::bind_rows(selected_data_traits,
                         all_vals) %>%
        dplyr::filter(!is.na(issue), grepl("more than one observation", issue)) %>%
        dplyr::select(-issue) %>%
        dplyr::group_by(id_data_individuals, id_trait, id_liste_plots, id_sub_plots) %>%
        dplyr::count() %>%
        dplyr::filter(n > 2)

      # %>% #
      #   dplyr::filter(!grepl("more than one observation", issue))
      duplicated_rows <-
        dplyr::bind_rows(duplicated_rows, duplicated_rows_with_issue_no_double,
                  duplicated_rows_with_issue_double)

      # duplicated_rows %>%
      #   filter(!id_data_individuals %in% selected_data_traits$id_data_individuals)

      if (nrow(duplicated_rows) > 1) {
        message("\n Some values are already in DB")
        print(duplicated_rows %>%
                dplyr::ungroup() %>%
                dplyr::select(id_data_individuals, id_liste_plots, id_sub_plots))

        # duplicated_rows %>%
        #   ungroup() %>%
        #   dplyr::select(id_data_individuals, id_liste_plots, id_sub_plots) %>%
        #   left_join(tbl(mydb, "data_liste_plots") %>%
        #               select(id_liste_plots, plot_name) %>%
        #               collect(), by=c("id_liste_plots"="id_liste_plots")) %>%
        #   distinct(plot_name, id_sub_plots)

        message(paste("\n Excluding", nrow(duplicated_rows), "values because already in DB"))
        data_trait <-
          data_trait %>%
          dplyr::filter(!id_data_individuals %in% duplicated_rows$id_data_individuals)

        if(nrow(data_trait)<1) stop("no new values anymore to import after excluding duplicates")
      }

      # data_trait <-
      #   data_trait %>%
      #   dplyr::rename_at("trait", ~ "trait_value")
      #
      # data_trait <-
      #   data_trait %>%
      #   dplyr::rename_at("id_n", ~ "id_data_individuals")

      cli::cli_h3(".add_modif_field")
      data_trait <-
        .add_modif_field(dataset = data_trait)

      cli::cli_h3("data_to_add")
      data_to_add <-
        dplyr::tibble(
          id_table_liste_plots = data_trait$id_liste_plots,
          id_data_individuals = data_trait$id_data_individuals,
          id_specimen = data_trait$id_specimen,
          id_diconame = data_trait$id_diconame,
          id_colnam = data_trait$id_colnam,
          id_sub_plots = data_trait$id_sub_plots,
          country = data_trait$country,
          decimallatitude = data_trait$decimallatitude,
          decimallongitude = data_trait$decimallongitude,
          elevation = ifelse(rep(
            any(colnames(data_trait) == "elevation"), nrow(data_trait)
          ), data_trait$elevation, NA),
          verbatimlocality = ifelse(rep(
            any(colnames(data_trait) == "verbatimlocality"), nrow(data_trait)
          ), data_trait$verbatimlocality, NA),
          basisofrecord = data_trait$basisofrecord,
          references = ifelse(rep(
            any(colnames(data_trait) == "references"), nrow(data_trait)
          ), data_trait$references, NA),
          year = ifelse(rep(
            any(colnames(data_trait) == "year"), nrow(data_trait)
          ), data_trait$year, NA),
          month = ifelse(rep(
            any(colnames(data_trait) == "month"), nrow(data_trait)
          ), data_trait$month, NA),
          day = ifelse(rep(any(
            colnames(data_trait) == "day"
          ), nrow(data_trait)), data_trait$day, NA),
          measurementremarks = ifelse(rep(
            any(colnames(data_trait) == "measurementremarks"),
            nrow(data_trait)
          ), data_trait$measurementremarks, NA),
          measurementmethod = ifelse(rep(
            any(colnames(data_trait) == "measurementmethod"), nrow(data_trait)
          ), data_trait$measurementmethod, NA),
          traitid = data_trait$id_trait,
          traitvalue = ifelse(
            rep(valuetype$valuetype == "numeric", nrow(data_trait)),
            data_trait$trait,
            NA
          ),
          traitvalue_char = ifelse(
            rep(valuetype$valuetype == "character", nrow(data_trait)),
            data_trait$trait,
            NA
          ),
          original_tax_name = ifelse(rep(
            any(colnames(data_trait) == "original_tax_name"), nrow(data_trait)
          ), data_trait$original_tax_name, NA),
          original_plot_name = ifelse(rep(
            any(colnames(data_trait) == "original_plot_name"), nrow(data_trait)
          ), data_trait$original_plot_name, NA),
          original_specimen = ifelse(rep(
            any(colnames(data_trait) == "original_specimen"), nrow(data_trait)
          ), data_trait$original_specimen, NA),
          issue = data_trait$issue,
          date_modif_d = data_trait$date_modif_d,
          date_modif_m = data_trait$date_modif_m,
          date_modif_y = data_trait$date_modif_y
        )

      if(no_linked_measures)
        list_add_data[[i]] <-
        data_no_specimen_no_individual

      list_add_data[[i]] <-
        data_to_add

      print(data_to_add)

      # print(data_to_add %>%
      #         dplyr::left_join(tbl(mydb, "data_liste_sub_plots") %>%
      #                            select(typevalue, id_type_sub_plot, id_sub_plots) %>%
      #                            collect(), by=c("id_sub_plots"="id_sub_plots"))) %>%
      #   dplyr::left_join(tbl(mydb, "subplotype_list") %>%
      #                      select(id_subplotype, type ) %>%
      #                      collect(), by=c("id_type_sub_plot"="id_subplotype")) %>%
      #   View()

      if (data_to_add %>% dplyr::distinct() %>% nrow() != nrow(data_to_add)) {

        duplicates_lg <- duplicated(data_to_add)

        cli::cli_alert_warning("Duplicates in new data for {trait} concerning {length(duplicates_lg[duplicates_lg])} id(s)")

        cf_merge <-
          askYesNo(msg = "confirm merging duplicates?")

        if (cf_merge) {

          issues_dup <- data_to_add %>%
            filter(id_data_individuals %in% data_to_add[duplicates_lg, "id_data_individuals"]) %>%
            dplyr::select(issue, id_data_individuals)

          ## resetting issue
          if(any(grepl("identical value", issues_dup$issue))) {

            issues_dup_modif_issue <-
              issues_dup[grepl("identical value", issues_dup$issue),]

            data_to_add <-
              data_to_add %>%
              mutate(issue = replace(issue, id_data_individuals %in% issues_dup_modif_issue$id_data_individuals, NA))

          }

          data_to_add <- data_to_add %>% dplyr::distinct()
        } else{
          stop()
        }

      }

      response <-
        utils::askYesNo("Confirm add these data to data_traits_measures table?")

      if(add_data & response) {
        cli::cli_alert_success("Adding data : {nrow(data_to_add)} values added")
        DBI::dbWriteTable(mydb, "data_traits_measures",
                          data_to_add, append = TRUE, row.names = FALSE)
      }

    }else{

      cli::cli_alert_info("no added data for {trait} - no values different of 0")

    }
  }

  # linked_problems_individuals_list <-
  #   linked_problems_individuals_list %>%
  #   dplyr::select(plot_name,
  #                 ind_num_sous_plot,
  #                 country,
  #                 leaf_area,
  #                 specific_leaf_area,
  #                 dbh.x,
  #                 dbh.y,
  #                 original_tax_name,
  #                 corrected.name,
  #                 full_name_no_auth,
  #                 id_table_liste_plots_n,
  #                 ddlon,
  #                 ddlat) %>%
  #   left_join(tbl(mydb, "data_liste_plots") %>%
  #               dplyr::select(plot_name, id_liste_plots) %>%
  #               collect(), by=c("id_table_liste_plots_n"="id_liste_plots")) %>%
  #   rename(dbh_provided = dbh.x,
  #          dbh_database = dbh.y,
  #          name_provided = original_tax_name,
  #          name_provided_corrected = corrected.name,
  #          name_database = full_name_no_auth,
  #          plot_name_provided = plot_name.x,
  #          plot_name_corrected = plot_name.y)


  if(exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data, unlinked_individuals = unlinked_individuals))

  if(!exists('unlinked_individuals'))
    return(list(list_traits_add = list_add_data))

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
#' @param plot_name_field integer indicate which name of col_names_select is the id for matching liste plots table
#' @param collector_field integer indicate which name of col_names_select is the id for matching collector
#'
#' @param launch_adding_data logical FALSE whether adding should be done or not
#'
#' @return No return value individuals updated
#' @export
add_specimens <- function(new_data ,
                          col_names_select,
                          col_names_corresp,
                          # id_col,
                          plot_name_field = NULL,
                          collector_field = NULL,
                          launch_adding_data = FALSE) {

  logs <-
    dplyr::tibble(
      column = as.character(),
      note = as.character()
    )

  if(!exists("mydb")) call.mydb()

  if(length(col_names_select)!=length(col_names_corresp))
    stop("Provide same numbers of corresponding and selected colnames")

  new_data_renamed <-
    new_data %>%
    tibble::add_column(id_new_data=1:nrow(.))

  for (i in 1:length(col_names_select)) {
    if (any(colnames(new_data_renamed) == col_names_select[i])) {
      new_data_renamed <-
        new_data_renamed %>%
        dplyr::rename_at(dplyr::vars(col_names_select[i]), ~ col_names_corresp[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_names_select[i]
      ))
    }
  }

  col_names_corresp <-
    c(col_names_corresp, "id_new_data")

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  ### check diconame id
  if(!any(colnames(new_data_renamed)=="idtax_n")) stop("idtax_n column missing")

  if (any(new_data_renamed$idtax_n == 0))
    stop(paste(
      "idtax_n is NULL for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  if (any(is.na(new_data_renamed$idtax_n)))
    stop(paste(
      "idtax_n is missing for",
      sum(new_data_renamed$idtax_n == 0),
      "individuals"
    ))

  unmatch_id_diconame <-
    new_data_renamed %>%
    dplyr::select(idtax_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "table_taxa") %>%
                       dplyr::select(idtax_n, idtax_good_n) %>%
                       dplyr::collect() %>%
                       dplyr::mutate(tag = 1), by=c("idtax_n" = "idtax_n")) %>%
    dplyr::filter(is.na(tag)) %>%
    dplyr::pull(idtax_n)

  if (length(unmatch_id_diconame) > 0)
    stop(paste("idtax_n not found in table_taxa", unmatch_id_diconame))


  ### check locality and adding it if link to plots
  if(!any(colnames(new_data_renamed) == "locality"))
    warning("locality column missing"
    )

  ### Linking plot names
  if (!is.null(plot_name_field) &
      !any(colnames(new_data_renamed) == "locality")) {
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
      dplyr::left_join(all_plot_names)

    all_plot_name_new_dataset_no_match <-
      all_plot_name_new_dataset %>%
      dplyr::filter(is.na(id_liste_plots))

    id_plotname <-
      vector(mode = "integer", nrow(new_data_renamed))
    for (i in 1:nrow(all_plot_name_new_dataset_no_match)) {
      print(all_plot_name_new_dataset$plot_name[i])
      sorted_matches <-
        .find_similar_string(
          input = all_plot_name_new_dataset$plot_name[i],
          compared_table = all_plot_names,
          column_name = "plot_name"
        )
      print(sorted_matches)
      selected_name <-
        readline(prompt = "Choose which name fit (if none enter 0): ")

      selected_name <- as.integer(selected_name)

      if (is.na(selected_name))
        stop("Provide integer value for standardizing plot name")

      if (selected_name == 0) {
        print(paste(all_plot_name_new_dataset$plot_name[i], " not found"))
      }

      if (selected_name > 0) {
        selected_name_id <-
          sorted_matches %>%
          slice(selected_name) %>%
          dplyr::select(id_liste_plots) %>%
          dplyr::pull()

        id_plotname[new_data_renamed$plot_name == all_plot_name_new_dataset_no_match$plot_name[i]] <-
          selected_name_id
      }
    }

    new_data_renamed <-
      new_data_renamed %>%
      tibble::add_column(id_liste_plots = id_plotname)

    if (new_data_renamed %>%
        dplyr::filter(is.na(id_liste_plots),!is.na(plot_name)) %>%
        nrow() > 0) {
      print("Plot name not found !!")

    }

    ### importing plots information about specimens
    new_data_renamed <-
      new_data_renamed %>%
      dplyr::left_join(
        dplyr::tbl(mydb, "data_liste_plots") %>%
          dplyr::select(
            id_liste_plots,
            country,
            ddlat,
            ddlon,
            elevation,
            locality_name,
            province
          ) %>%
          dplyr::collect(),
        by = c("id_liste_plots" = "id_liste_plots")
      )

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::select(-id_liste_plots,-plot_name) %>%
      dplyr::rename(locality = locality_name,
                    majorarea = province)

  }

  ### Linking collectors names
  if (!is.null(collector_field)) {
    # data_stand = new_data_renamed
    # collector_field = 1
    new_data_renamed <-
      .link_colnam(data_stand = new_data_renamed,
                   collector_field = collector_field)
  } else{
    if (!any(colnames(new_data_renamed) == "id_colnam"))
      stop("indicate the field if of collector name for standardizing")
  }

  ### check determination data
  if (any(colnames(new_data_renamed) == "detd")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detd = as.numeric(detd))
  }

  if(any(colnames(new_data_renamed) == "detm")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(detm = as.numeric(detm))
  }

  if (any(colnames(new_data_renamed) == "dety")) {
    new_data_renamed <-
      new_data_renamed %>%
      mutate(dety = as.numeric(dety))
  }

  if (!any(names(new_data_renamed) == "suffix")) {

    new_data_renamed <-
      new_data_renamed %>%
      dplyr::mutate(suffix = NA) %>%
      dplyr::mutate(suffix = as.character(suffix))

  }

  ## check if not duplicates in new specimens
  dup_imported_datasets <-
    new_data_renamed %>%
    dplyr::select(colnbr, id_colnam, suffix, id_new_data) %>%
    group_by(colnbr, id_colnam, suffix) %>%
    count() %>%
    filter(n > 1)
    # dplyr::mutate(combined = paste(colnbr, id_colnam, sep = "-")) %>%
    # dplyr::group_by(combined) %>%
    # dplyr::count() %>%
    # dplyr::filter(n > 1)

  if (nrow(dup_imported_datasets) > 0) {
    print(dup_imported_datasets)
    stop("Duplicates in imported dataset")
  }

  # link to table individuals
  # unmatched_specimens_from_individuals <-
  #   .query_unmatched_specimens()
  #
  # all_herbarium_individuals_not_linked <-
  #   unmatched_specimens_from_individuals$all_herbarium_individuals_not_linked

  ## checking for duplicates in unmatched specimens in data individuals
  # dup_stand_specimens <-
  #   all_herbarium_individuals_not_linked %>%
  #   dplyr::group_by(colnbr, id_colnam) %>%
  #   dplyr::count() %>%
  #   dplyr::filter(n>1)
  #
  # if(nrow(dup_stand_specimens)) {
  #   print(dup_stand_specimens)
  #   warning("duplicates in specimens not linked in individuals table and carrying different identification")
  # }

  ### comparing id diconames from individuals and specimens
  # linked_new_specimens_to_table_individuals <-
  #   new_data_renamed %>%
  #   # dplyr::select(colnbr, id_colnam, id_new_data, id_diconame_n) %>%
  #   dplyr::mutate(colnbr = as.numeric(colnbr)) %>%
  #   dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
  #   dplyr::select(combined, id_new_data, id_diconame_n) %>%
  #   dplyr::left_join(all_herbarium_individuals_not_linked %>%
  #                      dplyr::select(colnbr, id_colnam, id_diconame_n) %>%
  #                      dplyr::mutate(colnbr = as.numeric(colnbr)) %>%
  #                      dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")),
  #                    by=c("combined"="combined"))
  #
  # cat(paste("Matchs from table individuals found for", nrow(linked_new_specimens_to_table_individuals), "specimens", "\n"))
  #
  # ## checking if taxo is different between table individuals and new specimens
  # ## only select incongruent genus
  # unmatch_taxo <-
  #   linked_new_specimens_to_table_individuals %>%
  #   dplyr::filter(id_diconame_n.x != id_diconame_n.y) %>%
  #   dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
  #                      dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
  #                      dplyr::collect(),
  #                    by=c("id_diconame_n.x"="id_n")) %>%
  #   dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
  #                      dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
  #                      dplyr::collect(),
  #                    by=c("id_diconame_n.y"="id_n")) %>%
  #   dplyr::filter(tax_gen.x!=tax_gen.y)
  #
  # if(nrow(unmatch_taxo)>0) {
  #   warning("Different genus between new specimens and corresponding matched individuals")
  #   print(unmatch_taxo)
  # }

  ## check if specimens are not already in database
  matched_specimens <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::select(colnbr, id_colnam, id_specimen) %>%
    dplyr::filter(!is.na(id_colnam)) %>%
    dplyr::collect() %>%
    dplyr::left_join(
      new_data_renamed %>%
        dplyr::select(colnbr, id_colnam, id_new_data),
      by = c("colnbr" = "colnbr", "id_colnam" = "id_colnam")
    ) %>%
    dplyr::filter(!is.na(id_new_data))

    # dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    # dplyr::left_join(new_data_renamed %>%
    #                    dplyr::select(colnbr, id_colnam, id_new_data) %>%
    #                    dplyr::mutate(combined = paste(colnbr, id_colnam, sep="-")) %>%
    #                    dplyr::select(combined, id_new_data)) %>%
    # dplyr::filter(!is.na(id_new_data))

  if (nrow(matched_specimens) > 0) {
    warning(paste("New specimens already in database", nrow(matched_specimens)))
    print(matched_specimens)
  }

  new_data_renamed <-
    new_data_renamed %>%
    tibble::add_column(data_modif_d=lubridate::day(Sys.Date()),
                       data_modif_m=lubridate::month(Sys.Date()),
                       data_modif_y=lubridate::year(Sys.Date()))

  if (any(colnames(new_data_renamed) == "col_name"))
    new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-col_name)

  new_data_renamed <-
    new_data_renamed %>%
    dplyr::select(-id_new_data)

  if(launch_adding_data) {

    print(list(new_data_renamed, logs))

    confirmed <- utils::askYesNo("Confirm adding?")

    if(confirmed) {
      message(paste0(nrow(new_data_renamed), " records added to specimens table"))
      DBI::dbWriteTable(mydb, "specimens", new_data_renamed, append = TRUE, row.names = FALSE)
    }

  }

  return(list(new_data_renamed, logs))

}


#' Internal function
#'
#' Get for each trait, a tibble of individuals with measures or observations, deal with several observations
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param traits string vector with trait needed
#' @param id_individuals numeric vector with id_n individuals requested
#' @param ids_plot numeric vector with id_plots requested
#' @param skip_dates logical whether include day, month and year of observations
#' @param show_multiple_measures logical whether multiple measures (i.e. census or sometimes more than one value for given measure)
#' @param collapse_multiple_val logical whether multiple traits measures should be collapsed (resulting values as character, separated by dash)
#'
#'
#' @export
.get_trait_individuals_values <- function(traits,
                                          src_individuals = NULL,
                                          id_individuals = NULL,
                                          ids_plot = NULL,
                                          skip_dates = TRUE,
                                          show_multiple_measures = FALSE,
                                          collapse_multiple_val = FALSE) {

  ## merging traits measures and trait informations
  traits_measures <-
    dplyr::tbl(mydb, "data_traits_measures") %>%
    dplyr::left_join(dplyr::tbl(mydb, "traitlist"), by = c("traitid" = "id_trait")) %>%
    dplyr::select(-id_specimen,-id_diconame)

  if (is.null(src_individuals)) {

    res_individuals_full <-
      merge_individuals_taxa(id_individual = id_individuals, id_plot = ids_plot)

  } else {

    res_individuals_full <- src_individuals

  }

  # ## filtering individuals based on ids of individuals and/or plots
  # if (is.null(id_individuals) & is.null(ids_plot))
  #   res_individuals_full <-
  #     dplyr::tbl(mydb, "data_individuals")
  #
  # if (!is.null(id_individuals) & is.null(ids_plot))
  #   res_individuals_full <-
  #     dplyr::tbl(mydb, "data_individuals") %>%
  #     dplyr::filter(id_n %in% id_individuals)
  #
  # if (is.null(id_individuals) & !is.null(ids_plot))
  #   res_individuals_full <-
  #     dplyr::tbl(mydb, "data_individuals") %>%
  #     dplyr::filter(id_table_liste_plots_n %in% ids_plot)
  #
  # ## getting specimens id and identification
  # specimens_id_diconame <-
  #   dplyr::tbl(mydb, "specimens") %>%
  #   dplyr::select(id_specimen, id_diconame_n)
  #
  # ## getting synonimies of identification
  # diconames_id <-
  #   dplyr::tbl(mydb, "diconame") %>%
  #   dplyr::select(id_n, id_good_n)
  #
  # ## merging specimens and good identification
  # specimens_linked <-
  #   specimens_id_diconame %>%
  #   dplyr::left_join(diconames_id, by=c("id_diconame_n"="id_n")) %>%
  #   dplyr::rename(id_dico_name_specimen=id_good_n) %>%
  #   dplyr::select(id_specimen, id_dico_name_specimen)
  #
  # ## getting link between specimen and individual
  # links_specimens <-
  #   dplyr::tbl(mydb, "data_link_specimens") %>%
  #   dplyr::select(id_n, id_specimen) %>%
  #   dplyr::group_by(id_n) %>%
  #   dplyr::summarise(id_specimen = max(id_specimen, na.rm = T))
  #
  # ## adding id of specimens to individual (and removing old link)
  # res_individuals_full <-
  #   res_individuals_full %>%
  #   dplyr::select(-id_specimen) %>%
  #   dplyr::left_join(links_specimens,
  #                    by = c("id_n" = "id_n"))
  #
  # res_individuals_full <-
  #   res_individuals_full %>%
  #   dplyr::left_join(specimens_linked, by=c("id_specimen"="id_specimen")) %>% # adding id_diconame for specimens
  #   dplyr::mutate(id_diconame_final=ifelse(!is.na(id_dico_name_specimen), id_dico_name_specimen, id_diconame_n)) %>% #### selecting id_dico_name from specimens if any
  #   dplyr::left_join(dplyr::tbl(mydb, "diconame"), by=c("id_diconame_final"="id_n")) ## getting taxa information based on id_diconame_final

  traits_linked <-
    res_individuals_full %>%
    dplyr::left_join(traits_measures, by = c("id_n" = "id_data_individuals")) %>%
    dplyr::select(id_n, id_old, id_trait_measures, id_sub_plots, traitvalue, traitvalue_char, trait, issue, day, month, year) %>%
    dplyr::filter(!is.na(trait)) %>%
    dplyr::filter(trait  %in% traits) %>%
    dplyr::collect()

  all_traits_list <- list()
  if(nrow(traits_linked) > 0) {

    all_trait <- dplyr::distinct(traits_linked, trait) %>% dplyr::pull() %>% sort()
    print(all_trait)

    for (i in 1:length(all_trait)) {

      # cat(" ",i)
      traits_linked_subset <-
        traits_linked %>%
        dplyr::filter(trait == all_trait[i])

      valuetype <-
        dplyr::tbl(mydb, "traitlist") %>%
        dplyr::filter(trait == !!all_trait[i]) %>%
        dplyr::distinct(valuetype) %>%
        dplyr::pull()

      if(valuetype == "numeric") {
        traits_linked_subset <-
          traits_linked_subset %>%
          dplyr::select(trait, traitvalue, issue, day, month, year, id_n, id_old, id_trait_measures,
                        id_sub_plots)
      }

      if(valuetype == "character") {
        traits_linked_subset <-
          traits_linked_subset %>%
          dplyr::select(trait, traitvalue_char, issue, day, month, year, id_n, id_old, id_trait_measures,
                        id_sub_plots) %>%
          dplyr::mutate(traitvalue = traitvalue_char) %>%
          dplyr::select(-traitvalue_char)
      }

      issue_name <- paste0(all_trait[i], "_issue")
      issue_name_enquo <-
        rlang::parse_expr(rlang::quo_name(rlang::enquo(issue_name)))
      trait_name <- all_trait[i]
      trait_name_enquo <-
        rlang::parse_expr(rlang::quo_name(rlang::enquo(trait_name)))

      # print(issue_name)

      traits_linked_subset <-
        traits_linked_subset %>%
        dplyr::rename(!!issue_name := issue)

      nbe_individuals_double <-
        traits_linked_subset %>%
        dplyr::group_by(id_n) %>%
        dplyr::count() %>%
        dplyr::filter(n > 1) %>%
        # dplyr::collect() %>%
        nrow()

      multiple_census <- FALSE
      if(nbe_individuals_double > 0) {

        warning(paste("more than one trait value for at least one individual for", all_trait[i]))

        if(!show_multiple_measures) {

          traits_linked_subset <-
            traits_linked_subset %>%
            dplyr::collect() %>%
            dplyr::group_by(id_n) %>%
            dplyr::summarise(traitvalue = dplyr::last(traitvalue),
                             trait = dplyr::last(trait),
                             !!issue_name:= dplyr::last(!!issue_name_enquo),
                             day = dplyr::last(day),
                             month = dplyr::last(month),
                             year= dplyr::last(year),
                             id_old= dplyr::last(id_old),
                             id_trait_measures = dplyr::last(id_trait_measures),
                             id_sub_plots = dplyr::last(id_sub_plots)) %>%
            dplyr::select(trait, traitvalue, !!issue_name_enquo, day, month, year, id_n, id_old, id_trait_measures,
                          id_sub_plots)
        }else{

          ids_subs <-
            traits_linked_subset %>%
            dplyr::distinct(id_sub_plots) %>%
            dplyr::pull()

          subs_plots_concerned <-
            dplyr::tbl(mydb, "data_liste_sub_plots") %>%
            dplyr::filter(id_sub_plots %in% ids_subs) %>%
            dplyr::select(id_sub_plots, id_table_liste_plots, id_type_sub_plot, typevalue, month, year) %>%
            dplyr::left_join(dplyr::tbl(mydb, "subplotype_list") %>%
                               dplyr::select(type, id_subplotype),
                             by=c("id_type_sub_plot"="id_subplotype")) %>%
            dplyr::select(-id_type_sub_plot) %>%
            dplyr::collect()

          if(nrow(subs_plots_concerned)>0 & !collapse_multiple_val) {

            ## if the multiple values for individuals are for different census, putting multiple_census as TRUE
            if(all(subs_plots_concerned$type == "census") &
               max(subs_plots_concerned$typevalue) > 1) {

              for (j in 1:max(subs_plots_concerned$typevalue)) {

                cat(paste("\n", j, "census(es) for", trait_name), "for",
                    nrow(subs_plots_concerned %>%
                           dplyr::filter(type == "census", typevalue == j)), "plots")
              }

              multiple_census <- TRUE

            }
          }

          if(collapse_multiple_val) {

            traits_linked_subset <-
              traits_linked_subset %>%
              dplyr::group_by_at(dplyr::vars(-traitvalue, -id_trait_measures)) %>%
              dplyr::summarise(traitvalue = paste(traitvalue, collapse = "-"),
                               id_trait_measures = paste(id_trait_measures, collapse = "-")) %>%
              dplyr::ungroup()

            multiple_census <- FALSE
          }

          # traits_linked_subset <-
          #   traits_linked_subset %>%
          #   dplyr::collect()
        }

      }else{
        # traits_linked_subset <-
        #   traits_linked_subset %>%
        #   dplyr::collect()
      }

      traits_linked_subset_spread_list <- list()

      ## spreading measures
      if(multiple_census) {

        for (j in 1:max(subs_plots_concerned$typevalue)) {

          ids_subs_select <- subs_plots_concerned %>%
            dplyr::filter(typevalue == j) %>%
            dplyr::pull(id_sub_plots)

          if(length(ids_subs_select)>0) {

            traits_linked_subset_spread <-
              traits_linked_subset %>%
              dplyr::filter(id_sub_plots %in% ids_subs_select) %>%
              dplyr::select(-day, -month, -year) %>%
              # collect() %>%
              tidyr::spread(key = trait, value = traitvalue)

            traits_linked_subset_spread_list[[length(traits_linked_subset_spread_list)+1]] <-
              traits_linked_subset_spread

            names(traits_linked_subset_spread_list)[length(traits_linked_subset_spread_list)] <-
              paste("census", j, sep=("_"))
          }
        }
      } else{

        traits_linked_subset_spread <-
          traits_linked_subset %>%
          dplyr::select(-day, -month, -year) %>%
          tidyr::spread(key = trait, value = traitvalue)

        traits_linked_subset_spread_list[[1]] <- traits_linked_subset_spread
      }

      ## getting dates of traits measures
      for (j in 1:length(traits_linked_subset_spread_list)) {

        traits_linked_subset_spread_list[[j]] <-
          traits_linked_subset_spread_list[[j]] %>%
          dplyr::left_join(traits_linked_subset %>%
                             dplyr::select(id_trait_measures, day, month, year) %>%
                             dplyr::collect(),
                           by=c("id_trait_measures"="id_trait_measures"))
      }

      # traits_linked_subset_spread <-
      #   traits_linked_subset_spread %>%
      #   dplyr::left_join(traits_linked_subset %>%
      #                      dplyr::select(id_trait_measures, day, month, year) %>%
      #                      dplyr::collect(),
      #             by=c("id_trait_measures"="id_trait_measures"))

      if(skip_dates)
        for (j in 1:length(traits_linked_subset_spread_list))
          traits_linked_subset_spread_list[[j]] <-
        traits_linked_subset_spread_list[[j]] %>%
        dplyr::select(-day, -month, -year)

      ## renaming measures table
      for (j in 1:length(traits_linked_subset_spread_list)) {

        if(multiple_census) {

          id_new_name <- paste("id_trait_measures", all_trait[i],
                               names(traits_linked_subset_spread_list)[j], sep="_")
          traits_linked_subset_spread_list[[j]] <-
            traits_linked_subset_spread_list[[j]] %>%
            dplyr::rename(!!id_new_name := id_trait_measures)

          id_sub_plots_new_name <- paste("id_sub_plots", all_trait[i],
                                         names(traits_linked_subset_spread_list)[j], sep="_")
          traits_linked_subset_spread_list[[j]] <-
            traits_linked_subset_spread_list[[j]] %>%
            dplyr::rename(!!id_sub_plots_new_name := id_sub_plots)

          trait_name_new <- paste(all_trait[i], names(traits_linked_subset_spread_list)[j], sep="_")
          traits_linked_subset_spread_list[[j]] <-
            traits_linked_subset_spread_list[[j]] %>%
            dplyr::rename(!!trait_name_new := !!trait_name_enquo)

          issue_trait_name_new <- paste(issue_name, names(traits_linked_subset_spread_list)[j], sep="_")
          traits_linked_subset_spread_list[[j]] <-
            traits_linked_subset_spread_list[[j]] %>%
            dplyr::rename(!!issue_trait_name_new := !!issue_name_enquo)

          if(!skip_dates) {

            day_new_name <- paste("day", names(traits_linked_subset_spread_list)[j], sep="_")
            traits_linked_subset_spread_list[[j]] <-
              traits_linked_subset_spread_list[[j]] %>%
              dplyr::rename(!!day_new_name := day)
            month_new_name <- paste("month", names(traits_linked_subset_spread_list)[j], sep="_")
            traits_linked_subset_spread_list[[j]] %>%
              dplyr::rename(!!month_new_name := month)
            year_new_name <- paste("year", names(traits_linked_subset_spread_list)[j], sep="_")
            traits_linked_subset_spread_list[[j]] %>%
              dplyr::rename(!!year_new_name := year)

          }
        }else{

          id_new_name <- paste("id_trait_measures", all_trait[i], sep="_")
          traits_linked_subset_spread_list[[j]] <-
            traits_linked_subset_spread_list[[j]] %>%
            dplyr::rename(!!id_new_name := id_trait_measures)
          traits_linked_subset_spread_list[[j]] <-
            traits_linked_subset_spread_list[[j]] %>%
            dplyr::select(-id_sub_plots)

        }
      }

      # if(!all(is.na(traits_linked_subset_spread$id_sub_plots))) {
      #   ids_subs <- traits_linked_subset_spread$id_sub_plots
      #   ids_subs <- unique(ids_subs[!is.na(ids_subs)])
      #
      #   traits_linked_subset_spread <-
      #     traits_linked_subset_spread %>%
      #     dplyr::left_join(tbl(mydb, "data_liste_sub_plots") %>%
      #                        dplyr::filter(id_sub_plots %in% ids_subs) %>%
      #                        dplyr::select(id_sub_plots, id_type_sub_plot, typevalue, month, year) %>%
      #                        left_join(tbl(mydb, "subplotype_list") %>%
      #                                    dplyr::select(type, id_subplotype),
      #                                  by=c("id_type_sub_plot"="id_subplotype")) %>%
      #                        dplyr::select(-id_type_sub_plot) %>%
      #                        collect(), by=c("id_sub_plots"="id_sub_plots"))
      #
      # }


      # traits_linked_subset_spread <-
      #   traits_linked_subset_spread %>%
      #   dplyr::select(!!trait_name_enquo, !!issue_name_enquo, !!id_new_name,
      #                 id_n, id_old, id_sub_plots)

      for (j in 1:length(traits_linked_subset_spread_list))
        all_traits_list[[length(all_traits_list)+1]] <- traits_linked_subset_spread_list[[j]]

      # print(traits_linked_subset_spread)
    }


  }else(

    all_traits_list

  )

  return(all_traits_list)

}

#' Internal function
#'
#' Replace NA by 9999
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param vec vector
#'
#' @export
replace_NA <- function(vec, inv = FALSE) {

  vec_num <- names(vec)[unlist(lapply(vec, is.numeric))]
  vec_char <- names(vec)[unlist(lapply(vec, is.character))]

  if(!inv) {
    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_num
      ))),
      ~ tidyr::replace_na(. , -9999))

    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_char
      ))),
      ~ tidyr::replace_na(. , "-9999"))

  }

  if(inv) {

    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_char
      ))),
      ~ dplyr::na_if(. , "-9999"))

    vec <- vec %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(c(
        vec_num
      ))),
      ~ dplyr::na_if(. , -9999))

  }






  return(vec)

}


#' Internal function
#'
#' Compare values between of given columns and identify different values based on id matches
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble contain values to compare and id for matching
#' @param col_new string vector containing column names of dataset
#' @param id_col_nbr string vector
#' @param type_data string indicate which table of database is targetted. e.g. 'individuals'
#'
#' @export
.find_ids <- function(dataset, col_new, id_col_nbr, type_data) {

  ids_new_data <-
    dataset %>%
    dplyr::select(!!col_new[id_col_nbr]) %>%
    dplyr::pull()

  if(type_data=="individuals")
    corresponding_data <-
      dplyr::tbl(mydb, "data_individuals")

  if(type_data == "trait") {
    # all_data <-
    #   dplyr::tbl(mydb, "data_traits_measures")

    if(col_new[id_col_nbr] %in% c("id_n", "id_old")) {

      corresponding_data <-
        .get_trait_individuals_values(traits = col_new[-id_col_nbr])

      corresponding_data <- corresponding_data[[1]]
    }

    if(col_new[id_col_nbr] %in% c("id_trait_measures")) {

      new_name <- col_new[-id_col_nbr]

      corresponding_data <-
        dplyr::tbl(mydb, "data_traits_measures") %>%
        dplyr::select(id_trait_measures, traitvalue) %>%
        dplyr::collect() %>%
        rename(!!new_name := traitvalue)

    }
  }

  id <- "id"

  corresponding_data_full <-
    corresponding_data %>%
    dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id) %>%
    dplyr::filter(id %in% ids_new_data)

  corresponding_data <-
    corresponding_data %>%
    dplyr::select(dplyr::all_of(col_new)) %>%
    dplyr::rename_at(dplyr::all_of(dplyr::vars(col_new[id_col_nbr])), ~ id) %>%
    dplyr::filter(id %in% ids_new_data) %>%
    dplyr::collect()

  all_tb_update <- vector('list', length(col_new[-id_col_nbr]))
  for (i in 1:length(col_new[-id_col_nbr])) {
    cat(" ", col_new[-id_col_nbr][i])
    # var <- enquo(col_names_corresp[-id_col][i])

    var_ <- col_new[-id_col_nbr][i]

    var <- rlang::enquo(var_)

    var_new <- paste0(col_new[-id_col_nbr][i], "_new")
    var_old <- paste0(col_new[-id_col_nbr][i], "_old")
    id <- col_new[id_col_nbr]
    var_id <- rlang::enquo(id)

    quo_var <- rlang::quo_name(rlang::enquo(id))

    select_col_new <-
      dplyr::select(dataset, !!var_id, !!var) %>%
      dplyr::rename(!!var_new := !!var)

    id <- "id"
    select_col_new <-
      select_col_new %>%
      dplyr::rename_at(dplyr::vars(col_new[id_col_nbr]), ~ id)

    select_col_old <-
      dplyr::select(corresponding_data, "id", !!var) %>%
      dplyr::rename(!!var_old := !!var)

    matches <-
      dplyr::left_join(select_col_new, select_col_old, by = c("id"="id"))

    matches <- replace_NA(vec = matches)


    # matches[,2] <-
    #   replace_NA(vec = matches[,2])
    # matches[,3] <-
    #   replace_NA(vec = matches[,3])

    # matches[is.na(matches[,2]), 2] <- -9999
    # matches[is.na(matches[,3]), 3] <- -9999

    quo_var <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_new)))
    quo_var_old <- rlang::parse_expr(rlang::quo_name(rlang::enquo(var_old)))

    matches <-
      matches %>%
      dplyr::filter(!!quo_var != !!quo_var_old)

    if (nrow(matches) > 0) {

      matches[, 2] <- replace_NA(vec = matches[, 2], inv = T)
      matches[, 3] <- replace_NA(vec = matches[, 3], inv = T)

    }

    all_tb_update[[i]] <- matches
    names(all_tb_update)[i] <- col_new[-id_col_nbr][i]
  }

  return(list(corresponding_data_full, all_tb_update))
}







#' Internal function
#'
#' rename columns based on new and old columns names
#'
#' @return tibble
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset tibble
#' @param col_old string vector
#' @param col_new string vector
#'
#' @export
.rename_data <- function(dataset, col_old, col_new) {

  if (length(col_old) != length(col_new))
    stop("number of new columns names different of number of selected column names")

  for (i in 1:length(col_old)) {
    if (any(colnames(dataset) == col_old[i])) {
      dataset <-
        dataset %>%
        dplyr::rename_at(dplyr::vars(col_old[i]), ~ col_new[i])
    } else{
      stop(paste(
        "Column name provided not found in provided new dataset",
        col_old[i]
      ))
    }
  }
  return(dataset)
}

#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand string tibble
#' @param collector_field string vector
#'
#' @export
.link_colnam <- function(data_stand, collector_field) {
  col_name <- "col_name"

  data_stand <-
    data_stand %>%
    dplyr::rename_at(dplyr::vars(all_of(collector_field)), ~ col_name)

  all_names_collector <-
    dplyr::distinct(data_stand, col_name)

  id_colname <-
    vector(mode = "integer", nrow(data_stand))

  for (i in 1:nrow(all_names_collector)) {

    all_colnames <-
      dplyr::tbl(mydb, "table_colnam") %>%
      dplyr::collect()

    sorted_matches <-
      .find_cat(
        value_to_search = dplyr::pull(all_names_collector)[i],
        compared_table = all_colnames,
        column_name = "colnam",
        prompt_message = "Choose corresponding name (G for searching a pattern, 0 if none corresponding): "
      )

    if(sorted_matches$selected_name == 0) {

      add <- utils::askYesNo(msg = "Add a new name?")

      if(add) {
        new_colname <-
          readline(prompt="Provide a new collector name following same format: ")

        new_family_name <-
          readline(prompt="Provide a new family_name name following same format: ")

        new_surname <-
          readline(prompt="Provide a new surname name following same format: ")

        new_nationality <-
          readline(prompt="Provide a nationality following same format: ")

        new_rec <- tibble::tibble(
          colnam = new_colname,
          family_name = new_family_name,
          surname = new_surname,
          nationality = new_nationality
        )

        DBI::dbWriteTable(mydb, "table_colnam", new_rec, append = TRUE,
                          row.names = FALSE)

        selected_name_id <-
          dplyr::tbl(mydb, "table_colnam") %>%
          dplyr::filter(colnam == new_colname) %>%
          dplyr::select(id_table_colnam) %>%
          dplyr::collect() %>%
          dplyr::pull()

      } else {

        selected_name_id <- NA

      }

    } else {

      selected_name_id <-
        sorted_matches$sorted_matches %>%
        slice(sorted_matches$selected_name) %>%
        pull(id_table_colnam)

    }

    # if(!any(all_colnames$colnam==ifelse(!is.na(dplyr::pull(all_names_collector)[i]),
    #                                     dplyr::pull(all_names_collector)[i],
    #                                     "kk"))) {
    #   print(dplyr::pull(all_names_collector)[i])
    #   sorted_matches <-
    #     .find_similar_string(input = dplyr::pull(all_names_collector)[i],
    #                          compared_table = all_colnames, column_name = "colnam")
    #
    #   selected_name <- ""
    #   slide <- 0
    #   while(selected_name=="") {
    #     print(dplyr::pull(all_names_collector)[i])
    #     slide <- slide + 1
    #     sorted_matches %>%
    #       tibble::add_column(ID=seq(1, nrow(.), 1)) %>%
    #       dplyr::select(-id_table_colnam) %>%
    #       dplyr::select(ID, colnam, family_name, surname, nationality) %>%
    #       dplyr::slice((1+(slide-1)*10):((slide)*10)) %>%
    #       print()
    #     selected_name <-
    #       readline(prompt="Choose ID whose name fit (if none enter 0): ")
    #     if(slide*10>nrow(sorted_matches)) slide <- 0
    #   }
    #
    #   selected_name <- as.integer(selected_name)
    #
    #   if(is.na(selected_name))
    #     stop("Provide integer value for standardizing collector name")
    #

    #
    #   if(selected_name>0) {
    #     selected_name_id <-
    #       sorted_matches %>%
    #       dplyr::slice(selected_name) %>%
    #       dplyr::select(id_table_colnam) %>%
    #       dplyr::pull()
    #   }
    #
    # }else{
    #   selected_name <-
    #     which(all_colnames$colnam==dplyr::pull(all_names_collector)[i])
    #
    #   selected_name_id <-
    #     all_colnames %>%
    #     dplyr::slice(selected_name) %>%
    #     dplyr::select(id_table_colnam) %>%
    #     dplyr::pull()
    # }




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
#' @param data_stand string tibble
#' @param plot_name_field string vector
#'
#' @export
.link_plot_name <- function(data_stand, plot_name_field) {
  plot_name <- "plot_name"

  data_stand <-
    data_stand %>%
    dplyr::rename_at(dplyr::vars(plot_name_field), ~ plot_name)

  all_plot_names <-
    dplyr::tbl(mydb, "data_liste_plots") %>%
    dplyr::select(id_liste_plots, plot_name) %>%
    dplyr::collect()

  all_plot_name_new_dataset <-
    dplyr::distinct(data_stand, plot_name)

  all_plot_name_new_dataset <-
    all_plot_name_new_dataset %>%
    dplyr::left_join(all_plot_names)

  all_plot_name_new_dataset_no_match <-
    all_plot_name_new_dataset %>%
    dplyr::filter(is.na(id_liste_plots))

  data_stand <-
    data_stand %>%
    dplyr::left_join(all_plot_name_new_dataset, by=c("plot_name"="plot_name"))

  id_plotname <-
    data_stand$id_liste_plots
  if(nrow(all_plot_name_new_dataset_no_match)>0) {
    for (i in 1:nrow(all_plot_name_new_dataset_no_match)) {
      print(all_plot_name_new_dataset_no_match$plot_name[i])
      sorted_matches <-
        .find_similar_string(input = all_plot_name_new_dataset_no_match$plot_name[i],
                             compared_table = all_plot_names, column_name = "plot_name")

      selected_name <- ""
      slide <- 0
      while(selected_name=="") {
        if(slide > 0) print(all_plot_name_new_dataset_no_match$plot_name[i])
        slide <- slide + 1
        sorted_matches %>%
          tibble::add_column(ID=seq(1, nrow(.), 1)) %>%
          dplyr::select(-id_liste_plots) %>%
          dplyr::select(ID, plot_name) %>%
          dplyr::slice((1+(slide-1)*10):((slide)*10)) %>%
          print()
        selected_name <-
          readline(prompt="Choose ID whose plot_name fit (if none enter 0): ")
        if(slide*10>nrow(sorted_matches)) slide <- 0
      }

      selected_name <- as.integer(selected_name)

      if(is.na(selected_name)) stop("Provide integer value for standardizing plot name")

      if(selected_name==0) {
        print(paste(all_plot_name_new_dataset_no_match$plot_name[i]," not found"))
      }

      if(selected_name>0) {
        selected_name_id <-
          sorted_matches %>%
          slice(selected_name) %>%
          dplyr::select(id_liste_plots) %>%
          dplyr::pull()

        if(!all(is.na(id_plotname[data_stand$plot_name==all_plot_name_new_dataset_no_match$plot_name[i]])))
          stop("finding plot name with no na values")

        id_plotname[data_stand$plot_name==all_plot_name_new_dataset_no_match$plot_name[i]] <-
          selected_name_id
      }
    }
    data_stand <-
      data_stand %>%
      dplyr::mutate(id_liste_plots=id_plotname)
  }

  if(data_stand %>%
     dplyr::filter(is.na(id_liste_plots), !is.na(plot_name)) %>%
     nrow()>0) {
    print("Plot name not found !!")

  }
  return(data_stand)
}


#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param trait string vector
#'
#' @export
.link_trait <- function(data_stand, trait) {

  all_traits <-
    dplyr::tbl(mydb, "traitlist") %>%
    dplyr::collect()

  sorted_matches <-
    .find_similar_string(input = trait,
                         compared_table = all_traits, column_name = "trait")
  print(trait)

  selected_name <- ""
  slide <- 0
  while(selected_name=="") {
    slide <- slide + 1
    sorted_matches %>%
      tibble::add_column(ID=seq(1, nrow(.), 1)) %>%
      dplyr::select(ID, trait, traitdescription) %>%
      dplyr::slice((1+(slide-1)*10):((slide)*10)) %>%
      print()
    selected_name <-
      readline(prompt="Choose ID whose trait fit (if none enter 0): ")
    if(slide*10>nrow(sorted_matches)) slide <- 0
  }

  selected_name <- as.integer(selected_name)

  if(is.na(selected_name))
    stop("Provide integer value for standardizing trait name")

  selected_trait_id <-
    sorted_matches %>%
    dplyr::slice(selected_name) %>%
    dplyr::select(id_trait) %>%
    dplyr::pull()

  select_trait_features <-
    sorted_matches %>%
    dplyr::slice(selected_name)


  if (select_trait_features$valuetype == "numeric") {

    if (!is.numeric(data_stand$trait)) {

      cli::cli_alert_warning("Expected numeric values but some are not")

      data_stand %>%
        mutate(trait = as.numeric(trait)) %>%
        filter(is.na(trait)) %>%
        print()

      cli::cli_alert_warning("Removing non numeric values")

      data_stand <-
        data_stand %>%
        mutate(trait = as.numeric(trait)) %>%
        filter(!is.na(trait))

    }
  }

  issues <- vector(mode = "character", length = nrow(data_stand))
  if(select_trait_features$valuetype != "character") {
    if(any(data_stand$trait<select_trait_features$minallowedvalue)) {
      warning(paste(trait, "values lower than minallowedvalue for", trait, "for",
                    sum(data_stand$trait<select_trait_features$minallowedvalue), "entries"))
      issues[data_stand$trait<select_trait_features$minallowedvalue] <-
        paste(select_trait_features$trait, "lower than minallowedvalue")
    }

    if(any(data_stand$trait>select_trait_features$maxallowedvalue)) {
      warning(paste(trait, "values higher than maxallowedvalue for", trait, "for",
                    sum(data_stand$trait>select_trait_features$maxallowedvalue), "entries"))
      issues[data_stand$trait>select_trait_features$maxallowedvalue] <-
        paste(select_trait_features$trait, "higher than maxallowedvalue")
    }
  }

  issues[issues==""] <- NA

  data_stand <-
    data_stand %>%
    tibble::add_column(id_trait = rep(selected_trait_id, nrow(.)))

  data_stand <-
    data_stand %>%
    tibble::add_column(issue = issues)

  return(data_stand)
}



#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param trait string vector
#'
#' @export
.link_method <- function(method) {

  all_method <-
    dplyr::tbl(mydb, "methodslist") %>%
    dplyr::collect()

  if(any(all_method$method == method)) {

    selected_id <- all_method %>%
      filter(method == !!method) %>%
      pull(id_method)

  } else {

    sorted_matches <-
      .find_similar_string(input = method,
                           compared_table = all_method, column_name = "method")
    print(method)

    selected_name <- ""
    slide <- 0
    while (selected_name == "") {
      slide <- slide + 1
      sorted_matches %>%
        tibble::add_column(ID = seq(1, nrow(.), 1)) %>%
        dplyr::select(ID, method, description_method) %>%
        dplyr::slice((1 + (slide - 1) * 10):((slide) * 10)) %>%
        print()
      selected_name <-
        readline(prompt = "Choose ID whose method fit (if none enter 0): ")
      if (slide * 10 > nrow(sorted_matches))
        slide <- 0
    }

    selected_name <- as.integer(selected_name)

    if(is.na(selected_name))
      stop("Provide integer value for standardizing method name")

    selected_id <-
      sorted_matches %>%
      dplyr::slice(selected_name) %>%
      dplyr::select(id_method) %>%
      dplyr::pull()

  }

  return(selected_id)
}



#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param data_stand tibble
#' @param subplotype string vector
#'
#' @export
.link_subplotype <- function(data_stand, subplotype) {

  all_subplotype <-
    dplyr::tbl(mydb, "subplotype_list") %>%
    dplyr::collect()

  sorted_matches <-
    .find_cat(
      value_to_search = subplotype,
      compared_table = all_subplotype,
      column_name = "type",
      prompt_message = "Choose subplot feature (G for pattern searching): "
    )

  # sorted_matches <-
  #   .find_similar_string(input = subplotype,
  #                        compared_table = all_subplotype,
  #                        column_name = "type")
  #
  # print(subplotype)
  #
  #
  # selected_name <- "S"
  # slide <- 0
  # while(any(selected_name == c("", "G", "S"))) {
  #
  #   slide <- slide + 1
  #
  #   if(any(selected_name == c("S"))) {
  #     slide = 1
  #     sorted_matches <-
  #       .find_similar_string(input = subplotype,
  #                            compared_table = all_subplotype,
  #                            column_name = "type")
  #   }
  #
  #
  #   if (any(selected_name == c("G"))) {
  #
  #     slide = 1
  #     grep_name <-
  #       readline(prompt = "Which string to look for:")
  #     sorted_matches <-
  #       all_subplotype %>%
  #       filter(grepl(grep_name, type))
  #
  #   }
  #
  #   if(nrow(sorted_matches) > 0) {
  #
  #     sorted_matches_print <-
  #       sorted_matches %>%
  #       tibble::add_column(ID=seq(1, nrow(.), 1)) %>%
  #       dplyr::select(ID, type, typedescription) %>%
  #       dplyr::slice((1+(slide-1)*10):((slide)*10))
  #
  #   } else {
  #     sel_loc <-
  #       sorted_matches
  #   }
  #
  #
  #   # print(sel_loc)
  #
  #   sorted_matches_print <-
  #     sorted_matches_print %>%
  #     kableExtra::kable(format = "html", escape = F) %>%
  #     kableExtra::kable_styling("striped", full_width = F) %>%
  #
  #   print(sorted_matches_print)
  #
  #   print(subplotype)
  #
  #   selected_name <-
  #     readline(prompt="Choose ID whose type names fit : ")
  #
  #   if (slide * 10 > nrow(sorted_matches))
  #     slide <- 0
  # }

  selected_name <- as.integer(sorted_matches$selected_name)

  if(is.na(selected_name))
    stop("Provide integer value for standardizing subplotype name")

  selected_type_id <-
    sorted_matches$sorted_matches %>%
    dplyr::slice(selected_name) %>%
    dplyr::select(id_subplotype) %>%
    dplyr::pull()

  select_type_features <-
    sorted_matches$sorted_matches %>%
    dplyr::slice(selected_name)

  if(select_type_features$valuetype == "numeric") {
    if(any(is.na(as.numeric(data_stand$subplottype)))) {
      warning("Numeric value expected but some are not")
      print(data_stand[which(is.na(as.numeric(data_stand$subplottype))),])
    }

    data_stand$subplottype <-
      as.numeric(data_stand$subplottype)
  }

  issues <- vector(mode = "character", length = nrow(data_stand))
  if(select_type_features$valuetype == "numeric") {
    if(any(data_stand$subplottype[!is.na(data_stand$subplottype)] < select_type_features$minallowedvalue)) {
      warning(paste(subplotype, "values lower than minallowedvalue for", subplottype, "for",
                    sum(data_stand$subplottype < select_type_features$minallowedvalue), "entries"))
      issues[data_stand$subplottype < select_type_features$minallowedvalue] <-
        paste(subplottype, "lower than minallowedvalue")
    }
  }

  if(select_type_features$valuetype == "numeric") {
    if(any(data_stand$subplottype[!is.na(data_stand$subplottype)] > select_type_features$maxallowedvalue)) {
      warning(paste(subplottype, "values lower than maxallowedvalue for", subplottype, "for",
                    sum(data_stand$subplottype > select_type_features$maxallowedvalue), "entries"))
      issues[data_stand$subplottype > select_type_features$maxallowedvalue] <-
        paste(subplottype, "higher than maxallowedvalue")
    }
  }

  issues[issues == ""] <- NA

  data_stand <-
    data_stand %>%
    dplyr::mutate(id_subplottype = rep(selected_type_id, nrow(.)))

  data_stand <-
    data_stand %>%
    dplyr::mutate(issue = issues)

  return(data_stand)
}

#' Internal function
#'
#' Add modification day month and year column before adding/updating
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param dataset string tibble to add dates fields
#'
#' @export
.add_modif_field <- function(dataset) {
  dataset <-
    dataset %>%
    tibble::add_column(date_modif_d = lubridate::day(Sys.Date()),
                       date_modif_m = lubridate::month(Sys.Date()),
                       date_modif_y = lubridate::year(Sys.Date()))
  return(dataset)
}


#' Internal function
#'
#' Compute
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @export
.query_unmatched_specimens <- function() {

  all_herbarium_individuals <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::select(herbarium_nbe_char, herbarium_code_char, herbarium_nbe_type, id_diconame_n, id_specimen, id_n) %>%
    dplyr::filter(!is.na(herbarium_nbe_char) | !is.na(herbarium_code_char) | !is.na(herbarium_nbe_type)) %>%
    dplyr::collect()

  ### all specimens with more than one id_diconame in table individuals
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n>1)

  #### all specimens with more than one id_diconame in table individuals with names
  all_herbarium_individuals_not_linked_diff_tax <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                dplyr::select(id_n, full_name_no_auth, tax_gen) %>%
                  dplyr::collect(),
              by=c("id_diconame_n"="id_n")) %>%
    dplyr::filter(herbarium_nbe_char %in% dplyr::pull(all_herbarium_individuals_not_linked_diff_tax,
                                        herbarium_nbe_char)) %>%
    dplyr::arrange(herbarium_nbe_char)

  ### all specimens with more than one genus in table individuals
  herb_specimen_diff_gen <-
    all_herbarium_individuals_not_linked_diff_tax %>%
    dplyr::distinct(herbarium_nbe_char, tax_gen) %>%
    dplyr::group_by(herbarium_nbe_char) %>%
    dplyr::count() %>%
    dplyr::filter(n>1)

  ### all individuals concerned by specimens with more than one genus in table individuals
  data_individuals_concerned <-
    dplyr::tbl(mydb, "data_individuals") %>%
    dplyr::filter(herbarium_nbe_char %in% !!herb_specimen_diff_gen$herbarium_nbe_char) %>%
    dplyr::collect() %>%
    dplyr::select(dbh, code_individu, sous_plot_name, ind_num_sous_plot, herbarium_nbe_char,
                  herbarium_code_char, herbarium_nbe_type, id_diconame_n) %>%
    dplyr::left_join(dplyr::tbl(mydb, "diconame") %>%
                       dplyr::select(id_n, full_name_no_auth, tax_gen, tax_esp, tax_fam) %>%
                       dplyr::collect(),
                     by=c("id_diconame_n"="id_n")) %>%
    dplyr::arrange(herbarium_nbe_char) %>%
    dplyr::collect()

  ### extraction of all specimens not linked to specimens table excluding problematic specimens
  all_herbarium_individuals_not_linked <-
    all_herbarium_individuals %>%
    dplyr::filter(is.na(id_specimen), !is.na(herbarium_nbe_char)) %>%
    dplyr::distinct(herbarium_nbe_char, id_diconame_n) %>%
    dplyr::filter(!herbarium_nbe_char %in%
                    unique(dplyr::pull(all_herbarium_individuals_not_linked_diff_tax,
                                herbarium_nbe_char)))

  regexp <- "[[:digit:]]+"
  num_extracted <-
    stringr::str_extract(all_herbarium_individuals_not_linked$herbarium_nbe_char, regexp)

  df <-
    dplyr::tibble(full = all_herbarium_individuals_not_linked$herbarium_nbe_char,
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






#' Process trimble data
#'
#' Process raw trimble data and provide individuals data ready to import and log file with potential problems
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param PATH string path to trimble data
#' @param plot_name string plot name to give to plot
#'
#' @return A list with two tibbles
#' @export
process_trimble_data <- function(PATH = NULL, plot_name = NULL, format = "dbf") {

  if (!any(rownames(utils::installed.packages()) == "foreign"))
    stop("foreign package needed, please install it")

  all_dirs <-
    list.dirs(path = PATH,
              full.names = FALSE, recursive = FALSE)

  all_dirs <-
    list.dirs(path = PATH,
              full.names = FALSE, recursive = FALSE)


  regexp <- "[[:digit:]]+"

  if(format == "dbf") {
    files_prop <-
      dplyr::tibble(dirs = all_dirs,
                    number = as.numeric(stringr::str_extract(all_dirs, regexp)),
                    gps = grepl("GPS", all_dirs))

    count_nbe_file <-
      files_prop %>%
      dplyr::filter(!is.na(number)) %>%
      dplyr::group_by(number) %>%
      dplyr::count()
    if (any(count_nbe_file$n != 2))
      stop("some plot do not have two directories!")

  }

  if(format == "excell") {
    files_prop <-
      dplyr::tibble(dirs = all_dirs,
                    number = as.numeric(stringr::str_extract(all_dirs, regexp)))

    count_nbe_file <-
      files_prop %>%
      dplyr::filter(!is.na(number)) %>%
      dplyr::group_by(number) %>%
      dplyr::count()

  }

  ncol_list <- list()
  nbe_herb <- list()
  all_plot_list <- list()
  all_traits_list <- list()
  logs_process <- list()
  gps_plot_list <- list()
  gps_quadrat_list <- vector(mode = 'list', length(unique(files_prop$number)))
  for (j in sort(unique(files_prop$number))) {

    logs_tb <-
      dplyr::tibble(plot_name = as.character(),
                    field = as.character(),
                    issue = as.character(),
                    id = as.character())
    # print(j)
    select_plot_dir <-
      files_prop %>%
      dplyr::filter(number == j)

    if(format == "dbf") {
      occ_data_dir <-
        select_plot_dir %>%
        dplyr::filter(gps == FALSE) %>%
        dplyr::select(dirs) %>%
        dplyr::pull()
    } else {

      occ_data_dir <-
        select_plot_dir %>%
        dplyr::select(dirs) %>%
        dplyr::pull()

    }

    if(format == "dbf") {
      gps_data_dir <-
        select_plot_dir %>%
        dplyr::filter(gps==TRUE) %>%
        dplyr::select(dirs) %>%
        dplyr::pull()
    } else {

    }


    ## gps data
    data.gps <-
      list.files(path = paste0(PATH,
                               gps_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")
    data.gps <- data.gps[grep(".dbf", data.gps)]

    gps_data <-
      foreign::read.dbf(file = data.gps) %>%
      dplyr::as_tibble()

    gps_data_subset <-
      gps_data %>%
      dplyr::mutate(date = as.character(Date)) %>%
      dplyr::mutate(date_y = as.numeric(unlist(lapply(
        strsplit(date, "-"),
        FUN = function(x)
          x[[1]]
      )))) %>%
      dplyr::mutate(date_m = as.numeric(unlist(lapply(
        strsplit(date, "-"),
        FUN = function(x)
          x[[2]]
      )))) %>%
      dplyr::mutate(date_d = as.numeric(unlist(lapply(
        strsplit(date, "-"),
        FUN = function(x)
          x[[3]]
      )))) %>%
      dplyr::select(Longitude, Latitude, date, date_y, date_m, date_d, From_X, From_Y)

    gps_quadrat_list[[j]] <-
      gps_data_subset %>%
      dplyr::rename(ddlat = Latitude,
                    ddlon = Longitude) %>%
      dplyr::mutate(plot_name = rep(paste0(plot_name, ifelse(j < 10, "00", "0") , j), nrow(.)))

    gps_data_subset_one_plot <-
      gps_data_subset %>%
      dplyr::mutate(plot_name = rep(paste0(plot_name, ifelse(j < 10, "00", "0") , j), nrow(.))) %>%
      dplyr::group_by(plot_name) %>%
      dplyr::summarise(
        ddlat = mean(Latitude),
        ddlon =  mean(Longitude),
        date_y = min(date_y),
        date_m = min(date_m),
        date_d = min(date_d)
      )


    gps_data_subset_one_plot <-
      gps_data_subset_one_plot %>%
      dplyr::mutate(data_provider = "AMAP") %>%
      dplyr::mutate(co_authorship = "AMAP") %>%
      dplyr::mutate(country = "Cameroun") %>%
      dplyr::mutate(area_plot = 10000) %>%
      dplyr::mutate(method = "1ha-IRD") %>%
      dplyr::mutate(locality_name = plot_name)


    gps_plot_list[[j]] <- gps_data_subset_one_plot

    ### individuals data
    print(occ_data_dir)

    if(format == "dbf") {

      data. <-
        list.files(path = paste0(PATH,
                                 occ_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")
      data. <- data.[grep(".dbf", data.)]

      occ_data <-
        foreign::read.dbf(file = data.) %>%
        dplyr::as_tibble()

    } else {


      data. <-
        list.files(path = paste0(PATH,
                                 occ_data_dir, "/"), full.names = TRUE, pattern = "^[^~]")
      data. <- data.[grep(".xlsx", data.)]

      if(length(data.)==0) stop("not xlsx file in directory")

      occ_data <-
        readxl::read_excel(data.)

    }


    # check ID
    dupl_id <-
      occ_data %>%
      dplyr::filter(duplicated(.[["ID"]]))

    if(nrow(dupl_id)>0) {
      stop(paste("id duplicated", "plot", j))
      print(dupl_id)
    }

    if (occ_data$ID[1] != 1) {
      corrected_id <- vector(mode = "numeric", length = nrow(occ_data))
      for (m in 1:nrow(occ_data))
        corrected_id[m] <- occ_data$ID[m] - occ_data$ID[1] + 1

      logs_tb <- dplyr::bind_rows(
        logs_tb,
        dplyr::tibble(
          plot_name =
            paste0(plot_name, ifelse(j <
                                       10, "00", "0") , j),
          field = "ID",
          issue = "ID not starting at 1",
          id = "all"
        )
      )
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
    if (sum(occ_data$New_quadra[!is.na(occ_data$New_quadra)] == "yes") !=
        25) {
      warning(paste(
        "25 quadrat expected but",
        sum(occ_data$New_quadra == "yes"),
        "found"
      ))
    }

    all_expected_combination <-
      expand.grid(seq(0, 80, 20),seq(0, 80, 20)) %>%
      dplyr::as_tibble() %>%
      dplyr::mutate(quadrat = paste(Var1, Var2, sep="_"))

    all_quadrat <-
      occ_data %>%
      dplyr::filter(New_quadra == "yes") %>%
      dplyr::select(ID, New_quadra, X_theo, Y_theo) %>%
      dplyr::mutate(quadrat = paste(X_theo, Y_theo, sep = "_"))

    missing_quadrats <-
      all_expected_combination %>%
      dplyr::left_join(all_quadrat, by = c("quadrat" = "quadrat")) %>%
      dplyr::filter(is.na(ID)) %>%
      dplyr::select(quadrat) %>%
      dplyr::pull()

    if(length(missing_quadrats)>0) {
      print("missing quadrat")
      print(missing_quadrats)
    }

    duplicates_quadrat <-
      all_quadrat %>%
      dplyr::group_by(quadrat) %>%
      dplyr::count() %>%
      dplyr::filter(n>1)

    if (nrow(duplicates_quadrat) > 0) {
      all_quadrat %>%
        dplyr::filter(quadrat %in% duplicates_quadrat$quadrat) %>%
        print()

      ### ### ### ### ### ### ### ### ### ### ### ###
      ### correcting quadrat names ### ### ### ### ###
      ### ### ### ### ### ### ### ### ### ### ### ###
      path <- NULL
      quad1 <-
        all_quadrat %>%
        dplyr::slice(1)

      quad2 <-
        all_quadrat %>%
        dplyr::slice(2)

      if (quad1$X_theo == quad2$X_theo)
        path <- "vertical"
      if (quad1$Y_theo == quad2$Y_theo)
        path <- "horizontal"
      if (is.null(path))
        stop("quadrat progression not logical")

      Y <-
        c(seq(0, 80, 20), rev(seq(0, 80, 20)), c(seq(0, 80, 20), rev(seq(0, 80, 20))),
          seq(0, 80, 20))
      X <-
        c(rep(0, 5), rep(20, 5), rep(40, 5), rep(60, 5), rep(80, 5))

      if (path == "horizontal") {
        expected_quadrats <-
          dplyr::tibble(x = Y, y = X) %>%
          dplyr::mutate(quadrat = paste(x, y, sep = "_"))
      }
      if (path == "vertical") {
        expected_quadrats <-
          dplyr::tibble(x = X, y = Y) %>%
          dplyr::mutate(quadrat = paste(x, y, sep = "_"))
      }

      quadrat_corrected <-
        vector(mode = "character", length = nrow(all_quadrat))
      for (k in 1:nrow(all_quadrat)) {
        quadrat_found <-
          all_quadrat %>%
          dplyr::slice(k) %>%
          dplyr::pull(quadrat)
        quadrat_expected <-
          expected_quadrats %>%
          dplyr::slice(k) %>%
          dplyr::pull(quadrat)
        quadrat_found
        quadrat_expected
        if (quadrat_found != quadrat_expected) {
          message(paste(
            "replacing observed quadrat",
            quadrat_found,
            "by",
            quadrat_expected
          ))
          quadrat_corrected[k] <- quadrat_expected

          ids <-
            all_quadrat %>%
            dplyr::slice(k) %>% dplyr::pull(ID)
          ids <- paste(ids, collapse = ", ")

          logs_tb <- dplyr::bind_rows(
            logs_tb,
            dplyr::tibble(
              plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
              field = "New_quadra",
              issue = "error in quadrat automatically corrected",
              id = ids
            )
          )

        } else
          (
            quadrat_corrected[k] <- quadrat_found
          )
      }

      if (length(unique(quadrat_corrected)) != 25)
        stop("still not 25 unique quadrat name")

      all_quadrat <-
        all_quadrat %>%
        tibble::add_column(quadrat_corrected = quadrat_corrected)

      occ_data <-
        occ_data %>%
        dplyr::left_join(all_quadrat %>%
                           dplyr::select(ID, quadrat_corrected),
                         by = c("ID" = "ID"))

    }

    if (any(colnames(all_quadrat) == "quadrat_corrected"))
      all_quadrat <-
      all_quadrat %>%
      dplyr::rename(quadrat_good_format = quadrat_corrected)

    # add_column(quadrat_good_format = unlist(lapply(strsplit(x = all_quadrat$quadrat_corrected, split = "_"), FUN = function(x) paste("(", x[1], ";", x[2], ")",sep=""))))

    if (!any(colnames(all_quadrat) == "quadrat_corrected"))
      all_quadrat <-
      all_quadrat %>%
      dplyr::mutate(quadrat_good_format = quadrat)
    # add_column(quadrat_good_format = unlist(lapply(strsplit(x = all_quadrat$quadrat, split = "_"), FUN = function(x) paste("(", x[1], ";", x[2], ")",sep=""))))

    quadrats <-
      vector(mode = "character", length = nrow(occ_data))
    for (k in 1:(nrow(all_quadrat))) {
      if (k == nrow(all_quadrat)) {
        quadrats[all_quadrat$ID[k]:nrow(occ_data)] <-
          all_quadrat$quadrat_good_format[k]
      } else {
        quadrats[all_quadrat$ID[k]:all_quadrat$ID[k + 1]] <-
          all_quadrat$quadrat_good_format[k]
        # print(all_quadrat$quadrat_good_format[k])
      }
    }

    occ_data <-
      occ_data %>%
      tibble::add_column(quadrat = quadrats)

    ### adding plot name
    occ_data <-
      occ_data %>%
      tibble::add_column(plot_name = rep(paste0(plot_name, ifelse(j < 10, "00", "0") , j),
                                         nrow(occ_data)))

    ### rename and check dbh
    occ_data <-
      occ_data %>%
      dplyr::rename(dbh = DBH)
    if(any(occ_data$dbh>400)) stop("unrealistic dbh value")

    if (any(occ_data$dbh < 10)) {
      message("dbh value lower than 10")

      ids <-
        occ_data %>%
        dplyr::filter(dbh < 10) %>% dplyr::pull(ID)
      ids <- paste(ids, collapse = ", ")

      logs_tb <-
        logs_tb %>%
        dplyr::bind_rows(
          dplyr::tibble(
            plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
            field = "dbh",
            issue = "dbh lower than 10",
            id = as.character(ids)
          )
        )

    }

    if(any(is.na(occ_data$dbh))) stop("missing dbh value")

    ### rename and check DBH_height
    if (!any(colnames(occ_data) == "dbh_height")) {
      occ_data <-
        occ_data %>%
        dplyr::rename(dbh_height = DBH_height)
    }

    if (any(is.na(occ_data$dbh_height))) {
      print(occ_data %>%
              dplyr::filter(is.na(dbh_height)))
      message("missing dbh_height value")

      ids <-
        occ_data %>%
        dplyr::filter(is.na(dbh_height)) %>%
        dplyr::pull(ID)
      ids <- paste(ids, collapse = ", ")

      logs_tb <-
        logs_tb %>%
        dplyr::bind_rows(
          dplyr::tibble(
            plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
            field = "dbh_height",
            issue = "missing values",
            id = ids
          )
        )

    }
    if (any(occ_data$dbh_height[!is.na(occ_data$dbh_height)] > 20))
      stop("unrealistic dbh_height value")

    ### rename and check original taxa name
    occ_data <-
      occ_data %>%
      dplyr::mutate(Species = as.character(Species),
                    Genus = as.character(Genus),
                    Family = as.character(Family),
                    Identif_co = as.character(Identif_co)) %>%
      dplyr::mutate(original_tax_name =
                      ifelse(!is.na(Species), Species, ifelse(!is.na(Genus), Genus, Family))) %>%
      dplyr::mutate(original_tax_name =
                      ifelse(!is.na(original_tax_name), original_tax_name, Identif_co)) %>%
      dplyr::mutate(original_tax_name = tidyr::replace_na(original_tax_name, "indet."))

    ### multistem check
    multi_stem_ind <-
      occ_data %>%
      dplyr::select(ID, Rainfor_Tr, original_tax_name, Observatio) %>%
      dplyr::filter(grepl("multiple", Rainfor_Tr) | grepl("multiple", Observatio))

    ## identify and check coherence of multi stem
    list_multiple_stem <- list()
    if (nrow(multi_stem_ind) > 0) {
      for (k in 1:nrow(multi_stem_ind)) {
        # print(k)
        if (length(list_multiple_stem) > 0) {
          ids <- unlist(lapply(list_multiple_stem, function(w)
            w$ID))
        } else{
          ids <- 0
        }

        if (!multi_stem_ind$ID[k] %in% ids) {
          ### screnning of multiple stem : check for ID tag continuity and taxa similarity. Stop when one is not true
          q <- 1
          found <- TRUE
          add_tag <- NULL
          while (found) {
            ## if ID tag continuity is true
            if (multi_stem_ind$ID[k] + q == multi_stem_ind$ID[ifelse((k +
                                                                      q) > nrow(multi_stem_ind),
                                                                     nrow(multi_stem_ind),
                                                                     k + q)]) {
              q <- q + 1

              multi_stem_chunk <-
                multi_stem_ind %>%
                dplyr::slice(k:(k + q - 1))

              if (length(unique(multi_stem_chunk$original_tax_name)) > 1) {
                dist. <-
                  .pairwise_string_similarity(string_vector =
                                                multi_stem_chunk$original_tax_name[!is.na(multi_stem_chunk$original_tax_name)])
                dist.[is.na(dist.)] <- 1
              } else{
                dist. <- 1
              }

              if (any(dist. < 0.8)) {
                found <- FALSE
                q <- q - 1

                if (multi_stem_ind %>%
                    dplyr::slice(k:(k + q - 1)) %>%
                    nrow() == 1) {
                  message("multiple stem chunk with very different identification")

                  ### putting into log the problems
                  ids <-
                    multi_stem_chunk %>%
                    dplyr::pull(ID)

                  ids <- paste(ids, collapse = ", ")

                  logs_tb <-
                    logs_tb %>%
                    dplyr::bind_rows(
                      dplyr::tibble(
                        plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
                        field = "multistem",
                        issue = "different identification",
                        id = ids
                      )
                    )


                }
              }

            } else{
              if (q == 1) {
                ## check if following Tag is same identification and has not been put into multiple stem
                next_tag <-
                  occ_data %>%
                  dplyr::filter(ID == multi_stem_ind$ID[k] + 1) %>%
                  dplyr::select(ID, Rainfor_Tr, original_tax_name)

                dist. <-
                  .pairwise_string_similarity(
                    string_vector = c(
                      next_tag$original_tax_name,
                      multi_stem_ind$original_tax_name[k]
                    )
                  )
                dist.[is.na(dist.)] <- 1

                if (dist. < 0.8) {
                  print(multi_stem_ind %>%
                          slice(k))
                  print(next_tag)
                  message(
                    "expected following tag in multiple stem and next individual of different taxa"
                  )

                  found <- FALSE

                  ### putting into log the problems
                  ids <-
                    multi_stem_ind %>%
                    dplyr::slice(k) %>%
                    dplyr::bind_rows(next_tag) %>%
                    dplyr::pull(ID)

                  ids <- paste(ids, collapse = ", ")

                  logs_tb <-
                    logs_tb %>%
                    dplyr::bind_rows(
                      dplyr::tibble(
                        plot_name = paste0(plot_name, ifelse(j < 10, "00", "0") , j),
                        field = "multistem",
                        issue = "expected following tag in multiple stem and next individual of different taxa",
                        id = as.character(ids)
                      )
                    )

                } else{
                  add_tag <-
                    next_tag
                  found <- FALSE
                }

              } else{
                found <- FALSE
              }
            }
          }

          multi_stem_chunk <-
            multi_stem_ind %>%
            dplyr::slice(k:(k + q - 1))

          if (!is.null(add_tag))
            multi_stem_chunk <-
            dplyr::bind_rows(multi_stem_chunk, add_tag)

          if (nrow(multi_stem_chunk) > 1) {
            list_multiple_stem[[length(list_multiple_stem) + 1]] <-
              multi_stem_chunk
          }
        }
      }
    }

    multi_tiges_id <-
      vector(mode = "numeric", length = nrow(occ_data))
    if (length(list_multiple_stem) > 0) {
      for (k in 1:length(list_multiple_stem)) {
        multi_stem_chunk <-
          list_multiple_stem[[k]]
        for (d in 2:nrow(multi_stem_chunk))
          multi_tiges_id[occ_data$ID == multi_stem_chunk$ID[d]] <-
            multi_stem_chunk$ID[1]
      }
    }

    multi_tiges_id <-
      replace(multi_tiges_id, multi_tiges_id == 0, NA)

    occ_data <-
      occ_data %>%
      tibble::add_column(multi_tiges_id = multi_tiges_id)

    ### rename observations
    occ_data <-
      occ_data %>%
      dplyr::rename(observation = Observatio)

    ### rename TAG number
    occ_data <-
      occ_data %>%
      dplyr::rename(id = ID)

    if(any(occ_data$Total_heig > 0)) {

      cli::cli_alert_info("tree height data processing")

      occ_data <-
        occ_data %>%
        mutate(tree_height = Total_heig) %>%
        mutate(tree_height = replace(tree_height, tree_height == 0, NA))

      # stop("height values different of 0")

    }

    if(any(occ_data$H_branch > 0)) stop("branch values different of 0")

    ## subsetting traits datasets
    ## collection date
    missing_coll_date <-
      occ_data %>%
      dplyr::filter(is.na(Date))

    if (nrow(missing_coll_date) > 0) {
      message(paste("missing collection date for", missing_coll_date$id))
      for (i in 1:nrow(missing_coll_date)) {
        date_for_miss <-
          occ_data %>%
          dplyr::filter(id == missing_coll_date$id[i] - 1) %>%
          dplyr::pull(Date)
        occ_data[occ_data$id == missing_coll_date$id[i], "Date"] <-
          date_for_miss
      }
    }

    occ_data <-
      occ_data %>%
      dplyr::mutate(date_char = as.character(Date)) %>%
      dplyr::mutate(year = as.numeric(unlist(lapply(strsplit(date_char, "-"), function(x)
        x[[1]])))) %>%
      dplyr::mutate(month = as.numeric(unlist(lapply(strsplit(date_char, "-"), function(x)
        x[[2]])))) %>%
      dplyr::mutate(day = as.numeric(unlist(lapply(strsplit(date_char, "-"), function(x)
        x[[3]]))))

    if (any(occ_data$day > 31) |
        any(occ_data$day < 1))
      stop("day out of range")
    if (any(occ_data$month > 12) |
        any(occ_data$month < 1))
      stop("month out of range")
    if (any(occ_data$year > lubridate::year(Sys.Date())) |
        any(occ_data$year < 1900))
      stop("year out of range")

    traits_data <-
      occ_data %>%
      dplyr::select(plot_name, id, dbh, original_tax_name, year, month, day, dbh_height, Total_heig, Crown_spre, observation)

    all_traits_list[[j]] <-
      traits_data

    all_plot_list[[j]] <-
      occ_data %>%
      dplyr::select(plot_name, id, original_tax_name, quadrat, multi_tiges_id)

    logs_process[[j]] <- logs_tb

  }

  final_output <-
    dplyr::bind_rows(all_plot_list)

  final_output_traits <-
    dplyr::bind_rows(all_traits_list)

  final_logs <-
    dplyr::bind_rows(logs_process)

  final_gps <-
    dplyr::bind_rows(gps_plot_list)

  final_gps_quadrat <-
    dplyr::bind_rows(gps_quadrat_list)

  writexl::write_xlsx(final_gps, paste("final_meta_data_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_output, paste("final_process_plot_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_output_traits, paste("final_process_traits_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_logs, paste("final_process_plot_logs_", plot_name,".xlsx", sep = ""))

  writexl::write_xlsx(final_gps_quadrat, paste("final_gps_quadrat_", plot_name,".xlsx", sep = ""))


  return(list(final_output = final_output,
              final_output_traits = final_output_traits,
              final_gps = final_gps,
              final_logs = final_logs,
              final_gps_quadrat = final_gps_quadrat))
}



#' Split plot data into census
#'
#' split plot data into a list where each element is a census
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param meta tibble output of query_plot with no export individuals
#' @param dataset tibble output of query_plot with export individuals
#'
#' @importFrom rlang parse_expr
#'
#' @return A list with as many tibble as census
#' @export
.split_censuses <- function(meta, dataset) {

  split_census <- list()
  for (i in 1:nrow(meta)) {
    select_census <-
      dataset %>%
      dplyr::select(dplyr::contains(paste0("census_", i)),
                    dplyr::contains(paste0("date_census_", i)),
                    dplyr::contains(paste0("date_census_julian_", i)),
             id_n)

    stem_census <- paste0("stem_diameter_census_", i)
    stem_census_enquo <-
      rlang::parse_expr(rlang::quo_name(rlang::enquo(stem_census)))

    select_census <-
      select_census %>%
      dplyr::filter(!is.na(!!stem_census_enquo),
             !!stem_census_enquo>0)

    split_census[[i]] <- select_census
    names(split_census)[[i]] <- paste0("census_", meta$typevalue[i])
  }

  return(split_census)

}



#' Add time difference in number of days for two census
#'
#' Add
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param census1 tibble with first census
#' @param census2 tibble with second census
#'
#' @return tibble joining both census and with a added column indicating time intervall
#' @export
.time_diff <- function(census1, census2) {

  ## renaming first and second census to 1 and 2
  select_census_1 <-
    census1 %>%
    dplyr::select(dplyr::contains("date_census_julian")) %>%
    colnames() %>%
    strsplit(split = "_") %>%
    unlist()
  select_census_1 <- select_census_1[length(select_census_1)]

  census1 <-
    census1 %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
                          funs(stringr::str_replace(., paste0("_", select_census_1), "_1")))

  select_census_2 <-
    census2 %>%
    dplyr::select(dplyr::contains("date_census_julian")) %>%
    colnames() %>%
    strsplit(split = "_") %>%
    unlist()
  select_census_2 <- select_census_2[length(select_census_2)]

  census2 <-
    census2 %>%
    dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
                          funs(stringr::str_replace(., paste0("_", select_census_2), "_2")))

  ## excluding NA stem diameter (new recruits and dead stems)
  joined_census <-
    dplyr::left_join(census1, census2, by=c("id_n"="id_n")) %>%
    dplyr::filter(!is.na(stem_diameter_census_1), !is.na(stem_diameter_census_2))

  joined_census <-
    joined_census %>%
    dplyr::mutate(time_diff =  (date_census_julian_2 - date_census_julian_1 )/365.25)

  joined_census
}



#' Identify potential errors for estimating growth
#'
#' Add a column of individuals to be excluded because of potential errors
#' Adapted from http://ctfs.si.edu/Public/CTFSRPackage/
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param censuses tibble with first census
#' @param slope numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param intercept numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param err.limit integer any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
#' @param maxgrow numeric any growth in mm/year higher than maxgrow will be excluded
#' @param mindbh numeric minimum diameter in mm for excluding measures
#'
#' @return tibble joining both census and with a added column indicating logical whether inidividual should be excluded
#' @export
.trim.growth <- function(censuses,
                         slope = 0.006214,
                         intercept = 0.9036,
                         err.limit = 4,
                         maxgrow = 75,
                         # pomcut = 0.05,
                         mindbh = 100
                         # dbhunit = "cm",
) {

  # if(dbhunit=='cm') intercept=intercept/10

  censuses <-
    censuses %>%
    dplyr::mutate(dbh_mm_census1 = stem_diameter_census_1*10) %>%
    dplyr::mutate(dbh_mm_census2 = stem_diameter_census_2*10)


  ## get the standard deviation of linear model
  stdev.dbh1 <- slope * censuses$dbh_mm_census1 + intercept
  growth <- (censuses$dbh_mm_census2- censuses$dbh_mm_census1) / censuses$time_diff
  bad.neggrow <- which(censuses$dbh_mm_census2 <= (censuses$dbh_mm_census1 - err.limit *
                                                     stdev.dbh1))

  bad.posgrow <- which(growth > maxgrow)
  # homdiff <- abs(as.numeric(cens2$hom) - as.numeric(cens1$hom)) / as.numeric(cens1$hom)

  accept <- rep(TRUE, nrow(censuses))
  # accept[homdiff > pomcut] <- FALSE
  accept[bad.neggrow] <- FALSE
  accept[bad.posgrow] <- FALSE
  accept[is.na(growth)] <- FALSE
  # if (exclude.stem.change) {
  #   accept[cens1$stemID != cens2$stemID] <- FALSE
  # }
  accept[censuses$dbh_mm_census1 < mindbh] <- FALSE

  accept[is.na(censuses$dbh_mm_census1) | is.na(censuses$dbh_mm_census2) | censuses$dbh_mm_census2 <= 0] <- FALSE

  censuses <-
    censuses %>%
    tibble::add_column(accepted_growth = accept)
  return(censuses)
}



#' Growth computing for multiple census
#'
#' Growth computing for multiple census
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param dataset tibble, ouput of query_plots
#' @param metadata tibble, ouput of query_plots
#' @param mindbh numeric see http://ctfs.si.edu/Public/CTFSRPackage/index.php/web/topics/growth~slash~growth.r/trim.growth
#' @param err.limit integer any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
#' @param maxgrow numeric any growth in mm/year higher than maxgrow will be excluded
#' @param method string either 'I' or 'E'
#' @param export_ind_growth whether growth per individuals should be exported
#'
#' @importFrom date as.date date.ddmmmyy
#'
#' @return tibble
#' @export
growth_computing <- function(dataset,
                             metadata,
                             mindbh = NULL,
                             err.limit = 4, # any measure of second diameter higher than err.limit standard deviation below the first measure will be excluded
                             maxgrow = 75, # any growth (mm/year) higher than maxgrow will be excluded
                             method = "I",
                             export_ind_growth = TRUE) {

  if (!any(rownames(utils::installed.packages()) == "date")) {
    stop("date package needed, please install it")
  }


  if(!any(metadata[[2]]$typevalue>1))
    stop("Only one census recorded for all selected plots")

  all_ids_plot <-
    metadata[[2]] %>%
    dplyr::filter(typevalue >1) %>%
    dplyr::distinct(id_table_liste_plots) %>%
    dplyr::pull()

  plot_names <-
    metadata[[2]] %>%
    dplyr::filter(typevalue >1) %>%
    dplyr::distinct(id_table_liste_plots, plot_name) %>%
    dplyr::pull(plot_name)

  cli::cli_alert_info(paste("Multiple census recorded for", length(all_ids_plot), "plots"))

  full_results <-
    full_results_ind <-
    full_results_mortality <-
    vector('list', length = length(all_ids_plot)*10)
  for (plot in 1:length(all_ids_plot)) {

    cli::cli_alert_info(plot_names[plot])

    selected_dataset <-
      dataset %>%
      dplyr::filter(id_table_liste_plots_n == all_ids_plot[plot])
    selected_metadata_census <-
      metadata[[2]] %>%
      dplyr::filter(id_table_liste_plots == all_ids_plot[plot])

    skipped_census_missing_dates <-
      selected_metadata_census %>%
      dplyr::filter(is.na(year) | is.na(month))

    not_run <- FALSE
    if(nrow(skipped_census_missing_dates)>0) {

      message(paste("Census excluded because missing year and/or month"))
      print(skipped_census_missing_dates)
      not_run <- TRUE

    }

    if (length(unique(selected_metadata_census$year)) == 1 &
        length(unique(selected_metadata_census$month)) == 1 &
        length(unique(selected_metadata_census$day)) == 1) {
      message(paste("Dates do not differ between censuses"))
      not_run <- TRUE
    }

    selected_metadata_census <-
      selected_metadata_census %>%
      dplyr::arrange(typevalue) %>%
      dplyr::mutate(date =
                      paste(ifelse(!is.na(month),
                                   month, 1), # if day is missing, by default 1
                            ifelse(!is.na(day),
                                   day, 1), # if month is missing, by default 1
                            ifelse(!is.na(year),
                                   year, ""),
                            sep = "/")) %>%
      dplyr::mutate(date_julian = date::as.date(date))

    arranged_sub_plots <-
      selected_metadata_census %>%
      dplyr::arrange(typevalue) %>%
      dplyr::select(date_julian, id_sub_plots) %>%
      dplyr::arrange(date_julian) %>%
      dplyr::pull(id_sub_plots)

    if (!paste(arranged_sub_plots, collapse = "_") ==
        paste(selected_metadata_census$id_sub_plots, collapse = "_")) {
      message(paste("Dates are not in chronological order"))
      not_run <- TRUE
    }

    selected_metadata_census <-
      selected_metadata_census %>%
      dplyr::filter(!is.na(year) & !is.na(month))

    if(!not_run) {
      for (i in 1:(nrow(selected_metadata_census) - 1)) {

        splitted_census <-
          .split_censuses(meta = selected_metadata_census,
                          dataset = selected_dataset)


        ## renaming first and second census to 1 and 2
        select_census_1 <-
          splitted_census[[i]] %>%
          dplyr::select(dplyr::contains('stem_diameter_census')) %>%
          colnames() %>%
          strsplit(split = "_") %>%
          unlist()

        select_census_1 <-
          select_census_1[length(select_census_1)]

        # splitted_census[[i]] <-
        #   splitted_census[[i]] %>%
        #   dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
        #                    funs(stringr::str_replace(., paste0("_", select_census_1), "_1")))

        splitted_census[[i]] <-
          splitted_census[[i]] %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_1))),
                           list(~ stringr::str_replace(., paste0("_", select_census_1), "_1")))


        select_census_2 <-
          splitted_census[[i+1]] %>%
          dplyr::select(dplyr::contains('stem_diameter_census')) %>%
          colnames() %>%
          strsplit(split = "_") %>%
          unlist()
        select_census_2 <- select_census_2[length(select_census_2)]

        # splitted_census[[i+1]] <-
        #   splitted_census[[i+1]] %>%
        #   dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
        #                    funs(stringr::str_replace(., paste0("_", select_census_2), "_2")))

        splitted_census[[i+1]] <-
          splitted_census[[i+1]] %>%
          dplyr::rename_at(dplyr::vars(dplyr::ends_with(paste0("_", select_census_2))),
                           list(~ stringr::str_replace(., paste0("_", select_census_2), "_2")))


        ### detecting dead stems by filtering either 0 stem diameter or NA
        deads <-
          splitted_census[[i]] %>%
          dplyr::select(id_n, stem_diameter_census_1) %>%
          dplyr::left_join(
            splitted_census[[i + 1]] %>%
              dplyr::select(id_n, stem_diameter_census_2),
            by = c("id_n" = "id_n")
          ) %>%
          dplyr::filter(is.na(stem_diameter_census_2) |
                          stem_diameter_census_2 == 0)

        recruits <-
          splitted_census[[i + 1]] %>%
          dplyr::select(id_n, stem_diameter_census_2) %>%
          dplyr::left_join(
            splitted_census[[i]] %>%
              dplyr::select(id_n, stem_diameter_census_1),
            by = c("id_n" = "id_n")
          ) %>%
          dplyr::filter(is.na(stem_diameter_census_1) |
                          stem_diameter_census_1 == 0)

        censuses <- .time_diff(census1 = splitted_census[[i]], census2 = splitted_census[[i+1]])

        censuses <- .trim.growth(censuses = censuses,
                                 err.limit = err.limit,
                                 maxgrow = maxgrow,
                                 mindbh = mindbh)

        size1 <- censuses$dbh_mm_census1
        size2 <- censuses$dbh_mm_census2

        if (method == "I") {

          growthrate <- (size2 - size1) / censuses$time_diff

        } else if (method == "E") {

          growthrate <- (log(size2) - log(size1)) / censuses$time_diff

        }

        # setting NA to values considered to be problematic
        growthrate[!censuses$accepted_growth] <- NA

        censuses <-
          censuses %>%
          tibble::add_column(growthrate = growthrate)

        if (export_ind_growth) {

          ind_growth <-
            censuses %>%
            left_join(
              selected_dataset %>%
                dplyr::select(
                  id_n,
                  idtax_individual_f,
                  tax_sp_level,
                  tax_fam,
                  tax_gen,
                  tax_esp,
                  plot_name,
                  sous_plot_name,
                  ind_num_sous_plot,
                  id_table_liste_plots_n
                ),
              by = c("id_n" = "id_n")
            ) %>%
            dplyr::relocate(plot_name,
                            sous_plot_name,
                            ind_num_sous_plot,
                            tax_sp_level,
                            tax_fam,
                            tax_gen,
                            tax_esp,
                            .before = date_census_1)

          full_results_ind[[length(full_results_ind[unlist(lapply(full_results_ind, function(x) !is.null(x)))]) + 1]] <-
            ind_growth

        }


        ### growth computation

        ## number of stems at the outset (first census) excluding "exclude" stems
        N_outset <-
          splitted_census[[i]] %>%
          dplyr::left_join(censuses %>%
                      dplyr::select(id_n, growthrate) %>%
                      dplyr::filter(is.na(growthrate)) %>%
                      dplyr::mutate(growthrate = stringr::str_replace_na(growthrate, "no")) %>%
                      dplyr::rename(exclude = growthrate) %>%
                        distinct(),
                    by = c("id_n" = "id_n")) %>%
          dplyr::filter(is.na(exclude)) %>%
          nrow()

        N_survivor <-
          N_outset - nrow(deads)

        averaged_time_diff <-
          mean(censuses$time_diff)

        mortality_rate <-
          (log(N_outset)-log(N_survivor))/averaged_time_diff



        result <-
          list(
          plot_name = selected_metadata_census$plot_name[1],
          censuses = paste(names(splitted_census[i]), names(splitted_census[i+1]), collapse = "_", sep = "_"),
          growthrate = mean(growthrate, na.rm = TRUE),
          nbe_dead = nrow(deads),
          dbhmean_deads = ifelse(nrow(deads)>0, mean(deads$stem_diameter_census_1), NA),
          mortality_rate = mortality_rate,
          nbe_recruits = nrow(recruits),
          dbhmean_recruits = ifelse(nrow(recruits) > 0, mean(recruits$stem_diameter_census_2), NA),
          dbhmax_recruits = ifelse(nrow(recruits) > 0, max(recruits$stem_diameter_census_2), NA),
          N_survivor = N_survivor,
          N_outset = N_outset,
          N_excluded = length(growthrate[is.na(growthrate)]),
          sd_growthrate = sd(growthrate, na.rm = TRUE),
          dbhmean1 = mean(censuses$dbh_mm_census1[censuses$accepted_growth], na.rm = TRUE),
          dbhmean2 = mean(censuses$dbh_mm_census2[censuses$accepted_growth], na.rm = TRUE),
          nbe_days_intervall = mean(censuses$time_diff[censuses$accepted_growth], na.rm = TRUE)*365.25,
          date1 = date::date.ddmmmyy(mean(censuses$date_census_julian_1[censuses$accepted_growth],
                                          na.rm = TRUE)),
          date2 = date::date.ddmmmyy(mean(censuses$date_census_julian_2[censuses$accepted_growth],
                                          na.rm = TRUE))
        )

        full_results[[length(full_results[unlist(lapply(full_results, function(x) !is.null(x)))]) + 1]] <-
          result

        deads <-  deads %>%
          left_join(
            selected_dataset %>%
              dplyr::select(
                id_n,
                idtax_individual_f,
                tax_sp_level,
                tax_fam,
                tax_gen,
                tax_esp,
                plot_name,
                sous_plot_name,
                ind_num_sous_plot,
                id_table_liste_plots_n
              ),
            by = c("id_n" = "id_n")
          )


        full_results_mortality[[length(full_results_mortality[unlist(lapply(full_results_mortality, function(x) !is.null(x)))]) + 1]] <-
          deads

      }
    }else{

      cli::cli_alert_warning(paste(
        "No growth analysis for",
        selected_metadata_census$plot_name[1],
        "because dates are not conform"
      ))
    }
  }

  full_results <-
    full_results[unlist(lapply(full_results, function(x) !is.null(x)))]

  full_results_ind <-
    full_results_ind[unlist(lapply(full_results_ind, function(x) !is.null(x)))]

  full_results_mortality <-
    full_results_mortality[unlist(lapply(full_results_mortality, function(x) !is.null(x)))]

  if(!export_ind_growth)
    return(plot_results = full_results,
           mortality = full_results_mortality)

  if(export_ind_growth)
    return(list(plot_results = full_results,
                ind_results = full_results_ind,
                mortality = full_results_mortality))

}





#' Internal function
#'
#' Semi automatic matching with a table for comparison
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param value_to_search string vector of one element
#' @param compared_table tibble with one column where the value should be compared
#' @param column_name string name of the column of compared_table
#'
#' @export
.find_cat <- function(value_to_search, compared_table, column_name, prompt_message = "Choose") {

  print(value_to_search)

  compared_table <- .rename_data(dataset = compared_table,
                                 col_old = column_name,
                                 col_new = "comp_value")

  compared_table <-
    compared_table %>%
    mutate(perfect_match = comp_value == value_to_search)


  if(any(compared_table$perfect_match)) {

    sorted_matches <-
      compared_table

    selected_name <- which(compared_table$perfect_match)

  } else {
    selected_name <- "S"
    slide <- 0
    while(any(selected_name == c("", "G", "S"))) {

      slide <- slide + 1

      if(any(selected_name == c("S"))) {
        slide = 1
        sorted_matches <-
          .find_similar_string(input = value_to_search,
                               compared_table = compared_table,
                               column_name = "comp_value")
      }


      if (any(selected_name == c("G"))) {

        # var <- rlang::parse_expr(rlang::quo_name(rlang::enquo(column_name)))

        slide = 1
        grep_name <-
          readline(prompt = "Which string to look for:")
        sorted_matches <-
          compared_table %>%
          filter(grepl(grep_name, comp_value))

      }

      if(nrow(sorted_matches) > 0) {
        sel_loc <-
          sorted_matches %>%
          tibble::add_column(ID = seq(1, nrow(.), 1)) %>%
          dplyr::slice((1 + (slide - 1) * 10):((slide) * 10))
      } else {
        sel_loc <-
          sorted_matches
      }


      # print(sel_loc)

      sel_loc_html <-
        sel_loc %>%
        kableExtra::kable(format = "html", escape = F) %>%
        kableExtra::kable_styling("striped", full_width = F)

      print(sel_loc_html)

      print(value_to_search)

      selected_name <-
        readline(prompt = prompt_message)

      if (slide * 10 > nrow(sorted_matches))
        slide <- 0
    }

    selected_name <- as.integer(selected_name)

  }



  return(list(selected_name = selected_name,
              sorted_matches = sorted_matches))

}



#' List, extract taxa
#'
#' Provide list of selected taxa
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param class character string of class
#' @param family string
#' @param genus string
#' @param order string
#' @param species string
#' @param tax_nam01 string
#' @param tax_nam02 string
#' @param only_genus logical
#' @param only_family logical
#' @param only_class logical
#' @param ids integer id of searched taxa
#' @param verbose logical
#' @param exact_match logical
#' @param check_synonymy logical
#' @param extract_traits logical#'
#'
#' @return A tibble of plots or individuals if extract_individuals is TRUE
#' @export
query_taxa <-
  function(
    class = c("Magnoliopsida", "Pinopsida", "Lycopsida", "Pteropsida"),
    family = NULL,
    genus = NULL,
    order = NULL,
    species = NULL,
    tax_nam01 = NULL,
    tax_nam02 = NULL,
    only_genus = FALSE,
    only_family = FALSE,
    only_class = FALSE,
    ids = NULL,
    verbose = TRUE,
    exact_match = FALSE,
    check_synonymy =TRUE,
    extract_traits = TRUE
  ) {

    if(!exists("mydb")) call.mydb()

    if(!is.null(class)) {

      all_res <- vector('list', length(class))

        for (i in 1:length(class)) {
          if(!exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_tax_famclass WHERE tax_famclass ILIKE '%",
                                                class[i], "%'"))
          if(exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_tax_famclass WHERE tax_famclass ='",
                                                class[i], "'"))

          res <- DBI::dbFetch(rs)
          DBI::dbClearResult(rs)

          all_res[[i]] <-
            tbl(mydb, "table_taxa") %>%
            filter(id_tax_famclass %in% !!res$id_tax_famclass) %>%
            dplyr::select(idtax_n, idtax_good_n) %>%
            collect()
        }


      res_class <- bind_rows(all_res)

    }

    if(is.null(ids)) {

      if(!is.null(order)) {
        all_res <- list()
        for (i in 1:length(order)) {
          if(!exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_order ILIKE '%",
                                                order[i], "%'"))
          if(exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_order ='",
                                                order[i], "'"))

          res <- DBI::dbFetch(rs)
          DBI::dbClearResult(rs)
          all_res[[i]] <- dplyr::as_tibble(res) %>%
            dplyr::select(idtax_n, id_good)
        }
        res_order <- bind_rows(all_res)
      }

      if(!is.null(family)) {
        all_res <- list()
        for (i in 1:length(family)) {
          if(!exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_fam ILIKE '%",
                                                family[i], "%'"))
          if(exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_fam ='",
                                                family[i], "'"))

          res <- DBI::dbFetch(rs)
          DBI::dbClearResult(rs)
          all_res[[i]] <- dplyr::as_tibble(res) %>%
            dplyr::select(idtax_n, id_good)
        }
        res_family <- bind_rows(all_res)
      }

      if(!is.null(genus)) {

        if (!exact_match) {

          all_res <- list()
          for (i in 1:length(genus)) {

            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_gen ILIKE '%",
                                                genus[i], "%'"))
            res <- DBI::dbFetch(rs)
            DBI::dbClearResult(rs)
            all_res[[i]]  <- dplyr::as_tibble(res) %>%
              dplyr::select(idtax_n, id_good)

          }
          res_genus <- bind_rows(all_res)

        }

        if (exact_match) {
          query <- "SELECT * FROM table_taxa WHERE MMM"
          query <-
            gsub(
              pattern = "MMM",
              replacement = paste0("tax_gen IN ('",
                                   paste(unique(genus), collapse = "', '"), "')"),
              x = query
            )

          rs <- DBI::dbSendQuery(mydb, query)
          res_genus <- DBI::dbFetch(rs)
          DBI::dbClearResult(rs)
          res_genus <- dplyr::as_tibble(res_genus)

        }
            # rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_gen ='",
            #                                     genus[i], "'"))


      }

      if(!is.null(species)) {
        all_res <- list()
        for (i in 1:length(species)) {
          if(!exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_esp ILIKE '%",
                                                species[i], "%'"))
          if(exact_match)
            rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE tax_esp ='",
                                                species[i], "'"))

          res <- DBI::dbFetch(rs)
          DBI::dbClearResult(rs)
          all_res[[i]]  <- dplyr::as_tibble(res) %>%
            dplyr::select(idtax_n, id_good)
        }
        res_species <- bind_rows(all_res)
      }

      # if(!is.null(habit)) {
      #   all_res <- list()
      #   for (i in 1:length(habit)) {
      #     rs <- DBI::dbSendQuery(mydb, paste0("SELECT * FROM table_taxa WHERE a_habit ILIKE '%",
      #                                         habit[i], "%'"))
      #     res <- DBI::dbFetch(rs)
      #     DBI::dbClearResult(rs)
      #     all_res[[i]]  <- dplyr::as_tibble(res) %>%
      #       dplyr::select(idtax_n, id_good)
      #   }
      #   res_habit <- bind_rows(all_res)
      # }

      no_match <- FALSE
      res <-
        tbl(mydb, "table_taxa")

      if(!is.null(class))
        res <- res %>%
        filter(idtax_n %in% !!res_class$idtax_n)

      if (!is.null(order))
        if (nrow(res_order) > 0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_order$idtax_n)
        } else{
          message("\n no match for order")
          no_match <- TRUE
        }

      if(!is.null(family))
        if(nrow(res_family)>0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_family$idtax_n)
        }else{
          message("\n no match for family")
          no_match <- TRUE
        }

      if(!is.null(genus))
        if(nrow(res_genus)>0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_genus$idtax_n)
        }else{
          message("\n no match for genus")
          no_match <- TRUE
        }

      if (!is.null(species))
        if (nrow(res_species) > 0) {
          res <-
            res %>%
            filter(idtax_n %in% !!res_species$idtax_n)
        } else{
          message("\n no match for species")
          no_match <- TRUE
        }

      # if (!is.null(habit))
      #   if (nrow(res_habit) > 0) {
      #     res <-
      #       res %>%
      #       filter(idtax_n %in% !!res_habit$idtax_n)
      #   } else{
      #     message("\n no match for habit")
      #     no_match <- TRUE
      #   }

      if(!no_match) {
        res <- res %>% collect()
        # query <- gsub(pattern = "AND MMM", replacement = "", query)
        # rs <- DBI::dbSendQuery(mydb, query)
        # res <- DBI::dbFetch(rs)
        # DBI::dbClearResult(rs)
        # res <- dplyr::as_tibble(res)
      } else {
        res <- NULL
        message("no matching names")
      }

    } else {

      if(!is.null(class)) {

        # message("\n Filtering by class.")

        ids <-
          ids[ids %in% res_class$idtax_n]

        if (length(ids) == 0) {

          stop("id provided not found in the class queried")

        }

      }

      query <- "SELECT * FROM table_taxa WHERE MMM"
      query <-
        gsub(
          pattern = "MMM",
          replacement = paste0("idtax_n IN ('",
                               paste(unique(ids), collapse = "', '"), "')"),
          x = query
        )

      rs <- DBI::dbSendQuery(mydb, query)
      res <- DBI::dbFetch(rs)
      DBI::dbClearResult(rs)
      res <- dplyr::as_tibble(res)

    }

    if(only_genus) {

      res <-
        res %>%
        dplyr::filter(is.na(tax_esp))

    }

    if(only_family) {

      res <-
        res %>%
        dplyr::filter(is.na(tax_esp),
                      is.na(tax_gen))

    }

    if(only_class) {

      res <-
        res %>%
        dplyr::filter(is.na(tax_esp),
                      is.na(tax_gen),
                      is.na(tax_order),
                      is.na(tax_fam))

    }

    ## checking synonymies
    if(!is.null(res) & check_synonymy) {

      ## if selected taxa are synonyms
      if(any(!is.na(res$idtax_good_n))) {

        if (any(res$idtax_good_n > 1)) {

          if (verbose) {

            cli::cli_alert_info("{sum(res$idtax_good_n > 1, na.rm = TRUE)} taxa selected is/are synonym(s)")

            cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")

          }

          ## retrieving good idtax_n if selected ones are considered synonyms
          idtax_accepted <-
            res %>%
            dplyr::select(idtax_n, idtax_good_n) %>%
            dplyr::mutate(idtax_f = ifelse(!is.na(idtax_good_n),
                                           idtax_good_n, idtax_n)) %>%
            dplyr::distinct(idtax_f) %>%
            dplyr::rename(idtax_n = idtax_f)

          res <-
            tbl(mydb, "table_taxa") %>%
            dplyr::filter(idtax_n %in% !!idtax_accepted$idtax_n) %>%
            collect()

          # %>%
          #   dplyr::select(tax_gen, idtax_n)

          if (verbose) cli::cli_alert_info("{nrow(res)} taxa selected after checking synonymies")

        }
      }


      ## retrieving all synonyms from selected taxa
      id_synonyms <-
        tbl(mydb, "table_taxa") %>%
        filter(idtax_good_n %in% !!res$idtax_n) %>% ## all taxa synonyms of selected taxa
        # filter(idtax_n %in% !!res$idtax_n) %>% ## excluding taxa already in extract
        dplyr::select(idtax_n, idtax_good_n) %>%
        collect()

      if(nrow(id_synonyms) > 0) {

        if(verbose) {
          cli::cli_alert_info("{sum(id_synonyms$idtax_good_n > 0, na.rm = TRUE)} taxa selected have synonym(s)")
          cli::cli_alert_info("{nrow(res)} taxa selected before checking synonymies")
        }

        synonyms <- query_taxa(ids = id_synonyms$idtax_n,
                               check_synonymy = FALSE,
                               verbose = FALSE,
                               class = NULL,
                               extract_traits = FALSE)

        res <-
          res %>%
          bind_rows(synonyms)

      }
    }

    if(!is.null(res)) {

      res <-
        res %>%
        mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
        mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                        paste0(tax_gen,
                                               " ",
                                               tax_esp,
                                               ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                               ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                               ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                               ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                        NA)) %>%
        mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                             paste0(tax_gen,
                                                    " ",
                                                    tax_esp,
                                                    ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                    ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                    ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                    ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                    ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                    ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                    ifelse(!is.na(author3), paste0(" ", author3), "")),
                                             NA)) %>%
        dplyr::mutate(introduced_status = stringr::str_trim(introduced_status)) %>%
        dplyr::mutate(tax_sp_level = as.character(tax_sp_level),
                      tax_infra_level = as.character(tax_infra_level),
                      tax_infra_level_auth = as.character(tax_infra_level_auth)) %>%
        dplyr::select(-tax_famclass) %>%
        left_join(dplyr::tbl(mydb, "table_tax_famclass") %>%
                    dplyr::collect(),
                  by = c("id_tax_famclass" = "id_tax_famclass")) %>%
        dplyr::relocate(tax_famclass, .after = tax_order)


      if (extract_traits) {

        traitsqueried <-
          query_traits_measures(idtax = res$idtax_n, idtax_good = res$idtax_good_n)

        if (any(class(traitsqueried$traits_idtax_num) == "data.frame"))
          res <-
            res %>%
            left_join(traitsqueried$traits_idtax_num,
                      by = c("idtax_n" = "idtax"))

        if (any(class(traitsqueried$traits_idtax_char) == "data.frame"))
          res <-
            res %>%
            left_join(traitsqueried$traits_idtax_char,
                      by = c("idtax_n" = "idtax"))





      }



    }
    # if(!any(colnames(res)=="accepted_name"))
    #   res <- res %>%
    #   tibble::add_column(accepted_name=NA) %>%
    #   mutate(accepted_name = as.character(accepted_name))

    if(!is.null(res)) {
      if (verbose & nrow(res) < 50) {

        res_print <-
          res %>%
          dplyr::select(-fktax,-id_good,-tax_tax) %>%
          dplyr::relocate(tax_infra_level_auth, .before = tax_order) %>%
          dplyr::relocate(idtax_n, .before = tax_order) %>%
          dplyr::relocate(idtax_good_n, .before = tax_order)

        res_print <-
          res_print %>%
          mutate_all(~ as.character(.)) %>%
          mutate_all(~ tidyr::replace_na(., ""))

        as_tibble(cbind(columns = names(res_print), record = t(res_print))) %>%
          kableExtra::kable(format = "html", escape = F) %>%
          kableExtra::kable_styling("striped", full_width = F) %>%
          print()

      }


      if(verbose & nrow(res) >= 20)
        message("\n Not showing html table because too many taxa")
    }

    if(!is.null(res)) return(res)
  }

#' Merge individuals and taxonomic backbone
#'
#' Merge idividuals and taxonomic backbone
#'#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param id_individual numeric id of individuals to be extracted if any
#' @param id_plot numeric id of plots to be extracted if any
#'
#'
#' @return A src object with merged taxa
#' @export
merge_individuals_taxa <- function(id_individual = NULL, id_plot = NULL) {


  # getting taxo identification from specimens
  specimens_id_diconame <-
    dplyr::tbl(mydb, "specimens") %>%
    dplyr::select(id_specimen,
                  idtax_n,
                  id_colnam,
                  colnbr,
                  suffix,
                  id_tropicos,
                  id_brlu)

  # getting all ids from diconames
  diconames_id <-
    dplyr::tbl(mydb, "table_taxa") %>%
    dplyr::select(idtax_n, idtax_good_n) %>%
    dplyr::mutate(idtax_f = ifelse(is.na(idtax_good_n), idtax_n, idtax_good_n))

  # getting correct id_diconame considering synonymies
  specimens_linked <-
    specimens_id_diconame %>%
    dplyr::left_join(diconames_id, by = c("idtax_n" = "idtax_n")) %>%
    dplyr::rename(idtax_specimen_f = idtax_f) %>%
    dplyr::left_join(tbl(mydb, "table_colnam") %>%
                       dplyr::select(id_table_colnam, colnam),
                     by = c("id_colnam" = "id_table_colnam"))

  if (!is.null(id_individual)) {

    res_individuals_full <-
      dplyr::tbl(mydb, "data_individuals") %>%
      dplyr::filter(id_n %in% id_individual)

  } else{

    res_individuals_full <-
      dplyr::tbl(mydb, "data_individuals")

  }

  ### getting links to specimens -
  ## TO BE e_dED POTENTIALLY IF MORE THAN ONE SPECIMEN IS LINKED TO ONE INDIVIDUAL
  ## CODE TO EXTRACT THE "BEST" IDENTIFICATION IF DIFFERENT TO BE IMPLEMENETED
  links_specimens <-
    dplyr::tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n, id_specimen) %>%
    dplyr::group_by(id_n) %>%
    dplyr::summarise(id_specimen = max(id_specimen, na.rm = T))

  res_individuals_full <-
    res_individuals_full %>%
    dplyr::select(-id_specimen) %>%
    dplyr::left_join(links_specimens,
                     by=c("id_n"="id_n"))

  if (!is.null(id_plot))
    res_individuals_full <-
    res_individuals_full %>%
    dplyr::filter(id_table_liste_plots_n %in% !!id_plot)   #filtering for selected plots

  res_individuals_full <-
    res_individuals_full %>%
    dplyr::left_join(diconames_id %>%
                       dplyr::select(idtax_n, idtax_f),
                     # dplyr::rename(id_diconame_good_n = idtax_good_n),
                     by = c("idtax_n" = "idtax_n")) %>%
    dplyr::left_join(
      specimens_linked %>%
        dplyr::select(
          id_specimen,
          idtax_specimen_f,
          colnam,
          colnbr,
          suffix,
          id_tropicos,
          id_brlu
        ),
      by = c("id_specimen" = "id_specimen")
    ) %>% # adding id_diconame for specimens
    dplyr::mutate(idtax_individual_f = ifelse(!is.na(idtax_specimen_f),
                                              idtax_specimen_f,
                                              idtax_f))

  res_individuals_full <-
    res_individuals_full %>% #### selecting id_dico_name from specimens if any
    dplyr::left_join(
      add_taxa_table_taxa() %>%
        dplyr::select(-data_modif_d,-data_modif_m,-data_modif_y),
      by = c("idtax_individual_f" = "idtax_n")
    )

  return(res_individuals_full)

}

#' Query for traits
#'
#' Query for traits for given taxa, include synonyms or not
#'
#' @return vector
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param idtax vector of idtax_n
#' @param idtax_good vector of idtax_good; NA if no synonym
#' @param add_taxa_info logical
#' @param trait_cat_mode vector string if "most_frequent" then the most frequent value for categorical trait is given, if "all_unique" then all unique value separated by comma
#'
#' @export
query_traits_measures <- function(idtax,
                                  idtax_good = NULL,
                                  add_taxa_info = FALSE,
                                  trait_cat_mode = "most_frequent") {

  if (!is.null(idtax_good))
    idtax_tb <- tibble(idtax = idtax, idtax_good = idtax_good)

  # temp_table <-
  #   tibble(id = unique(idtax), look_for = 1)
  #
  # ## create a temporary table with new data
  # DBI::dbWriteTable(
  #   mydb,
  #   "temp_table",
  #   temp_table,
  #   overwrite = T,
  #   fileEncoding = "UTF-8",
  #   row.names = F
  # )
  #
  # traits_found <-
  #   dplyr::tbl(mydb, "table_traits_measures") %>%
  #   dplyr::left_join(
  #     dplyr::tbl(mydb, "temp_table"),
  #     by = c("idtax" = "id")
  #   ) %>%
  #   filter(!is.na(look_for)) %>%
  #   dplyr::select(-look_for) %>%
  #   dplyr::left_join(tbl(mydb, "table_traits") %>%
  #                      dplyr::select(trait, id_trait, valuetype),
  #                    by = c("id_trait" = "id_trait")) %>%
  #   dplyr::collect()

  traits_found <-
    dplyr::tbl(mydb, "table_traits_measures") %>%
    dplyr::filter(idtax %in% !!idtax) %>%
    dplyr::left_join(tbl(mydb, "table_traits") %>%
                       dplyr::select(trait, id_trait, valuetype),
                     by = c("id_trait" = "id_trait")) %>%
    dplyr::collect()

  if (add_taxa_info) {

    taxa_infos <-
      add_taxa_table_taxa(ids = traits_found$idtax) %>%
      collect() %>%
      dplyr::select(idtax_n, idtax_good_n, tax_fam, tax_gen, tax_esp, tax_infra_level)

    traits_found <-
      traits_found %>%
      left_join(taxa_infos,
                by = c("idtax" = "idtax_n"))

  }

  if(nrow(traits_found) > 0) {

    if (any(traits_found$valuetype == "categorical")) {

      traits_idtax_char <-
        traits_found %>%
        dplyr::filter(valuetype == "categorical") %>%
        dplyr::select(idtax,
                      trait,
                      traitvalue_char,
                      basisofrecord,
                      id_trait_measures) %>%
        dplyr::mutate(rn = data.table::rowid(trait)) %>%
        tidyr::pivot_wider(
          names_from = trait,
          values_from = c(traitvalue_char, basisofrecord, id_trait_measures)
        ) %>%
        dplyr::select(-rn)
      # %>%
      #   dplyr::mutate(across(starts_with("id_trait_"), as.character))

      if (!is.null(idtax_good))
        traits_idtax_char <-
          traits_idtax_char %>%
          dplyr::left_join(idtax_tb %>%
                      dplyr::select(idtax, idtax_good),
                    by = c("idtax" = "idtax")) %>%
          # dplyr::select(idtax, idtax_good) %>%
          dplyr::mutate(idtax = ifelse(is.na(idtax_good), idtax, idtax_good)) %>%
          dplyr::select(-idtax_good)

      names(traits_idtax_char) <- gsub("traitvalue_char_", "", names(traits_idtax_char))

      traits_idtax_concat <-
        traits_idtax_char %>%
        dplyr::select(idtax, starts_with("id_trait_")) %>%
        dplyr::mutate(across(starts_with("id_trait_"), as.character)) %>%
        dplyr::group_by(idtax) %>%
        dplyr::mutate(dplyr::across(where(is.character),
                                    ~ stringr::str_c(.[!is.na(.)],
                                                     collapse = ", "))) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()


      if (trait_cat_mode == "all_unique") {

        cli::cli_alert_info("Extracting all unique values for categorical traits")

        ### concatenate all distinct values
        traits_idtax_char <-
          traits_idtax_char %>%
          dplyr::select(-starts_with("id_trait_")) %>%
          dplyr::group_by(idtax) %>%
          dplyr::mutate(dplyr::across(where(is.character),
                                      ~ stringr::str_c(.[!is.na(.)],
                                                       collapse = ", "))) %>%
          dplyr::distinct() %>%
          dplyr::ungroup()

      }

      if (trait_cat_mode == "most_frequent") {

        cli::cli_alert_info("Extracting most frequent value for categorical traits")

        traits_idtax_char <-
          traits_idtax_char %>%
          dplyr::select(-starts_with("id_trait_")) %>%
          group_by(idtax, across(where(is.character))) %>%
          count() %>%
          arrange(idtax, desc(n)) %>%
          ungroup() %>%
          group_by(idtax) %>%
          dplyr::summarise_if(is.character, ~ first(.x[!is.na(.x)]))

      }

      traits_idtax_char <-
        left_join(traits_idtax_char,
                  traits_idtax_concat, by = c("idtax" = "idtax"))

      if (add_taxa_info) {

        traits_idtax_char <-
          traits_idtax_char %>%
          left_join(taxa_infos,
                    by = c("idtax" = "idtax_n"))
      }


    } else {

      traits_idtax_char <- NA

    }

    if (any(traits_found$valuetype == "numeric")) {

      traits_idtax_num <-
        traits_found %>%
        filter(valuetype == "numeric") %>%
        dplyr::select(idtax,
                      trait,
                      traitvalue,
                      basisofrecord,
                      id_trait_measures) %>%
        dplyr::mutate(rn = data.table::rowid(trait)) %>%
        tidyr::pivot_wider(
          names_from = trait,
          values_from = c(traitvalue, basisofrecord, id_trait_measures)
        ) %>%
        dplyr::select(-rn) %>%
        dplyr::mutate(across(starts_with("id_trait_"), as.character))


      if (!is.null(idtax_good))
        traits_idtax_num <-
          traits_idtax_num %>%
          left_join(idtax_tb %>%
                      dplyr::select(idtax, idtax_good),
                    by = c("idtax" = "idtax")) %>%
          # dplyr::select(idtax, idtax_good) %>%
          dplyr::mutate(idtax = ifelse(is.na(idtax_good), idtax, idtax_good)) %>%
          dplyr::select(-idtax_good)

      names(traits_idtax_num) <- gsub("traitvalue_", "", names(traits_idtax_num))

      traits_idtax_concat <-
        traits_idtax_num %>%
        dplyr::select(idtax, starts_with("id_trait_")) %>%
        dplyr::mutate(dplyr::across(starts_with("id_trait_"), as.character)) %>%
        dplyr::group_by(idtax) %>%
        dplyr::mutate(dplyr::across(where(is.character),
                                    ~ stringr::str_c(.[!is.na(.)],
                                                     collapse = ", "))) %>%
        dplyr::ungroup() %>%
        dplyr::distinct()

      ### concatenate all distinct values
      traits_idtax_num_summarized <-
        traits_idtax_num %>%
        dplyr::select(-starts_with("id_trait_")) %>%
        dplyr::group_by(idtax) %>%
        dplyr::summarise(dplyr::across(where(is.numeric),
                                       .fns= list(mean = mean,
                                                  sd = sd,
                                                  n = length),
                                       .names = "{.col}_{.fn}"))

      traits_idtax_num <-
        traits_idtax_num_summarized %>%
        left_join(traits_idtax_concat, by = c("idtax" = "idtax"))
      # %>%
      #   left_join(traits_idtax_num %>%
      #               dplyr::select(idtax,
      #                             starts_with("basisofrecord_")) %>%
      #               dplyr::distinct(),
      #             by = c("idtax" = "idtax"))


      if (add_taxa_info) {

        traits_idtax_num <-
          traits_idtax_num %>%
          left_join(taxa_infos,
                    by = c("idtax" = "idtax_n"))
      }


    } else {

      traits_idtax_num <- NA

    }

  } else {

    traits_found <- NA
    traits_idtax_num <- NA
    traits_idtax_char <- NA

  }

  return(list(traits_found = traits_found,
              traits_idtax_num = traits_idtax_num,
              traits_idtax_char = traits_idtax_char))
}


#' List, extract taxa
#'
#' Provide list of selected taxa
#'
#' @return A tibble of all taxa
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#' @param ids idtax to filter the table
#'
#'
#' @return A src object with merged taxa
#' @export
add_taxa_table_taxa <- function(ids = NULL) {

  table_taxa <-
    dplyr::tbl(mydb, "table_taxa") %>%
    mutate(tax_sp_level = ifelse(!is.na(tax_esp), paste(tax_gen, tax_esp), NA)) %>%
    mutate(tax_infra_level = ifelse(!is.na(tax_esp),
                                    paste0(tax_gen,
                                           " ",
                                           tax_esp,
                                           ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                           ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                           ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                           ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), "")),
                                    NA)) %>%
    mutate(tax_infra_level_auth = ifelse(!is.na(tax_esp),
                                         paste0(tax_gen,
                                                " ",
                                                tax_esp,
                                                ifelse(!is.na(author1), paste0(" ", author1), ""),
                                                ifelse(!is.na(tax_rank01), paste0(" ", tax_rank01), ""),
                                                ifelse(!is.na(tax_nam01), paste0(" ", tax_nam01), ""),
                                                ifelse(!is.na(author2), paste0(" ", author2), ""),
                                                ifelse(!is.na(tax_rank02), paste0(" ", tax_rank02), ""),
                                                ifelse(!is.na(tax_nam02), paste0(" ", tax_nam02), ""),
                                                ifelse(!is.na(author3), paste0(" ", author3), "")),
                                         NA))

  if (!is.null(ids)) {

    # temp_table <-
    #   tibble(id = ids, look_for = 1)
    #
    # ## create a temporary table with new data
    # DBI::dbWriteTable(
    #   mydb,
    #   "temp_table",
    #   temp_table,
    #   overwrite = T,
    #   fileEncoding = "UTF-8",
    #   row.names = F
    # )
    #
    # table_taxa <-
    #   table_taxa %>%
    #   dplyr::left_join(
    #     dplyr::tbl(mydb, "temp_table"),
    #     by = c("idtax_n" = "id")
    #   ) %>%
    #   filter(!is.na(look_for)) %>%
    #   dplyr::select(-look_for)


    # dplyr::collect()

    table_taxa <-
      table_taxa %>%
      filter(idtax_n %in% ids)

  }
  return(table_taxa)
}


#' Table taxa
#'
#'
#' Rainbio taxonomic backbone
#'
#' @docType data
#'
#' @usage data(table_taxa_tb)
#'
#' @format An object data.frame/tibble
#'
#' @keywords datasets
#'
#'
#' @examples
#' data(table_taxa_tb)
"table_taxa_tb"


#' phylo_tree
#'
#' Phylogeny from Janssens et al 2020
#'
#' @docType data
#'
#' @usage data(phylo_tree)
#'
#' @format An object of class phylo
#'
#' @keywords datasets
#'
#'
#' @examples
#' data(phylo_tree)
"phylo_tree"


#' Get species-plot data frame
#'
#' Convert the extract of database into a species-plot data frame
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#'
#' @return A data frame with taxa as row, plot as columns and values as number of individuals
#' @export
species_plot_matrix <- function(data_tb, tax_col = "tax_sp_level", plot_col = "plot_name") {

  tax_col_enquo <-
    rlang::parse_expr(rlang::quo_name(rlang::enquo(tax_col)))
  plot_col_enquo <-
    rlang::parse_expr(rlang::quo_name(rlang::enquo(plot_col)))

  nbe_row_identified <-
    data_tb %>%
    filter(!is.na(!!tax_col_enquo)) %>%
    nrow()

  if (nbe_row_identified != nrow(data_tb)) {

      cli::cli_alert_info("Removing {nrow(data_tb) - nbe_row_identified} unidentified individuals")

    data_tb <-
      data_tb %>%
      filter(!is.na(!!tax_col_enquo))

  }

  data_tb_grouped <-
    data_tb %>%
    dplyr::mutate(ab = 1) %>%
    dplyr::group_by(!!plot_col_enquo, !!tax_col_enquo) %>%
    summarise(ab = sum(ab)) %>%
    ungroup()

  data_mat_tb <-
    tidyr::pivot_wider(data = data_tb_grouped,
                       names_from = !!plot_col_enquo,
                       values_from = ab,
                       values_fill = list(ab = 0))

  ## format the tibble into a matrix
  data_mat <- data_mat_tb %>% dplyr::select(-!!tax_col_enquo) %>%  as.data.frame()
  row.names(data_mat) <-
    data_mat_tb %>%
    dplyr::select(!!tax_col_enquo) %>%
    dplyr::pull()

  return(data_mat)
}




#' Add link between specimen and individual
#'
#' Generate link between individual and specimens
#'
#' @return A tibble of all subplots
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param new_data tibble fuzzy person name to look for
#' @param col_names_select a vector of string that select columns of new_data to consider, it must be 3 columns
#' @param col_names_corresp a vector of string of same length of col_names_select, should not be changed
#' @param launch_adding_data logical, if TRUE links are added, by default it is FALSE for security
#'
#' @export
.add_link_specimens <- function(new_data,
                                col_names_select = NULL,
                                col_names_corresp = c("id_specimen", "id_n", "type"),
                                launch_adding_data = FALSE) {

  if (is.null(col_names_select))
    col_names_select <- names(new_data)

  new_data_renamed <-
    .rename_data(dataset = new_data,
                 col_old = col_names_select,
                 col_new = col_names_corresp)

  message(paste0("Number of new links: ", nrow(new_data_renamed)))

  message(paste0("adding link for: ", nrow(distinct(new_data_renamed, id_specimen)), " different specimens"))

  cli::cli_alert_info("Prepare to add links for: {nrow(distinct(new_data_renamed, id_specimen))} different specimens")


  check_dup <-
    tbl(mydb, "data_link_specimens") %>%
    dplyr::select(id_n, id_specimen) %>%
    collect() %>%
    bind_rows(new_data_renamed %>%
                dplyr::select(id_n, id_specimen)) %>%
    group_by(id_n, id_specimen) %>%
    count() %>%
    filter(n > 1) %>%
    filter(id_n %in% unique(new_data_renamed$id_n)) %>%
    ungroup()

  if(nrow(check_dup) > 0) {

    print(check_dup)
    message("some link to be added are already in the database")
    message("Excluding existing link from the new data")

    new_data_renamed <-
      new_data_renamed %>%
      filter(!id_n %in% check_dup$id_n)

    message(paste0("New links to be added are now: ", nrow(new_data_renamed)))

  }

  data_to_add <-
    new_data_renamed %>%
    dplyr::select(all_of(col_names_corresp))

  print(data_to_add)

  if(launch_adding_data) {

    cli::cli_alert_success("Added links : {nrow(data_to_add)} rows to link table")

    DBI::dbWriteTable(mydb, "data_link_specimens",
                      data_to_add, append = TRUE, row.names = FALSE)
  }

}





#' Update subplottype table
#'
#' Update subplottype table
#'
#'
#' @author Gilles Dauby, \email{gilles.dauby@@ird.fr}
#'
#' @param subplottype_searched string genus name searched
#' @param id_subplotype string genus name searched
#' @param new_subplottype string new subplottype name
#' @param new_maxallowedvalue numeric new maxallowedvalue
#' @param new_minallowedvalue numeric new minallowedvalue
#' @param new_traitdescription string new traitdescription
#' @param new_expectedunit string new expectedunit
#' @param ask_before_update logical TRUE by default, ask for confirmation before updating
#' @param add_backup logical TRUE by default, add backup of modified data
#' @param show_results logical TRUE by default, show the data that has been modified
#'
#' @return No return value individuals updated
#' @export
update_subplottype_list_table <- function(subplottype_searched = NULL,
                                    id_subplotype = NULL,
                                    new_subplottype = NULL,
                                    new_maxallowedvalue = NULL,
                                    new_minallowedvalue = NULL,
                                    new_typedescription = NULL,
                                    new_expectedunit = NULL,
                                    ask_before_update = TRUE,
                                    add_backup = TRUE,
                                    show_results=TRUE) {

  if(!exists("mydb")) call.mydb()

  if(all(is.null(c(subplottype_searched, id_subplotype))))
    stop("\n Provide subplottype_searched or id_subplotype to update")

  ### checking if at least one modification is asked
  new_vals <- c(new_subplottype, new_maxallowedvalue, new_minallowedvalue,
                new_typedescription, new_expectedunit)
  if(!any(!is.null(new_vals))) stop("\n No new values to be updated.")

  ### querying for entries to be modified
  if (!is.null(subplottype_searched)) {
    query <- 'SELECT * FROM subplotype_list WHERE MMM'
    query <- gsub(
      pattern = "MMM",
      replacement = paste0(" type ILIKE '%",
                           subplottype_searched, "%'"),
      x = query
    )

    rs <- DBI::dbSendQuery(mydb, query)
    query_trait <- DBI::dbFetch(rs)
    DBI::dbClearResult(rs)

  } else {
    query_subplotype <-
      dplyr::tbl(mydb, "subplotype_list") %>%
      dplyr::filter(id_subplotype == !!id_subplotype) %>%
      dplyr::collect()
  }

  print(query_subplotype %>% as.data.frame())
  if (nrow(query_subplotype) > 1)
    stop("more than one subplotype selected, select one")
  if (nrow(query_subplotype) == 0)
    stop("no subplotype selected, select one")

  modif_types <-
    vector(mode = "character", length = nrow(query_subplotype))

  new_vals <-
    dplyr::tibble(type = ifelse(!is.null(new_subplottype), as.character(new_subplottype),
                                 query_subplotype$type),
                  maxallowedvalue = ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
                                           query_subplotype$maxallowedvalue),
                  minallowedvalue = ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
                                           query_subplotype$minallowedvalue),
                  typedescription = ifelse(!is.null(new_typedescription), as.character(new_typedescription),
                                            query_subplotype$typedescription),
                  expectedunit = ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
                                        query_subplotype$expectedunit))

  new_vals <-
    new_vals %>%
    replace(., is.na(.), -9999)

  sel_query_subplotype <-
    dplyr::bind_rows(new_vals, query_subplotype %>%
                       dplyr::select(-valuetype, -id_subplotype))

  # sel_query_subplotype <-
  #   sel_query_subplotype %>%
  #   replace(., is.na(.), -9999)

  sel_query_subplotype <-
    replace_NA(sel_query_subplotype)

  comp_vals <-
    apply(sel_query_subplotype, MARGIN = 2, FUN = function(x) x[1]!=x[2:length(x)])

  # if(!is.null(nrow(comp_vals))) {
  #   query_trait <-
  #     query_trait[apply(comp_vals, MARGIN = 1, FUN = function(x) any(x)),]
  #   comp_vals <-
  #     apply(comp_vals, MARGIN = 2, FUN = function(x) any(x))
  # }else{
  #   query_trait <- query_trait
  # }

  if(any(is.na(comp_vals))) comp_vals <- comp_vals[!is.na(comp_vals)]

  modif_types[1:length(modif_types)] <-
    paste(modif_types, rep(paste(names(comp_vals)[comp_vals], sep=", "),
                           length(modif_types)), collapse ="__")

  # if(!any(comp_vals)) stop("No update performed because no values are different.")

  if(any(comp_vals)) {

    cat(paste("\n Number of rows selected to be updated :", nrow(query_subplotype), "\n"))

    if (ask_before_update) {

      sel_query_subplotype %>%
        dplyr::select(!!names(comp_vals)) %>%
        dplyr::select(which(comp_vals)) %>%
        print()

      Q <-
        utils::askYesNo(msg = "Do you confirm you want to update these rows for selected fields?",
                        default = FALSE)
    } else{

      Q <- TRUE

    }

    if (Q) {

      if (add_backup) {
        message("no back up for this table yet")
        # query_trait <-
        #   query_trait %>%
        #   tibble::add_column(date_modified=Sys.Date()) %>%
        #   tibble::add_column(modif_type=modif_types)
        #
        #
        # DBI::dbWriteTable(mydb, "followup_updates_diconames", query_tax, append = TRUE, row.names = FALSE)

      }

      # query_subplotype <-
      #   query_subplotype %>%
      #   dplyr::select(-date_modif_d, -date_modif_m, -date_modif_y)

      # query_subplotype <-
      #   .add_modif_field(query_subplotype)

      rs <-
        DBI::dbSendQuery(mydb,
                         statement="UPDATE subplotype_list SET type=$2, valuetype=$3, maxallowedvalue=$4, minallowedvalue=$5, typedescription=$6, expectedunit=$7 WHERE id_subplotype = $1", # , date_modif_d=$9 date_modif_m=$10, date_modif_y=$11
                         params=list(query_subplotype$id_subplotype, # $1
                                     rep(ifelse(!is.null(new_subplottype), as.character(new_subplottype),
                                                query_subplotype$type), nrow(query_subplotype)), # $2
                                     # rep(ifelse(!is.null(new_relatedterm), as.character(new_relatedterm),
                                     #            query_trait$relatedterm), nrow(query_trait)), # $3
                                     rep(query_subplotype$valuetype, nrow(query_subplotype)), # $3
                                     rep(ifelse(!is.null(new_maxallowedvalue), as.numeric(new_maxallowedvalue),
                                                query_subplotype$maxallowedvalue), nrow(query_subplotype)), # $4
                                     rep(ifelse(!is.null(new_minallowedvalue), as.numeric(new_minallowedvalue),
                                                query_subplotype$minallowedvalue), nrow(query_subplotype)), # $5
                                     rep(ifelse(!is.null(new_typedescription), as.character(new_typedescription),
                                                query_subplotype$typedescription), nrow(query_subplotype)), # $6
                                     # rep(query_subplotype$factorlevels, nrow(query_subplotype)), # $7
                                     rep(ifelse(!is.null(new_expectedunit), as.character(new_expectedunit),
                                                query_subplotype$expectedunit), nrow(query_subplotype)))  # $7
                         # ,
                         #             rep(query_subplotype$date_modif_d, nrow(query_subplotype)), # $9
                         #             rep(query_subplotype$date_modif_m, nrow(query_subplotype)), # $10
                         #             rep(query_subplotype$date_modif_y, nrow(query_subplotype))# $11
        )

      DBI::dbClearResult(rs)

      rs <-
        DBI::dbSendQuery(mydb, statement="SELECT *FROM subplotype_list WHERE id_subplotype = $1",
                         params=list(query_subplotype$id_subplotype))
      if(show_results) print(DBI::dbFetch(rs))
      DBI::dbClearResult(rs)

    }
  } else{

    if(!any(comp_vals)) print("No update performed because no values are different.")
  }

  # dbDisconnect(mydb)

}
