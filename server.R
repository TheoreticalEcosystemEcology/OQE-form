library("shiny")
library("leaflet")
library("leaflet.extras")
library("shinyjs")

# milliseconds in the .RDS file
op <- options(digits.secs=3)

# add data in the list
shinyServer(function(input, output, session) {

    source("./opForm.R", local = TRUE)

    ######### GEO DATA #########

    # Créé responses$res s'il n'existe pas
    rec <- reactiveValues(map = list(), form = df)

    responses <- reactiveValues(res = list())

    output$map <- renderLeaflet({
        leaflet(options = leafletOptions(worldCopyJump = TRUE)) %>% addProviderTiles("Esri.WorldTopoMap") %>% setView(-71.5, 54,
            5) %>% addDrawToolbar(targetGroup = rv$page, polygonOptions = drawPolygonOptions(),
            markerOptions = drawMarkerOptions(), editOptions = editToolbarOptions(),
            polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE)
    })

    observeEvent(input$map_draw_new_feature, {
        rec$map[[length(rec$map) + 1]] <- input$map_draw_new_feature
    })

    observeEvent(input$map_draw_edited_features, {

        for(i in 1:length(input$map_draw_edited_features$features)) {
          ids_edit[i] <- input$map_draw_edited_features$features[[i]]$properties$`_leaflet_id`
            }

        for (j in 1:length(rec$map)) {
          for (k in 1:length(ids_edit)){
            if (rec$map[[j]]$properties$`_leaflet_id` == ids_edit[k]) {
                rec$map[[j]] <- input$map_draw_edited_features$features[[k]]
            }
          }
        }

    })

    observeEvent(input$map_draw_deleted_features, {

        for(i in 1:length(input$map_draw_deleted_features$features)) {
          ids_delete[i] <<- input$map_draw_deleted_features$features[[i]]$properties$`_leaflet_id`
        }

        for (j in 1:length(rec$map)) {
          for (k in 1:length(ids_delete))  {
            if (rec$map[[j]]$properties$`_leaflet_id` == ids_delete[k]) {
                rec$map[j] <- list(NULL)
                break
            }
          }
        }
        rec$map <- rec$map[!sapply(rec$map, is.null)]
    })


    ######### FORM DATA #########

    # Whenever a field is filled, aggregate all form data
    response <- reactive({

        # Réponses aux questions
        rec$form[1, 1] <- input$sample
        rec$form[1, 2] <- as.character(input$yearRange)[1]
        rec$form[1, 3] <- as.character(input$yearRange)[2]
        rec$form[1, 4] <- input$sampleNo
        rec$form[1, 5] <- input$context
        rec$form[1, 6:18] <- taxa_ref %in% input$taxa
        rec$form[1, 19] <- input$status
        rec$form[1, 20:24] <- status_ref %in% input$sp_status
        rec$form[1, 25] <- as.character(input$autres_spec_status)
        rec$form[1, 26:29] <- type_ref %in% input$type
        rec$form[1, 30] <- as.character(input$autres_spec_type)
        rec$form[1, 31:33] <- enviro_ref %in% input$enviro
        rec$form[1, 34:36] <- finance_ref %in% input$finance
        rec$form[1, 37] <- as.character(input$autres_spec_finance)
        rec$form[1, 38] <- as.character(input$doi)
        rec$form[1, 39] <- input$shared
        rec$form[1, 40:51] <- db_ref %in% input$db
        rec$form[1, 52] <- as.character(input$autres_spec_db)
        rec$form[1, 53] <- as.character(input$comments)

        # Liste des réponses
        response <- list(form = rec$form, map = rec$map)
        response
    })

    # ########### PAGES BEHAVIOUR (PREVIOUS AND NEXT) ######### # declare page var as
    # reactive La page commence à 1 (Correspond à la page sur # lequel nous nous
    # trouvons présentement. Donc au départ, on est sur la 1Ã¨re page # que nous
    # allons remplir)

    rv <- reactiveValues(page = 1)

    # # Disable/enable prev and observe page
    observe({
        toggleState(id = "prev", condition = rv$page > 1)
        toggleState(id = "nxt", condition = rv$page <= length(responses$res))
        toggleState(id = "erase", condition = rv$page <= length(responses$res))
    })

    ########### PREVIOUS prev button behaviour
    observeEvent(input$prev, {

        # On change la page
        rv$page <- rv$page - 1

        # On met à jour les données
        rec$map <- responses$res[[rv$page]]$map
        rec$form <- responses$res[[rv$page]]$form
        # On update le form
        updateForm(rec$form)

        # On met à jour la carte
        leafletProxy("map") %>% addGeoJSON(rec$map) %>% addDrawToolbar(polygonOptions = FALSE,
            markerOptions = FALSE, editOptions = FALSE, polylineOptions = FALSE,
            circleOptions = FALSE, rectangleOptions = FALSE)

     })

    ######### NEXT button behaviour
    observeEvent(input$nxt, {

        if (rv$page < length(responses$res)) {

            # On change la page
            rv$page <- rv$page + 1

            # On met à jour les données
            rec$map <- responses$res[[rv$page]]$map
            rec$form <- responses$res[[rv$page]]$form

            # On update le form
            updateForm(rec$form)

            # On met à jour la carte
            leafletProxy("map") %>% addGeoJSON(rec$map) %>% addDrawToolbar(polygonOptions = FALSE,
                markerOptions = FALSE, editOptions = FALSE, polylineOptions = FALSE,
                circleOptions = FALSE, rectangleOptions = FALSE)

        } else if (rv$page == length(responses$res)) {

            rv$page <- rv$page + 1

            # On réinitialise le formulaire à blanc
            reinit(rec)
        }
    })
    # ######### ADD button behaviour
    observeEvent(input$add, {

        # s'il n'y a pas d'enregistrements
        if (length(responses$res) == 0) {
            responses$res[[1]] <<- response()
        } else {
            # On ajoute les données
            responses$res[[rv$page]] <<- response()
        }
        # On ajoute +1 au compteur de page
        rv$page <- rv$page + 1

        # On réinitialise le formulaire à blanc
        reinit(rec)

    })

    # ########### ERASE FORM
    observeEvent(input$erase, {

        if (rv$page > 1) {
            responses$res[[rv$page]] <<- NULL
            rv$page <- rv$page - 1

            # On met à jour les données
            rec$map <- responses$res[[rv$page]]$map
            rec$form <- responses$res[[rv$page]]$form

            # On update le form
            updateForm(rec$form)

            leafletProxy("map") %>% addGeoJSON(rec$map) %>% addDrawToolbar(polygonOptions = FALSE,
              markerOptions = FALSE, editOptions = FALSE, polylineOptions = FALSE,
              circleOptions = FALSE, rectangleOptions = FALSE)

        } else {
            if (length(responses$res) > 1) {
                responses$res[[rv$page]] <<- NULL
                rv$page <- rv$page + 1
                rv$page <- rv$page - 1
                reinit(rec)
                rec$map <- responses$res[[rv$page]]$map
                rec$form <- responses$res[[rv$page]]$form
                updateForm(rec$form)
                leafletProxy("map") %>% addGeoJSON(rec$map) %>% addDrawToolbar(polygonOptions = FALSE,
                  markerOptions = FALSE, editOptions = FALSE, polylineOptions = FALSE,
                  circleOptions = FALSE, rectangleOptions = FALSE)
            } else {
                responses$res[[rv$page]] <<- NULL
                reinit(rec)
                rv$page <- rv$page + 1
                rv$page <- rv$page - 1
            }
        }

    })

    output$nCamp <- renderUI({
        HTML(paste("<p style='font-size:13px;margin-top:10px'> Le nombre de campagnes d'échantillonage enregistrées est de: <b>",
            length(responses$res), "</b> </p>"))
    })

    output$nPage <- renderUI({
      HTML(paste("<p style = 'font-size:13px;margin-top10px'> Vous êtes à la page <b>", rv$page, "de ", (length(responses$res)+1), "</b> </p>"))
      })

    # ########### SUBMIT FORM submit form
    observeEvent(input$submit, {

        # On ajoute les données
        responses$res[[rv$page]] <<- response()

        ## Déclenche Modal pour demander les infos de la personne si elle a coché qu'elle
        ## était interessé à partager les données
        if (input$share == 1) {
        showModal(modalDialog(
          title ="Coordonnées de la personne en charge des données",
          p("Veuillez indiquer les coordonnées de la personne responsable des données."),
          p("Le partage des coordonnées est facultatif.", style = "font-style:italic;font-weight:bold"),
          textInput("name", label = h5("Nom, Prénom")),
          textInput("email", label = h5("Adresse courriel")),
          p("Cliquez sur « Enregistrer » pour confirmer l'envoi du formulaire."),
          p("Pour quitter, cliquez à l'extérieur de la fenêtre ou appuyez sur Esc."),
          footer = actionButton("save", class = "btn-success", label = h5("Enregistrer")),
          easyClose = TRUE
          ))

        } else {
          # On sauvegarde
          saveRDS(responses$res, file = paste0("./data/", gsub("[.]", "_", format(Sys.time(), "%Y%m%d_%H%M%OS4_")),
              "data_set.rds"))
          showModal(modalDialog(
            title = "Confirmation",
            p("Les informations ont bien été enregistrées."),
            p("Vous pouvez maintenant quitter la page."),
            p("Merci!"),
            footer = NULL)
          )
          responses$res <- list()
        }
    })

    # Save coordo and send result
    observeEvent(input$save, {
      coordo <- list(name = input$name, adress = input$adress, telNo = input$telNo, email = input$email)
      final <- list(coordo, responses$res)
      # On sauvegarde
      saveRDS(final, file = paste0("./data/", gsub("[.]", "_", format(Sys.time(), "%Y%m%d_%H%M%OS4_")),
          "data_set.rds"))
      showModal(modalDialog(
        title = "Confirmation",
        p("Les informations ont bien été enregistrées."),
        p("Vous pouvez maintenant quitter la page."),
        p("Merci!"),
        footer = NULL)
      )
      responses$res <- list()
    })
})
