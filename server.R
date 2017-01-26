library("shiny")
library("leaflet")
library("leaflet.extras")


# add data in the list
shinyServer(function(input, output, session) {

    source("./opForm.R", local = TRUE)


    ######### GEO DATA #########

    # Créé responses$res s'il n'existe pas
    rec <- reactiveValues(map=list(),form=df)

    responses <- reactiveValues(res=list())

    output$map <- renderLeaflet({
        leaflet() %>% addProviderTiles("Esri.WorldTopoMap") %>% setView(1010, 54, 5) %>%
        addDrawToolbar(targetGroup=rv$page,polygonOptions = drawPolygonOptions(),
            markerOptions = drawMarkerOptions(), editOptions = editToolbarOptions(),
            polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE)
    })

    observeEvent(input$map_draw_new_feature,{
      rec$map[[length(rec$map)+1]] <- input$map_draw_new_feature
    })

    observeEvent(input$map_draw_edited_features, {

      id_edit <- input$map_draw_edited_features$features[[1]]$properties$`_leaflet_id`

      for(i in 1:length(rec$map)){
        if(rec$map[[i]]$properties$`_leaflet_id` == id_edit ){
          rec$map[[i]] <- input$map_draw_edited_features
        }
      }

    })

    observeEvent(input$map_draw_deleted_features, {

      id_edit <- input$map_draw_edited_features$features[[1]]$properties$`_leaflet_id`

      for(i in 1:length(rep$map)){
        if(rec$map[[i]]$properties$`_leaflet_id` == id_edit ) rec$map[[i]] <- input$map_draw_edited_features
      }

    })


    ######### FORM DATA #########

    # Whenever a field is filled, aggregate all form data
    response <- reactive({

        # Réponses aux questions
        rec$form[1, 1] <- input$sample
        rec$form[1, 2] <- as.character(input$yearRange)[1]
        rec$form[1, 3] <- as.character(input$yearRange)[2]
        rec$form[1, 4] <- input$sampleNo
        rec$form[1, 5] <- as.character(input$year)
        rec$form[1, 6] <- input$context
        rec$form[1, 7:19] <- taxa_ref %in% input$taxa
        rec$form[1, 20:23] <- type_ref %in% input$type
        rec$form[1, 24:26] <- enviro_ref %in% input$enviro
        rec$form[1, 27] <- input$share
        rec$form[1, 28:38] <- db_ref %in% input$db

        # Liste des réponses
        response <- list(form=rec$form,map=rec$map)
        response
    })

    # ########### PAGES BEHAVIOUR (PREVIOUS AND NEXT) #########
    #
    # # declare page var as reactive La page commence à 1 (Correspond à la page sur
    # # lequel nous nous trouvons présentement. Donc au départ, on est sur la 1Ã¨re page
    # # que nous allons remplir)

    rv <- reactiveValues(page=1)

    # # Disable/enable prev and observe page
    observe({
      toggleState(id = "prev", condition = rv$page > 1)
      toggleState(id = "nxt", condition = rv$page <= length(responses$res))
      toggleState(id="erase",condition = rv$page <= length(responses$res))
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
      leafletProxy("map") %>%
      addGeoJSON(rec$map) %>%
      addDrawToolbar(polygonOptions =  FALSE,
          markerOptions = FALSE, editOptions = FALSE,
          polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE)

      # On update responses$res
      responses$res[[rv$page]] <<- response()

    })


    ######### NEXT button behaviour
    observeEvent(input$nxt, {

      if(rv$page < length(responses$res)){

        # On change la page
        rv$page <- rv$page + 1

        # On met à jour les données
        rec$map <- responses$res[[rv$page]]$map
        rec$form <- responses$res[[rv$page]]$form

        # On update le form
        updateForm(rec$form)

        # On met à jour la carte
        leafletProxy("map") %>%
        addGeoJSON(rec$map) %>%
        addDrawToolbar(polygonOptions =  FALSE,
            markerOptions = FALSE, editOptions = FALSE,
            polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE)

        # On update responses$res
        responses$res[[rv$page]] <<- response()

      } else if (rv$page == length(responses$res)) {

        rv$page <- rv$page + 1

        # On réinitialise le formulaire à blanc
        reinit(rec)

      }
      print(rv$page)

    })
    #
    # ######### ADD button behaviour
    #
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
      if(rv$page>1){
        responses$res[[rv$page]] <<- NULL
        rv$page <- rv$page - 1

        # On met à jour les données
        rec$map <- responses$res[[rv$page]]$map
        rec$form <- responses$res[[rv$page]]$form

        # On update le form
        updateForm(rec$form)
      } else {
        if (length(responses$res) > 1) {
          responses$res[[rv$page]] <<- NULL
          reinit(rec)
          rec$map <- responses$res[[rv$page]]$map
          rec$form <- responses$res[[rv$page]]$form
          updateForm(rec$form)
          leafletProxy("map") %>%
          addGeoJSON(rec$map) %>%
          addDrawToolbar(polygonOptions =  FALSE,
              markerOptions = FALSE, editOptions = FALSE,
              polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE)
          }
          else {
        responses$res[[rv$page]] <<- NULL
        reinit(rec)
          }
        }

        # On reinit la map comme rv$page change pas
        output$map <- renderLeaflet({
            leaflet() %>% addProviderTiles("Esri.WorldTopoMap") %>% setView(1010, 54, 5) %>%
            addDrawToolbar(targetGroup=rv$page,polygonOptions = drawPolygonOptions(),
                markerOptions = drawMarkerOptions(), editOptions = editToolbarOptions(),
                polylineOptions = FALSE, circleOptions = FALSE, rectangleOptions = FALSE)
        })
        print(length(responses$res))
      })

    output$nCamp <- renderUI({
      HTML(paste("<p style='font-size:13px;margin-top:10px'> Le nombre de campagnes d'échantillonage enregistrées est de: <b>",length(responses$res),"</b> </p>"))
    })

    # ########### SUBMIT FORM submit form
    observeEvent(input$submit, {

      # On ajoute les données
      responses$res[[rv$page]] <<- response()

      # On sauvegarde
      saveRDS(responses$res,file=paste0("./data/",format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.rds"))


      ## Déclenche Modal pour demander les infos de la personne si elle a coché qu'elle était interessé à partager les données

      ## Déclenche Modal pour dire que l'info a bien été enregistré.



    })

})
