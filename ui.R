library("shiny")
library("leaflet")
library("shinyjs")
library("shinythemes")
library("shinyBS")


shinyUI(fluidPage(useShinyjs(), theme = shinytheme("yeti"),
  includeCSS("stylesheet.css"),
  sidebarLayout(position = "right",
    sidebarPanel(style = "overflow-y:scroll; max-height: 100vh !important",
    h3("Sondage sur vos campagnes d'échantillonage",align = "center",style="font-weight:bold;margin-bottom:15px"),
    HTML("<hr width='75%'>"),
    div(align="center",
    actionButton("prev", label = "Précédente"),
    actionButton("nxt", label = "Suivante"),
    actionButton("erase", class = "btn-danger", label = "Effacer cette campagne")),
        HTML("<hr width='75%'>"),
        h4("Étape 1: Localisation de la campagne d'échantillonage"),
        p("Veuillez ajouter la localisation géographique de votre campagne en utilisant la carte à gauche (le menu permet de dessiner des polygones et des points).",style="font-style:italic;font-size:13px;"),
        HTML("<hr width='75%'>"),
        h4("Étape 2: Formulaire"),
        p("Veuillez compléter les informations relatives à la campagne d'échantillonage que vous venez de localiser sur la carte.",style="font-style:italic;font-size:13px;"),
              div(id = "formulaire",
                    # Campagne d'échantillonnage
                    selectInput("sample", label =  h5("Campagne d'échantillonnage:"),
                              choices = list("Ponctuelle" = 1, "Récurrente" = 2)),

                    # Année d'échantillonnage (dateRange or date)
                    conditionalPanel(
                      condition = "input.sample == 2",
                      dateRangeInput("yearRange",label = h5("Première et dernière année:"))
                    ),
                    conditionalPanel(
                      condition = "input.sample == 2",
                      numericInput("sampleNo",label = h5("Nombre d'échantillons durant cette période?"),
                                   value = 1)
                    ),
                    conditionalPanel(
                      condition = "input.sample == 1",
                      dateInput("year", label = h5("Année:"))
                    ),

                    # Type d'échantillonnage
                    radioButtons("context", label = h5("Type d'échantillonnage:"),
                                 choices = list("Observations" = 1, "Données expérimentales" = 2)),

                  # Groupes taxonomiques étudiés
                  checkboxGroupInput("taxa", label = h5("Groupe taxonomique:"),
                                     choices = list("Mammifères" = "mamm", "Mammifères marins" ="mamm_marins", "Oiseaux" = "oiseaux",
                                                    "Amphibiens" = "amph", "Reptiles" = "rept", "Poissons" = "poiss", "Arthropodes" = "arthr",
                                                    "Invertébrés autre qu'arthropodes" = "autre_arthr", "Plantes vasculaires" = "plantes_vasc",
                                                    "Plantes non-vasculaires" = "plantes_nonvasc", "Champignons, moisissures, levures" = "champ",
                                                    "Organismes unicellulaires" = "unicell", "Bactéries" = "bact"), inline=TRUE),

                  # Type de données
                  checkboxGroupInput("type", label = h5("Type de données:"),
                                     choices = list("Occurence" = "occur", "Abondance/Frequence" = "abond",
                                                    "Données individuelles (Traits, Génétiques)" = "individu", "Autres" = "autres_type"),inline=TRUE),
                  conditionalPanel(
                    condition = "$.inArray('autres_type', input.type) > -1",
                    textInput("autres_spec_type", label = h5("Vous avez séléctionné 'Autres', pourriez-vous préciser?"))
                  ),

                  # Environnement du/des site(s)
                  checkboxGroupInput("enviro", label = h5("Type d'écosystème:"),
                                     choices = list("Aquatique" = "aqua", "Marin" = "marin", "Terrestre" = "terre"),inline=TRUE),

                  # Versées vers autres bases?
                  radioButtons("shared", label = h5("Est-ce que les données de cette campagne ont déjà été versées vers d'autres bases de données ouvertes?"),
                               choices = list("Oui" = 1, "Non" = 2), selected = 2,inline=TRUE),
                  conditionalPanel(
                    condition = "input.shared == 1",
                    checkboxGroupInput("db", label = h5("Si oui, le(s)quel(s)?"),
                                       choices = list("Canadensis/GBIF" = "gbif", "DataONE" = "dataone", "Dryad" = "dryad", "Encyclopedia of Life (EOL)" = "eol",
                                                      "(ESA) data.esa.org" = "esa", "Fig Share" = "fig", "Mangal" = "mangal", "Le Naturaliste" = "naturaliste",
                                                      "Nordicana D" = "nordi", "Quebio" = "quebio", "Autres" = "autres_db"),inline=TRUE)),

                  conditionalPanel(
                    condition = "$.inArray('autres_db', input.db) > -1",
                    textInput("autres_spec_db", label = h5("Vous avez séléctionné 'Autres', pourriez-vous préciser?"))
                  ),

                  textAreaInput("comments",label= h5("Des commentaires/informations supplémentaires sur cette campagne?"),rows = 3)
                  ),
    HTML("<hr width='75%'>"),
    h4("Étape 3: Ajouter une nouvelle campagne"),
    p("Pour ajouter une nouvelle campagne d'échantillonnage, cliquez sur le bouton « Nouvelle campagne ».",style="font-style:italic;font-size:13px;"),
    actionButton("add", class = "btn-primary", label = "Nouvelle campagne"),
    uiOutput("nCamp"),
    p("Utiliser les boutons 'suivant' et 'précédent' (en entête) pour les consulter.",style="font-style:italic;font-size:13px;"),
    HTML("<hr width='75%'>"),
    h4("Étape 4: Intérêt à partager les données"),
    radioButtons("share", label = h5("Seriez-vous intéressé(e), avec notre aide, à rendre disponible ces données?"), choices = list("Oui" = 1, "Non" = 2), selected = 2, inline = TRUE),
    HTML("<hr width='75%'>"),
    h4("Étape 5: Soumettre le formulaire"),
    p("Lorsque vous avez ajoutez toutes vos campagnes, veuillez soumettre votre formulaire à l'aide du bouton « Soumettre le formulaire »",style="font-style:italic;font-size:13px;"),
    actionButton("submit", class = "btn-success", label = "Soumettre")),

    mainPanel(
      leafletOutput("map")
      )
    )
))
