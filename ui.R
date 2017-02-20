library("shiny")
library("leaflet")
library("shinyjs")
library("shinythemes")


shinyUI(fluidPage
   (useShinyjs(), theme = shinytheme("yeti"), includeCSS("stylesheet.css"),
    sidebarLayout(position = "right",
                  sidebarPanel(style = "overflow-y:scroll; max-height: 100vh !important",

                      # TITLE
                      h3("Sondage sur vos campagnes d'échantillonage",align = "center",style="font-weight:bold;margin-bottom:15px"),
                      HTML("<hr width='75%'>"),

                      # NEXT, PREVIOUS and ERASE BUTTON
                      tags$div(title = "Vous pouvez visualiser les différentes pages du formulaire que vous avez remplies.  Pour corriger l'une d'entre elles, veuillez cliquer sur « Effacer cette campagne » et compléter à nouveau les étapes 1 et 2 pour la même campagne.", align="center",
                      uiOutput("nPage"),
                      actionButton("prev", label = "Précédente"),
                      actionButton("nxt", label = "Suivante"),
                      actionButton("erase", class = "btn-danger", label = "Effacer cette campagne")),

                      # STEP 1 : Coordinates
                      tags$div(title = "IMPORTANT: Vous devez ajouter la localisation d'une seule campagne à la fois.                                                                                Vous pouvez également modifier les polygones ou points que vous avez dessinés à l'aide du bouton « Edit layers » du menu de la carte.",
                      HTML("<hr width='75%'>"),
                      h4("Étape 1: Localisation de la campagne d'échantillonage"),
                      p("Veuillez ajouter la localisation géographique de votre campagne en utilisant la carte à gauche (le menu permet de dessiner des polygones et des points).",style="font-style:italic;font-size:13px;"),
                      HTML("<hr width='75%'>")
                      ),

                      # STEP 2 : Form
                      tags$div(title = "Vous devez remplir une page du formulaire (étapes 1 et 2) par campagne. Le formulaire contiendra donc autant de pages que vous avez de campagnes d'échantillonnage.",

                      h4("Étape 2: Formulaire"),
                      p("Veuillez compléter les informations relatives à la campagne d'échantillonage que vous venez de localiser sur la carte.",style="font-style:italic;font-size:13px;"),

                      div(id = "formulaire",

                      ## Versées vers autres bases?
                      radioButtons("shared", label = h5("Est-ce que les données de cette campagne ont déjà été versées vers une ou plusieurs autres bases de données ouvertes?"),
                                   choices = list("Oui" = 1, "Non" = 0), selected = 1,inline=TRUE),
                      conditionalPanel(
                        condition = "input.shared == 1",
                        checkboxGroupInput("db", label = h5("Si oui, le(s)quel(s)?"),
                                           choices = list("Canadensys/GBIF" = "gbif", "DataONE" = "dataone", "Dryad" = "dryad", "Encyclopedia of Life (EOL)" = "eol",
                                                          "(ESA) data.esa.org" = "esa", "Fig Share" = "fig", "Mangal" = "mangal", "Le Naturaliste" = "naturaliste",
                                                          "Nordicana D" = "nordi", "Quebio" = "quebio", "Autres" = "autres_db"),inline=TRUE)),
                      conditionalPanel(
                        condition = "$.inArray('autres_db', input.db) > -1",
                        textInput("autres_spec_db", label = h5("Vous avez séléctionné 'Autres', veuillez préciser:"))
                        ),

                      # Si données déjà versées vers d'autres bases, cette section n'est pas à remplir
                      conditionalPanel(
                        condition = "input.shared == 0",

                        ## Campagne d'échantillonnage
                        tags$div(title = "Ponctuelle:   Campagne d'échantillonnage effectuée une seule fois.
                                                                                                                                 Récurrente:    Campagne d'échantillonnage effectuée à plusieurs reprises , sur une base régulière ou non.",
                        selectInput("sample", label =  h5("Campagne d'échantillonnage:"), choices = list("Ponctuelle" = 1, "Récurrente" = 2))
                        ),

                        ## Année d'échantillonnage
                        dateRangeInput("yearRange",label = h5("Date de début et de fin de la campagne d'échantillonnage:")),

                        ## Nombre d'échantillons
                        conditionalPanel(
                          condition = "input.sample == 2",
                          numericInput("sampleNo",label = h5("Nombre d'échantillons durant cette période:"), value = 1)
                          ),

                        ## Type d'échantillonnage
                        radioButtons("context", label = h5("Type de données:"),
                                     choices = list("Données empiriques" = 1, "Données expérimentales" = 2), inline = TRUE),

                        ## Groupes taxonomiques étudiés
                        checkboxGroupInput("taxa", label = h5("Groupe(s) taxonomique(s):"),
                                           choices = list("Mammifères" = "mamm", "Mammifères marins" ="mamm_marins", "Oiseaux" = "oiseaux",
                                                          "Amphibiens" = "amph", "Reptiles" = "rept", "Poissons" = "poiss", "Arthropodes" = "arthr",
                                                          "Invertébrés autre qu'arthropodes" = "autre_arthr", "Plantes vasculaires" = "plantes_vasc",
                                                          "Plantes non-vasculaires" = "plantes_nonvasc", "Champignons, moisissures, levures" = "champ",
                                                          "Organismes unicellulaires" = "unicell", "Bactéries" = "bact"), inline=TRUE),

                        ## Espèces à statut
                        radioButtons("status", label = h5("Avez-vous échantillonné une ou plusieurs espèces ayant un statut particulier?"),
                                     choices = list("Oui" = 1, "Non" = 0), selected = 0, inline=TRUE),

                        conditionalPanel(
                          condition = "input.status == 1",
                          checkboxGroupInput("sp_status", label = h5("Quel(s) est ce statut(s)?"),
                                             choices = list("Préoccupante" = "preoccupante", "Menacée" = "menacee", "En voie de disparition" = "voie_disp", "Espèces d'importance commerciale" = "commerciale", "Autres" = "autres_status"), inline = TRUE)
                          ),
                        conditionalPanel(
                          condition = "$.inArray('autres_status', input.sp_status) > -1",
                          textInput("autres_spec_status", label = h5("Vous avez sélectionné 'Autres', veuillez préciser:"))
                          ),

                        ## Types de données
                        checkboxGroupInput("type", label = h5("Type(s) d'observations:"),
                                           choices = list("Occurences" = "occur", "Abondances/Frequences" = "abond",
                                                          "Données individuelles (Traits, Génétiques)" = "individu",
                                                          "Autres" = "autres_type"),inline=TRUE),

                        ## Autres types de données
                        conditionalPanel(
                          condition = "$.inArray('autres_type', input.type) > -1",
                          textInput("autres_spec_type", label = h5("Vous avez sélectionné 'Autres', veuillez préciser:"))
                          ),

                        ## Environnement du/des site(s)
                        checkboxGroupInput("enviro", label = h5("Type(s) d'écosystème:"),
                                           choices = list("Aquatique" = "aqua", "Marin" = "marin", "Terrestre" = "terre"),
                                           inline=TRUE),

                        ## Financement reçu pour la campagne d'échantillonnage
                        checkboxGroupInput("finance", label = h5("Financement reçu pour cette campagne d'échantillonnage:"),
                                           choices = list("CRSNG" = "crsng", "FRQNT" = "frqnt", "Autres" = "autres_finance"), inline = TRUE),

                        conditionalPanel(
                           condition = "$.inArray('autres_finance', input.finance) > -1",
                           textInput("autres_spec_finance", label = h5("Vous avez séléctionné 'Autres', veuillez préciser:"))
                           )
                      ),

                      ## DOI
                      tags$div(title = "Si vous avez plus d'un DOI, veuillez les séparer avec des point-virgules.", textInput("doi", label = h5("DOI de l'article ou des données liés à cette campagne:"))
                      ),

                      ## Commentaires
                      textAreaInput("comments",label= h5("Des commentaires/informations supplémentaires sur cette campagne?"),rows = 3))
                      ),

                      # Step 3 : Add a campaign
                      tags$div(title = "Lorsque vous avez complété le questionnaire (étape 1 et 2) de votre dernière campagne, ne cliquez pas sur « Nouvelle campagne ». Passez directement à l'étape 4. En cliquant sur « Soumettre », votre dernière campagne sera enregistrée avec les autres.",
                      HTML("<hr width='75%'>"),
                      h4("Étape 3: Ajouter une nouvelle campagne"),
                      p("Si vous avez une nouvelle campagne d'échantillonnage à ajouter, cliquez sur le bouton « Nouvelle campagne ».",style="font-style:italic;font-size:13px;"),
                      actionButton("add", class = "btn-primary", label = "Nouvelle campagne"),
                      uiOutput("nCamp")
                      ),

                      # Step 4 : Share your data?
                      HTML("<hr width='75%'>"),
                      h4("Étape 4: Intérêt à partager les données"),
                      radioButtons("share", label = h5("Si vous disposez d'outils et d'aide appropriés, seriez-vous intéressé(e) à rendre disponibles ces données?"), choices = list("Oui" = 1, "Non" = 0), selected = 0, inline = TRUE),
                      p("Si oui, une fenêtre vous demandera les coordonnées de la personne responsable des données (facultatif).", style = "font-style:italic;font-size:13px"),

                      # Step 5 : Submit the form
                      HTML("<hr width='75%'>"),
                      h4("Étape 5: Soumettre le formulaire"),
                      p("Lorsque vous avez ajouté toutes vos campagnes, veuillez soumettre votre formulaire à l'aide du bouton « Soumettre »",style="font-style:italic;font-size:13px;"),
                      actionButton("submit", class = "btn-success", label = "Soumettre")),

                      # MAP
                  mainPanel(
                        leafletOutput("map")
                        )
)))
