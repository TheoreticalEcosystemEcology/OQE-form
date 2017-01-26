# data.frame vide pour ajouter les réponses
df <- data.frame(sample = character(), yearBegin = character(), yearEnd = character(),
    sampleNo = integer(), year = character(), context = character(), mamm = logical(),
    mamm_marins = logical(), oiseaux = logical(), amph = logical(), rept = logical(),
    poiss = logical(), arthr = logical(), autre_arthr = logical(), plantes_vasc = logical(),
    plantes_nonvasc = logical(), champ = logical(), unicell = logical(), bact = logical(),
    occur = logical(), abond = logical(), individu = logical(), autres = logical(),
    aqua = logical(), marin = logical(), terre = logical(), share = character(),
    gbif = logical(), dataone = logical(), dryad = logical(), eol = logical(), esa = logical(),
    fig = logical(), mangal = logical(), naturaliste = logical(), nordi = logical(),
    quebio = logical(), autre = logical(), stringsAsFactors = FALSE)

# Références
taxa_ref <- colnames(df)[7:19]
type_ref <- colnames(df)[20:23]
enviro_ref <- colnames(df)[24:26]
db_ref <- colnames(df)[28:38]



# Reinit form and current rec
reinit <- function(rec){
  # On réinitialise le formulaire à blanc
  reset("formulaire")
  rec$form <- df
  rec$map <- list()
}

# Save if rec change


# Update form based on responses
updateForm <- function(data){
  updateSelectInput(session, "sample",
                    choices = list("Ponctuel" = 1, "Récurrent" = 2),
                    selected = data$form[,1])

  # update date range or date
  updateDateRangeInput(session, "yearRange",
                       start = paste(data$form[,2]),
                       end = data$form[,3])

  updateNumericInput(session, "sampleNo",
                     value = data$form[,4])

  updateDateInput(session, "year",
                value = data$form[,5])

  #update context
  updateRadioButtons(session, "context",
                     choices = list("Observations" = 1, "Données expérimentales" = 2),
                     selected = data$form[,6])

  # update taxa
  updateCheckboxGroupInput(session, "taxa",
                           choices = list("Mammifères" = "mamm", "Mammifères marins" ="mamm_marins", "Oiseaux" = "oiseaux",
                                "Amphibiens" = "amph", "Reptiles" = "rept", "Poissons" = "poiss", "Arthropodes" = "arthr",
                                "Invertébrés autre qu'arthropodes" = "autre_arthr", "Plantes vasculaires" = "plantes_vasc",
                                "Plantes non-vasculaires" = "plantes_nonvasc", "Champignons, moisissures, levures" = "champ",
                                "Organismes unicellulaires" = "unicell", "Bactéries" = "bact"),
                           selected = colnames(data$form[, which(data$form[,7:19] == TRUE) + 6]),inline=TRUE)

  # update data type
  updateCheckboxGroupInput(session, "type",
                           choices = list("Occurence" = "occur", "Abondance/Frequence" = "abond",
                                          "Données individuelles (Traits, Génétiques)" = "individu", "Autres" = "autres"),
                           selected = colnames(data$form[, which(data$form[,20:23] == TRUE) + 19]),inline=TRUE)

  # update enviro
  updateCheckboxGroupInput(session, "enviro",
                           choices = list("Aquatique" = "aqua", "Marin" = "marin", "Terrestre" = "terre"),
                           selected = colnames(data$form[, which(data$form[,24:26] == TRUE) + 23]))

  # update share
  updateRadioButtons(session, "share",
                     choices = list("Oui" = 1, "Non" = 2),
                     selected = data$form[,27])

  # update db
  updateCheckboxGroupInput(session, "db",
                           choices = list("Canadensis/GBIF" = "gbif", "DataONE" = "dataone", "Dryad" = "dryad", "Encyclopedia of Life (EOL)" = "eol",
                                          "(ESA) data.esa.org" = "esa", "Fig Share" = "fig", "Mangal" = "mangal", "Le Naturaliste" = "naturaliste",
                                          "Nordicana D" = "nordi", "Quebio" = "quebio", "Autre" = "autre"),
                           selected = colnames(data$form[, which(data$form[,28:38] == TRUE) + 27]),inline=TRUE)
}
