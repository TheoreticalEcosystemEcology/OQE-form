# data.frame vide pour ajouter les réponses
df <- data.frame(sample = character(), yearBegin = character(), yearEnd = character(),
    sampleNo = integer(), year = character(), context = character(), mamm = logical(),
    mamm_marins = logical(), oiseaux = logical(), amph = logical(), rept = logical(),
    poiss = logical(), arthr = logical(), autre_arthr = logical(), plantes_vasc = logical(),
    plantes_nonvasc = logical(), champ = logical(), unicell = logical(), bact = logical(),
    occur = logical(), abond = logical(), individu = logical(), autres_type = logical(),
    autres_spec_type = character(), aqua = logical(), marin = logical(), terre = logical(),
    share = character(), gbif = logical(), dataone = logical(), dryad = logical(),
    eol = logical(), esa = logical(), fig = logical(), mangal = logical(), naturaliste = logical(),
    nordi = logical(), quebio = logical(), autres_db = logical(), autres_spec_db = character(),
    comments = character(), stringsAsFactors = FALSE)

# Références
taxa_ref <- colnames(df)[7:19]
type_ref <- colnames(df)[20:23]
enviro_ref <- colnames(df)[25:27]
db_ref <- colnames(df)[29:39]



# Reinit form and current rec
reinit <- function(rec) {
    # On réinitialise le formulaire à blanc
    reset("formulaire")
    rec$form <- df
    rec$map <- list()
}

# Save if rec change


# Update form based on responses
updateForm <- function(data) {
    updateSelectInput(session, "sample", choices = list(Ponctuelle = 1, Récurrente = 2),
        selected = data[1, 1])

    # update date range or date
    updateDateRangeInput(session, "yearRange", start = paste(data[1, 2]), end = data[1,
        3])

    updateNumericInput(session, "sampleNo", value = data[1, 4])

    updateDateInput(session, "year", value = data[1, 5])

    # update context
    updateRadioButtons(session, "context", choices = list(Observations = 1, `Données expérimentales` = 2),
        selected = data[1, 6])

    # update taxa
    updateCheckboxGroupInput(session, "taxa", choices = list(Mammifères = "mamm",
        `Mammifères marins` = "mamm_marins", Oiseaux = "oiseaux", Amphibiens = "amph",
        Reptiles = "rept", Poissons = "poiss", Arthropodes = "arthr", "Invertébrés autre qu'arthropodes" = "autre_arthr",
        `Plantes vasculaires` = "plantes_vasc", `Plantes non-vasculaires` = "plantes_nonvasc",
        `Champignons, moisissures, levures` = "champ", `Organismes unicellulaires` = "unicell",
        Bactéries = "bact"), selected = colnames(data[, which(data[1, 7:19] ==
        TRUE) + 6]), inline = TRUE)

    # update data type
    updateCheckboxGroupInput(session, "type", choices = list(Occurence = "occur",
        `Abondance/Frequence` = "abond", `Données individuelles (Traits, Génétiques)` = "individu",
        Autres = "autres_type"), selected = colnames(data[, which(data[1, 20:23] ==
        TRUE) + 19]), inline = TRUE)

    # update text input for "other" option in data type
    updateTextInput(session, "autres_spec_type", value = paste(data[1, 24]))

    # update enviro
    updateCheckboxGroupInput(session, "enviro", choices = list(Aquatique = "aqua",
        Marin = "marin", Terrestre = "terre"), selected = colnames(data[, which(data[1,
        25:27] == TRUE) + 24]))

    # update share
    updateRadioButtons(session, "share", choices = list(Oui = 1, Non = 2), selected = data[1,
        28])

    # update db
    updateCheckboxGroupInput(session, "db", choices = list(`Canadensis/GBIF` = "gbif",
        DataONE = "dataone", Dryad = "dryad", `Encyclopedia of Life (EOL)` = "eol",
        `(ESA) data.esa.org` = "esa", `Fig Share` = "fig", Mangal = "mangal", `Le Naturaliste` = "naturaliste",
        `Nordicana D` = "nordi", Quebio = "quebio", Autre = "autres_db"), selected = colnames(data[,
        which(data[1, 29:39] == TRUE) + 28]), inline = TRUE)

    # update text input for "other" option in db
    updateTextInput(session, "autres_db_type", value = paste(data[1, 40]))

    # update text area for comments section
    updateTextAreaInput(session, "comments", value = paste(data[1, 41]))
}
