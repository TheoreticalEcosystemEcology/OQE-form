# data.frame vide pour ajouter les réponses
df <- data.frame(sample = character(), yearBegin = character(), yearEnd = character(),
    sampleNo = integer(), context = character(), mamm = logical(),
    mamm_marins = logical(), oiseaux = logical(), amph = logical(), rept = logical(),
    poiss = logical(), arthr = logical(), autre_arthr = logical(), plantes_vasc = logical(),
    plantes_nonvasc = logical(), champ = logical(), unicell = logical(), bact = logical(),
    status = logical(), preoccupante = logical(), menacee = logical(), voie_disp = logical(),
    commerciale = logical(), autres_status = logical(), autres_spec_status = character(),
    occur = logical(), abond = logical(), individu = logical(), autres_type = logical(),
    autres_spec_type = character(), aqua = logical(), marin = logical(), terre = logical(),
    crsng = logical(), frqnt = logical(), autres_finance = logical(), autres_spec_finance = character(),
    doi = character(), shared = character(), gbif = logical(), dataone = logical(),
    dryad = logical(), eol = logical(), esa = logical(), fig = logical(), mangal = logical(),
    naturaliste = logical(), nordi = logical(), quebio = logical(), autres_db = logical(),
    autres_spec_db = character(), comments = character(), stringsAsFactors = FALSE)

# Références
taxa_ref <- colnames(df)[6:18]
status_ref <- colnames(df)[20:24]
type_ref <- colnames(df)[26:29]
enviro_ref <- colnames(df)[31:33]
finance_ref <- colnames(df)[34:36]
db_ref <- colnames(df)[40:50]

taxa_possib <- c("mamm", "mamm_marins", "oiseaux", "amph", "rept", "poiss", "arthr", "autre_arthr", "plantes_vasc", "plantes_nonvasc", "champ", "unicell", "bact")
status_possib <- c("preoccupante", "menacee", "voie_disp", "commerciale", "autres_status")
type_possib <- c("occur", "abond", "individu", "autres_type")
enviro_possib <- c("aqua", "marin", "terre")
finance_possib <- c("crsng", "frqnt", "autres_finance")
db_possib <- c("gbif", "dataone", "dryad", "eol", "esa", "fig", "mangal", "naturaliste", "nordi", "quebio", "autres_db")

# References Edited and Deleted
ids_edit <- c()
ids_delete <- c()
pos_delete <- c()



# Reinit form and current rec
reinit <- function(rec) {
    # On réinitialise le formulaire à blanc
    reset("formulaire")
    rec$form <- df
    rec$map <- list()
}

# Update form based on responses
updateForm <- function(data) {
    updateSelectInput(session, "sample", choices = list(Ponctuelle = 1, Récurrente = 2),
        selected = data[1, 1])

    # update date range or date
    updateDateRangeInput(session, "yearRange", start = paste(data[1, 2]), end = data[1,
        3])

    updateNumericInput(session, "sampleNo", value = data[1, 4])

    # update context
    updateRadioButtons(session, "context", choices = list(`Données empiriques` = 1, `Données expérimentales` = 2),
        selected = data[1, 5], inline = TRUE)

    # update taxa
    updateCheckboxGroupInput(session, "taxa", choices = list(Mammifères = "mamm",
        `Mammifères marins` = "mamm_marins", Oiseaux = "oiseaux", Amphibiens = "amph",
        Reptiles = "rept", Poissons = "poiss", Arthropodes = "arthr", "Invertébrés autres qu'arthropodes" = "autre_arthr",
        `Plantes vasculaires` = "plantes_vasc", `Plantes non vasculaires` = "plantes_nonvasc",
        `Champignons, moisissures, levures` = "champ", `Organismes unicellulaires` = "unicell",
        Bactéries = "bact"), selected = taxa_possib[which(data[1,taxa_possib] == TRUE)], inline = TRUE)

    # update status
    updateRadioButtons(session, "status", choices = list(Oui = 1, Non = 0), selected = data[1, 19], inline = TRUE)

    updateCheckboxGroupInput(session, "sp_status", choices = list(Préoccupante = "preoccupante",
        Menacée = "menacee", `En voie de disparition` = "voie_disp", "Espèces d'importance commerciale" = "commerciale",
        Autres = "autres_status"), selected = status_possib[which(data[1,status_possib] == TRUE)], inline = TRUE)

    # update autres_spec_status
    updateTextInput(session, "autres_spec_status", value = paste(data[1, 25]))

    # update data type
    updateCheckboxGroupInput(session, "type", choices = list(Occurrences = "occur",
        `Abondances/Frequences` = "abond", `Données individuelles (traits, génétiques)` = "individu",
        Autres = "autres_type"), selected = type_possib[which(data[1,type_possib] == TRUE)], inline = TRUE)

    # update text input for 'other' option in data type
    updateTextInput(session, "autres_spec_type", value = paste(data[1, 30]))

    # update enviro
    updateCheckboxGroupInput(session, "enviro", choices = list(Aquatique = "aqua",
        Marin = "marin", Terrestre = "terre"), selected = enviro_possib[which(data[1,enviro_possib] == TRUE)], inline = TRUE)

    # update finance
    updateCheckboxGroupInput(session, "finance", choices = list(CRSNG = "crsng",
        frqnt = "frqnt", Autres = "autres_finance"), selected = finance_possib[which(data[1,finance_possib] == TRUE)], inline = TRUE)

    # update autres_spec_finance
    updateTextInput(session, "autres_spec_finance", value = paste(data[1, 37]))

    # update doi
    updateTextInput(session, "doi", value = paste(data[1, 38]))

    # update shared
    updateRadioButtons(session, "shared", choices = list(Oui = 1, Non = 0), selected = data[1, 39], inline = TRUE)

    # update db
    updateCheckboxGroupInput(session, "db", choices = list(`Canadensys/GBIF` = "gbif",
        DataONE = "dataone", Dryad = "dryad", `Encyclopedia of Life (EOL)` = "eol",
        `(ESA) data.esa.org` = "esa", `Fig shared` = "fig", Mangal = "mangal", `Le Naturaliste` = "naturaliste",
        `Nordicana D` = "nordi", Quebio = "quebio", Autres = "autres_db"), selected = db_possib[which(data[1,db_possib] == TRUE)], inline = TRUE)

    # update text input for 'other' option in db
    updateTextInput(session, "autres_spec_db", value = paste(data[1, 51]))

    # update text area for comments section
    updateTextAreaInput(session, "comments", value = paste(data[1, 52]))
}
