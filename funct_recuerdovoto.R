recuerdovoto_completo <- function(x) {  
### Creating a complete voting behaviour variable combining two columns from each
# For this project usage only!
# This is not a general application function, and will not work out of the CISsurvey project.
  # Setting working directory for current survey file
  setwd(paste0(project_root, general[x, "Folder"]))
  
    # Reading survey data from SPSS into R with `foreign`. Alternatively: haven::read_spss(file = general[[x, "Savfile"]], user_na = TRUE)
  CIS <- foreign_to_labelled(read.spss(file = general[[x, "Savfile"]],
                                       to.data.frame = TRUE, 
                                       reencode = 'utf-8',
                                       use.value.labels = TRUE))
  
  # Assign relevant variable into a dictinctly named new variable # Voto.reciente
  CIS$Voto.reciente <- subset(CIS, select = general[[x, "Voto.reciente"]])
  CIS$Voto.reciente <- as_vector(CIS$Voto.reciente)
  
  # Assign relevant variable into a dictinctly named new variable # Otro.reciente
  CIS$Otro.reciente <- subset(CIS, select = general[[x, "Otro.reciente"]])
  CIS$Otro.reciente <- as_vector(CIS$Otro.reciente)
  
  CIS$RECUERDO <- as.character(levels(CIS$Voto.reciente))[1]
  
  # Loop proposal for complete voting behaviour	# Loop proposal for complete voting behaviour
  for (y in 1:nrow(CIS)) {
    if (CIS[y, "Otro.reciente"][[1]] == "Fue a votar y vot.") {
      CIS[y, "RECUERDO"][[1]] <- as.character(CIS[y, "Voto.reciente"][[1]])
    } else if (CIS[y, "Otro.reciente"][[1]] == "S. que vot.") {  
      CIS[y, "RECUERDO"][[1]] <- as.character(CIS[y, "Voto.reciente"][[1]])
    } else if (CIS[y, "Otro.reciente"] == "N.C.") {
      CIS[y, "RECUERDO"][[1]] <- "N.C. participacion"
    } else {
      CIS[y, "RECUERDO"][[1]] <- "Abstencion"
    }
  }
  table(CIS$RECUERDO, CIS$Voto.reciente, useNA = "always")
  return(CIS$RECUERDO)
}
View(general)
