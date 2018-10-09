
### Creating a complete voting behaviour variable combining two columns from each

# Set up --------------------------------------------

# Loading packages required install.packages("tidyverse", "labelled", "foreign", "haven", "survey", "WriteXLS")
library(tidyverse)
library(labelled)
library(foreign)
library(haven)
library(survey)
library(WriteXLS)
library(surveydata)

# Set your working directory
project_root <- "D:/Dropbox/AI_ELEC_AUT/Encuestas"
setwd(project_root)
# Reading index database into R
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )

# Importing data ------------------------------------

recuerdodevoto <- function(x) {
  # Setting working directory for current survey file
  setwd(paste0(project_root, general[x, "Folder"]))
  
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
  
  CIS$Recuerdo.reciente <- as.character(levels(CIS$Voto.reciente))[1]
  
  # Loop proposal for complete voting behaviour	# Loop proposal for complete voting behaviour
  for (y in 1:nrow(CIS)) {
    if (CIS[y, "Otro.reciente"][[1]] == "Fue a votar y vot.") {
      CIS[y, "Recuerdo.reciente"][[1]] <- as.character(CIS[y, "Voto.reciente"][[1]])
    } else if (CIS[y, "Otro.reciente"][[1]] == "S. que vot.") {  
      CIS[y, "Recuerdo.reciente"][[1]] <- as.character(CIS[y, "Voto.reciente"][[1]])
    } else if (CIS[y, "Otro.reciente"] == "N.C.") {
      CIS[y, "Recuerdo.reciente"][[1]] <- "N.C. participacion"
    } else {
      CIS[y, "Recuerdo.reciente"][[1]] <- "Abstencion"
    }
  }
  
  table(CIS$Recuerdo.reciente, CIS$Voto.reciente, useNA = "always")
}
