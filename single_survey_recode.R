######################
### Creating a complete voting behaviour variable combining two columns from each

#################### 
### Set up ###
#################### 

# Loading packages required install.packages("tidyverse", "labelled", "foreign", "haven", "survey", "WriteXLS")
library(tidyverse)
library(labelled)
library(foreign)
library(haven)
library(survey)
library(WriteXLS)
devtools::install_github("r-lib/lobstr")
library(lobstr)

# Set working directory
project_root <- "D:/Dropbox/AI_ELEC_AUT/Encuestas"
setwd(project_root)
# Reading index database into R
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )

#################### 
### Importing data ###
#################### 

# For this toy example:
x <- 116

# Setting working directory for current survey file
setwd(paste0(project_root, general[x, "Folder"]))

# Choose import method!
CIS1 <- read_spss(file = general[[x, "Savfile"]], user_na = TRUE)

CIS2 <- foreign_to_labelled(read.spss(file = general[[x, "Savfile"]],
                                     to.data.frame = TRUE, 
                                     reencode = 'utf-8',
                                     use.value.labels = TRUE))
CIS3 <- read.spss(file = general[[x, "Savfile"]],
                                      to.data.frame = TRUE, 
                                      reencode = 'utf-8',
                                      use.value.labels = TRUE)

# Import method comparison
  # CIS1 analysis (read_spss)
  CIS1[,general[[x, "Voto.reciente"]]]
  str(CIS1[,general[[x, "Voto.reciente"]]])
  # CIS1 analysis (read.spss)
  CIS2[,general[[x, "Voto.reciente"]]]
  str(CIS2[,general[[x, "Voto.reciente"]]])
  # CIS3 analysis (read_spss)
  CIS3[,general[[x, "Voto.reciente"]]]
  str(CIS3[,general[[x, "Voto.reciente"]]])


#################### 
### Variables transformation ###
#################### 

#Visualizacion del cruce que queremos agrupar en la nueva variable de voto

#In the loop it could be useful to extract the variable specification with these assginmnets:

CIS$"Voto.reciente" <- as.vector(CIS[general[[x, "Voto.reciente"]]])
CIS$"Otro.reciente" <- as.vector(CIS[,general[[x, "Otro.reciente"]]])
"Otro.reciente.valor.voto" <- general[[x, "Otro.reciente.valor.voto"]]


# Then, once the variables we want to modify are set, this part of the function would work in a more elegant way:
table(CIS$Voto.reciente, CIS$Otro.reciente, useNA = "always")


# For the next loop to work, I need `CIS` to be a standard data.frame, and not a tibble.
"https://stackoverflow.com/questions/11612235/select-rows-from-a-data-frame-based-on-values-in-a-vector"

######################################################
CIS <- as.data.frame(CIS)


################ Loop proposal for complete voting behaviour ################

#Placeholder
CIS$Recuerdo.reciente <- 0

for (y in 1:length(CIS)) {
  if (CIS[[y, "Otro.reciente"]] == CIS[[y, "Otro.reciente.valor.voto"]]) {
    CIS[y, "Recuerdo.reciente"] <- CIS[y, "Voto.reciente"]
  }
  }
# else if (CIS[y, "Otro.reciente"] == 9) {
#     CIS[y, "Recuerdo.reciente"] <- "N.C. participacion"
#   } else {
#     CIS[y, "Recuerdo.reciente"] <- "Abstencion"
#   }
# }

################ Alternative method if_else ################

CIS[,"Recuerdo.reciente"] <- if_else(condition = CIS[, "Otro.reciente"] == Otro.reciente.valor.voto, 
                                    true = CIS[, "Voto.reciente"],
                                    false =  if_else(condition = CIS[, "Otro.reciente"] == 9,
                                                true = "N.C. participacion",
                                                false = "Abstención"))
"Error: NA column indexes not supported"


################ Alternative method case_when ################

## Crear con esta fÃ³rmula un conjunto de reglas que cumplir y los datos a introducri segÃºn se cumplan. 
## Luego en TRUE pones quÃ© hacer si ninguna de las condiciones se cumpla

case_when(CIS[, "Otro.reciente"] == Otro.reciente.valor.voto ~ CIS[, "Voto.reciente"],
          CIS[, "Otro.reciente"] == 9 ~ "N.C. participacion")
"Error: NA column indexes not supported"


#Checking
CIS$Recuerdo.reciente
levels(CIS$Recuerdo.reciente)
table(CIS$Recuerdo.reciente, CIS$Voto.reciente, useNA = "always")

