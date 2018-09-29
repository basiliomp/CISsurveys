
### Creating a complete voting behaviour variable combining two columns from each

# Set up --------------------------------------------

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


# Importing data ------------------------------------


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
# So far, CIS2 and CIS3 seem to be equal and less useful than CIS1.

##################
# MAIN PROBLEM: DATA OF DIFFERENT TYPES DEPENDING ON HOW WE CALL IT OR SUBSET IT!
##################

  # CIS1 analysis (read_spss) <Labelled SPSS double>
  CIS1[,general[[x, "Voto.reciente"]]]
  class(CIS1[,general[[x, "Voto.reciente"]]])
  class(CIS1$p18a)
  typeof(CIS1[,general[[x, "Voto.reciente"]]])
  typeof(CIS1$p18a)
  attributes(CIS1[,general[[x, "Voto.reciente"]]])
  attributes(CIS1$p18a)
  
  # CIS2 analysis (foreign_to_labelled + read.spss)
  CIS2[,general[[x, "Voto.reciente"]]]
  str(CIS2[,general[[x, "Voto.reciente"]]])
  class(CIS2$p18a)
  typeof(CIS2$p18a)
  attributes(VIS2$p18a)
  # CIS3 analysis (read_spss)
  CIS3[,general[[x, "Voto.reciente"]]]
  str(CIS3[,general[[x, "Voto.reciente"]]])
  class(CIS3$p18a)
  typeof(CIS3$p18a)
  attributes(CIS3$p18a)


# Variable transformation ---------------------------

#Visualizacion del cruce que queremos agrupar en la nueva variable de voto

#In the loop it could be useful to extract the variable specification with these assginmnets:

CIS1$"Voto.reciente" <- CIS1[general[[x, "Voto.reciente"]]]
CIS1$"Otro.reciente" <- CIS1[,general[[x, "Otro.reciente"]]]
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
## The problem is getting the right type of data here while subsetting the rows we want to modify 
## or assign to a new vector/variable.


################ Alternative method case_when ################

## The function `case_when` allows for several rules to be applied in order over a vector.
## The problem is getting the right type of data here while subsetting the rows we want to modify 
## or assign to a new vector/variable.

table(CIS1$p15, CIS1$p18a) #Checking the bivariate distribution

case_when(CIS1[, "Otro.reciente"] == Otro.reciente.valor.voto ~ CIS1[, "Voto.reciente"],
          CIS1[, "Otro.reciente"] == 9 ~ "N.C. participacion")
"Error: NA column indexes not supported"

# But this one partially works, because the second condition is never applied 
# (there are no cases with 88 value once it is run)
case_when(CIS1$p15 == 1 ~ 81,
          CIS1$p15 == 9 ~ 88)



#Checking
CIS$Recuerdo.reciente
levels(CIS$Recuerdo.reciente)
table(CIS$Recuerdo.reciente, CIS$Voto.reciente, useNA = "always")

