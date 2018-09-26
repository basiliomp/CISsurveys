### Data manipulation from SPSS: Encuestas electorales autonómicas del CIS

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
#library(Writexl) # ¿PROBAR ESTE PAQUETE NUEVO?

agegroup <- function(agevector) { 
  new_age_vector <- as.numeric(agevector)
  
  factor(ifelse(new_age_vector < 25, "18-24", 
                ifelse(new_age_vector > 24 & new_age_vector < 35, "25-34", 
                       ifelse(new_age_vector > 34 & new_age_vector < 45, "35-44", 
                              ifelse(new_age_vector > 44 & new_age_vector < 55, "45-54", 
                                     ifelse(new_age_vector > 54 & new_age_vector < 65, "55-64", 
                                            ifelse(new_age_vector > 64 & new_age_vector < 75, "65-74", 
                                                   ifelse(new_age_vector > 74, "75+", "NA")
                                            )
                                     )
                              )
                       )
                )
  )
  )
  
}

# Set working directory
project_root <- "D:/Dropbox/AI_ELEC_AUT/Encuestas"
setwd(project_root)

#Fichero general de trabajo
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )


################# LOOP ###################

for (x in 1:nrow(general)) { 
  
  #################### 
  ### Importing data ###
  #################### 
  # Setting working directory for current survey file
  setwd(paste0(project_root, general[x, "Folder"]))
  
  # Leyendo los datos en formato SPSS en R con la función `foreign`:
  CIS <- foreign_to_labelled(read.spss(file = general[[x, "Savfile"]],
                                       to.data.frame = TRUE, 
                                       reencode = 'utf-8',
                                       use.value.labels = TRUE))
  
  #Visualización del cruce que queremos agrupar en la nueva variable de voto
#  table(CIS[[general[[x,"Otro.reciente"]]]],CIS[[general[[x,"Voto.reciente"]]]], useNA = "always")
  

  if (!is.na(general[x, "Otro.reciente"])) {
      if (CIS[[general[[x, "Otro.reciente"]]]] == CIS[[general[[x, "Otro.reciente.valor.voto"]]]]
          CIS[[general[[x, "Otro.reciente.valor.voto"]]]] == 5) {
        sapply(X = CIS[[general[[x, "Otro.reciente"]]]], FUN = print)
  } if_else(condition = CIS[[general[[x, "Otro.reciente"]]]] == "N.C.",
            true = CIS$RVCOMPLETO <- CIS[[general[[x,"Otro.reciente"]]]],
            false = CIS$RVCOMPLETO <- CIS[[general[[x,"Voto.reciente"]]]])

  }
  
  if (!is.na(general[[x,"Otro.reciente.valor.voto"]])) {
    print(getwd())
    print(general[[x, "Otro.reciente"]])
    print(levels(CIS[[general[[x, "Otro.reciente"]]]]))
    print(paste("_______________________", x, " out of", nrow(general), "_______________________"))
    }
  
}

