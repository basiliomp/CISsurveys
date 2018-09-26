
#PANEL 2000
pre <- foreign_to_labelled(read.spss(file = "AUT/ANDAL/2000/MD2382-pre2000AND/MD2382-pre2000.sav", to.data.frame = TRUE, use.value.labels = TRUE))
pos <- foreign_to_labelled(read.spss(file = "AUT/ANDAL/2000/MD2384-pos2000AND/MD2384-pos2000.sav", to.data.frame = TRUE, use.value.labels = TRUE))
panel <- inner_join(pre, pos, by="CUES")

#Pesos distintos para elecciones andaluzas y para elecciones generales analizadas en esa encuesta panel.






######################

general[[x, "Voto.reciente"]]
general[[x, "Otro.reciente"]]


nueva_var <- if_else(is.na(CIS[[general[[x, "Voto.reciente"]]]]),
        CIS[[general[[x, "Otro.reciente"]]]],
        CIS[[general[[x, "Voto.reciente"]]]])


## Crear con esta fórmula un conjunto de reglas que cumplir y los datos a introducri según se cumplan. 
## Luego en TRUE pones qué hacer si ninguna de las condiciones se cumplía.

case_when(condition 1 ~ 1,
          condiion 2 & condition 3 & conditin 4 ~ 2,
          TRUE ~ 3)






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

general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )

#################### 
### Importing data ###
#################### 

CIS <- foreign_to_labelled(read.spss(file = "MD2795-pos2009PV.sav",
  to.data.frame = TRUE, 
  reencode = 'utf-8',
  use.value.labels = TRUE))


#################### 
### Variables transformation ###
#################### 

#Visualización del cruce que queremos agrupar en la nueva variable de voto

#In the loop it could be useful to extract the variable specification with these assginmnets:
# For this toy example:
x <- 118

CIS$"Voto.reciente" <- CIS[,general[[x, "Voto.reciente"]]]
CIS$"Otro.reciente" <- CIS[,general[[x, "Otro.reciente"]]]

# Then, once the variables we want to modify are set, this part of the function would work in a more elegant way:
table(CIS[[Oreciente]], CIS[[Vreciente]], useNA = "always")


# For the next loop to work, I need `CIS` to be a standard data.frame, and not a tibble.
https://stackoverflow.com/questions/11612235/select-rows-from-a-data-frame-based-on-values-in-a-vector

######################################################
CIS <- as.data.frame(CIS)

? "%in%"
1:10 %in% c(1,3,5,9)


# Loop proposal for complete voting behaviour
for (x in 1:length(CIS)) {
  if (CIS[x, "Otro.reciente"] == "Fue a votar y vot.") {
    CIS[x, "Recuerdo.reciente"] <- CIS[x, "Voto.reciente"]
  } else if (CIS[x, "Otro.reciente"] == "S. que vot.") {
    CIS[x, "Recuerdo.reciente"] <- CIS[x, "Voto.reciente"]
  } else if (CIS[x, "Otro.reciente"] == "N.C.") {
    CIS[x, "Recuerdo.reciente"] <- "N.C. participación"
  } else {
    CIS[x, "Recuerdo.reciente"] <- "Abstención"
  }
}

#Checking
CIS$Recuerdo.reciente
levels(CIS$Recuerdo.reciente)
table(CIS$Recuerdo.reciente, CIS$Voto.reciente, useNA = "always")

