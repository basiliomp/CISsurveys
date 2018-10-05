
### Creating a complete voting behaviour variable combining two columns from each

# Set up --------------------------------------------

# Loading packages required install.packages("tidyverse", "labelled", "foreign", "haven", "survey", "WriteXLS")
library(tidyverse)
library(labelled)
library(foreign)
library(haven)
library(survey)
library(WriteXLS)

# Set your working directory
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

# Import method comparison:
# So far, CIS2 and CIS3 seem to be equal and less useful than CIS1.

##################
# MAIN PROBLEM: DATA OF DIFFERENT TYPES DEPENDING ON HOW WE CALL IT OR SUBSET IT!
##################

# Function to perform analysis of the different objects of interest (variables from surveys) and
# their features, which may change depending on how we call the object.

analyse <- function(object) {
  #Print the object to analyse
  print(object)
  #Print the structure of the object to analyse
  str(object)
  #Print the class of the object to analyse
  class(object)
  #Print the type of the object to analyse
  typeof(object)
  #Print the attributes related to the object to analyse
  attributes(object)
}
  
# CIS1 analysis (read_spss) <Labelled SPSS double>
  #Testing the object result of subsetting with second level subsetting
  analyse(CIS1[,general[[x, "Voto.reciente"]]])
  #Testing the object result of  subsetting with explicit call by variable name
  analyse(CIS1$p18a)
  
# CIS2 analysis (foreign_to_labelled + read.spss)
  #Testing the object result of  subsetting with second level subsetting
  analyse(CIS2[,general[[x, "Voto.reciente"]]])
  #Testing the object result of  subsetting with explicit call by variable name
  analyse(CIS2$p18a)
  
# CIS3 analysis (read.spss)
  #Testing the object result of  subsetting with second level subsetting
  analyse(CIS3[,general[[x, "Voto.reciente"]]])
  #Testing the object result of  subsetting with explicit call by variable name
  analyse(CIS3$p18a)


# Variable transformation ---------------------------

# Visualization of the two variables we want to consider for the transformation (example for the case in general[116,])
attributes(CIS1$p15)[5]
attributes(CIS1$p18a)[5]
table(CIS1$p15, CIS1$p18a, useNA = "always")

#In the loop it could be useful to extract the variable specification with these assginmnets:
CIS$"Voto.reciente" <- CIS[general[[x, "Voto.reciente"]]]
CIS$"Otro.reciente" <- CIS[,general[[x, "Otro.reciente"]]]
"Otro.reciente.valor.voto" <- general[[x, "Otro.reciente.valor.voto"]]


# Then, once the variables we want to modify are set, this part of the function would work in a more elegant way:
table(CIS$Voto.reciente, CIS$Otro.reciente, useNA = "always")

# For the next loop to work, I need `CIS` to be a standard data.frame, and not a tibble.
"https://stackoverflow.com/questions/11612235/select-rows-from-a-data-frame-based-on-values-in-a-vector"

######################################################
CIS <- as.data.frame(CIS)

################ Loop proposal for complete voting behaviour ################

#Placeholder
CIS$Recuerdo.reciente <- NA

for (y in 1:nrow(CIS)) {
  if (CIS[y, "Otro.reciente"][[1]] == "Fue a votar y vot.") {
    CIS[y, "Recuerdo.reciente"][[1]] <- CIS[y, "Voto.reciente"][[1]]
  }
  } #*Once that is working, adapt and add these other conditions:
# else if (CIS[y, "Otro.reciente"] == 9) {
#     CIS[y, "Recuerdo.reciente"] <- "N.C. participacion"
#   } else {
#     CIS[y, "Recuerdo.reciente"] <- "Abstencion"
#   }
# }

# Here we can test the result of the loop against the original variable
table(CIS$Recuerdo.reciente, CIS$p18a)
# NAs from the original variable have value 0 in the new one because the stay unchanged.
sum(is.na(CIS$p18a))


################ Alternative method if_else ################

CIS[,"Recuerdo.reciente"] <- if_else(condition = CIS[, "Otro.reciente"] == Otro.reciente.valor.voto, 
                                    true = CIS[, "Voto.reciente"],
                                    false =  if_else(condition = CIS[, "Otro.reciente"] == 9,
                                                true = "N.C. participacion",
                                                false = "Abstenc."))
"Error: NA column indexes not supported" # Using CIS1

"Error in `[.data.frame`(true, rep(NA_integer_, length(condition))) : 
  undefined columns selected" # Using CIS3

## The problem is getting the right type of data here while subsetting the rows we want to modify 
## or assign to a new vector/variable.


################ Alternative method case_when ################

## The function `case_when` allows for several rules to be applied in order over a vector.
## The problem is getting the right type of data here while subsetting the rows we want to modify 
## or assign to a new vector/variable.

table(CIS1$p15, CIS1$p18a) #Checking the bivariate distribution

case_when(CIS[, "Otro.reciente"] == Otro.reciente.valor.voto ~ CIS[, "Voto.reciente"],
          CIS[, "Otro.reciente"] == 9 ~ "N.C. participacion")
"Error: NA column indexes not supported"

# But this one partially works, because the second condition is never applied 
# (there are no cases with 88 value once it is run)
case_when(CIS1$p15 == 1 ~ 81,
          CIS1$p15 == 9 ~ 88)

################ Alternative method tidyverse-purrr? ################

cisvoto <- CIS1 %>%
          replace_na()
          
CIS1 <- left_join(CIS1, cisvoto)  


#Checking
CIS$Recuerdo.reciente
levels(CIS$Recuerdo.reciente)
table(CIS$Recuerdo.reciente, CIS$Voto.reciente, useNA = "always")

