### Data manipulation from SPSS: Encuestas electorales autonómicas del CIS

#################### 
### Set up ###
#################### 

# Loading packages required
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
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet= "tabla", skip = 1, col_names = T )

######################
# # Objeto con el listado completo de archivos de trabajo (¡ojo con el working directory activo!)
# files <- list.files(full.names = T, recursive = T)
# 
# # Listado de ficheros .sav con ruta completa (spss)
# # El símbolo $ indica que hay que empezar a buscar por el final de la string.
# # El punto indica que puede venir cualquier símbolo antes de "sav"
# savpath <- grep(value = T, pattern = ".sav$", files)
# savpath <- str_replace(savpath, "^\\.", "") #sin el punto que tenían todas las entradas al inicio.
# 
# # Listado de rutas donde se encuentran los ficheros .sav
# savfolders <- str_extract(savpath, "/?[^/]*/?[^/]*/?[^/]*/?[^/]*")
# 
# # Listado de los nombres de los ficheros .sav (spss)
# # El código "[^/]" busca cualquier símbolo excepto "/" empezando por .sav desde el final ($)
# savfiles <- str_extract(string = savpath, pattern = "/[^/]*\\.sav$")
# 
# # Token identificador de cada encuesta para identificar archivos derivados
# # ¿Es necesario esto o vamso a sacar los ficheros usando files?
# general$Token <- paste0("MD", 
#                        gsub(pattern = ".*/MD|\\.sav.*", replacement = "", x = general$Path))
# 
# pattern = "/[^/]*\\.sav$"  # ! # ! # ! 
# 
# # Ruta en el sistema de archivos donde leer y guardar los datos de cada encuesta
# #Con esto extraes los tres caracteres del tipo de convocatoria
# regmatches(general$Path, regexpr("[^/]{3}", general$Path))
# regmatches(general$Path, regexpr("\\w{3}", general$Path)) #Hace lo mismo
# 
# # Con replace pattern = "" podría ir eliminando partes del path y almacenando los valores en distintas
# # columnas del data frame general. Así tendría por separado el tipo de elección, el año, la ComAut, etc.
# 
# regmatches(general$Path, regexpr("/?[^/]*/?[^/]*/?[^/]*", general$Path)) #Esto saca tipo elecc + ComAut
# 
# general$Route <- regmatches(general$Path, regexpr("/?[^/]*/?[^/]*/?[^/]*/?[^/]*", general$Path)) # Path de carpeta encuesta
# 
# regmatches(general$Path, regexpr("/?[^/]*/?[^/]*/?[^/]*/?[^/]*/?[^/]*", general$Path)) # Path for Sav file
# 
# general$Folder <- paste("MD", 
#                        gsub(pattern = ".*/MD|\\.sav.*", replacement = "", x = general$Path),
#                        sep = "")


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
                                       reencode='utf-8',
                                       use.value.labels = TRUE))
  
  #Visualización del cruce que queremos agrupar en la nueva variable de voto
  table(CIS[[general[[x,"Otro.reciente"]]]],CIS[[general[[x,"Voto.reciente"]]]], useNA = "always")
  
  mylist <- as.list(CIS[[general[[x, "Otro.reciente"]]]])
  if (!is.na(general[x, "Otro.reciente"])) {
      if (CIS[[general[[x, "Otro.reciente"]]]] == "Fue a votar y vot." |
          CIS[[general[[x, "Otro.reciente"]]]] == "S. que vot.") {
        sapply(X = CIS[[general[[x, "Otro.reciente"]]]], FUN = print)
  } if_else(condition = CIS[[general[[x, "Otro.reciente"]]]] == "N.C.", 
            true = CIS$RVCOMPLETO <- CIS[[general[[x,"Otro.reciente"]]]], 
            false = CIS$RVCOMPLETO <- CIS[[general[[x,"Voto.reciente"]]]]) 
  
  print(getwd())
  print(levels(CIS[[general[[x, "Otro.reciente"]]]]))
  print(paste("_______________________", x, " out of", nrow(general), "_______________________"))
}
}