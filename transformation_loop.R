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
  
  #Alternatively
  # CIS <- read_spss(file = general[[x, "Savfile"]],
  #                                      to.data.frame = TRUE, 
  #                                      reencode='utf-8',
  #                                      use.value.labels = TRUE)
  
  #################### 
  ### Variables transformation ###
  #################### 
  
  # RECUERDO para el voto reciente (no missing values)
  if (!is.na(general[[x,"Voto.reciente"]]) & general[[x,"Voto.reciente"]] != "-") {
      CIS$RECUERDO <- CIS[[general[[x,"Voto.reciente"]]]]
      # #########
      #   } else if ("¿cómo debería combinar la variable votoreciente con la complementaria otro.reciente?") {
      #   CIS$RECUERDO <- CIS[[general[[x,"Otro.reciente"]]]]
    }  else {
      general[x, "Looperror"] <- print(paste("Lack of VOTO RECIENTE in", general$Token[[x]]))
  }
  
  # RVOTOAUT para las elecciones autonómicas del ciclo pasado (no missing values)
  if (!is.na(general[x,"Voto.pasado"])){
      CIS$RVOTOAUT <- CIS[[general[[x,"Voto.pasado"]]]]
    } else {
      general[x, "Looperror"] <- print(paste("Lack of VOTO PASADO in", general$Token[[x]]))
  }
  
  # # RVOTOGEN para las elecciones generales del ciclo pasado (no missing values)
  if (!is.na(general[x,"Voto.generales"])){
      CIS$RVOTOGEN <- CIS[[general[[x,"Voto.generales"]]]]
    } else {
      general[x, "Looperror"] <- print(paste("Lack of GENERALES in", general$Token[[x]]))
  }
  
  # ESTUDIOS (renombrar)
  if (!is.na(general[x, "Estudios"])){
      CIS$ESTUDIOS <- CIS[[general[[x,"Estudios"]]]]
    # } else {
    #   general[x, "Looperror"] <- print(paste("Lack of ESTUDIOS in", general$Token[[x]]))
  }
  
  # EDAD (7 grupos)
  if (!is.na(general[x,"Edad"])){
      CIS$EDAD <- CIS[[general[[x,"Edad"]]]]
    } else {
      general[x, "Looperror"] <- print(paste("Lack of EDAD in", general$Token[[x]]))
  }
  
  # OCUPACIÓN (renombrar)
  if (!is.na(general[x,"Ocupacion"])) {
      CIS$OCUPA <- CIS[[general[[x,"Ocupacion"]]]]
    # } else {
    #   general[x, "Looperror"] <- print(paste("Lack of OCUPACION in", general$Token[[x]]))
  }

  #################### 
  ### Tabulación y exportación a EXCEL ###
  #################### 
  
  # Nueva función para darle un título a las tablas de recuerdo de voto
  write.table.header <- function(x, file, header){
    cat(header, '\n',  file = file)
    write.table(x= x, file = file, col.names = NA, sep = ";", dec = ",", append = T, row.names = T, na = "")
  }
  
  # Tabulation Loop
  if (general[x,"Encuesta"] != "post") {
    print("TABLES NOT AVAILABLE")
  } else {
  # TABLAS COMPARATIVAS CON ELECCIONES AUTONÓMICAS
  # Aquí difiere el tratamiento de las encuestas con ponderaciones y las que no tienen.
    if (!is.na(general[x,"Ponderacion"]) & !is.na(general[x,"Estrato"]) & general[x,"Encuesta"] == "post") {
    # Declare data to be survey data and weight it accordingly (if needed)
    CISweight <- svydesign(ids= ~1, strata=~CIS[,general[[x,"Estrato"]]],
                           weights=~CIS[,general[[x,"Ponderacion"]]], data = CIS)
    tab.auto <- svytable(~RECUERDO+RVOTOAUT, design = CISweight)
    } else if (general[x,"Encuesta"] == "pre") {
      print("TABLES NOT AVAILABLE")
      } else {
      # Guardamos como una tabla el elemento que llamaremos desde las distintas tabulaciones a realizar
      tab.auto <- table(CIS$RECUERDO, CIS$RVOTOAUT)
      }
  }
  
    
  if (general[x,"Encuesta"] == "pre") {
      print("TABLES NOT AVAILABLE")
    } else {
      #Tabla de transferencias desde pasadas autonómicas en términos absolutos
      write.table.header(x = tab.auto, file = paste(general[x,"Token"], "AUTO_abs.csv", sep = "_"),
                        header = "Voto reciente (filas) y en pasadas elecciones autonómicas (columnas) en número absolutos")
      
      #* #Tabla de transferencias porcentuales desde pasadas autonómicas por fila
      write.table.header(x = round(prop.table(tab.auto, margin = 1), digits = 4)*100, #margin=1 es para % por fila
                         file = paste(general[x,"Token"], "AUTO_perc_fila.csv", sep = "_"),
                         header = "Voto reciente (filas) y en pasadas elecciones autonómicas (columnas) en % por fila")
      
      #*  #Tabla de transferencias porcentuales desde pasadas autonómicas por columna
      write.table.header(x = round(prop.table(tab.auto, margin = 2), digits = 4)*100, #margin=2 es para % por columna
                         file = paste(general[x,"Token"], "AUTO_perc_colu.csv", sep = "_"),
                         header = "Voto reciente (filas) y en pasadas elecciones autonómicas (columnas) en % por columnas")
  }
  
  
  # TABLAS COMPARATIVAS CON ELECCIONES GENERALES
  # Aquí difiere el tratamiento de las encuestas con ponderaciones y las que no tienen.
  if (!is.na(general[x,"Ponderacion"]) & !is.na(general[x,"Estrato"]) & general[x,"Encuesta"] == "post") {
        # Declare data to be survey data and weight it accordingly (if needed)
        CISweight <- svydesign(ids= ~1, strata=~CIS[,general[[x,"Estrato"]]],
                               weights=~CIS[,general[[x,"Ponderacion"]]], data = CIS)
        tab.gen <- svytable(~RECUERDO+RVOTOGEN, design = CISweight)
      } else if (general[x,"Encuesta"] == "pre") {
        print("TABLES NOT AVAILABLE")
      } else {
        # Guardamos como una tabla el elemento que llamaremos desde las distintas tabulaciones a realizar
        tab.gen <- table(CIS$RECUERDO, CIS$RVOTOGEN)
      }
  
  if (general[x,"Encuesta"] == "pre") {
      print("TABLES NOT AVAILABLE")
        } else {
      #Tabla de transferencias desde pasadas generales absolutas
      write.table.header(x = tab.gen, file = paste(general[x,"Token"], "GEN_abs.csv", sep = "_"),
                         header = "Voto reciente (filas) y en pasadas elecciones generales (columnas) en número absolutos")
      
      #*  #Tabla de transferencias porcentuales desde pasadas generales por fila
      write.table.header(x = round(prop.table(tab.gen, margin = 1), digits = 4)*100, #margin=1 es para % por fila
                         file = paste(general[x,"Token"], "GEN_perc_fila.csv", sep = "_"),
                         header = "Voto reciente (filas) y en pasadas elecciones generales (columnas) en % por fila")
      
      #*  #Tabla de transferencias porcentuales desde pasadas generales por columna
      write.table.header(x = round(prop.table(tab.gen, margin = 2), digits = 4)*100, #margin=2 es para % por columna
                         file = paste(general[x,"Token"], "GEN_perc_colu.csv", sep = "_"),
                         header = "Voto reciente (filas) y en pasadas elecciones generales (columnas) en % por columna")
            }

  #################### 
  ### Exportación a SPSS ###
  #################### 
  
  #For EXPORTING the data it is better to use haven's function. Labelled data is read correctly by SPSS.
  write_sav(CIS, path = paste("nuevo", general[x,"Savfile"]))
  
  # Pequeño contador de iteraciones con sello temporal
  if (x %% 50 == 0) { 
    paste0("Progress: ", x, " out of ", nrow(general), " iterations completed.")
    timestamp()
  }

}
}

################# END OF LOOP ###################