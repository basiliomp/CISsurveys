### Data manipulation from SPSS: Encuestas electorales autonómicas del CIS

# Set up --------------------------------------------------------------

# Loading packages required
library(tidyverse)
library(labelled)
library(foreign)
library(haven)
library(readxl)
library(survey)
library(WriteXLS)
#library(Writexl) # ¿PROBAR ESTE PAQUETE NUEVO?

# Set your working directory
project_root <- "D:/Dropbox/AI_ELEC_AUT/Encuestas"
setwd(project_root)

# Reading index database into R
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )

# Loop ----------------------------------------------------------------

for (x in 1:nrow(general)) { 
  
  # Setting working directory for current survey file
  setwd(paste0(project_root, general[x, "Folder"]))

  # Importing data ------------------------------------------------------
  
    # Reading survey data from SPSS into R with `foreign`. Alternatively: haven::read_spss(file = general[[x, "Savfile"]], user_na = TRUE)
    CIS <- foreign_to_labelled(read.spss(file = general[[x, "Savfile"]],
                                         to.data.frame = TRUE, 
                                         reencode='utf-8',
                                         use.value.labels = TRUE))

  # Variable transformation ---------------------------------------------
  
    # RECUERDO para el voto reciente (no missing values)
    ## Si el valor de Voto.reciente no está vacío o es un guion, lo asignamos a CIS$RECUERDO
    if (!is.na(general[[x,"Voto.reciente"]]) & general[[x,"Voto.reciente"]] != "-") {
        CIS$RECUERDO <- CIS[[general[[x,"Voto.reciente"]]]]
        #   } else if ("¿cómo debería combinar la variable votoreciente con la complementaria otro.reciente?") {
         "Fue a votar y vot." | "S. que vot." 
        #   CIS$RECUERDO <- CIS[[general[[x,"Otro.reciente"]]]]
        
      }  else {
        general[x, "Looperror"] <- print(paste("Lack of VOTO RECIENTE in", general$Token[[x]]))
    }
    
    # RVOTOAUT para las elecciones autonómicas del ciclo pasado (no missing values)
    if (!is.na(general[x,"Voto.pasado"])){
        CIS$RVAUTAGR <- CIS[[general[[x,"Voto.pasado"]]]]
      } else {
        general[x, "Looperror"] <- print(paste("Lack of VOTO PASADO in", general$Token[[x]]))
    }
    
    # # RVOTOGEN para las elecciones generales del ciclo pasado (no missing values)
    if (!is.na(general[x,"Voto.generales"])){
        CIS$RVGENAGR <- CIS[[general[[x,"Voto.generales"]]]]
      } else {
        general[x, "Looperror"] <- print(paste("Lack of GENERALES in", general$Token[[x]]))
    }
    
    # ESTUDIOS (renombrar)
    if (!is.na(general[x, "Estudios"])){
        CIS$ESTUDIOSAGR <- CIS[[general[[x,"Estudios"]]]]
      # } else {
      #   general[x, "Looperror"] <- print(paste("Lack of ESTUDIOS in", general$Token[[x]]))
    }
    
    # EDAD (7 grupos)
    if (!is.na(general[x,"Edad"])){
        CIS$EDADAGR <- CIS[[general[[x,"Edad"]]]]
      } else {
        general[x, "Looperror"] <- print(paste("Lack of EDAD in", general$Token[[x]]))
    }
    
    # OCUPACION (renombrar)
    if (!is.na(general[x,"Ocupacion"])) {
        CIS$OCUPAAGR <- CIS[[general[[x,"Ocupacion"]]]]
      # } else {
      #   general[x, "Looperror"] <- print(paste("Lack of OCUPACION in", general$Token[[x]]))
    }
    
    # ORIGEN (para encuestas en Catalunya) agregada manualmente CIS$ORIGENAGR
    
    
  # Writing tables into Excel --------------------------------------------
  
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
        autonotab(tab.auto)
    }
    
    
    # TABLAS COMPARATIVAS CON ELECCIONES GENERALES
    # Aquí difiere el tratamiento de las encuestas con ponderaciones y las que no tienen.
    if (!is.na(general[x,"Ponderacion"]) & !is.na(general[x,"Estrato"]) & general[x,"Encuesta"] == "post") {
          # Declare data to be survey data and weight it accordingly (if needed)
          CISweight <- svydesign(ids= ~1, strata=~CIS[,general[[x,"Estrato"]]],
                                 weights=~CIS[,general[[x,"Ponderacion"]]], data = CIS)
          tab.gen <- svytable(~RECUERDO+RVOTOGEN, design = CISweight)
        } else if (general[x,"Encuesta"] == "pre" & general[x,"Voto.generales"] != "") {
          print("TABLES NOT AVAILABLE")
          } else {
            # Guardamos como una tabla el elemento que llamaremos desde las distintas tabulaciones a realizar
            tab.gen <- table(CIS$RECUERDO, CIS$RVOTOGEN)
        }
    
    if (general[x,"Encuesta"] == "pre") {
        print("TABLES NOT AVAILABLE")
          } else {
        generaltab(tab.gen)
              }
  
  
  # Export to SPSS ------------------------------------------------------
  
    #For EXPORTING the data it is better to use haven's function. Labelled data is read correctly by SPSS.
    write_sav(CIS, path = paste("nuevo", general[x,"Savfile"]))
    
    # Contador de iteraciones con sello temporal
    if (x %% 50 == 0) { 
      paste0("Progress: ", x, " out of ", nrow(general), " iterations completed.")
      timestamp()
    }
  
}
