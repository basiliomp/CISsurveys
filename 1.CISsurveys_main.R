### Data manipulation from SPSS: Encuestas electorales autonómicas del CIS

# Set up --------------------------------------------------------------

# Loading packages required
library(tidyverse)
library(lubridate)
library(labelled)
library(foreign)
library(haven)
library(readxl)
library(survey)
library(WriteXLS)
#library(Writexl) # Is it better?

# Set your working directory
project_root <- "D:/Dropbox/AI_ELEC_AUT/Encuestas"
setwd(project_root)

# Running code for functions thata are specific for this project
requiredfunctions <- list("agegroup",
                          "write_tab_header",
                          "autonotab",
                          "generaltab",
                          "intentab",
                          "voterecall",
                          "votevarname")

# Security check: Are all required functions loaded into the working space?
sum(map_lgl(requiredfunctions, exists)) == length(requiredfunctions)

# Reading index database into R
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )
# Special case surveys are pulled out of the main analysis
general <- filter(general, (!is.na(general$Token)))

# Loop ----------------------------------------------------------------
looplist <- list()
starttime <- now() #at the end now() - starttime
for (x in 1:nrow(general)) { 
  
  # Setting working directory for current survey file
  setwd(paste0(project_root, general[x, "Folder"]))
  
  # Importing data ------------------------------------------------------
  
  # Reading survey data from SPSS into R with `foreign`. Alternatively: haven::read_spss(file = general[[x, "Savfile"]], user_na = TRUE)
  CIS <- foreign_to_labelled(read.spss(file = general[[x, "Savfile"]],
                                       to.data.frame = TRUE, 
                                       reencode = 'utf-8',
                                       use.value.labels = TRUE))
  
  # Variable transformation ---------------------------------------------
  
  # RECUERDO para el voto reciente (*no missing values*)
  ## If Voto.reciente is not empty or "-", it is assigned to CIS$RECUERDO
  if (!is.na(general[[x,"Otro.reciente"]])) {
    # Assign relevant variable into a dictinctly named new variable # Voto.reciente
    CIS$Voto.reciente <- subset(CIS, select = general[[x, "Voto.reciente"]])
    CIS$Voto.reciente <- as_vector(CIS$Voto.reciente)
    
    # Assign relevant variable into a dictinctly named new variable # Otro.reciente
    CIS$Otro.reciente <- subset(CIS, select = general[[x, "Otro.reciente"]])
    CIS$Otro.reciente <- as_vector(CIS$Otro.reciente)
    
    #Placeholder for the vote recall variable (THIS SHOULD USE THE VOTEVARNAME FUNCTION)
    CIS$RECUERDO <- factor(x = 0, levels = unique(c(levels(CIS$Voto.reciente), 
                                                    levels(CIS$Otro.reciente),
                                                    "Abstencion", "N.C. participacion")))
    
    # Apply the tailored function in order to get a complete voting behaviour variable
    CIS <- voterecall(df = CIS)
    
    # Rename new variable to include data from year, time of survey and type of variable with votevarname()
    names(CIS)[which(names(CIS) == "RECUERDO")] <- as.character(votevarname(x))
    
    } else if (!is.na(general[[x,"Voto.reciente"]]) & general[[x,"Voto.reciente"]] != "-") {
       CIS$RECUERDO <- CIS[[general[[x,"Voto.reciente"]]]]
       
       # Rename new variable to include data from year, time of survey and type of variable with votevarname()
       names(CIS)[which(names(CIS) == "RECUERDO")] <- as.character(votevarname(x))
       
       } else {
    general[x, "Looperror"] <- print(paste("Lack of VOTO RECIENTE in", general$Token[[x]]))
  }
  
  # RVAUTAGR para las elecciones autonomicas del ciclo pasado (*no missing values*)
  if (!is.na(general[x,"Voto.pasado"])) {
    CIS$RVAUTAGR <- CIS[[general[[x,"Voto.pasado"]]]]
  } else {
    general[x, "Looperror"] <- print(paste("Lack of VOTO PASADO in", general$Token[[x]]))
  }
  
  # RVGENAGR para las elecciones generales del ciclo pasado (*no missing values*)
  if (!is.na(general[x,"Voto.generales"])) {
    CIS$RVGENAGR <- CIS[[general[[x,"Voto.generales"]]]]
  } else {
    general[x, "Looperror"] <- print(paste("Lack of GENERALES in", general$Token[[x]]))
  }
  
  # INTV para las elecciones generales del ciclo pasado (*no missing values*)
  if (!is.na(general[x,"Intencion.voto"])) {
    CIS$INTVAGR <- CIS[[general[[x,"Intencion.voto"]]]]
  } else {
    general[x, "Looperror"] <- print(paste("Lack of Intencion.voto in", general$Token[[x]]))
  }
  
  # ESTUDIOS (renombrar)
  if (!is.na(general[x, "Estudios"])) {
    CIS$ESTUDIOSAGR <- CIS[[general[[x,"Estudios"]]]]
    # } else {
    #   general[x, "Looperror"] <- print(paste("Lack of ESTUDIOS in", general$Token[[x]]))
  }
  
  # EDAD (7 grupos)
  if (!is.na(general[x,"Edad"])) {
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
  
  ### Tabulation Loop

  ### REGIONAL ELECTION VOTING TABLES  
  
  if (general[x,"Encuesta"] == "post") {

    # Different code required for weighted and not weighted surveys.
    if (!is.na(general[x,"Ponderacion"]) & !is.na(general[x,"Estrato"])) {
      # Declare data to be survey data and weight it accordingly (if needed)
      CISweight <- svydesign(ids =  ~1, strata = ~CIS[,general[[x,"Estrato"]]],
                             weights = ~CIS[,general[[x,"Ponderacion"]]], data = CIS)
      
      tab_auto <- svytable(~RECUERDO + RVAUTAGR, design = CISweight)
      tab_gen <- svytable(~RECUERDO + RVGENAGR, design = CISweight)
      
      # Finally, we create and export the table into an Excel file with autonotab() and generaltab(), which rely on write_tab_header.
      autonotab(tab_auto)
      generaltab(tab_gen)
    
    } else if (!is.na(general[x,"Ponderacion"]) & is.na(general[x,"Estrato"])) {
        
        # Declare data to be survey data and weight it accordingly (if needed)
        CISweight <- svydesign(ids =  ~1, strata = NULL, weights = ~CIS[,general[[x,"Ponderacion"]]], data = CIS)
        
        tab_auto <- svytable(~RECUERDO + RVAUTAGR, design = CISweight)
        tab_gen <- svytable(~RECUERDO + RVGENAGR, design = CISweight)
        
        # Finally, we create and export the table into an Excel file with autonotab() and generaltab(), which rely on write_tab_header.
        autonotab(tab_auto)
        generaltab(tab_gen)
      
    } else {
      
      # Save as table the data tabulation in order to call it later for display and export.
      tab_auto <- table(CIS$RECUERDO, CIS$RVAUTAGR)
      tab_gen <- table(CIS$RECUERDO, CIS$RVGENAGR)
      
      # Finally, we create and export the table into an Excel file with autonotab() and generaltab(), which rely on write_tab_header.
      autonotab(tab_auto)
      generaltab(tab_gen)
    }
  }

  ### VOTING INTENTION TABLES
  
  if (general[x,"Encuesta"] == "pre" & !is.na(general[x,"Intencion.voto"])) {
    
    if (!is.na(general[x,"Ponderacion"]) & !is.na(general[x,"Estrato"]) )  {
      
      # Declare data to be survey data and weight it accordingly (if needed)
      CISweight <- svydesign(ids =  ~1, strata = ~CIS[,general[[x,"Estrato"]]],
                             weights = ~CIS[,general[[x,"Ponderacion"]]], data = CIS)
      
      tab_inten <- svytable(~RECUERDO + INTVAGR, design = CISweight)
      
      # Creation of the table and exportation to Excel with intentab(), which relies on write_tab_header
      intentab(tab_inten)
      
    } else if (!is.na(general[x,"Ponderacion"]) & is.na(general[x,"Estrato"])) {
      
      # Declare data to be survey data and weight it accordingly (if needed)
      CISweight <- svydesign(ids =  ~1, strata = NULL, weights = ~CIS[,general[[x,"Ponderacion"]]], data = CIS)
      
      tab_inten <- svytable(~RECUERDO + INTVAGR, design = CISweight)
      
      # Creation of the table and exportation to Excel with intentab(), which relies on write_tab_header
      intentab(tab_inten)
      
    } else {
      
      # Save as table the data tabulation in order to call it later for display and export.
      tab_inten <- table(CIS$RECUERDO, CIS$INTVAGR)
      
      # Creation of the table and exportation to Excel with intentab(), which relies on write_tab_header
      intentab(tab_inten)
    }
  }
 
  # Export to SPSS ------------------------------------------------------
  
  #For EXPORTING the data it is better to use haven's function. Labelled data is read correctly by SPSS.
  write_sav(CIS, path = paste("nuevo", general[x,"Savfile"]))
  
  # Contador de iteraciones con sello temporal
  if (x %% 50 == 0) { 
    print(paste0("Progress: ", x, " out of ", nrow(general), " iterations completed."))
    print(timestamp())
  }
}
