### Data manipulation from SPSS: Encuestas electorales autonÃ³micas del CIS

# Set up --------------------------------------------------------------

# Loading packages required
library(tidyverse)
library(lubridate)
library(labelled)
library(foreign)
library(haven)
library(readxl)
library(survey)
library(WriteXLS) # Is it better? I think so since it allows for output format code (e.g. UTF-16) to be specified.

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

# Set your working directory
project_root <- "D:/Dropbox/AI_ELEC_AUT/Encuestas"
setwd(project_root)

# Reading index database into R
general <- readxl::read_xlsx("progreso trabajo.xlsx", sheet = "tabla", skip = 1, col_names = T )
# Special case surveys are pulled out of the main analysis
general <- filter(general, (!is.na(general$Token)))

# Loop ----------------------------------------------------------------

starttime <- now() #at the end now() - starttime
for (x in 1:nrow(general)) { 
  
  # Setting working directory for current survey file
  setwd(paste0(project_root, general[x, "Folder"]))
  
  # Importing data ------------------------------------------------------
  
  # Reading survey data from SPSS into R with `foreign`. Alternatively: haven::read_spss(file = general[[x, "Savfile"]], user_na = TRUE)
  CIS <- foreign_to_labelled(read.spss(file = general[[x, "Savfile"]],
                                       to.data.frame = TRUE, 
                                       reencode = 'ISO-8859-1', #This preserves Spanish special characters.
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
    
    #Placeholder for the vote recall variable
    CIS$RECUERDO <- factor(x = 0, levels = unique(c(levels(CIS$Voto.reciente), 
                                                    levels(CIS$Otro.reciente),
                                                    "Abstención", "N.C. participación")))
    
    # Apply the tailored function in order to get a complete voting behaviour variable
    CIS <- voterecall(df = CIS)
    
    # Rename new variable to include data from year, time of survey and type of variable with votevarname()
    # The "RECUERDO" variable is kept for later usage in tabulation.
    CIS <- cbind(CIS, CIS$RECUERDO)
    names(CIS)[which(names(CIS) == "CIS$RECUERDO")] <- as.character(votevarname(x))
    
    } else if (!is.na(general[[x,"Voto.reciente"]]) & general[[x,"Voto.reciente"]] != "-") {
       CIS$RECUERDO <- CIS[[general[[x,"Voto.reciente"]]]]
       
       # Rename new variable to include data from year, time of survey and type of variable with votevarname()
       # The "RECUERDO" variable is kept for later usage in tabulation.
       CIS <- cbind(CIS, CIS$RECUERDO)
       names(CIS)[which(names(CIS) == "CIS$RECUERDO")] <- as.character(votevarname(x))
       
       } else {
    general[x, "Looperror"] <- print(paste("Lack of VOTO RECIENTE in", general$Token[[x]]))
  }
  
  # RVAUTAGR para las elecciones autonomicas del ciclo pasado (*no missing values*)
  if (!is.na(general[x,"Voto.pasado"])) {
    CIS$RVAUTAGR <- CIS[[general[[x,"Voto.pasado"]]]]
    
    # Rename new variable to include data from year, time of survey and type of variable with votevarname()
    simplepastyear <- str_extract(string = as.character(general[[x, "Year]] - 4), pattern = "..$")
    names(CIS)[which(names(CIS) == "CIS$RVAUTAGR")] <- paste0("RVAUT", simplepastyear, "AGR")
    
  } else {
    general[x, "Looperror"] <- print(paste("Lack of VOTO PASADO in", general$Token[[x]]))
  }
  
  # RVGENAGR para las elecciones generales del ciclo pasado (*no missing values*)
  if (!is.na(general[x,"Voto.generales"])) {
    CIS$RVGENAGR <- CIS[[general[[x,"Voto.generales"]]]]
    
    # Rename new variable to include data from year, time of survey and type of variable with votevarname()
    simplepastyear <- case_when(general[[x, "Year]] < 2000 ~ "96",
                      general[[x, "Year]] > 2000 & general[[x, "Year]] < 2004 ~ "00",
                      general[[x, "Year]] > 2004 & general[[x, "Year]] < 2008 ~ "04",
                      general[[x, "Year]] > 2008 & general[[x, "Year]] < 2011 ~ "08",
                      general[[x, "Year]] > 2011 & general[[x, "Year]] < 2015 ~ "11",
                      general[[x, "Year]] > 2015 & general[[x, "Year]] < 2016 ~ "15",
                      general[[x, "Year]] > 2016 & general[[x, "Year]] < 2018 ~ "16")
    names(CIS)[which(names(CIS) == "CIS$RVGENAGR")] <- paste0("RVGEN", simplepastyear, "AGR")
    
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
  
  if (general[x,"Encuesta"] == "post" & !is.null(CIS$RECUERDO)) {

    # Different code required for weighted and not weighted surveys.
    if (!is.na(general[x,"Ponderacion"])) {
      
      # Security checks before defining weights
      if (is.factor(CIS[,general[[x,"Ponderacion"]]])) {
          
        CISweight <- svydesign(ids = ~1, weights = as.numeric(as.character(CIS[,general[[x,"Ponderacion"]]])), data = CIS)
          
          if (anyNA(CIS[,general[[x,"Ponderacion"]]])) { #Check wether there are NAs in the weights vector.
            
            CIS[is.na(CIS[,general[[x,"Ponderacion"]]]),general[[x,"Ponderacion"]]] <- 0 # Replace NAs with 0s
            
            # Weights specification for survey design definition
            CISweight <- svydesign(ids = ~1, weights = CIS[,general[[x,"Ponderacion"]]], data = CIS)
          }
        
        } else if (anyNA(CIS[,general[[x,"Ponderacion"]]])) { #Check wether there are NAs in the weights vector.
        
          CIS[is.na(CIS[,general[[x,"Ponderacion"]]]),general[[x,"Ponderacion"]]] <- 0 # Replace NAs with 0s
          
          # Weights specification for survey design definition
          CISweight <- svydesign(ids = ~1, weights = CIS[,general[[x,"Ponderacion"]]], data = CIS)
        
          } else {
          CISweight <- svydesign(ids = ~1, weights = CIS[,general[[x,"Ponderacion"]]], data = CIS)
      }
      
      # Finally, we create and export the table into an Excel file with autonotab() and generaltab(), which rely on write_tab_header.
      if (!is.null(CIS$RVAUTAGR)) {
        autonotab(RECUERDO = CIS$RECUERDO, RVAUT = CIS$RVAUTAGR, weight = CISweight)
      }
      if (!is.null(CIS$RVGENAGR)) {
        generaltab(RECUERDO = CIS$RECUERDO, RVGEN = CIS$RVGENAGR, weight = CISweight)
      }
  } else {
      
      # Finally, we create and export the table into an Excel file with autonotab() and generaltab(), which rely on write_tab_header.
      if (!is.null(CIS$RVAUTAGR)) {
        autonotab(RECUERDO = CIS$RECUERDO, RVAUTAGR = CIS$RVAUTAGR)
      }
      if (!is.null(CIS$RVGENAGR)) {
        generaltab(RECUERDO = CIS$RECUERDO, RVGENAGR = CIS$RVGENAGR)
      }
    }
  }

  ### VOTING INTENTION TABLES (compared to vote recall from past election, usually 4 years ago)
  
  if (!is.na(general[x,"Intencion.voto"]) & !is.na(general[x,"Voto.pasado"])) {
    
    #Check wether there are NAs in the weights vector.
    if (!is.na(general[x,"Ponderacion"])) {
      if (anyNA(CIS[,general[[x,"Ponderacion"]]])) {
      CIS[is.na(CIS[,general[[x,"Ponderacion"]]]),general[[x,"Ponderacion"]]] <- 0 # Replace NAs with 0s
      }
    }
    if (!is.na(general[x,"Ponderacion"]) ) {
      
      # Declare data to be survey data and weight it accordingly (if needed)
      CISweight <- svydesign(ids = ~1, weights = CIS[,general[[x,"Ponderacion"]]], data = CIS)
      
      # Creation of the table and exportation to Excel with intentab(), which relies on write_tab_header
      intentab(RVAUTAGR = CIS$RVAUTAGR, INTVAGR = CIS$INTVAGR, weight = CISweight)
      
    } else {
      
      # Creation of the table and exportation to Excel with intentab(), which relies on write_tab_header
      intentab(RVAUTAGR = CIS$RVAUTAGR, INTVAGR = CIS$INTVAGR)
    }
  }
 
  # Export to SPSS ------------------------------------------------------
  
  # Removing the variable RECUERDO from the data frame CIS. It is used as a token during the loop.
  if ("RECUERDO" %in% names(CIS)) {
    CIS$RECUERDO <- NULL
  }
  
  # Better to use haven's function. Labelled data is read correctly by SPSS.
  write_sav(CIS, path = paste("nuevo", general[x,"Savfile"]))
  
# Loop tracking -------------------

  if (x %% 50 == 0) { 
    print(paste0("Progress: ", x, " out of ", nrow(general), " iterations completed."))
    print(timestamp())
  }
}
