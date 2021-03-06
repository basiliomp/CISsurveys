# write_tab_header funct --------
# New function for creating a meaningful title for voting recall tables
write_tab_header <- function(x, file, header){
    cat(header, '\n',  file = file)
    write.table(x = x, file = file, col.names = NA, sep = ";", dec = ",", append = T, row.names = T, na = "")
  }
  

# Tabular functions ---------------
  
# Consider adding a second argument to the function below so `x` is specified explicitly?

#Function for tabulating answers from reported vote on general elections
generaltab <- function(RECUERDO, RVGENAGR, weight = NA) { 
  
    # Weighted tables
  if (!is.na(weight)) {
    tab_gen <- svytable(~RECUERDO + RVGENAGR, design = weight)
  } else {
    tab_gen <- table(RECUERDO, RVGENAGR)
  }
  # Tabla de transferencias desde anteriores generales en cifras absolutas
    write_tab_header(x = round((tab_gen)), file = paste(general[x,"Token"], "GEN_abs.csv", sep = "_"), 
                    header = "Voto reciente (filas) y en anteriores elecciones generales (columnas) en número absolutos")
  
  # Tabla de transferencias porcentuales desde anteriores generales por fila
    write_tab_header(x = round(prop.table(tab_gen, margin = 1), digits = 4)*100, #margin=1 es para % por fila
                     file = paste(general[x,"Token"], "GEN_perc_fila.csv", sep = "_"), 
                     header = "Voto reciente (filas) y en anteriores elecciones generales (columnas) en % por fila")
  
  # Tabla de transferencias porcentuales desde anteriores generales por columna
    write_tab_header(x = round(prop.table(tab_gen, margin = 2), digits = 4)*100, #margin=2 es para % por columna
                     file = paste(general[x,"Token"], "GEN_perc_colu.csv", sep = "_"), 
                     header = "Voto reciente (filas) y en anteriores elecciones generales (columnas) en % por columna")
}


#Function for tabulating answers from reported vote on regional elections
autonotab <- function(RECUERDO, RVAUTAGR, weight = NA) {
  
  
  
  # Weighted tables
  if (!is.na(weight)) {
    tab_auto <- svytable(~RECUERDO + RVAUTAGR, design = weight)
  } else {
    tab_auto <- table(RECUERDO, RVAUTAGR)
  }
  
  # Tabla de transferencias desde anteriores autonómicas en cifras absolutas
  write_tab_header(x = round(tab_auto), file = paste(general[x,"Token"], "AUTO_abs.csv", sep = "_"), 
                 header = "Voto reciente (filas) y en anteriores elecciones autonómicas (columnas) en números absolutos")
  
  # Tabla de transferencias porcentuales desde anteriores autonómicas por fila
  write_tab_header(x = round(prop.table(tab_auto, margin = 1), digits = 4)*100, #margin=1 es para % por fila
                   file = paste(general[x,"Token"], "AUTO_perc_fila.csv", sep = "_"), 
                   header = "Voto reciente (filas) y en anteriores elecciones autonómicas (columnas) en % por fila")
  
  # Tabla de transferencias porcentuales desde anteriores autonómicas por columna
  write_tab_header(x = round(prop.table(tab_auto, margin = 2), digits = 4)*100, #margin=2 es para % por columna
                   file = paste(general[x,"Token"], "AUTO_perc_colu.csv", sep = "_"), 
                   header = "Voto reciente (filas) y en anteriores elecciones autonómicas (columnas) en % por columnas")
}


#Function for tabulating answers from voting intention from pre election surveys
intentab <- function(RVAUTAGR, INTVAGR, weight = NA) { 

  # Weighted tables
  if (!is.na(weight)) {
    tab_inten <- svytable(~RVAUTAGR + INTVAGR, design = weight)
  } else {
    tab_inten <- table(RVAUTAGR, INTVAGR)
  }
  # Tabla de transferencias desde anteriores autonómicas en cifras absolutas
  write_tab_header(x = round(tab_inten), file = paste(general[x,"Token"], "INTEN_abs.csv", sep = "_"), 
                   header = "Intención de voto (filas) y recuerdo de voto anteriores elecciones autonómicas (columnas) en números absolutos")
  
  # Tabla de transferencias porcentuales desde anteriores micas por fila
  write_tab_header(x = round(prop.table(tab_inten, margin = 1), digits = 4)*100, #margin=1 es para % por fila
                   file = paste(general[x,"Token"], "INTEN_perc_fila.csv", sep = "_"), 
                   header = "Intencón de voto (filas) y recuerdo de voto en anteriores elecciones autonómicas (columnas) en % por fila")
  
  # Tabla de transferencias porcentuales desde anteriores intennómicas por columna
  write_tab_header(x = round(prop.table(tab_inten, margin = 2), digits = 4)*100, #margin=2 es para % por columna
                   file = paste(general[x,"Token"], "INTEN_perc_colu.csv", sep = "_"), 
                   header = "Intención de voto (filas) y recuerdo de voto en anteriores elecciones autonómicas (columnas) en % por columnas")
}
