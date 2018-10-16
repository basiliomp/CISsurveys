# Write.table.header funct --------
# New function for creating a meaningful title for voting recall tables
  write.table.header <- function(x, file, header){
    cat(header, '\n',  file = file)
    write.table(x= x, file = file, col.names = NA, sep = ";", dec = ",", append = T, row.names = T, na = "")
  }
  

# Tabular functions ---------------
  
# Consider adding a second argument to the function below so `x` is specified explicitly?

#Function for tabulating answers from reported vot on general elections
generaltab <- function(tab.gen) { 
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



#Function for tabulating answers from reported vot on regional elections
autonotab <- function(tab.auto) { 
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
