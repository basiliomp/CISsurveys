### Creating a complete voting behaviour variable combining two columns from each
# For this project usage only!
# This is not a general application function, and will not work out of the CISsurvey project.

voterecall <- function(df) {  #voterecall(CIS)
  
  RECUERDO <- factor(x = 0, levels = c(levels(df$Voto.reciente), 
                                           levels(df$Otro.reciente)[1:length(levels(df$Otro.reciente)) - 1],
                                           "Abstención", "N.C. participación") )
  #The code works until here. Then :
  
  # Loop proposal for complete voting behaviour
  for (y in 1:nrow(df)) {
    if (is.na(df[y, "Otro.reciente"][[1]]) == TRUE) {
      df[y, "RECUERDO"][[1]] <- NA
    } else if (df[y, "Otro.reciente"][[1]] == "Fue a votar y votó") {
      df[y, "RECUERDO"][[1]] <- (df[y, "Voto.reciente"][[1]])
    } else if (df[y, "Otro.reciente"][[1]] == "Sí que voté") {  
      df[y, "RECUERDO"][[1]] <- (df[y, "Voto.reciente"][[1]])
    } else if (df[y, "Otro.reciente"] == "N.C.") {
      df[y, "RECUERDO"][[1]] <- "N.C. participación"
    } else {
      df[y, "RECUERDO"][[1]] <- "Abstención"
    }
  }
  df$RECUERDO <- droplevels(df$RECUERDO)
  
  print(table(df$RECUERDO, df$Voto.reciente, useNA = "always"))
  
  return(df)
}      
