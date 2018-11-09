### Creating a complete voting behaviour variable combining two columns from each
# For this project usage only!
# This is not a general application function, and will not work out of the CISsurvey project.

voterecall <- function(df) {  #voterecall(CIS)
  
  RECUERDO <- factor(x = 0, levels = c(levels(df$Voto.reciente), 
                                           levels(df$Otro.reciente)[1:length(levels(df$Otro.reciente)) - 1],
                                           "Abstencion", "N.C. participacion") )
  #The code works until here. Then :
  # Error in if (CIS[y, "Otro.reciente"][[1]] == "Fue a votar y vot.") { : 
  # missing value where TRUE/FALSE needed
  
  # Loop proposal for complete voting behaviour
  for (y in 1:nrow(df)) {
    if (is.na(df[y, "Otro.reciente"][[1]]) == TRUE) {
      df[y, "RECUERDO"][[1]] <- NA
    } else if (df[y, "Otro.reciente"][[1]] == "Fue a votar y vot.") {
      df[y, "RECUERDO"][[1]] <- (df[y, "Voto.reciente"][[1]])
    } else if (df[y, "Otro.reciente"][[1]] == "S. que vot.") {  
      df[y, "RECUERDO"][[1]] <- (df[y, "Voto.reciente"][[1]])
    } else if (df[y, "Otro.reciente"] == "N.C.") {
      df[y, "RECUERDO"][[1]] <- "N.C. participacion"
    } else {
      df[y, "RECUERDO"][[1]] <- "Abstencion"
    }
  }
  df$RECUERDO <- droplevels(df$RECUERDO)
  
  print(table(df$RECUERDO, df$Voto.reciente, useNA = "always"))
  
  return(df)
}      
