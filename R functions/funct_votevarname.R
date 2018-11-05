#Function for creating explicit names to voting behaviour related variables

# Actual function -----------------
#Extracting the year for each survey as simplified two characters version
votevarname <- function (i) {
  if (i %in% 1:nrow(general) && !is.na(general[[i,"Elecciones"]]) ) { #Security check
    #Naming of pre or post surveys, according to the variable of interest (voting intention/behaviour)
    if (general$Encuesta[[i]] == "pre") {
      preorpost <- "INTE"
      } else preorpost <- "RV"
    #Type of election, regarding political arena (state level or regional one)
    if (general$Elecciones[[i]] == "Congreso") {
    elect_ambit <- "GEN"
      } else elect_ambit <- "AUT"
  
  #Extrac the year of the electoral event, in 2 digits format.
  simpleyear <-  str_extract(string = as.character(general$Year[[i]]), pattern = "..$")
  
  #Combine all the pieces of information together
  nombrevar <- paste0(preorpost, elect_ambit, simpleyear,  "AGR")
  return(nombrevar)
  
  } else { #Error path for those number inputs without a reference in 'general'
    print("Invalid input. Use a row number from the reference database `general`")
  }
}

#  Work in progress. Code for assigning this new name to the corresponding variable.
# rename.variable <- function (df, old, new) {
#   names(df)[which(names(df) == old)] <- new
#   df
# }
