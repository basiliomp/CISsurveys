#Function for creating explicit names to voting behaviour related variables

  # ! Check what to do with these survey entries, because they are NAs!
  filter(general, is.na(Elecciones)) %>%
    select(1:3)

#Extracting the year for each survey as simplified two characters version
votevarname <- function (i) {
  if (i %in% 1:nrow(general)) { #Security check
  preorpost <- general$Encuesta[[i]]
    if (general$Elecciones[[i]] == "Congreso") {
    elect_ambit <- "GEN"
    } else elect_ambit <- "AUT"
  
  simpleyear <-  str_extract(string = as.character(general$Year[[i]]), pattern = "..$")
  nombrevar <- paste0(preorpost, elect_ambit, simpleyear,  "AGR")
  return(nombrevar)
  } else {
    print("Invalid input. Use a row number from the reference database `general`")
  }
}
