# Age groups function

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