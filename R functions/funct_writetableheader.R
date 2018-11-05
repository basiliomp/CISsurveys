# Tab with header function
# New function for creating a meaningful title for voting recall tables
write.table.header <- function(x, file, header){
  cat(header, '\n',  file = file)
  write.table(x= x, file = file, col.names = NA, sep = ";", dec = ",", append = T, row.names = T, na = "")
}
