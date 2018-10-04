# Tab with header function
# Nueva función para darle un título a las tablas de recuerdo de voto
write.table.header <- function(x, file, header){
  cat(header, '\n',  file = file)
  write.table(x= x, file = file, col.names = NA, sep = ";", dec = ",", append = T, row.names = T, na = "")
}
