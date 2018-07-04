
#PANEL 2000
pre <- foreign_to_labelled(read.spss(file = "AUT/ANDAL/2000/MD2382-pre2000AND/MD2382-pre2000.sav", to.data.frame = TRUE, use.value.labels = TRUE))
pos <- foreign_to_labelled(read.spss(file = "AUT/ANDAL/2000/MD2384-pos2000AND/MD2384-pos2000.sav", to.data.frame = TRUE, use.value.labels = TRUE))
panel <- inner_join(pre, pos, by="CUES")

#Pesos distintos para elecciones andaluzas y para elecciones generales analizadas en esa encuesta panel.






######################

general[[x, "Voto.reciente"]]
general[[x, "Otro.reciente"]]


nueva_var <- if_else(is.na(CIS[[general[[x, "Voto.reciente"]]]]),
        CIS[[general[[x, "Otro.reciente"]]]],
        CIS[[general[[x, "Voto.reciente"]]]])


## Crear con esta fórmula un conjunto de reglas que cumplir y los datos a introducri según se cumplan. 
## Luego en TRUE pones qué hacer si ninguna de las condiciones se cumplía.

case_when(condition 1 ~ 1,
          condiion 2 & condition 3 & conditin 4 ~ 2,
          TRUE ~ 3)


