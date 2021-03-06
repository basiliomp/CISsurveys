# Panel 2015 dataset elaboration

Loading the two parts of the panel survey - data was previously downloaded from www.cis.es
and adapted to the SPSS .sav format for improved compatibility in R.

```{r}
Pre election survey (CIS study 3117)
pre15 <- foreign_to_labelled(read.spss(file = "MD3117-pre2015ESP.sav",
                                       to.data.frame = TRUE, 
                                       reencode = 'utf-8',
                                       use.value.labels = TRUE))
Pos election survey (CIS study 3126)
pos15 <- foreign_to_labelled(read.spss(file = "MD3126-pos2015ESP.sav",
                                       to.data.frame = TRUE, 
                                       reencode = 'utf-8',
                                       use.value.labels = TRUE))
```

Searching common variables across datasets.
```{r}
common_names <- names(pre15[which(names(pre15) %in% names(pos15))])
```

Using a for loop we can have a sense of the variation across variables that are common for the two datasets.
```{r}
for (i in 1:length(common_names)) {
  print(head(pre15[i]))
}
```
After inspection, the variable 'CUES' is the only one that looks like an repondant identifier.

A 'naive' juntion of the two surveys datasets throws several problems for variables that do not
hold the same meaning across them.
```{r}
panel15 <- inner_join(pre15, pos15)
```

Removing the variables related to questions and leaving those recording fixed-characteristics
```{r}
panel15 <- dplyr::inner_join(pre15, pos15, 
                             by = c("CUES", "CCAA", "PROV", "TAMUNI", "CAPITAL", "DISTR", "SECCION"))
```

We can be sure no observations were dropped using this later match since it preserves as many 
of them as one the two datasets it comes from.
```{r}
sapply(list(pre15, pos15, panel15), nrow)
```

Finally, we can export the data back to the SPSS format using the haven's write_sav function. Labelled data is read correctly by SPSS.
```{r}
haven::write_sav(panel15, path = "panel2015ESP.sav")
```
