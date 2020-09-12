---
layout: default
title: puntr
---

# puntr, an R package for puntalytics

We've been compiling our code for importing, processing, analyzing, and visualizing punting data into an R package, `puntr`. 
This is mostly for our own ease of use, but it's also available on github for anyone interested; it should all work, though we can't guarantee that it's bug-free. Full documentation also might come eventually, but for now, it's a little bit of a hodgepodge.   
  
Install via:
```R
devtools::install_github("Puntalytics/puntr")
```
Once that's installed, you can do
```R
library(puntr)

punts_raw <- import_punts(1999:2019)
punts_cleaned <- trust_the_process(punts_raw)
punts <- calculate_all(punts_cleaned)
```
To get a dataframe `punts` where each row is a punt, and each column is a stat relevant to punting (including our custom metrics).  
To compare punters, continue with
```R
mini <- create_mini(punts)
```
to get a data frame where each row is a punter, and each column is an average stat for that punter.  
To compare punter **seasons**, instead use
```R
miniY <- create_miniY(punts)
```
which gives every unique punter season a row.  

If you're just looking for the data directly, you can find .rds files of all punts from each season 1999-2019 [here](https://github.com/Puntalytics/puntr/tree/master/data)
