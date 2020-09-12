# puntr, an R package for puntalytics

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

If you're just looking for the data directly, you can find .rds files of all punts from each season 1999-2019 [here](https://github.com/Puntalytics/puntr-data/tree/master/data)
