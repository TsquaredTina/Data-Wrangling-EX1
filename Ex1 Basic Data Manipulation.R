library(tidyr)
library(dplyr)

setwd("C://Users//Tina Teng//Documents//Personal//Springboard - Foundation of Data Science//3.1 EX1")
refine <- read.csv(file = "refine_original.csv", head=TRUE, sep =",")

Refine <- refine
unique(Refine$company, incomparables = FALSE)
sort(Refine$company)


## 1: Clean brand names

Refine$company <- sub("ak zo", "akzo", Refine$company)
Refine$company <- sub("Akzo", "akzo", Refine$company)
Refine$company <- sub("AKZO", "akzo", Refine$company)
Refine$company <- sub("akz0", "akzo", Refine$company)

Refine$company <- sub("Phillips", "philips", Refine$company)
Refine$company <- sub("phillips", "philips", Refine$company)
Refine$company <- sub("phllips", "philips", Refine$company)
Refine$company <- sub("phillps", "philips", Refine$company)
Refine$company <- sub("phillipS", "philips", Refine$company)
Refine$company <- sub("fillips", "philips", Refine$company)
Refine$company <- sub("phlips", "philips", Refine$company)

Refine$company <- sub("Van Houten", "van houten", Refine$company)
Refine$company <- sub("van Houten", "van houten", Refine$company)

Refine$company <- sub("unilver", "unilever", Refine$company)
Refine$company <- sub("Unilever", "unilever", Refine$company)

##2 Separate ProductCode and Number

Refine_tidy <- Refine %>%
  separate( Product.code...number, into = c("ProductCode", "number"), sep = "\\-") 

##3 Add product categories
index<- c("p", "v", "x", "q")
value<- c("Smartphone", "TV", "Laptop", "Tablet")

Refine_tidy <- mutate(Refine_tidy, Category = value[match(Refine_tidy$ProductCode, index)])


##4 Add full address for gecoding

Refine_tidy <- mutate(Refine_tidy, full_address = paste(Refine_tidy$address, Refine_tidy$city, Refine_tidy$country, sep = ","))

##5 Create dummy variables for company and product categories

Refine_tidy$company_philips <- ifelse(match(Refine_tidy$company,"philips"), 1, 0)
Refine_tidy$company_akzo <- ifelse(match(Refine_tidy$company,"akzo"), 1, 0)
Refine_tidy$company_van_houten <- ifelse(match(Refine_tidy$company,"van houten"), 1, 0)
Refine_tidy$company_unilever <- ifelse(match(Refine_tidy$company,"unilever"), 1, 0)

Refine_tidy$product_laptop <- ifelse(match(Refine_tidy$Category,"Laptop"), 1, 0)
Refine_tidy$product_smartphone <- ifelse(match(Refine_tidy$Category,"Smartphone"), 1, 0)
Refine_tidy$product_tablet <- ifelse(match(Refine_tidy$Category,"Tablet"), 1, 0)
Refine_tidy$product_tv <- ifelse(match(Refine_tidy$Category,"TV"), 1, 0)
Refine_tidy[is.na(Refine_tidy)] <- 0

##6 Save File
write.table(Refine_tidy, file = "refine_clean.csv", append = FALSE, quote = TRUE,
            sep = ",", col.names = TRUE, row.names = FALSE)
