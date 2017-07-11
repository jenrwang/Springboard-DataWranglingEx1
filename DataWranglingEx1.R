# Load dplyr Package

library(dplyr)

# 0: Load raw data into RStudio

library(readr)
refine_original <- read_csv("~/Springboard/DataWranglingEx1/refine_original.csv")
View(refine_original)

# Create tbl of dataset

refine = tbl_df(refine_original)

# 1: Refine company names, using the following as key.
  # phillips = ps
  # akzo = ak
  # van houten = van
  # unilever = lev

refine$refine_company <- as.character(paste(refine$company))
refine = mutate(refine, refine_company = tolower(company))

refine$refine_company <- gsub(glob2rx("*ps*"),"phillips",refine$refine_company)
refine$refine_company <- gsub(glob2rx("*ak*$"),"akzo",refine$refine_company)
refine$refine_company <- gsub(glob2rx("*van*$"),"van houten",refine$refine_company)
refine$refine_company <- gsub(glob2rx("^*ver*"),"unilever",refine$refine_company)

# 2: Separate product and code number by creating two new columns, 'product_code' and 'product_number'

refine$`Product code / number`<-as.character(refine$`Product code / number`)
separation<-strsplit(refine$`Product code / number`, split = "-")
select_el <-function(x, index) {x[index]}
product_code <- lapply(separation, select_el, index=1)
product_number <- lapply(separation, select_el, index=2)
refine$product_code <- product_code
refine$product_number <- product_number

# 3: Add product categories using the following as key.
  # p = Smartphone
  # v = TV
  # x = Laptop
  # q = Tablet

refine$product_category <- as.character(paste(refine$product_code))

refine$product_category[refine$product_category == 'p'] <- "Smartphone"
refine$product_category[refine$product_category == 'v'] <- "TV"
refine$product_category[refine$product_category == 'x'] <- "Laptop"
refine$product_category[refine$product_category == 'q'] <- "Tablet"

# 4: Paste the full address in a new column, 'full_address'.
  
refine$full_address <- as.character(paste(refine$address, refine$city, refine$country, sep = ", "))


# 5: Create dummy variables for company and product category.

refine <-
  refine %>%
    mutate(company_phillips = ifelse(refine_company == "phillips", 1, 0),
           company_akzo = ifelse(refine_company == "akzo", 1, 0),
           company_van_houten = ifelse(refine_company == "van houten", 1, 0),
           company_unilever = ifelse(refine_company == "unilever", 1, 0),
           product_smartphone = ifelse(product_category == "Smartphone", 1, 0),
           product_tv = ifelse(product_category == "TV", 1, 0),
           product_laptop = ifelse(product_category == "Laptop", 1, 0),
           product_tablet = ifelse(product_category == "Tablet", 1, 0))

# 6: Print refine

refine_clean <- refine
View(refine_clean)

#refine_clean<-as.data.frame(refine_clean)
#write.csv(refine_clean, "~/Springboard/DataWranglingEx1/refine_clean.csv")
