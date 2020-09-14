library(readr)
library(tidyverse)
library(gganimate)
library(plotly)
library(googleVis)
library(zoo)
library(dplyr)
library(lubridate)

extrafont::loadfonts(quiet = T) # add extra fonts to R

round7_afroberometer <- r7_merged_data_34ctry

#audit factor

class(round7_afroberometer)

dim(round7_afroberometer)

colnames(round7_afroberometer)

colnames(r7_merged_data_34ctry)

str(round7_afroberometer)

glimpse(round7_afroberometer)

summary(round7_afroberometer) #to get an idea on the idea

summary(round7_afroberometer$ID_country_name)

dataset1NAS <- subset(df_x2018, is.na(df_x2018$overall_governance))

head(round7_afroberometer)

head(round7_afroberometer, n = 15)

tail(round7_afroberometer) 

complete.cases(round7_afroberometer)


#create new data subset


round7_afro_test <- r7_merged_data_34ctry[1:159]

colnames(round7_afro_test)

round7_afro_test1 <- round7_afro_test[-10:-127]

colnames(round7_afro_test1)

round7_afro_test2 <- r7_merged_data_34ctry[c(1,6:159)]


# recode countries

round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 1] <- "Benin"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 2] <- "Botswana"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 3] <- "Burkina Faso"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 4] <- "Cabo Varde"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 5] <- "Cameroon"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 6] <- "Côte d’Ivoire"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 7] <- 'eSwatini'
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 8] <- "Gabon"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 9] <- "Gambia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 10] <- "Ghana"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 11] <- "Guinea"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 12] <- "Kenya"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 13] <- "Lesotho"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 14] <- "Liberia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 15] <- "Madagascar"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 16] <- "Malawi"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 17] <- "Mali"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 18] <- "Mauritius"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 19] <- "Morocco"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 20] <- "Mozambique"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 21] <- "Nambia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 22] <- "Niger"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 23] <- "Nigeria"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 24] <- "São Tomé and Príncipe"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 25] <- "Senegal"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 26] <- "Sierra Leone"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 27] <- "South Africa"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 28] <- "Sudan"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 29] <- "Tanzania"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 30] <- "Togo"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 31] <- "Tunisia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 32] <- "Uganda"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 33] <- "Zambia"
round7_afro_test1$COUNTRY[round7_afro_test1$COUNTRY == 34] <- "Zimbabwe"

# recode regions

round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 1] <- "West Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 2] <- "East Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 3] <- "Southern Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 4] <- "North Africa"
round7_afro_test1$COUNTRY.BY.REGION[round7_afro_test1$COUNTRY.BY.REGION == 5] <- "Central Africa"



colnames(round7_afroberometer) <- c("RESPNO", "COUNTRY", "COUNTRY_Old.order", "COUNTRY_R5List",
                        "COUNTRY_R6List", "COUNTRY_BY_REGION", "URBRUR", "REGION",
                        "LOCATION_LEVEL_1", "Q39B", "Q39C",  "Q40", "Q43A", "Q44A", "Q43B", "Q43C", "Q44A", 
                        "Q44B","Q44c", "Q44d", "Q44E", "Q44F", "Q44G", "Q44H", "Q44I", "Q44J", "Q45")


Round7_2 <- round7_afroberometer %>% 
  select(RESPNO, COUNTRY, COUNTRY_Old.order, COUNTRY_R5List,
         COUNTRY_R6List, COUNTRY_BY_REGION, URBRUR, REGION,
         LOCATION_LEVEL_1, Q39B, Q39C, Q40, Q43A, Q44A, Q43B, Q43C, Q44A, 
         Q44B, Q44c, Q44d, Q44e, Q44f, Q44g, Q44h, Q44i, Q44j, Q45)

rlang::last_error() 

subset(round7_afroberometer$select("RESPNO", "COUNTRY", "COUNTRY_Old.order", "COUNTRY_R5List",
                                    "COUNTRY_R6List", "COUNTRY_BY_REGION", "URBRUR", "REGION",
                                    "LOCATION_LEVEL_1", "Q39B", "Q39C",  "Q40", "Q43A", "Q44A", "Q43B", "Q43C", "Q44A", 
                                    "Q44B","Q44c", "Q44d", "Q44E", "Q44F", "Q44G", "Q44H", "Q44I", "Q44J", "Q45"))

