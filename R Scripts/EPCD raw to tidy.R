####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
library(dplyr); library(tidyr); library(tidyverse); library(psych); library(qdapRegex)

####################      LOAD THE RELEVANT DF + MAKE IT TIDY              ####################
epcd <- file.choose() %>%
                read.csv(sep="\t", header = FALSE,  quote="")

# REMOVE EXCESSIVE SPACES OR DASHES
epcd$V1 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", epcd$V1, perl=TRUE)
epcd$V1 <- gsub("-", "", epcd$V1, perl=TRUE)

# Remove spaces from country names
epcd$V1 <- tolower(epcd$V1)
epcd$V1 <- gsub("united states", "usa", epcd$V1); epcd$V1 <- gsub("u.s.", "usa", epcd$V1); epcd$V1 <- gsub("usa of america", "usa", epcd$V1); epcd$V1 <- gsub("united kingdom", "uk", epcd$V1); epcd$V1 <- gsub("new zealand", "nz", epcd$V1); epcd$V1 <- gsub("cook islands", "nz", epcd$V1); epcd$V1 <- gsub("south africa", "sa", epcd$V1); epcd$V1 <- gsub("saudi arabia", "saudia", epcd$V1); epcd$V1 <- gsub("vereinigte staaten", "usa", epcd$V1); epcd$V1 <- gsub("puerto rico", "usa", epcd$V1); epcd$V1 <- gsub("antigua & barbuda", "usa", epcd$V1); epcd$V1 <- gsub("hong kong", "china", epcd$V1); epcd$V1 <- gsub("faroe islands", "denmark", epcd$V1); epcd$V1 <- gsub("south korea", "sk", epcd$V1); epcd$V1 <- gsub("sri lanka", "sri_lanka", epcd$V1)

# Break strings based on spaces
epcd <- separate(epcd, V1, into = c("date", "hour", "bio", "epcd", "ip", "country", "email"), sep = "\\s")

# select only data of US users
table(epcd$country) %>% sort()
epcd <- epcd[epcd$country %in% c('us', 'united', 'usa', 'america'), ]

# Keep only emails in the email variable
epcd$email[!grepl("@", epcd$email)] <- NA

####################      CREATING THE BACKGROUND VARIABLES              ####################
# Create the age variable and remove out of age range (14-50)
epcd$age <- substr(epcd$bio, start = 1, stop = 2) %>% as.numeric
epcd <- epcd[between(epcd$age, 16, 50), ]

# create the education variable and remove out of range (10-30)
epcd$education <- substr(epcd$bio, start = 3, stop = 4) %>% as.numeric
epcd <- epcd[between(epcd$education, 10, 30), ]

# create the gender variable and remove out of range (1-2)
epcd$gender <- substr(epcd$bio, start = 5, stop = 5) %>% as.numeric
epcd <- epcd[between(epcd$gender, 1, 2), ]

# create the major_choice variable
epcd$major_choice <- substr(epcd$bio, start = 6, stop = 6) %>% as.numeric

# create the confidence variable
epcd$confidence <- substr(epcd$bio, start = 7, stop = 7) %>% as.numeric

####################      CREATING THE EPCD ITEMS              ####################

# remove corrupt cases
table(nchar(epcd$epcd))
epcd <- epcd[nchar(epcd$epcd) == 43, ]

# create the variables
names <- c('warmup', 'Pp1', 'Pw1', 'Pc1', 'Ap1', 'Au1', 'Ac1', 'Ao1', 'Sa1', 'Ss1', 'Si1', 'Sc1', 'val1', 
           'Pp2', 'Pw2', 'Pc2', 'Ap2', 'Au2', 'Ac2', 'Ao2', 'Sa2', 'Ss2', 'Si2', 'Sc2', 'val2',
           'Pp3', 'Pw3', 'Pc3', 'Ap3', 'Au3', 'Ac3', 'Ao3', 'Sa3', 'Ss3', 'Si3', 'Sc3', 
           'Pw4', 'Sc4', 'EPCD_difficulty', 'Time')

for (i in seq(39)) {
  epcd[names[i]] <- substr(epcd$epcd, start = i, stop = i) %>% as.numeric }

epcd[names[40]] <- substr(epcd$epcd, start = 40, stop = 43 ) %>% as.numeric; remove(names)

# remove based on validity
epcd <- epcd[between(epcd$val1, 3, 9), ]
epcd <- epcd[between(epcd$val2, 1, 7), ]

# remove based on time
epcd <- epcd[between(epcd$Time, 120, 9862), ]

# remove based lack of variance in responses

# remove based on email (checking for duplicates)
epcd <- epcd[!duplicated(epcd$email) | is.na(epcd$email), ]


####################      CREATING THE EPCD  MAIN VARIABLES              ####################

epcd$Pp <- (epcd$Pp1 + epcd$Pp2 + epcd$Pp3) / 3; epcd$Pp <- round(epcd$Pp, 2)
epcd$Pw <- (epcd$Pw1 + epcd$Pw2 + epcd$Pw3 + epcd$Pw4) / 4; epcd$Pw <- round(epcd$Pw, 2)
epcd$Pc <- (epcd$Pc1 + epcd$Pc2 + epcd$Pc3) / 3; epcd$Pc <- round(epcd$Pc, 2)

epcd$Ap <- (epcd$Ap1 + epcd$Ap2 + epcd$Ap3) / 3; epcd$Ap <- round(epcd$Ap, 2)
epcd$Au <- (epcd$Au1 + epcd$Au2 + epcd$Au3) / 3; epcd$Au <- round(epcd$Au, 2)
epcd$Ac <- (epcd$Ac1 + epcd$Ac2 + epcd$Ac3) / 3; epcd$Ac <- round(epcd$Ac, 2)
epcd$Ao <- (epcd$Ao1 + epcd$Ao2 + epcd$Ao3) / 3; epcd$Ao <- round(epcd$Ao, 2)

epcd$Sa <- (epcd$Sa1 + epcd$Sa2 + epcd$Sa3) / 3; epcd$Sa <- round(epcd$Sa, 2)
epcd$Ss <- (epcd$Ss1 + epcd$Ss2 + epcd$Ss3) / 3; epcd$Ss <- round(epcd$Ss, 2)
epcd$Si <- (epcd$Si1 + epcd$Si2 + epcd$Si3) / 3; epcd$Si <- round(epcd$Si, 2)
epcd$Sc <- (epcd$Sc1 + epcd$Sc2 + epcd$Sc3 + epcd$Sc4) / 4; epcd$Sc <- round(epcd$Sc, 2)

epcd$P <- (epcd$Pp + epcd$Pw + epcd$Pc) / 3; epcd$P <- round(epcd$P, 2)
epcd$A <- (epcd$Ap + epcd$Au + epcd$Ac + epcd$Ao) / 4; epcd$A <- round(epcd$A, 2)
epcd$S <- (epcd$Sa + epcd$Ss + epcd$Si + epcd$Sc) / 4; epcd$A <- round(epcd$A, 2)

epcd$Total <- (epcd$P + epcd$A + epcd$S) / 3; epcd$Total <- round(epcd$Total, 2)

####################      SAVE THE FILE              ####################
write.csv(epcd, file = "Tidy data\\epcd n 1530, created 17.06.2020")

