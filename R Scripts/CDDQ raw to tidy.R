####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(tidyr); library(psych); library(qdapRegex)

####################      LOAD THE RELEVANT DF + MAKE IT TIDY              ####################
cddq <- file.choose() %>%
                          read.csv(sep = '\t', header = FALSE)
# REMOVE EXCESSIVE SPACES OR DASHES
cddq$V1 <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", cddq$V1, perl=TRUE)
cddq$V1 <- gsub("-", "", cddq$V1, perl=TRUE)

# Remove spaces from country names
cddq$V1 <- tolower(cddq$V1); cddq$V1 <- gsub("united states", "usa", cddq$V1); cddq$V1 <- gsub("usa of america", "usa", cddq$V1); cddq$V1 <- gsub("united kingdom", "uk", cddq$V1); cddq$V1 <- gsub("new zealand", "nz", cddq$V1); cddq$V1 <- gsub("cook islands", "nz", cddq$V1); cddq$V1 <- gsub("south africa", "sa", cddq$V1); cddq$V1 <- gsub("saudi arabia", "saudia", cddq$V1); cddq$V1 <- gsub("vereinigte staaten", "usa", cddq$V1); cddq$V1 <- gsub("puerto rico", "usa", cddq$V1); cddq$V1 <- gsub("antigua & barbuda", "usa", cddq$V1); cddq$V1 <- gsub("hong kong", "china", cddq$V1); cddq$V1 <- gsub("faroe islands", "denmark", cddq$V1); cddq$V1 <- gsub("south korea", "sk", cddq$V1); cddq$V1 <- gsub("sri lanka", "sri_lanka", cddq$V1)

# Break strings based on spaces
cddq <- separate(cddq, V1, into = c("date", "hour", "bio", "cddq", "ip", "country", "email"), sep = "\\s")

# select only data of US users
table(cddq$country) %>% sort()
cddq <- cddq[cddq$country %in% c('us', 'united', 'usa', 'america'), ]

# Remove irrelevant chars
cddq$bio <- gsub("\\.5", "", cddq$bio, perl=TRUE)
cddq$bio <- gsub("[^0-9]", "", cddq$bio, perl=TRUE)

logic <- substr(cddq$bio[nchar(cddq$bio) == 10], start = 9, stop = 9) == substr(cddq$bio[nchar(cddq$bio) == 10], start = 10, stop = 10)
cddq$bio[nchar(cddq$bio) == 10][logic] <- substr(cddq$bio[nchar(cddq$bio) == 10][logic], start = 1, stop = 9); remove(logic)


# Check problem with the background variables
table(nchar(cddq$bio))
cddq[nchar(cddq$bio) == 10, ]
cddq <- cddq[nchar(cddq$bio) == 9, ]

# Keep only emails in the email variable
cddq$email[!grepl("@", cddq$email)] <- NA


####################      CREATING THE BACKGROUND VARIABLES              ####################
# Create the age variable and remove out of age range (14-50)
cddq$age <- substr(cddq$bio, start = 1, stop = 2) %>% as.numeric
cddq <- cddq[between(cddq$age, 16, 50), ]

# create the education variable and remove out of range (10-30)
cddq$education <- substr(cddq$bio, start = 3, stop = 4) %>% as.numeric
cddq <- cddq[between(cddq$education, 10, 30), ]

# create the gender variable and remove out of range (1-2)
cddq$gender <- substr(cddq$bio, start = 5, stop = 5) %>% as.numeric
cddq <- cddq[between(cddq$gender, 1, 2), ]

# create the difficulty variable
cddq$difficulty <- substr(cddq$bio, start = 6, stop = 6) %>% as.numeric

# create the stress variable
cddq$stress <- substr(cddq$bio, start = 7, stop = 7) %>% as.numeric

# create the first variable and remove out of range (2)
cddq$first <- substr(cddq$bio, start = 8, stop = 8) %>% as.numeric
cddq <- cddq[cddq$first == 2, ]

# create the RCA variable and remove out of range (2)
cddq$rca <- substr(cddq$bio, start = 9, stop = 9) %>% as.numeric

####################      CREATING THE CDDQ ITEMS              ####################

# remove corrupt cases
cddq <- cddq[!nchar(cddq$cddq) %in% c(9, 39, 40), ]

# create the variables
names <- c('Rm1', 'Rm2', 'Rm3', 'Ri1', 'Ri2', 'Ri3', 'Rd1', 'Rd2', 'Rd3', 'Rd4', 'Lp1', 'Lp2', 'Lp3', 'Ls1',
'Ls2', 'Ls3', 'Ls4', 'Lo1', 'Lo2', 'Lo3', 'La1', 'La2', 'Iu1', 'Iu2', 'Iu3', 'Ii1', 'Ii2', 'Ii3', 'Ii4', 'Ii5', 'Ie1',
'Ie2','val1', 'val2', 'time')

for (i in seq(34)) {
  cddq[names[i]] <- substr(cddq$cddq, start = i, stop = i) %>% as.numeric }

cddq[names[35]] <- substr(cddq$cddq, start = 35, stop = 38 ) %>% as.numeric; remove(names)

# remove based on validity
cddq <- cddq[between(cddq$val1, 3, 9), ]
cddq <- cddq[between(cddq$val2, 1, 7), ]

# remove based on time
cddq <- cddq[between(cddq$time, 120, 9308), ]

# remove based lack of variance in responses

# remove based on email (checking for duplicates)
cddq <- cddq[!duplicated(cddq$email) | is.na(cddq$email), ]

####################      CREATING THE CDDQ MAIN VARIABLES              ####################

cddq$Rm <- (cddq$Rm1 + cddq$Rm2 + cddq$Rm3) / 3; cddq$Rm <- round(cddq$Rm, 2)
cddq$Ri <- (cddq$Ri1 + cddq$Ri2 + cddq$Ri3) / 3; cddq$Ri <- round(cddq$Ri, 2)
cddq$Rd <- (cddq$Rd1 + cddq$Rd2 + cddq$Rd3 + cddq$Rd4) / 4; cddq$Rd <- round(cddq$Rd, 2)

cddq$Lp <- (cddq$Lp1 + cddq$Lp2 + cddq$Lp3) / 3; cddq$Lp <- round(cddq$Lp, 2)
cddq$Ls <- (cddq$Ls1 + cddq$Ls2 + cddq$Ls3 + cddq$Ls4) / 4; cddq$Ls <- round(cddq$Ls, 2)
cddq$Lo <- (cddq$Lo1 + cddq$Lo2 + cddq$Lo3) / 3; cddq$Lo <- round(cddq$Lo, 2)
cddq$La <- (cddq$La1 + cddq$La2) / 2; cddq$La <- round(cddq$La, 2)

cddq$Iu <- (cddq$Iu1 + cddq$Iu2 + cddq$Iu3) / 3; cddq$Iu <- round(cddq$Iu, 2)
cddq$Ii <- (cddq$Ii1 + cddq$Ii2 + cddq$Ii3 + cddq$Ii4 + cddq$Ii5) / 5; cddq$Ii <- round(cddq$Ii, 2)
cddq$Ie <- (cddq$Ie1 + cddq$Ie2) / 2; cddq$Ie <- round(cddq$Ie, 2)

cddq$LR <- (cddq$Rm + cddq$Ri + cddq$Rd) / 3; cddq$LR <- round(cddq$LR, 2)
cddq$LI <- (cddq$Lp + cddq$Ls + cddq$Lo + cddq$La) / 4; cddq$LI <- round(cddq$LI, 2)  
cddq$II <- (cddq$Iu + cddq$Ii + cddq$Ie) / 3; cddq$II <- round(cddq$II, 2)

cddq$Total <- (cddq$LR + cddq$LI + cddq$II) / 3; cddq$Total <- round(cddq$Total, 2)

####################      SAVE THE FILE              ####################

#write.csv(cddq, file = "Tidy data\\cddq (01.2018-05.2019) n 2857, created 17.06.2020")
write.csv(cddq, file = "Tidy data\\cddq (late) n 1573, created 17.06.2020")

