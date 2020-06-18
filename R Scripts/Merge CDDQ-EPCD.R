####################      RELEVANT LIBRARIES AND WORKING DIRECTORY                ####################
remove(list = ls())
library(dplyr); library(tidyr); library(mclust)

####################      LOAD TIDY FILES                ####################
cddq <- file.choose() %>% read.csv(header = TRUE, sep = ',')
cddq <- rbind(cddq, file.choose() %>% read.csv(header = TRUE))

epcd <- file.choose() %>% read.csv(header = TRUE)

####################      CHECK INTERSECTIONS                ####################

x <- intersect(cddq$email, epcd$email)
x <- x[-2]
temp <- cddq[, 51:60]

temp2 <- as.data.frame(cddq$email)
#temp2 <- cbind(cddq$ip, cddq$email)
tempi <- temp %>% t %>% scale %>% t %>% round(2) %>% as.data.frame()
temp <- temp[complete.cases(tempi), ]
temp2 <- as.data.frame(temp2[complete.cases(tempi), ])
cddq <- cddq[complete.cases(tempi), ]
tempi <- tempi[complete.cases(tempi), ]
temp2 <- as.data.frame(temp2)

sol <- Mclust(tempi, G = 4); sol
sol$parameters$mean %>% round(2)
x <- aggregate(temp, by = as.data.frame(sol$classification), FUN = mean) %>% round(2) %>% t

table(sol$classification[temp2$`temp2[complete.cases(tempi), ]` %in% x])
table(sol$classification)

cddq$classification <- sol$classification

####################      ADD TO THE EPCD DATA FRAME THE EPCD Z-SCORES              ####################
zcontrol11 <- epcd[, 54:64]; colnames(zcontrol11) <- paste0("Z_",colnames(zcontrol11))
epcd <- cbind(epcd, zcontrol11); remove(zcontrol11)

####################      MERGE THE CDDQ AND EPCD DATASETS BASED ON EMAILS              ####################
# Remove NAs from both datasets
cddq <- cddq[!is.na(cddq$email), ]
epcd <- epcd[!is.na(epcd$email), ]

join <- inner_join(cddq, epcd, by = c('email' = 'email'))



colnames(epcd)

describe(epcd[, 54:68])
describe(epcd[epcd$email %in% x, 54:68])
x <- aggregate(scale(join[, 118:132]), by = as.data.frame(join$classification), FUN = mean) %>% round(2)
x <- aggregate((join[, 118:132]), by = as.data.frame(join$classification), FUN = mean) %>% round(2)

y <- aggregate(scale(join[, 118:132]), by = as.data.frame(join$classification), FUN = sd) %>% round(2)


colnames(join)
####################      CALCULATE DIFFERENCES BASED ON RAW SCORES              ####################
raw11 <- join[, 118:128] 
res_raw11 <- aggregate(raw11, by = as.data.frame(join$classification), FUN = mean) %>% round(2)

####################      CALCULATE DIFFERENCES BASED ON DIMENSION Z-SCORES              ####################
z11 <- join[, 118:128] %>% scale %>% as.data.frame()
res_z11 <- aggregate(z11, by = as.data.frame(join$classification), FUN = mean) %>% round(2); res_z11

jzcontrol11 <- join[, 133:143]
res_zcontrol11 <- aggregate(zcontrol11, by = as.data.frame(join$classification), FUN = mean) %>% round(2)


####################      CALCULATE DIFFERENCES IN WITHIN-SUBJECT IPSATIVE SCORES              ####################
# Calculate ipsative scores for the 11 categories
ipsative11 <- join[, 118:128] %>% t %>% scale(scale = FALSE) %>% t %>% as.data.frame()

x <- aggregate(ipsative11, by = as.data.frame(join$classification), FUN = mean) %>% round(2)
describe(join$classification)
