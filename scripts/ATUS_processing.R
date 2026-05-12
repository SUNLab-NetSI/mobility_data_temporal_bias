## CODE TO PROCESS 2017 AMERICAN TIME USE-SURVEY (ATUS) DATA ##

# read in packages
library(tidyverse)


# read in the 2017 ATUS Activity file, it contains information about respondents spent their diary day
# also available from https://www.bls.gov/tus/data/datafiles_2017.htm
atusact_2017 <- read.table("atusact_2017.dat", header = TRUE, sep = ",")


# add column to atusact_2017 data frame that translates the numbers in TEWHERE column to place categories
where_descriptions <- read.csv ("atus_where_data_dictionary_2017.csv") # this CSV was created manually, with data pulled from: https://www.bls.gov/tus/dictionaries/atusintcodebk17.pdf 
atusact_2017 <- left_join(atusact_2017, where_descriptions, by = "TEWHERE")
atusact_2017 <- atusact_2017 %>% relocate(where_cat, .after = TEWHERE)  


# read in 2017 ATUS CPS data to add in CBSA column
# also available from https://www.bls.gov/tus/data/datafiles_2017.htm
atuscps_2017 <- read.table("atuscps_2017.dat", header = TRUE, sep = ",")

atuscps_2017_cbsa <- atuscps_2017 %>%
  distinct(TUCASEID, GTCBSA, .keep_all = TRUE) %>%
  select(TUCASEID, GTCBSA) 
atusact_2017 <- left_join(atusact_2017, atuscps_2017_cbsa, by = "TUCASEID")
atusact_2017 <- atusact_2017 %>% relocate(GTCBSA, .after = TUCASEID)


# filter 2017 ATUS to only include respondents from the 11 CBSAs in this article
atusact_2017_11cbsas <- atusact_2017 %>% 
  filter(GTCBSA == "14460" | GTCBSA == "16980" | GTCBSA == "19100" | GTCBSA == "19820" | GTCBSA == "31080" | GTCBSA == "33100" | GTCBSA == "35620" | GTCBSA == "37980" | GTCBSA == "41860" | GTCBSA == "42660" | GTCBSA == "47900")



# ensure all activity codes are 6 digits, the leading zero was dropped when reading in the data
atusact_2017_11cbsas$TRCODE <- sprintf("%06d", atusact_2017_11cbsas$TRCODE)

# read in csv, cleaned, edited and formatted version of ATUS 2017 Activity Coding Lexicons excel file
# originally available from: https://www.bls.gov/tus/data/datafiles_2017.htm
# under the 2017 Survey Documentation section, "ATUS 2017 Coding Lexicon: with activity examples (.XLS)"
codelex_atusact_2017 <- read.csv("activity_coding_lexicons_2017.csv")
codelex_atusact_2017$TRCODE <- sprintf("%06d", codelex_atusact_2017$TRCODE)

# join and move
atusact_2017_11cbsas <- left_join(atusact_2017_11cbsas, codelex_atusact_2017, by = "TRCODE")
atusact_2017_11cbsas <- atusact_2017_11cbsas %>% relocate(activity_cat, .after = TRCODE) 


# drop columns from data frame not needed for this article / analysis
atusact_2017_11cbsas <- atusact_2017_11cbsas %>%
  select(- TRTCCTOT_LN, -TRTCC_LN, -TRTCOC_LN,	-TRTEC_LN,	-TRTHH_LN,	-TRTNOHH_LN,	-TRTOHH_LN,	-TRTONHH_LN,	-TRTO_LN,	-TUCC5,	-TUCC5B,	-TUCC7,	-TUCC8,	-TUCUMDUR,	-TUCUMDUR24,	-TUDURSTOP, -TUEC24, -TUTIER1CODE,	-TUTIER2CODE,	-TUTIER3CODE,	-TRTIER2,	-TXWHERE)


# add in family income column from atuscps_2017
atus_family_income <- atuscps_2017 %>%
  filter(TULINENO == 1) %>%
  select(TUCASEID, HEFAMINC)

atusact_2017_11cbsas <- left_join(atusact_2017_11cbsas, atus_family_income, by = "TUCASEID")



# using family income to assign an income quartile to each ATUS respondent, based on the income quartile breaks of the mobility data as published in Moro et. al (2021)
# the atus definition of each of the HEFAM assignments (between 1 and 16) can be found in the ATUS-CPS Data Dictionary: https://www.bls.gov/tus/dictionaries/atuscpscodebk22.pdf
atusact_2017_11cbsas <- atusact_2017_11cbsas %>%
  mutate(inc_quart = case_when(GTCBSA == 14460 & HEFAMINC <=12 ~ 1,
                                     GTCBSA == 14460 & HEFAMINC == 13 ~ 2, 
                                     GTCBSA == 14460 & HEFAMINC == 14 ~ 2, 
                                     GTCBSA == 14460 & HEFAMINC == 15 ~ 3,
                                     GTCBSA == 14460 & HEFAMINC == 16 ~ 4,
                                     GTCBSA == 16980 & HEFAMINC <=11 ~ 1,
                                     GTCBSA == 16980 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 16980 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 16980 & HEFAMINC == 14 ~3,
                                     GTCBSA == 16980 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 19100 & HEFAMINC <= 11 ~ 1,
                                     GTCBSA == 19100 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 19100 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 19100 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 19100 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 19820 & HEFAMINC <= 10 ~ 1,
                                     GTCBSA == 19820 & HEFAMINC == 11 ~ 2,
                                     GTCBSA == 19820 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 19820 & HEFAMINC == 13 ~ 3,
                                     GTCBSA == 19820 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 19820 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 31080 & HEFAMINC <= 11 ~ 1,
                                     GTCBSA == 31080 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 31080 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 31080 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 31080 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 33100 & HEFAMINC <= 10 ~ 1,
                                     GTCBSA == 33100 & HEFAMINC == 11 ~ 2,
                                     GTCBSA == 33100 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 33100 & HEFAMINC == 13 ~ 3,
                                     GTCBSA == 33100 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 33100 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 35620 & HEFAMINC <= 12 ~ 1,
                                     GTCBSA == 35620 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 35620 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 35620 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 37980 & HEFAMINC <= 11 ~ 1,
                                     GTCBSA == 37980 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 37980 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 37980 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 37980 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 41860 & HEFAMINC <= 12 ~ 1,
                                     GTCBSA == 41860 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 41860 & HEFAMINC == 14 ~ 2,
                                     GTCBSA == 41860 & HEFAMINC == 15 ~ 3,
                                     GTCBSA == 41860 & HEFAMINC == 16 ~ 4,
                                     GTCBSA == 42660 & HEFAMINC <= 11 ~ 1,
                                     GTCBSA == 42660 & HEFAMINC == 12 ~ 2,
                                     GTCBSA == 42660 & HEFAMINC == 13 ~ 2,
                                     GTCBSA == 42660 & HEFAMINC == 14 ~ 3,
                                     GTCBSA == 42660 & HEFAMINC >= 15 ~ 4,
                                     GTCBSA == 47900 & HEFAMINC <= 13 ~ 1,
                                     GTCBSA == 47900 & HEFAMINC == 14 ~ 2,
                                     GTCBSA == 47900 & HEFAMINC == 15 ~ 3,
                                     GTCBSA == 47900 & HEFAMINC == 16 ~4))


# relocate new column
atusact_2017_11cbsas <- atusact_2017_11cbsas %>% relocate(inc_quart, .after = TUCASEID)



## ATUS Activity Minutes per Hour ##
# code to clean, transform, and calculate to create 24 additional columns with the number of minutes per hour for each activity (row)
atusact_2017_11cbsas_24h <- atusact_2017_11cbsas

atusact_2017_11cbsas_24h$tustarttime <- strptime(atusact_2017_11cbsas_24h$TUSTARTTIM, format = "%H: %M")
atusact_2017_11cbsas_24h$tustoptime24 <- atusact_2017_11cbsas_24h$tustarttime + (atusact_2017_11cbsas_24h$TUACTDUR24 * 60)

atusact_2017_11cbsas_24h$endhr <- str_sub(atusact_2017_11cbsas_24h$tustoptime24, start = 12, end = 13)
atusact_2017_11cbsas_24h$endhr <- as.integer(atusact_2017_11cbsas_24h$endhr)
atusact_2017_11cbsas_24h$endhr[is.na(atusact_2017_11cbsas_24h$endhr)] <- 0

# create new column for starthr by cropping TUSTARTTIM
atusact_2017_11cbsas_24h$starthr <- str_sub(atusact_2017_11cbsas_24h$TUSTARTTIM, 1, 2)
atusact_2017_11cbsas_24h$starthr <- as.integer(atusact_2017_11cbsas_24h$starthr)

# create new column for startmin by cropping TUSTARTTIM
atusact_2017_11cbsas_24h$startmin <- str_sub(atusact_2017_11cbsas_24h$TUSTARTTIM, 4, 5)
atusact_2017_11cbsas_24h$startmin <- as.integer(atusact_2017_11cbsas_24h$startmin)


atusact_2017_11cbsas_24h$endhr <- ifelse(atusact_2017_11cbsas_24h$endhr < atusact_2017_11cbsas_24h$starthr,atusact_2017_11cbsas_24h$endhr + 24,atusact_2017_11cbsas_24h$endhr)
summary(atusact_2017_11cbsas_24h$endhr)


for (i in 0:28) {
  new_col_name <- paste('hr_mins',(i),sep='_')
  atusact_2017_11cbsas_24h[new_col_name]<-ifelse((i) == atusact_2017_11cbsas_24h$starthr & atusact_2017_11cbsas_24h$starthr==atusact_2017_11cbsas_24h$endhr,atusact_2017_11cbsas_24h$TUACTDUR24,
                             ifelse((i) == atusact_2017_11cbsas_24h$starthr & atusact_2017_11cbsas_24h$starthr!=atusact_2017_11cbsas_24h$endhr,60 - atusact_2017_11cbsas_24h$startmin,
                                    ifelse((i) > atusact_2017_11cbsas_24h$starthr & (i) < atusact_2017_11cbsas_24h$endhr,60,
                                           ifelse((i) == atusact_2017_11cbsas_24h$endhr & atusact_2017_11cbsas_24h$starthr != atusact_2017_11cbsas_24h$endhr,atusact_2017_11cbsas_24h$TUACTDUR24-(60+60*(atusact_2017_11cbsas_24h$endhr - atusact_2017_11cbsas_24h$starthr-1)-atusact_2017_11cbsas_24h$startmin),0
                                           ))))
}


for (i in ncol(atusact_2017_11cbsas_24h):(ncol(atusact_2017_11cbsas_24h)-max(atusact_2017_11cbsas_24h$endhr)+24)) {
  atusact_2017_11cbsas_24h[i-24]<-atusact_2017_11cbsas_24h[i]+atusact_2017_11cbsas_24h[i-24]
}


atusact_2017_11cbsas_24h <- atusact_2017_11cbsas_24h %>%
  select(-hr_mins_24, -hr_mins_25, -hr_mins_26, -hr_mins_27, -hr_mins_28)



## ATUS Internal Weighting ##
# read in the ATUS 2017 Respondent file, available from: https://www.bls.gov/tus/data/datafiles_2017.htm
atusresp_2017 <- read.table("atusresp_2017.dat", header = TRUE, sep = ",")

# the final weight ATUS assigns to each respondent:
# ensures each demographic groups is correctly represented in the population (age, sex, race, and Hispanic ethnicity of respondent, educational attainment, and the presence of household children)
# calculated so that each day of the week is correctly represented for the sample quarter
# accounts for different response rates across demographic groups as described in the Methods section of the article 
atus_internal_wt <- atusresp_2017 %>%
  select(TUCASEID, TUFINLWGT)

atusact_2017_11cbsas_24h <- left_join(atusact_2017_11cbsas_24h, atus_internal_wt, by = "TUCASEID")
atusact_2017_11cbsas_24h <- atusact_2017_11cbsas_24h %>%
  relocate(TUFINLWGT, .after = TUCASEID)


# calculate ATUS denominator, to be use for all hrs (from Methods, Eq. 2)
# sum of all weights for our 2502 respondents x 60)

# One row per respondent
atusact_2017_11cbsas_24h_respondents <- atusact_2017_11cbsas_24h %>%
  select(TUCASEID, TUFINLWGT, inc_quart) %>%
  distinct()


# split by quartile 
atusact_2017_11cbsas_24h_respondentsQ1 <- atusact_2017_11cbsas_24h_respondents %>%
  filter(inc_quart == 1)
atusact_2017_11cbsas_24h_respondentsQ2 <- atusact_2017_11cbsas_24h_respondents %>%
  filter(inc_quart == 2)
atusact_2017_11cbsas_24h_respondentsQ3 <- atusact_2017_11cbsas_24h_respondents %>%
  filter(inc_quart == 3)
atusact_2017_11cbsas_24h_respondentsQ4 <- atusact_2017_11cbsas_24h_respondents %>%
  filter(inc_quart == 4)


# store 4 ATUS denominators as 4 values
atusact_2017_11cbsas_24h_respondentsQ1_denominator <- sum(atusact_2017_11cbsas_24h_respondentsQ1$TUFINLWGT * 60)
atusact_2017_11cbsas_24h_respondentsQ2_denominator <- sum(atusact_2017_11cbsas_24h_respondentsQ2$TUFINLWGT * 60)
atusact_2017_11cbsas_24h_respondentsQ3_denominator <- sum(atusact_2017_11cbsas_24h_respondentsQ3$TUFINLWGT * 60)  
atusact_2017_11cbsas_24h_respondentsQ4_denominator <- sum(atusact_2017_11cbsas_24h_respondentsQ4$TUFINLWGT * 60)  
  

# calculating the numerator of the ratio (R, Eq. 2 in the Methods) for each POI category, hour, and income quartile
### FOOD ESTABLISHMENTS ###
atusact_2017_11cbsas_24h_fc <- atusact_2017_11cbsas_24h %>%
  filter(TEWHERE == 4 | TEWHERE == 7 & TRCODE == 110101 | TEWHERE == 7 & TRCODE == 070103 | TEWHERE == 11 & TRCODE == 110101 | TEWHERE == 11 & TRCODE == 070103) %>%
  filter(TRCODE != "050101" & TRCODE != "050102" & TRCODE != "050104" & TRCODE != "180501")

# Q1 #
atusact_2017_11cbsas_24h_fc_Q1 <- atusact_2017_11cbsas_24h_fc %>%
  filter(inc_quart == 1)

atusact_2017_11cbsas_24h_fc_Q1 <- atusact_2017_11cbsas_24h_fc_Q1 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator)



# Food Q2 #
atusact_2017_11cbsas_24h_fc_Q2 <- atusact_2017_11cbsas_24h_fc %>%
  filter(inc_quart == 2)

atusact_2017_11cbsas_24h_fc_Q2 <- atusact_2017_11cbsas_24h_fc_Q2 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator)  



# Food Q3 #
atusact_2017_11cbsas_24h_fc_Q3 <- atusact_2017_11cbsas_24h_fc %>%
  filter(inc_quart == 3)

atusact_2017_11cbsas_24h_fc_Q3 <- atusact_2017_11cbsas_24h_fc_Q3 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator)



# Food Q4 #
atusact_2017_11cbsas_24h_fc_Q4 <- atusact_2017_11cbsas_24h_fc %>%
  filter(inc_quart == 4)

atusact_2017_11cbsas_24h_fc_Q4 <- atusact_2017_11cbsas_24h_fc_Q4 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator)


# bind rows into 1 data frame
atusact_2017_11cbsas_24h_fc_allQ <- bind_rows(atusact_2017_11cbsas_24h_fc_Q1, atusact_2017_11cbsas_24h_fc_Q2, atusact_2017_11cbsas_24h_fc_Q3, atusact_2017_11cbsas_24h_fc_Q4) 


# repeat process for remaining 2 POI categories
### GROCERY STORES ###
atusact_2017_11cbsas_24h_grocery <- atusact_2017_11cbsas_24h %>%
  filter(TEWHERE == 6) %>%
  filter(TRCODE != "050101" & TRCODE != "050102" & TRCODE != "050104" & TRCODE != "180501")

# Q1 #
atusact_2017_11cbsas_24h_grocery_Q1 <- atusact_2017_11cbsas_24h_grocery %>%
  filter(inc_quart == 1)

atusact_2017_11cbsas_24h_grocery_Q1 <- atusact_2017_11cbsas_24h_grocery_Q1 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator)



# Grocery Q2 #
atusact_2017_11cbsas_24h_grocery_Q2 <- atusact_2017_11cbsas_24h_grocery %>%
  filter(inc_quart == 2)

atusact_2017_11cbsas_24h_grocery_Q2 <- atusact_2017_11cbsas_24h_grocery_Q2 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator)  



# Grocery Q3 #
atusact_2017_11cbsas_24h_grocery_Q3 <- atusact_2017_11cbsas_24h_grocery %>%
  filter(inc_quart == 3)

atusact_2017_11cbsas_24h_grocery_Q3 <- atusact_2017_11cbsas_24h_grocery_Q3 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator)



# Grocery Q4 #
atusact_2017_11cbsas_24h_grocery_Q4 <- atusact_2017_11cbsas_24h_grocery %>%
  filter(inc_quart == 4)

atusact_2017_11cbsas_24h_grocery_Q4 <- atusact_2017_11cbsas_24h_grocery_Q4 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator)


# bind rows into 1 data frame
atusact_2017_11cbsas_24h_grocery_allQ <- bind_rows(atusact_2017_11cbsas_24h_grocery_Q1, atusact_2017_11cbsas_24h_grocery_Q2, atusact_2017_11cbsas_24h_grocery_Q3, atusact_2017_11cbsas_24h_grocery_Q4) 


### GYMS ###
atusact_2017_11cbsas_24h_gym <- atusact_2017_11cbsas_24h %>%
  filter(TEWHERE == 31) %>%
  filter(TRCODE != "050101" & TRCODE != "050102" & TRCODE != "050104" & TRCODE != "180501")

# Q1 #
atusact_2017_11cbsas_24h_gym_Q1 <- atusact_2017_11cbsas_24h_gym %>%
  filter(inc_quart == 1)

atusact_2017_11cbsas_24h_gym_Q1 <- atusact_2017_11cbsas_24h_gym_Q1 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ1_denominator)



# gym Q2 #
atusact_2017_11cbsas_24h_gym_Q2 <- atusact_2017_11cbsas_24h_gym %>%
  filter(inc_quart == 2)

atusact_2017_11cbsas_24h_gym_Q2 <- atusact_2017_11cbsas_24h_gym_Q2 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ2_denominator)  



# gym Q3 #
atusact_2017_11cbsas_24h_gym_Q3 <- atusact_2017_11cbsas_24h_gym %>%
  filter(inc_quart == 3)

atusact_2017_11cbsas_24h_gym_Q3 <- atusact_2017_11cbsas_24h_gym_Q3 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ3_denominator)



# gym Q4 #
atusact_2017_11cbsas_24h_gym_Q4 <- atusact_2017_11cbsas_24h_gym %>%
  filter(inc_quart == 4)

atusact_2017_11cbsas_24h_gym_Q4 <- atusact_2017_11cbsas_24h_gym_Q4 %>%
  summarize(hr_mins_0w = (sum(hr_mins_0*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_1w = (sum(hr_mins_1*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_2w = (sum(hr_mins_2*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_3w = (sum(hr_mins_3*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_4w = (sum(hr_mins_4*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_5w = (sum(hr_mins_5*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_6w = (sum(hr_mins_6*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_7w = (sum(hr_mins_7*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_8w = (sum(hr_mins_8*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_9w = (sum(hr_mins_9*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_10w = (sum(hr_mins_10*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_11w = (sum(hr_mins_11*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_12w = (sum(hr_mins_12*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_13w = (sum(hr_mins_13*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_14w = (sum(hr_mins_14*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_15w = (sum(hr_mins_15*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_16w = (sum(hr_mins_16*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_17w = (sum(hr_mins_17*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_18w = (sum(hr_mins_18*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_19w = (sum(hr_mins_19*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_20w = (sum(hr_mins_20*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_21w = (sum(hr_mins_21*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_22w = (sum(hr_mins_22*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator,
            hr_mins_23w = (sum(hr_mins_23*TUFINLWGT)) / atusact_2017_11cbsas_24h_respondentsQ4_denominator)


# bind rows into 1 data frame
atusact_2017_11cbsas_24h_gym_allQ <- bind_rows(atusact_2017_11cbsas_24h_gym_Q1, atusact_2017_11cbsas_24h_gym_Q2, atusact_2017_11cbsas_24h_gym_Q3, atusact_2017_11cbsas_24h_gym_Q4) 




#### END ####
