

# install.packages(c("arm", "foreign","haven","dplyr","readxl",
#                    "forcats","lme4","Matrix"))

packages <- c("arm", "foreign","haven","dplyr","readxl",
              "forcats","lme4","Matrix", 'tidyverse')

# Use lapply to load all packages
lapply(packages, library, character.only = TRUE)


#----------------------------------------------------


#define ad-hoc functions

#function to change the column names of the datasets 
#downloaded from the census website
change_census_colnames = function(df) {
  colnames(df)[colnames(df)=='Post.2019.Westminster.Parliamentary.constituencies.Code'] = 'constituency_code'
  colnames(df)[colnames(df)=='Post.2019.Westminster.Parliamentary.constituencies'] = 'constituency'
  colnames(df)[colnames(df)=='Age..6.categories..Code'] = 'ageGroup_numerised'
  colnames(df)[colnames(df)=='Age..6.categories.'] = 'ageGroup'
  colnames(df)[colnames(df)=='Highest.level.of.qualification..7.categories..Code'] = 'edlevel_numerised'
  colnames(df)[colnames(df)=='Highest.level.of.qualification..7.categories.'] = 'edlevel'
  colnames(df)[colnames(df)=='General.health..4.categories..Code'] = 'health_cat_code'
  colnames(df)[colnames(df)=='General.health..4.categories.'] = 'health_cat'
  colnames(df)[colnames(df)=='Ethnic.group..20.categories..Code'] = 'ethn_code'
  colnames(df)[colnames(df)=='Ethnic.group..20.categories.'] = 'ethn_cat'
  colnames(df)[colnames(df)=='Sex..2.categories..Code'] = 'sex_code'
  colnames(df)[colnames(df)=='Sex..2.categories.'] = 'sex'
  colnames(df)[colnames(df)=='National.Statistics.Socio.economic.Classification..NS.SeC...10.categories..Code'] = 'ses_code'
  colnames(df)[colnames(df)=='National.Statistics.Socio.economic.Classification..NS.SeC...10.categories.'] = 'ses'
  colnames(df)[colnames(df)=='Religion..10.categories.'] = 'religion'
  colnames(df)[colnames(df)=='Religion..10.categories..Code'] = 'religion_numerised'
  colnames(df)[colnames(df)=='Observation'] = 'frequency'
  
  return(df)
}

#function to convert character variables to factors
convert_char_to_factor <- function(df) {
  df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
  return(df)
}

#-------------------------

#process census data for area-level df

#load the census area-level dfs
census_age = read.csv('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/area-level variables/unprocessed/census_age.csv')
census_edu = read.csv('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/area-level variables/unprocessed/census_education.csv')
census_health = read.csv('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/area-level variables/unprocessed/census_health.csv')
census_ethn = read.csv('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/area-level variables/unprocessed/census_ethnicity.csv')

#census age

census_age = change_census_colnames(census_age)

census_age = convert_char_to_factor(census_age)

# reshape the dataframe from long to wide format
census_age <- census_age %>%
  pivot_wider(
    id_cols = c(constituency_code, constituency),  # Include identifiers
    names_from = ageGroup,
    values_from = frequency,
    values_fill = list(frequency = 0)  # Fill missing values with 0
  )

#get rid of under-15 column
census_age = census_age[,-3]

#calculate population for each constituency
census_age = census_age %>%
  mutate(const_pop = rowSums(select(., where(is.integer))))

# calculate the proportions for each age group and add new columns with '_prop' suffix
census_age <- census_age %>%
  mutate(across(where(is.integer) & !starts_with("const_pop"), 
                .fns = ~ round(. / const_pop, 2), 
                .names = "{.col}_prop"))

#drop all the age group columns with the absolute values
census_age <- census_age %>%
  select(-where(is.integer))

#rename columns
census_age <- census_age %>%
  rename_with(
    .cols = where(is.numeric), 
    .fn = ~ str_replace_all(str_remove_all(str_to_lower(.), "years"), " ", "_")
  )

write_csv(census_age, 'census_age.csv')


#--------------------------------------


#census education

census_edu = change_census_colnames(census_edu)

census_edu = convert_char_to_factor(census_edu)

#reshape the dataframe from long to wide format
census_edu <- census_edu %>%
  pivot_wider(
    id_cols = c(constituency_code, constituency),  # Include identifiers
    names_from = edlevel,
    values_from = frequency,
    values_fill = list(frequency = 0)  # Fill missing values with 0
  )

#calculate population for each constituency
census_edu = census_edu %>%
  mutate(const_pop = rowSums(select(., where(is.integer))))

#calculate the proportions for each age group and add new columns with '_prop' suffix
census_edu <- census_edu %>%
  mutate(across(where(is.integer) & !starts_with("const_pop"), 
                .fns = ~ round(. / const_pop, 2), 
                .names = "{.col}_prop"))

#drop all the edu group columns with the absolute values
census_edu <- census_edu %>%
  select(-where(is.integer))

colnames_edu = c("constituency_code","constituency",                                                                                                                                                                                                                                                                                                                             
                 "const_pop","edu_NA_prop",                                                                                                                                                                                                                                                                                                                      
                 "edu_no_qualifications_prop",
                 'edu_level1_prop','edu_level2_prop',
                 'edu_level3_prop','edu_level4_prop',
                 'edu_other_prop')

colnames(census_edu) = colnames_edu

write_csv(census_edu, 'census_edu.csv')


#------------------------------------------------------------------


#census health

census_health = change_census_colnames(census_health)

census_health = convert_char_to_factor(census_health)

#reshape the dataframe from long to wide format
census_health <- census_health %>%
  pivot_wider(
    id_cols = c(constituency_code, constituency),  # Include identifiers
    names_from = health_cat,
    values_from = frequency,
    values_fill = list(frequency = 0)  # Fill missing values with 0
  )

#get rid of 'does not apply' column
census_health = census_health[,-3]

#calculate population for each constituency
census_health = census_health %>%
  mutate(const_pop = rowSums(select(., where(is.integer))))

#calculate the proportions for each age group and add new columns with '_prop' suffix
census_health <- census_health %>%
  mutate(across(where(is.integer) & !starts_with("const_pop"), 
                .fns = ~ round(. / const_pop, 2), 
                .names = "{.col}_prop"))

#drop all the health group columns with the absolute values
census_health <- census_health %>%
  select(-where(is.integer))

#replace empty spaces with underscores in colnames
census_health <- census_health %>%
  rename_with(
    .cols = where(is.numeric), 
    .fn = ~ str_replace_all(str_to_lower(.), " ", "_")
  )

write_csv(census_health, 'census_health.csv')

#------------------------------------------------------------------


#census ethnicity

census_ethn = change_census_colnames(census_ethn)

census_ethn = convert_char_to_factor(census_ethn)

#reshape the dataframe from long to wide format
census_ethn <- census_ethn %>%
  pivot_wider(
    id_cols = c(constituency_code, constituency),  # Include identifiers
    names_from = ethn_cat,
    values_from = frequency,
    values_fill = list(frequency = 0)  # Fill missing values with 0
  )

#get rid of 'does not apply' column
census_ethn = census_ethn[,-3]

#calculate population for each constituency
census_ethn = census_ethn %>%
  mutate(const_pop = rowSums(select(., where(is.integer))))

#calculate the proportions for each age group and add new columns with '_prop' suffix
census_ethn <- census_ethn %>%
  mutate(across(where(is.integer) & !starts_with("const_pop"), 
                .fns = ~ round(. / const_pop, 2), 
                .names = "{.col}_prop"))

#drop all the ethn group columns with the absolute values
census_ethn <- census_ethn %>%
  select(-where(is.integer))

colnames_ethn = c('constituency_code' ,
                  'constituency' ,
                  'const_pop' ,
                  'bangladeshi_prop' ,
                  'chinese_prop' ,
                  'indian_prop' ,
                  'pakistani_prop' ,
                  'other_asian_prop' ,
                  'african_prop' ,
                  'caribbean_prop' ,
                  'other_black_prop' ,
                  'mixed_white_and_asian_prop',
                  'mixed_white_and_african_prop' ,
                  'mixed_white_and_caribbean_prop' ,
                  'other_mixed_prop' ,
                  'white_british_or_NI_prop' ,
                  'white_irish_prop' ,
                  'white_gypsy_or_irish_traveller_prop',
                  'white_roma_prop' ,
                  'other_white_prop' ,
                  'arab_prop' ,
                  'other_ethnicity_prop')

colnames(census_ethn) = colnames_ethn

write_csv(census_ethn, 'census_ethn.csv')

#---------------

#merge all the census dfs

#remove constituency name and constituency population 
#columns from all but one df (census_age)

census_edu_1 = census_edu[,-c(2,3)]
census_ethn_1 = census_ethn[,-c(2,3)]
census_health_1 = census_health[,-c(2,3)]

census_area_level = merge(census_age,census_edu_1, 
                          by = 'constituency_code')

census_area_level = merge(census_area_level,census_ethn_1,
                          by = 'constituency_code')

census_area_level = merge(census_area_level,
                          census_health_1,
                          by = 'constituency_code')


#--------------------------------------------------------------


#individual-level df

#load British Election Study (BES) file

individual_level = read.csv('./bes.csv')

colnames(individual_level)[colnames(individual_level)=='new_pcon_codeW28'] = 'new_constituency_code'

# add new ONS constituency codes

ons_constituency_codes = read.csv('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/Parliamentary_Constituencies_2024.csv')

ons_constituency_codes = ons_constituency_codes[,1:2]


#change column names
colnames(ons_constituency_codes)[colnames(ons_constituency_codes)=='PCON24NM'] = 'new_constituency'

colnames(ons_constituency_codes)[colnames(ons_constituency_codes)=="X...PCON24CD"] = 'new_constituency_code'

individual_level = merge(individual_level, ons_constituency_codes, by = 'new_constituency_code')

#-------------------------

# general election 2024 df
#you'll need this df to correlate constituency-level predictions 
#with the actual results

ge_2024 = read_excel('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/HoC-GE2024-results-by-constituency.xlsx',
                     skip = 2)

ge_2024 = convert_char_to_factor(ge_2024)

colnames(ge_2024)[colnames(ge_2024)== "Country name"]<- "country_name"
colnames(ge_2024)[colnames(ge_2024)== "Constituency name"]<- "constituency"

#create column with Labour's vote share
ge_2024$lab_vote_share = round(ge_2024$Lab/ge_2024$`Valid votes`, digits = 2)

#keep only England and Wales
ge_2024_eng_wales = ge_2024[ge_2024$country_name == 'England' | ge_2024$country_name == 'Wales',]

ge_2024_eng_wales$constituency = factor(ge_2024_eng_wales$constituency,
                                        exclude = NULL)

#----------------------


#get rid of NAs in two key columns
columns_of_interest <- c("generalElectionVoteW28", "generalElectionVoteW29")

# filter rows with complete cases in the columns of interest
individual_level <- individual_level[complete.cases(individual_level[, columns_of_interest]), ]

individual_level[columns_of_interest] = lapply(individual_level[columns_of_interest], factor)

levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 1] <- "Conservative"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 2] <- "Labour"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 3] <- "Libdem"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 4] <- "SNP"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 5] <- "Plaid_Cymru"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 7] <- "Green"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 9] <- 'Other'
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 12] <- "Reform"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 13] <- "Independent"
levels(individual_level$generalElectionVoteW28)[levels(individual_level$generalElectionVoteW28) == 9999] <- "dont_know"

levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 1] <- "Conservative"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 2] <- "Labour"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 3] <- "Libdem"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 4] <- "SNP"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 5] <- "Plaid_Cymru"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 7] <- "Green"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 9] <- 'Other'
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 12] <- "Reform"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 13] <- "Independent"
levels(individual_level$generalElectionVoteW29)[levels(individual_level$generalElectionVoteW29) == 9999] <- "dont_know"

#get rid of abstainers
individual_level = subset(individual_level,
                          generalElectionVoteW28 != 0)

individual_level$generalElectionVoteW28 = factor(individual_level$generalElectionVoteW28,
                                                 exclude = NULL)

#change column names

col_names = colnames(individual_level)

# remove strings from column names using rename()
individual_level <- individual_level %>%
  rename_at(vars(col_names), ~ gsub('W28', "", .))

col_names = colnames(individual_level)

individual_level <- individual_level %>%
  rename_at(vars(col_names), ~ gsub('p_', "", .))

colnames(individual_level)[colnames(individual_level)=='gor'] = 'region'
colnames(individual_level)[colnames(individual_level)=='gender'] = 'female'
colnames(individual_level)[colnames(individual_level)=='education'] = 'edlevel'
colnames(individual_level)[colnames(individual_level)=='religion'] = 'religion'
colnames(individual_level)[colnames(individual_level)=='generalElectionVoteW19'] = 'vote_2019'
colnames(individual_level)[colnames(individual_level)=='generalElectionVote'] = 'vote_intention'
colnames(individual_level)[colnames(individual_level)=='generalElectionVoteW29'] = 'vote_2024'

#remove empty factor levels
individual_level$vote_2019 = factor(individual_level$vote_2019, exclude = NULL)
individual_level$vote_intention = factor(individual_level$vote_intention, exclude = NULL)

#create ageGroup categories
individual_level$age = as.numeric(individual_level$age)

individual_level$ageGroup <- cut(individual_level$age,
                    breaks = c(16, 25, 35, 50, 65, Inf),
                    labels = c("Aged 16 to 24 years",
                               "Aged 25 to 34 years",
                               "Aged 35 to 49 years",
                               "Aged 50 to 64 years",
                               "Aged 65 years and over"))


#create dummy variables for the different values vote_intention
#can take on
dummy_vars <- model.matrix(~ vote_intention - 1, data = individual_level)
individual_level <- cbind(individual_level, dummy_vars)

#convert character variables to factors
char_to_factor = c("new_constituency_code",         
                   "id",                             "pcon",                          
                   "new_pcon_codeW19",               "vote_2019",                     
                   "vote_intention",                 "vote_2024",                     
                   "euRefVoteW9",                    "female",                        
                   "edlevel",                                                   
                   "region",                         "religion",                      
                   "new_constituency",               "ageGroup",                      
                   "vote_intentionConservative",    
                   "vote_intentionLabour",           "vote_intentionLibdem",          
                   "vote_intentionSNP",              "vote_intentionPlaid_Cymru",     
                   "vote_intentionGreen",            "vote_intentionOther",           
                   "vote_intentionReform",           "vote_intentionIndependent",     
                   "vote_intentiondont_know"       )

individual_level[char_to_factor] = lapply(individual_level[char_to_factor], factor)


#------------

#edlevel

individual_level$edlevel <- fct_collapse(individual_level$edlevel, "Does not apply" = c(19,20))

individual_level$edlevel <- fct_collapse(individual_level$edlevel,
                            "non_uni_qualification" = c(2,3,4,5,6,7,8,9,10,11,12))

individual_level$edlevel <- fct_collapse(individual_level$edlevel,
                            "uni_professional_qualifications" = c(15,16,17,13,14,18))

levels(individual_level$edlevel)[levels(individual_level$edlevel) == 1] <- "No qualifications"

individual_level = subset(individual_level, 
                          edlevel != 'Does not apply')

individual_level$edlevel = factor(individual_level$edlevel,
                                  exclude = NULL)

#--------

levels(individual_level$region)[levels(individual_level$region) == 1] <- "North East"
levels(individual_level$region)[levels(individual_level$region) == 2] <- "North West"
levels(individual_level$region)[levels(individual_level$region) == 3] <- "Yorkshire and the Humber"
levels(individual_level$region)[levels(individual_level$region) == 4] <- "East Midlands"
levels(individual_level$region)[levels(individual_level$region) == 5] <- "West Midlands"
levels(individual_level$region)[levels(individual_level$region) == 6] <- "East of England"
levels(individual_level$region)[levels(individual_level$region) == 7] <- "London"
levels(individual_level$region)[levels(individual_level$region) == 8] <- "South East"
levels(individual_level$region)[levels(individual_level$region) == 9] <- "South West"
levels(individual_level$region)[levels(individual_level$region) == 10] <- "Wales"
levels(individual_level$region)[levels(individual_level$region) == 11] <- "Scotland"

#remove Scottish constituencies as the ONS data are only for
#England and Wales

individual_level = subset(individual_level, individual_level$region != 'Scotland')

#remove empty level in regions and new_constituencies
individual_level$region = factor(individual_level$region, exclude = NULL)
individual_level$new_constituency = factor(individual_level$new_constituency,
                                           exclude = NULL)

#correct encoding of Ynys Mon
# levels(individual_level$new_constituency)[levels(individual_level$new_constituency) == "Ynys M<U+00F4>n"] <- "Ynys Mon"
# levels(area_level$constituency)[levels(area_level$constituency) == "Ynys M<U+00F4>n"] <- "Ynys Mon"
# levels(psf$constituency)[levels(psf$constituency) == "Ynys M<U+00F4>n"] <- "Ynys Mon"


#create variable indicating whether respondent voted for Labour 
individual_level = individual_level %>%
  mutate(voted_labour = ifelse(individual_level$vote_2024 == 'Labour', 1, 0))

#save df
write_csv(individual_level, 'individual_level.csv')

#create a lookup from individual_level to show which constituencies
#are in which regions. You will need this to add a region column in
#the poststratification frame

region_to_constituency = individual_level[,c(13,15)]

region_to_constituency <- region_to_constituency %>%
  distinct(new_constituency, .keep_all = TRUE) %>%
  select(region, new_constituency)

#--------------------------------------------------------


#merge 2019 general election results with census area-level dataset

#load the 2019 general election results formatted by Chris Hanretty as if the 2024 
#constituency boundaries had been used at the 2019 general election
ge_2019 = read_excel('/estimates-2019-ge-result-new-constituencies.xlsx',
                     sheet = '2. results')

ge_2019 = read_excel('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/estimates-2019-ge-result-new-constituencies.xlsx',
                     sheet = '2. results')

ge_2019 = data.frame(ge_2019)

ge_2019 = convert_char_to_factor(ge_2019)

#calculate party shares of the vote

colnames(ge_2019)[colnames(ge_2019)== "X.turnout."] = 'turnout'

#column with N of voters who turned out
ge_2019 <- ge_2019 %>%
  dplyr::mutate(voters_turned_out = round(electorate * (turnout / 100)))

ge_2019 = ge_2019 %>%
          mutate(con_share = round(Conv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(lab_share = round( Labv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(libdem_share = round( LDv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(snp_share = round( SNPv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(brexit_party_share = round( Brxv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(green_share = round( Grnv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(pc_share = round( PCv/voters_turned_out, digits = 2))

ge_2019 = ge_2019 %>%
  mutate(other_share = round(TotOthv/voters_turned_out, digits = 2))

colnames(ge_2019)[colnames(ge_2019)== "PA.name"] = 'constituency'

#replace all the '&' signs with 'and'
ge_2019$constituency <- gsub("&",
                             "and",
                              as.character(ge_2019$constituency))

ge_2019$constituency = as.factor(ge_2019$constituency)

# harmonise area_level constituency names with the others 

levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Basildon South and East Thurrock"] <- "South Basildon and East Thurrock"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Bedfordshire Mid"] <- "Mid Bedfordshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Bedfordshire North"] <- "North Bedfordshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Bridlington and the Wolds"] <- "Bridlington and The Wolds"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Buckinghamshire Mid"] <- "Mid Buckinghamshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cambridgeshire North East"] <- "North East Cambridgeshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cambridgeshire North West"] <- "North West Cambridgeshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cambridgeshire South"] <- "South Cambridgeshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cheshire Mid"] <- "Mid Cheshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cornwall North"] <- "North Cornwall"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cornwall South East"] <- "South East Cornwall"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cotswolds North"] <- "North Cotswolds"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Cotswolds South"] <- "South Cotswolds"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Derbyshire Mid"] <- "Mid Derbyshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Derbyshire North East"] <- "North East Derbyshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Derbyshire South"] <- "South Derbyshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Devon Central"] <- "Central Devon"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Devon North"] <- "North Devon"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Devon South"] <- "South Devon"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Devon South West"] <- "South West Devon"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Dorset Mid and North Poole"] <- "Mid Dorset and North Poole"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Dorset North"] <- "North Dorset"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Dorset South"] <- "South Dorset"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Dorset West"] <- "West Dorset"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Durham North"] <- "North Durham"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Durham, City of"] <- "City of Durham"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Essex North West"] <- "North West Essex"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hampshire East"] <- "East Hampshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hampshire North East"] <- "North East Hampshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hampshire North West"] <- "North West Hampshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Herefordshire North"] <- "North Herefordshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hertfordshire North East"] <- "North East Hertfordshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hertfordshire South West"] <- "South West Hertfordshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hull East"] <- "Kingston upon Hull East"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hull North and Cottingham"] <- "Kingston upon Hull North and Cottingham"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Hull West and Haltemprice"] <- "Kingston upon Hull West and Haltemprice"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Lancashire West"] <- "West Lancashire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Leicestershire Mid"] <- "Mid Leicestershire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Leicestershire North West"] <- "North West Leicestershire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Leicestershire South"] <- "South Leicestershire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Norfolk Mid"] <- "Mid Norfolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Norfolk North"] <- "North Norfolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Norfolk North West"] <- "North West Norfolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Norfolk South"] <- "South Norfolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Norfolk South West"] <- "South West Norfolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Northamptonshire South"] <- "South Northamptonshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Northumberland North"] <- "North Northumberland"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Pembrokeshire Mid and South"] <- "Mid and South Pembrokeshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Shropshire North"] <- "North Shropshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Shropshire South"] <- "South Shropshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Somerset North"] <- "North Somerset"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Somerset North East and Hanham"] <- "North East Somerset and Hanham"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "South Holland and the Deepings"] <- "South Holland and The Deepings"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Suffolk Central and North Ipswich"] <- "Central Suffolk and North Ipswich"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Suffolk South"] <- "South Suffolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Suffolk West"] <- "West Suffolk"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Surrey East"] <- "East Surrey"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Sussex Mid"] <- "Mid Sussex"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Thanet East"] <- "East Thanet"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Warwickshire North and Bedworth"] <- "North Warwickshire and Bedworth"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Wiltshire East"] <- "East Wiltshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Wiltshire South West"] <- "South West Wiltshire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Worcestershire West"] <- "West Worcestershire"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Worthing East and Shoreham"] <- "East Worthing and Shoreham"
levels(ge_2019$constituency)[levels(ge_2019$constituency) == "Wrekin, The"] <- "The Wrekin"

#exclude Scottish and Northern Irish constituencies
ge_2019 = subset(ge_2019, country_name != 'Scotland' &
                                 country_name != 'Northern Ireland')

#make sure you get rid of all the empty levels
ge_2019$constituency = factor(ge_2019$constituency,
                                        exclude = NULL)

#only keep relevant columns
ge_2019 = ge_2019[,c(2,3,5:18,110:118)]

# rename levels of 'region' column
levels(ge_2019$region_name)[levels(ge_2019$region) == "EE" ] <- "East of England"
levels(ge_2019$region_name)[levels(ge_2019$region) == "EM" ] <- "East Midlands"
levels(ge_2019$region_name)[levels(ge_2019$region) == "GL" ] <- "London"
levels(ge_2019$region_name)[levels(ge_2019$region) == "NE" ] <- "North East"
levels(ge_2019$region_name)[levels(ge_2019$region) == "NW" ] <- "North West"
levels(ge_2019$region_name)[levels(ge_2019$region) == "SE" ] <- "South East"
levels(ge_2019$region_name)[levels(ge_2019$region) == "SW" ] <- "South West"
levels(ge_2019$region_name)[levels(ge_2019$region) == "WA" ] <- "Wales"
levels(ge_2019$region_name)[levels(ge_2019$region) == "WM" ] <- "West Midlands"
levels(ge_2019$region_name)[levels(ge_2019$region) == "YH" ] <- "Yorkshire and the Humber"

#reorder levels to be consistent with the other dfs
ge_2019$region_name = factor(ge_2019$region_name,
                             levels = c("North East",
                                        "North West",
                                        "Yorkshire and the Humber",
                                        "East Midlands",
                                        "West Midlands",
                                        "East of England",
                                        "London",
                                        "South East",
                                        "South West",
                                        "Wales"))
                                         
colnames(ge_2019)[colnames(ge_2019)== "ONS.code"] = 'constituency_code'

#merge 2019 constituency-level vote share df 
#with census area-level df
area_level = merge(ge_2019,census_area_level,
                   by = 'constituency_code')

colnames(area_level)[colnames(area_level)=="constituency.x"] <- "constituency"

write_csv(area_level, 'area_level.csv')


#-------------------------------------------------------------------------------


#read in Census data, aka poststratification frame (psf)
psf = read.csv('./2021_census_age_edu.csv')
psf = read_csv('/Users/emahieux/Desktop/Campaign Lab Hackathon/Data/MRP/2021_census_age_edu.csv')

#psf = change_census_colnames(psf)

colnames(psf)[colnames(psf)=="Post-2019 Westminster Parliamentary constituencies Code"] = 'constituency_code'
colnames(psf)[colnames(psf)=="Post-2019 Westminster Parliamentary constituencies"] = 'constituency'
colnames(psf)[colnames(psf)=="Age (6 categories) Code"] = 'ageGroup_numerised'
colnames(psf)[colnames(psf)=="Age (6 categories)"] = 'ageGroup'
colnames(psf)[colnames(psf)=="Highest level of qualification (7 categories) Code"] = 'edlevel_numerised'
colnames(psf)[colnames(psf)=="Highest level of qualification (7 categories)"] = 'edlevel'
colnames(psf)[colnames(psf)=='Observation'] = 'frequency'


char_cols = c("constituency_code",
              "constituency",
              "ageGroup_numerised",
              "ageGroup",
              "edlevel_numerised",
              "edlevel")

psf[char_cols] = lapply(psf[char_cols], factor)

#remove all the under-15s from the df
psf = psf[psf$ageGroup_numerised != 1, ]
psf$ageGroup = factor(psf$ageGroup, exclude = NULL)

#order levels from youngest to oldest
psf$ageGroup = factor(psf$ageGroup,
                      levels = c("Aged 16 to 24 years",
                                 "Aged 25 to 34 years",
                                 "Aged 35 to 49 years",
                                 "Aged 50 to 64 years",
                                 "Aged 65 years and over"))

psf$ageGroup_numerised = factor(psf$ageGroup_numerised, exclude = NULL)
psf$ageGroup_numerised = as.integer(psf$ageGroup_numerised)

#add region variable
regions_constituencies = ge_2024_eng_wales[,c(1,3,4)]
colnames(regions_constituencies)[colnames(regions_constituencies)=='ONS ID'] = 'constituency_code'

psf = merge(psf, 
            regions_constituencies,
            by = 'constituency_code')

colnames(psf)[colnames(psf)=='constituency.y'] = 'constituency'
psf = psf[,-2]

colnames(psf)[colnames(psf)=='Region name'] = 'region'

#remove empty levels of Scotland and NI from the psf
psf$region = factor(psf$region,
                    exclude = NULL)

#order levels from youngest to oldest
psf$region = factor(psf$region,
                        levels = c("North East",
                                    "North West",
                                    "Yorkshire and the Humber",
                                    "East Midlands",
                                    "West Midlands",
                                    "East of England",
                                    "London",
                                    "South East",
                                    "South West",
                                    "Wales"))

#--------

#format edlevel column

#remove the 'Does not apply" level
psf = psf[psf$edlevel != "Does not apply", ]
psf$edlevel = factor(psf$edlevel, exclude = NULL)

#consolidate edlevel factor levels so that they are
#consistent with the BES levels
psf$edlevel = fct_collapse(psf$edlevel,
                           "non_uni_qualification" = c("Level 1 and entry level qualifications: 1 to 4 GCSEs grade A* to C, Any GCSEs at other grades, O levels or CSEs (any grades), 1 AS level, NVQ level 1, Foundation GNVQ, Basic or Essential Skills",
                                                       "Level 2 qualifications: 5 or more GCSEs (A* to C or 9 to 4), O levels (passes), CSEs (grade 1), School Certification, 1 A level, 2 to 3 AS levels, VCEs, Intermediate or Higher Diploma, Welsh Baccalaureate Intermediate Diploma, NVQ level 2, Intermediate GNVQ, City and Guilds Craft, BTEC First or General Diploma, RSA Diploma",
                                                       "Level 3 qualifications: 2 or more A levels or VCEs, 4 or more AS levels, Higher School Certificate, Progression or Advanced Diploma, Welsh Baccalaureate Advance Diploma, NVQ level 3; Advanced GNVQ, City and Guilds Advanced Craft, ONC, OND, BTEC National, RSA Advanced Diploma",
                                                       "Other: apprenticeships, vocational or work-related qualifications, other qualifications achieved in England or Wales, qualifications achieved outside England or Wales (equivalent not stated or unknown)"
                           ))

psf$edlevel = fct_collapse(psf$edlevel,
                           "uni_professional_qualifications" = c("Level 4 qualifications or above: degree (BA, BSc), higher degree (MA, PhD, PGCE), NVQ level 4 to 5, HNC, HND, RSA Higher Diploma, BTEC Higher level, professional qualifications (for example, teaching, nursing, accountancy)"
                           ))

#remove all the rows corresponding to "Does not apply" in the highest level
#of edlevel column
psf = psf[psf$edlevel_numerised != -8, ]

psf$edlevel = factor(psf$edlevel,
                     levels = c("No qualifications",
                                "non_uni_qualification",
                                "uni_professional_qualifications"))

#save df
write_csv(psf, file = 'psf.csv')

#-----------------------------------------------------------------------------------

#check that the different dfs have the same constituency names

psf_list = levels(psf$constituency)

individual_level_list = levels(individual_level$new_constituency)

area_level_list = levels(area_level$constituency)

ge_2024_constituency_list = levels(ge_2024_eng_wales$constituency)

#individual_level constituency names vs poststratification frame

only_in_individual_level <- setdiff(individual_level_list, 
                                    psf_list)

only_in_psf <- setdiff(psf_list, 
                       individual_level_list)

#if the below give the output character(0), then the dfs have 
#the same spelling and ordering of constituencies and all is fine
only_in_individual_level
only_in_psf

#individual-level vs area-level (with 2019 vote shares)

only_in_individual_level <- setdiff(individual_level_list, 
                                    area_level_list)

only_in_area_level = setdiff(area_level_list, 
                             individual_level_list)

only_in_individual_level
only_in_area_level

#area-level vs poststratification frame

only_in_area_level <- setdiff(area_level_list, 
                              psf_list)

only_in_poststratification = setdiff(psf_list, 
                                     area_level_list)

only_in_area_level
only_in_poststratification

#individual-level vs ge_2024
only_in_individual_level <- setdiff(individual_level_list, 
                                    ge_2024_constituency_list)

only_in_ge2024 <- setdiff(ge_2024_constituency_list, 
                          individual_level_list)

only_in_individual_level
only_in_ge2024




