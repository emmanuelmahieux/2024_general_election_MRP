#load packages

install.packages(c("arm", "foreign","haven","dplyr","readxl",
                   "forcats","lme4","Matrix", 'Metrics'))

packages <- c("arm", "foreign","haven","dplyr","readxl",
              "forcats","lme4","Matrix", 'Metrics')

# Use lapply to load all packages
lapply(packages, library, character.only = TRUE)

#---------

#define function to calculate the metrics
#to assess MRP model performance

calculate_metrics <- function(data) {
  mae <- mean(abs(data$prediction_error))
  correlation <- cor(data$lab_share_pred, data$lab_vote_share)
  rmse_value <- rmse(data$lab_share_pred, data$lab_vote_share)
  
  return(list(
    Correlation = correlation,
    MAE = mae,
    RMSE = rmse_value
  ))
}

#function to convert character variables to factors
convert_char_to_factor <- function(df) {
  df[] <- lapply(df, function(x) if (is.character(x)) as.factor(x) else x)
  return(df)
}

#----------------------------------


#load data
individual_level = read.csv('./individual_level.csv')

area_level = read.csv('./area_level.csv')

psf = read.csv('./psf.csv')

ge_2024 = read_excel('./HoC-GE2024-results-by-constituency.xlsx',
                     skip = 2)


#----------------------------------

#data transformations

#convert character variables to factors in individual_level dataframe

individual_level = convert_char_to_factor(individual_level)
individual_level$voted_labour = factor(individual_level$voted_labour)
psf = convert_char_to_factor(psf)

individual_level_v5 = read.csv('individual_level_v5.csv')
psf_v5 = read.csv('psf_v5.csv')

all.equal(individual_level, individual_level_v5)

#create vector of constituency names to tie each prediction 
#to a constituency
constituency = levels(psf$constituency)

#--------

#check that all the factor variables among the 
#random and fixed effects have the same number
#and ordering of levels in the individual-level df
#and the psf
#e.g. here we are checking this for the ageGroup factor
levels(individual_level$ageGroup)
levels(psf$ageGroup)

#add area-level variables to individual-level df and psf
# so that we can include them in the model as fixed effects

#individual-level df
individual_level$lab_share = area_level$lab_share[individual_level$new_constituency]
individual_level$con_share = area_level$con_share[individual_level$new_constituency]
individual_level$green_share = area_level$green_share[individual_level$new_constituency]
individual_level$brexit_party_share = area_level$brexit_party_share[individual_level$new_constituency]
individual_level$pc_share = area_level$pc_share[individual_level$new_constituency]
individual_level$aged_65__and_over_prop = area_level$aged_65__and_over_prop[individual_level$new_constituency]
individual_level$white_british_or_NI_prop = area_level$white_british_or_NI_prop[individual_level$new_constituency]
individual_level$edu_level4_prop = area_level$edu_level4_prop[individual_level$new_constituency]

#poststratification frame
psf$lab_share = area_level$lab_share[psf$constituency]
psf$con_share = area_level$con_share [psf$constituency]
psf$green_share = area_level$green_share[psf$constituency]
psf$brexit_party_share = area_level$brexit_party_share[psf$constituency]
psf$pc_share = area_level$pc_share[psf$constituency]
psf$aged_65__and_over_prop = area_level$aged_65__and_over_prop[psf$constituency]
psf$white_british_or_NI_prop = area_level$white_british_or_NI_prop[psf$constituency]
psf$edu_level4_prop = area_level$edu_level4_prop[psf$constituency]

#create column with the weights for each psf cell 

psf <- psf %>%
  group_by(constituency) %>%
  mutate(prop_by_constituency = frequency / sum(frequency)) %>%
  ungroup() 

#---


#model 1: party vote shares + aged 65 and over

individual_model_1 = glmer(formula = voted_labour ~ 
                             (1|ageGroup) +
                             (1|edlevel) +
                             (1|new_constituency) +
                             (1|region) +
                             lab_share +
                             con_share +
                             green_share +
                             brexit_party_share +
                             pc_share +
                             aged_65__and_over_prop,
                           data = individual_level,
                           family = binomial(link = 'logit'))

#model 2: party vote shares + edu level 4 qualifications

individual_model_2 = glmer(formula = voted_labour ~ 
                             (1|ageGroup) +
                             (1|edlevel) +
                             (1|new_constituency) +
                             (1|region) +
                             lab_share +
                             con_share +
                             green_share +
                             brexit_party_share +
                             pc_share +
                             edu_level4_prop,
                           data = individual_level,
                           family = binomial(link = 'logit'))

#model 3: party vote shares + White British or NI

individual_model_3 = glmer(formula = voted_labour ~ 
                             (1|ageGroup) +
                             (1|edlevel) +
                             (1|new_constituency) +
                             (1|region) +
                             lab_share +
                             con_share +
                             green_share +
                             brexit_party_share +
                             pc_share +
                             white_british_or_NI_prop,
                           data = individual_level,
                           family = binomial(link = 'logit'))

#model 4: random effects + Labour vote share

individual_model_4 = glmer(formula = voted_labour ~ 
                             (1|ageGroup) +
                             (1|edlevel) +
                             (1|new_constituency) +
                             (1|region) +
                             lab_share,
                           data = individual_level,
                           family = binomial(link = 'logit'))

#model 5: all predictors from the models above included here

individual_model_5 = glmer(formula = voted_labour ~ 
                              (1|ageGroup) +
                              (1|edlevel) +
                              (1|new_constituency) +
                              (1|region) +
                              lab_share +
                              con_share +
                              green_share +
                              brexit_party_share +
                              pc_share +
                              aged_65__and_over_prop +
                              white_british_or_NI_prop +
                              edu_level4_prop,
                            data = individual_level,
                            family = binomial(link = 'logit'))

#inspect model
display(individual_model_1)
summary(individual_model_1)

#-------------------------------------------

# predictions

#cellpred
my_cellpred <- invlogit( fixef(individual_model_4)["(Intercept)"]
                         + ranef(individual_model_4)$ageGroup[psf$ageGroup,1]
                         + ranef(individual_model_4)$edlevel[psf$edlevel,1]
                         + ranef(individual_model_4)$new_constituency[psf$constituency,1]
                         + ranef(individual_model_4)$region[psf$region,1]
                         + (fixef(individual_model_4)["lab_share"] *psf$lab_share)
                         # + (fixef(individual_model_4)["con_share"] *psf$con_share)
                         # + (fixef(individual_model_4)["green_share"] *psf$green_share)
                         # + (fixef(individual_model_4)["brexit_party_share"] *psf$brexit_party_share)
                         # + (fixef(individual_model_4)["pc_share"] *psf$pc_share)
                         # + (fixef(individual_model_4)["aged_65__and_over_prop"] *psf$aged_65__and_over_prop)
                         # + (fixef(individual_model_4)["white_british_or_NI_prop"] *psf$white_british_or_NI_prop)
                         # + (fixef(individual_model_4)["edu_level4_prop"] *psf$edu_level4_prop)
)

#weights the prediction by the freq of cell                                       

my_cellpredweighted = my_cellpred * psf$prop_by_constituency

#calculates the predicted Labour vote share 
#within each constituency (weighted average of responses)

lab_share_pred = 100* as.vector(tapply(my_cellpredweighted,
                                       psf$constituency,
                                       sum,
                                       na.rm = TRUE))

lab_share_pred = round(lab_share_pred,
                       digits = 2)

mrp_pred = data.frame(lab_share_pred, constituency)

mrp_pred = merge(mrp_pred, ge_2024_eng_wales, by = 'constituency')

mrp_pred$lab_vote_share = mrp_pred$lab_vote_share * 100

mrp_pred = mrp_pred %>% 
  mutate(prediction_error = lab_share_pred - lab_vote_share)

mrp_pred = dplyr::select(mrp_pred,
                         constituency,
                         lab_share_pred,
                         lab_vote_share,
                         prediction_error,
                         Result,
                         `First party`,
                         `Second party`,
                         everything())

calculate_metrics(mrp_pred)




subset

#save df with constituency-level predictions
write.csv(mrp_pred, 'model10_pred.csv')

