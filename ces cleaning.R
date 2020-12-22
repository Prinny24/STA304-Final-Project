#CES cleaning code

library('devtools')
library(janitor)
library(tidyverse)
library(labelled)
library(cesR)
library(tidyverse)
library(brms)
library(broom)
library(lme4)
library(knitr)

get_ces("ces2019_web")

ces2019_web <- labelled::to_factor(ces2019_web)
gss <- read_csv("gss.csv")

# y = vote choice
#level 1 = age, sex, and educ
#level 2 = province
#sd = survey data and cd = census data

sd <- ces2019_web %>%
  select(cps19_yob,
         cps19_gender,
         cps19_education,
         cps19_province,
         cps19_votechoice) %>%
  drop_na(cps19_votechoice,
          cps19_yob,
          cps19_gender,
          cps19_education,
          cps19_province) %>%
  filter(cps19_votechoice != "Don't know/ Prefer not to answer",
         cps19_education != "Don't know/ Prefer not to answer") %>%
  rename(sex = cps19_gender,
         educ = cps19_education,
         province = cps19_province,
         vote = cps19_votechoice) %>%
  mutate(age = 2020 - as.numeric(as.character(cps19_yob)),
         sex = case_when(sex == "A man" ~ "Male",
                         sex == "A woman" ~ "Female",
                         sex == "Other (e.g. Trans, non-binary, two-spirit, gender-queer)" ~ "Other"),
         educ = case_when(educ == "No schooling" ~ "No Highschool Diploma",
                          educ == "Some elementary school" ~ "No Highschool Diploma",
                          educ == "Completed elementary school" ~ "No Highschool Diploma",
                          educ == "Some secondary/ high school" ~ "No Highschool Diploma",
                          educ == "Completed secondary/ high school" ~ "Highschool Diploma",
                          educ == "Some technical, community college, CEGEP, College Classique" ~ "Highschool Diploma",
                          educ == "Some university" ~ "Highschool Diploma",
                          educ == "Bachelor's degree" ~ "Undergraduate Degree",
                          educ == "Professional degree or doctorate" ~ "Graduate Degree",
                          educ == "Master's degree" ~ "Graduate Degree"),
         vote_liberal = ifelse(vote == "Liberal Party", 1 , 0),
         vote_conservative = ifelse(vote == "Conservative Party", 1 , 0),
         vote_ndp = ifelse(vote == "ndp", 1 , 0),
         vote_bloc = ifelse(vote == "Bloc Québécois", 1 , 0),
         vote_green = ifelse(vote == "Green Party", 1 , 0),
         vote_people = ifelse(vote == "People's Party", 1 , 0),
         vote_other = ifelse(vote == "Another party (please specify)", 1 , 0))

sd <- sd %>%
  select(age,
         sex,
         educ,
         province,
         vote_liberal,
         vote_conservative,
         vote_ndp,
         vote_bloc,
         vote_green,
         vote_people,
         vote_other) %>%
  filter(sex != "Other",
         province != "Northwest Territories", 
         province != "Nunavut",
         province != "Yukon") %>%
  droplevels()
  
cd <- gss %>%
  select(age,sex,education,province) %>%
  rename(educ = education) %>%
  drop_na(age,sex,educ,province) %>%
  mutate(age = floor(age),
         educ = case_when(educ == "Bachelor's degree (e.g. B.A., B.Sc., LL.B.)" ~ "Undergraduate Degree",
                          educ == "College, CEGEP or other non-university certificate or di..." ~ "Undergraduate Degree",
                          educ == "High school diploma or a high school equivalency certificate" ~ "Highschool Diploma",
                          educ == "Less than high school diploma or its equivalent" ~ "No Highschool Diploma",
                          educ == "Trade certificate or diploma" ~ "Highschool Diploma",
                          educ == "University certificate or diploma below the bachelor's level" ~ "Highschool Diploma",
                          educ == "University certificate, diploma or degree above the bach..." ~ "Graduate Degree")) %>%
  droplevels() %>%
  count(age,sex,educ,province)


sd$educ <- factor(sd$educ, levels = c("No Highschool Diploma",
                                      "Highschool Diploma",
                                      "Undergraduate Degree",
                                      "Graduate Degree"))

sd$sex <- factor(sd$sex, levels = c("Male", "Female"))

#frequentist liberal

model_lib <- glmer(vote_liberal ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_lib <- model_lib %>% 
  predict(newdata = cd)

cd$estimate_lib <- exp(cd$logodds_estimate_lib)/(1+exp(cd$logodds_estimate_lib))

#frequentist conservative

model_conservative <- glmer(vote_conservative ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_conservative <- model_conservative %>% 
  predict(newdata = cd)

cd$estimate_conservative <- exp(cd$logodds_estimate_conservative)/(1+exp(cd$logodds_estimate_conservative))

#frequentist ndp

model_ndp <- glmer(vote_ndp ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_ndp <- model_ndp %>% 
  predict(newdata = cd)

cd$estimate_ndp <- exp(cd$logodds_estimate_ndp)/(1+exp(cd$logodds_estimate_ndp))


#frequentist bloc

model_bloc <- glmer(vote_bloc ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_bloc <- model_bloc %>% 
  predict(newdata = cd)

cd$estimate_bloc <- exp(cd$logodds_estimate_bloc)/(1+exp(cd$logodds_estimate_bloc))


#frequentist green

model_green <- glmer(vote_green ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_green <- model_green %>% 
  predict(newdata = cd)

cd$estimate_green <- exp(cd$logodds_estimate_green)/(1+exp(cd$logodds_estimate_green))


#frequentist people

model_people <- glmer(vote_people ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_people <- model_people %>% 
  predict(newdata = cd)

cd$estimate_people <- exp(cd$logodds_estimate_people)/(1+exp(cd$logodds_estimate_people))


#frequentist other

model_other <- glmer(vote_other ~ age + sex + educ + (1|province), data = sd, family = binomial)

cd$logodds_estimate_other <- model_other %>% 
  predict(newdata = cd)

cd$estimate_other <- exp(cd$logodds_estimate_other)/(1+exp(cd$logodds_estimate_other))


#summaries

cd %>%
  mutate(alp_predict_prop_lib = estimate_lib*n,
         alp_predict_prop_conservative = estimate_conservative*n,
         alp_predict_prop_ndp = estimate_ndp*n,
         alp_predict_prop_bloc = estimate_bloc*n,
         alp_predict_prop_green = estimate_green*n,
         alp_predict_prop_people = estimate_people*n,
         alp_predict_prop_other = estimate_other*n) %>%
  summarise(alp_predict_liberal = sum(alp_predict_prop_lib)/sum(n),
            alp_predict_conservative = sum(alp_predict_prop_conservative)/sum(n),
            alp_predict_ndp = sum(alp_predict_prop_ndp)/sum(n),
            alp_predict_bloc = sum(alp_predict_prop_bloc)/sum(n),
            alp_predict_green = sum(alp_predict_prop_green)/sum(n),
            alp_predict_people = sum(alp_predict_prop_people)/sum(n),
            alp_predict_other = sum(alp_predict_prop_other)/sum(n))




write.csv(sd, "sd.csv")
write.csv(cd, "cd.csv")











