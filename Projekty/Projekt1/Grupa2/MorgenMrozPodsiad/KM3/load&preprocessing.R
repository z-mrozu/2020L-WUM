data <- read.csv("german_credit_data_dataset.csv")
attr <- read.csv("attributes_german_credit_data.csv")
library(DataExplorer)
library(ggplot2)
library(dplyr)
library(mlr)
library(OpenML)

# konwertowanie na ładniejsze 

data %>% mutate(checking_account_status=recode(checking_account_status, 
                                               'A11'= '< 0 DM', 
                                               'A12'= '0-200 DM',
                                               'A13'= '>= 200 DM/salary assignments \n for 1+ year',
                                               'A14'= 'no checking account')) %>%
  mutate(credit_history=recode(credit_history, 
                                      'A30'= 'brak /splacone',
                                      'A31'= 'wsystkie w tym \n banku splacone', 
                                      'A32'= 'obecne splacane',
                                      'A33'= 'opoznienia w splacie', 
                                      'A34'= 'critical/inne \n w innym banku')) %>%
  mutate(savings=recode(savings, 
                        'A61'= '< 100 DM', 
                        'A62' = '100-500 DM', 
                        'A63' = '500-1000 DM', 
                        'A64' = '>= 1000 DM',
                        'A65' = 'unknown/ \n no savings account')) %>%
  mutate(purpose=recode(purpose, 
                        "A40"= "nowy_samochod",
                        "A41"= "uzywany_samochod",
                        "A42"= "meble/ekwipunek",
                        "A43"= "radio/telewizja",
                        "A44"= "domowe",
                        "A45"= "naprawy",
                        "A46"= "edukacja",
                        "A47"= "wakacje",
                        "A48"= "przeszkolenie",
                        "A49"= "biznes",
                        "A410"= "inne")) %>%
  mutate(present_employment=recode(present_employment, 
                                   "A71"= "bezrobotny",
                                   "A72"= "ponizej_rok",
                                   "A73"= "1-4 lata",
                                   "A74"= "4-7 lat",
                                   "A75"= "ponad_7")) %>%
  mutate(personal=recode(personal, 
                    "A91"= "rozwiedziony",
                    "A92"= "rozwiedziona/mezatka", 
                    "A93"= "singiel",
                    "A94"= "zonaty/wdowiec",
                    "A95"= "singielka")) %>%
  mutate(other_debtors=recode(other_debtors, 
                              "A101"= "nikt",
                              "A102"= "co-applicant",
                              "A103"= "guarantor")) %>%
  mutate(property=recode(property, 
                         'A121' = 'real estate',
                         'A122' = 'building society savings agreement/life insurance',
                         'A123' = 'car or other',
                         'A124' = 'unknown/no property')) %>%
  mutate(other_installment_plans=recode(other_installment_plans, 
                                        "A141"= "bank",
                                        "A142"= "sklepy",
                                        "A143"= "nikt")) %>%
  mutate(housing=recode(housing, 
                        "A151"= "wynajem",
                        "A152"= "na_wlasnosc",
                        "A153"= "za_darmo")) %>%
  mutate(job=recode(job, 
                    "A171"= "unemployed/unskilled (non-resident)",
                    "A172"= "unskilled (resident)",
                    "A173"= "skilled employee/official",
                    "A174"= "management/self-employed/highly qualified employee/officer")) %>%
  mutate(telephone=recode(telephone, 
                          "A191"= 'False',
                          "A192"= 'True')) %>%
  mutate(foreign_worker=recode(foreign_worker, 
                               "A201"= 'True',
                               "A202"= 'False')) %>%
  mutate(customer_type=recode(customer_type, 
                               `1`= 'Good',
                               `2`= 'Bad')) -> data2
data2$customer_type <- factor(data2$customer_type)

data_mod <- data2
data_mod <- normalizeFeatures(data_mod, target = "customer_type")
data_mod <- createDummyFeatures(
  data_mod, target = "customer_type",
  cols = c(
    "checking_account_status",
    "credit_history",
    "purpose",
    "savings",
    "present_employment",
    "personal",
    "other_debtors",
    "property",
    "other_installment_plans",
    "housing",
    "job",
    "telephone",
    "foreign_worker"
  )
)
