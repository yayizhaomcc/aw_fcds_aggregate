rm(list = ls())

library(readxl)
library(fcds)
library(tidyverse)
library(benford.analysis)
library(janitor)
library(lubridate)
library(readr)

########################### Section A. Data operations #########################

fcds_raw <- read.csv("FCDS_ADHOC_Raw_Data_DR358_Dataset.dat", sep=",", header=TRUE)

fcds_work <- 
  fcds_raw %>% 
  ## code variables needed 
  transmute(
    # year at diagnosis 
    dx_year = extract.digits(DATE_OF_DIAGNOSIS_N390,4)$data.digits,
    
    # race
    race =  case_when(
      RACE_1_N160 == 1 ~ "White",
      RACE_1_N160 == 2 ~ "Black",
      RACE_1_N160 == 3 ~ "American Indian/Alaska Native", #American Indian/Aleutian/Eskimo
      #RACE_1_N160 %in% c(4,5,6,8,10,11,12,13,14,15,16,17,96,7,20,21,22,25,26,27,28,30,31,32,97) ~ "Asian or Pacific Islander",
      RACE_1_N160 %in% c(4,5,6,8,10,11,12,13,14,96) ~ "Asian",
      RACE_1_N160 %in% c(15,16,17) ~ "Asian Indian",
      RACE_1_N160 %in% c(7,20,21,22,25,26,27,28,30,31,32,97) ~ "Pacific Islander",
      RACE_1_N160 == 98 ~ "Other",
      RACE_1_N160 == 99 ~ "Unknown"),
    
    # ethnicity 
    ethnicity = case_when(
      SPANISH_HISPANIC_ORIGIN_N190 %in% c(0,7) ~ "Non-Hispanic",
      SPANISH_HISPANIC_ORIGIN_N190 %in% c(1,2,3,4,5,6,8)  ~ "Hispanic",
      SPANISH_HISPANIC_ORIGIN_N190 == 9 ~ "Unknown"),
    
    # age at diagnosis 
    age_at_dx = AGE_AT_DIAGNOSIS_N230,
    age_at_dx_18 = ifelse(AGE_AT_DIAGNOSIS_N230 == 999, "Unknown",
                          ifelse(AGE_AT_DIAGNOSIS_N230 < 18, "0-17",
                                 ifelse(AGE_AT_DIAGNOSIS_N230 >= 18, "18+", NA))),
    
    # cancer status
    cancer_status = CANCER_STATUS_N1770, 
    
    # census tract per 2020
    census_tract_2020 = case_when(
      CENSUS_TRACT_2020_N125 == 000000 ~ NA_integer_,
      CENSUS_TRACT_2020_N125 == 999999 ~ NA_integer_,
      TRUE ~ as.integer(CENSUS_TRACT_2020_N125)),
    
    # behaviors 
    behavior = recode( 
      BEHAVIOR_CODE_ICD_O_3_N523, 
      `0` = "Benign", 
      `1` = "Borderline", 
      `2` = "Insitu", 
      `3` = "Invasive"), 
    
    # primary site grouped
    primary_site_group = case_when(
      PRIMARY_SITE_N400 %in% c('C000','C001',	'C002',	'C003',	'C004',	'C005',	'C006',	'C008',	'C009'
      ) ~ "Lip",
      PRIMARY_SITE_N400 %in% c('C019',	'C020',	'C021',	'C022',	'C023',	'C024',	'C028',	'C029'
      ) ~ "Tongue",
      PRIMARY_SITE_N400 %in% c('C079',	'C080',	'C081',	'C088',	'C089'
      ) ~ "Major salivary gland",
      PRIMARY_SITE_N400 %in% c('C090',	'C091',	'C092',	'C093',	'C094', 'C095', 'C096', 'C097', 'C098', 'C099'
      ) ~ "Tonsil",
      PRIMARY_SITE_N400 %in% c('C040',	'C041',	'C048',	'C049'
      ) ~ "Floor of mouth",
      PRIMARY_SITE_N400 %in% c('C030',	'C031',	'C039',	'C050',	'C051',	'C052',	'C058',	'C059',	'C060',	'C061',	'C062',	'C068',	'C069'
      ) ~ "Gum and other mouth",
      PRIMARY_SITE_N400 %in% c('C110',	'C111',	'C112',	'C113',	'C118',	'C119'
      ) ~ "Nasopharynx",
      PRIMARY_SITE_N400 %in% c('C100',	'C101',	'C102',	'C103',	'C104',	'C108',	'C109'
      ) ~ "Oropharynx",
      PRIMARY_SITE_N400 %in% c('C129',	'C130',	'C131',	'C132',	'C138',	'C139'
      ) ~ "Hypopharynx",
      PRIMARY_SITE_N400 %in% c('C140',	'C142',	'C148') ~ "Other Oral Cavity and Pharynx",
      PRIMARY_SITE_N400 %in% c('C150',	'C151',	'C152',	'C153',	'C154',	'C155',	'C158',	'C159'
      ) ~ "Esophagus",
      PRIMARY_SITE_N400 %in% c('C160',	'C161',	'C162',	'C163',	'C164',	'C165',	'C166',	'C168',	'C169'
      ) ~ "Stomach",
      PRIMARY_SITE_N400 %in% c('C170',	'C171',	'C172',	'C173',	'C178',	'C179'
      ) ~ "Small intestine",
      PRIMARY_SITE_N400 %in% c('C180',	'C181',	'C182',	'C183',	'C184',	'C185',	'C186',	'C187',	'C188',	'C189',	'C260'
      ) ~ "Colon excluding rectum",
      PRIMARY_SITE_N400 %in% c('C199','C209'
      ) ~ "Rectum and rectosigmoid",
      PRIMARY_SITE_N400 %in% c('C210',	'C211','C212',	'C218'
      ) ~ "Anus",
      PRIMARY_SITE_N400 %in% c('C220'
      ) ~ "Liver",
      PRIMARY_SITE_N400 %in% c('C239'
      ) ~ "Gallbladder",
      PRIMARY_SITE_N400 %in% c('C250',	'C251',	'C252',	'C253',	'C254',	'C257',	'C258',	'C259'
      ) ~ "Pancreas",
      PRIMARY_SITE_N400 %in% c('C221','C240',	'C241',	'C248',	'C249',	'C260',	'C268',	'C269',	'C480',	'C481',	'C482',	'C488'
      ) ~ "Other digestive system",
      PRIMARY_SITE_N400 %in% c('C320',	'C321',	'C322',	'C323',	'C328',	'C329'
      ) ~ "Larynx",
      PRIMARY_SITE_N400 %in% c('C340',	'C341',	'C342',	'C343',	'C348',	'C349'
      ) ~ "Lung and bronchus",
      PRIMARY_SITE_N400 %in% c('C300',	'C301',	'C310',	'C311',	'C312',	'C313',	'C318',	'C319',	'C339',	'C381',	
                               'C382',	'C383',	'C384',	'C388',	'C390', 'C398', 'C399'
      ) ~ "Other respiratory system",
      PRIMARY_SITE_N400 %in% c('C400',	'C401',	'C402',	'C403',	'C408',	'C409',	'C410',	'C411',	'C412',	'C413',	'C414',	'C418',	'C419'
      ) ~ "Bones and joints",
      PRIMARY_SITE_N400 %in% c('C380',	'C470',	'C471',	'C472',	'C473',	'C474',	'C475',	'C476',	'C478',	'C479',	'C490',	'C491',
                               'C492',	'C493',	'C494',	'C495',	'C496','C498',	'C499'
      ) ~ "Soft tissue (including heart)",
      PRIMARY_SITE_N400 %in% c('C440',	'C441',	'C442',	'C443',	'C444',	'C445',	'C446',	'C447',	'C448',	'C449'
      ) ~ "Skin (excluding basal and squamous)",   
      PRIMARY_SITE_N400 %in% c('C500',	'C501',	'C502',	'C503',	'C504',	'C505',	'C506',	'C508',	'C509'
      ) ~ "Breast",
      PRIMARY_SITE_N400 %in% c('C530',	'C531',	'C538',	'C539'
      ) ~ "Cervix uteri",
      PRIMARY_SITE_N400 %in% c('C540',	'C541',	'C542',	'C543',	'C548',	'C549'
      ) ~ "Corpus Uteri",
      PRIMARY_SITE_N400 %in% c('C559'
      ) ~ "Uterus, NOSi",
      PRIMARY_SITE_N400 %in% c('C569'
      ) ~ "Ovary",
      PRIMARY_SITE_N400 %in% c('C510',	'C511',	'C512',	'C518',	'C519',	'C529',	'C570',	'C571',	'C572',	
                               'C573',	'C574',	'C577',	'C578',	'C579',	'C589'
      ) ~ "Other female genital system",
      PRIMARY_SITE_N400 %in% c('C619'
      ) ~ "Prostate",
      PRIMARY_SITE_N400 %in% c('C620',	'C621',	'C629'
      ) ~ "Testis",
      PRIMARY_SITE_N400 %in% c('C600',	'C601',	'C602',	'C608',	'C609'
      ) ~ "Penis",
      PRIMARY_SITE_N400 %in% c('C630',	'C631',	'C632',	'C637',	'C638',	'C639'
      ) ~ "Other male genital system",
      PRIMARY_SITE_N400 %in% c('C670',	'C671',	'C672',	'C673',	'C674',	'C675',	'C676',	'C677',	'C678',	'C679'
      ) ~ "Bladder",
      PRIMARY_SITE_N400 %in% c('C649',	'C659'
      ) ~ "Kidney and renal pelvis",
      PRIMARY_SITE_N400 %in% c('C669'
      ) ~ "Ureter",
      PRIMARY_SITE_N400 %in% c('C680',	'C681',	'C688',	'C689'
      ) ~ "Other urinary system",
      PRIMARY_SITE_N400 %in% c('C690',	'C691',	'C692',	'C693',	'C694',	'C695',	'C696',	'C698',	'C699'
      ) ~ "Eye",
      PRIMARY_SITE_N400 %in% c('C700',	'C701',	'C709','C710',	'C711',	'C712',	'C713',	'C714',	'C715',	'C716',	'C717',	'C718',	'C719',
                               'C720',	'C721',	'C722',	'C723',	'C724','C725',	'C728',	'C729'
      ) ~ "Brain or Other nervous system",
      PRIMARY_SITE_N400 %in% c('C739'
      ) ~ "Thyroid",
      PRIMARY_SITE_N400 %in% c('C379',	'C740',	'C741',	'C749',	'C750',	'C751',	'C752',	'C753',	'C754',	'C755',	'C758',	'C759'
      ) ~ "Other endocrine",
      PRIMARY_SITE_N400 %in% c('C421',	'C424'
      ) ~ "Lyphomas",
      PRIMARY_SITE_N400 %in% c('C420','C421',	'C424'
      ) ~ "Leukemias",
      PRIMARY_SITE_N400 %in% c('C422',	'C423',	'C760',	'C761',	'C762',
                               'C763',	'C764',	'C765',	'C767',	'C768',	'C770',	'C771',	'C772',	'C773',	'C774',	'C775',	'C778',	'C779',	'C809'
      ) ~ "Other, ill-defined, and unknown"),
    
    # further grouping the site
    
    primary_site_group2 <- case_when(
      primary_site_group %in% c("Bones and joints"
      ) ~ "Bones and joints",
      primary_site_group %in% c("Brain and other nervous system"
      ) ~ "Brain and other nervous system",
      primary_site_group %in% c("Breast"
      ) ~ "Breast",
      primary_site_group %in% c("Lip", "Tongue", "Major salivary gland", "Tonsil", "Floor of mouth",
                                "Gum and other mouth", "Nasopharynx", "Oropharynx", "Hypopharynx"
      ) ~ "Buccal cavity and pharynx" ,
      primary_site_group %in% c("Colon excluding rectum", "Rectum and rectosigmoid"
      ) ~ "Colon and rectum",
      primary_site_group %in% c("Thyroid", "Other endocrine"
      ) ~ "Endocrine",
      primary_site_group %in% c("Eye"
      ) ~ "Eye"
      primary_site_group %in% c("Cervix uteri", "Corpus Uteri", "Uterus, NOSi",
                                "Ovary", "Other female genital system"
      ) ~ "Female genital system",
      primary_site_group %in% c("Lung and bronchus"
      ) ~ "Lung and bronchus",
      primary_site_group %in% c("Lyphomas", "Leukemias"
      ) ~ "Lymphomas/Leukemias",
      primary_site_group %in% c("Esophagus", "Stomach", "Small intestine",  "Anus", "Liver",
                                "Gallbladder", "Pancreas", "Other digestive system"
      ) ~ "Other digestive system",
      primary_site_group %in% c("Testis", "Penis", "Other male genital system",
      ) ~ "Other male genital system",
      primary_site_group %in% c("Larynx", "Other respiratory system"
      ) ~ "Other respiratory system",
      primary_site_group %in% c("Prostate"
      ) ~ "Prostate",
      primary_site_group %in% c("Skin (excluding basal and squamous)"
      ) ~ "Skin (excluding basal and squamous)",
      primary_site_group %in% c("Soft tissue (including heart)"
      ) ~ "Soft tissue (including heart)",
      primary_site_group %in% c("Bladder", "Kidney and renal pelvis", "Ureter",
                                "Other urinary system"
      ) ~ "Urinary system",
      primary_site_group %in% c("Other, ill-defined, and unknown"
      ) ~ "Other, ill-defined, and unknown"
    ) %>% 
    
    
    # county at diagnosis 
    
    county_name = recode( 
      COUNTY_AT_DX_N90, 
      `1` = "Alachua", 
      `3` = "Baker", 
      `5` = "Bay", 
      `7` = "Bradford", 
      `9` = "Brevard", 
      `11` = "Broward", 
      `13` = "Calhoun", 
      `15` = "Charlotte", 
      `17` = "Citrus", 
      `19` = "Clay", 
      `21` = "Collier", 
      `23` = "Columbia", 
      `27` = "DeSoto", 
      `29` = "Dixie", 
      `31` = "Duval", 
      `33` = "Escambia", 
      `35` = "Flagler", 
      `37` = "Franklin", 
      `39` = "Gadsden", 
      `41` = "Gilchrist", 
      `43` = "Glades", 
      `45` = "Gulf", 
      `47` = "Hamilton", 
      `49` = "Hardee", 
      `51` = "Hendry", 
      `53` = "Hernando", 
      `55` = "Highlands", 
      `57` = "Hillsborough", 
      `59` = "Holmes", 
      `61` = "Indian River", 
      `63` = "Jackson", 
      `65` = "Jefferson", 
      `67` = "Lafayette", 
      `69`  = "Lake", 
      `71` = "Lee", 
      `73` = "Leon", 
      `75` = "Levy", 
      `77` = "Liberty", 
      `79` = "Madison", 
      `81` = "Manatee", 
      `83` = "Marion", 
      `85` = "Martin", 
      `86` = "Miami-Dade", 
      `87` = "Monroe", 
      `89` = "Nassau", 
      `91` = "Okaloosa", 
      `93` = "Okeechobee", 
      `95` = "Orange", 
      `97` = "Osceola", 
      `99` = "Palm Beach", 
      `101` = "Pasco", 
      `103` = "Pinellas", 
      `105` = "Polk", 
      `107` = "Putnam", 
      `109` = "St. Johns", 
      `111` = "St. Lucie", 
      `113` = "Santa Rosa", 
      `115` = "Sarasota", 
      `117` = "Seminole", 
      `119` = "Sumter", 
      `121` = "Suwannee", 
      `123` = "Taylor", 
      `125` = "Union", 
      `127` = "Volusia", 
      `129` = "Wakulla", 
      `131` = "Walton", 
      `133` = "Washington", 
      `999` = "Unknown")
  ) %>% 
  
  ## apply filters
  # filter the data for recent 5 years 
  filter(dx_year %in% c(2016:2020)) %>%   # if 2020 is not available, use 2015-2019 to ensure 5 year of data
  
  # filter for adult patients - do not apply
  filter(age_at_dx_18 %in% "18+") %>%
  
  # filter out benign cases 
  filter(behavior %in% c("Benign"))  # %>%
  
  # # filter for the catchment counties - do not apply this
  # filter(county_name %in% c("Brevard",
  #                           "Charlotte",
  #                           "Citrus",
  #                           "Collier",
  #                           "DeSoto",
  #                           "Glades",
  #                           "Hardee",
  #                           "Hendry",
  #                           "Hernando",
  #                           "Highlands",
  #                           "Hillsborough",
  #                           "Lake",
  #                           "Lee",
  #                           "Manatee",
  #                           "Marion",
  #                           "Orange",
  #                           "Osceola",
  #                           "Pasco",
  #                           "Pinellas",
  #                           "Polk",
  #                           "Sarasota",
  #                           "Seminole",
  #                           "Sumter"))

    

######################### Section B. Aggregated counts #########################


## at a census tract level

fcds_tract_count <-
  fcds_work %>% 
  group_by(census_tract_2020)
  count(
    race, 
    ethnicity, 
    primary_site_group2
  ) %>% 
    ungroup()

## at a priority zone level 

catchment <- read_rds("J:/Rollison Hampras/Yayi Zhao/Analyses/U01 ACT WONDERS/Statistical analysis/1. Zone data/1. catchment_area.rdata")

fcds_work %>% 
  left_join(catchment, by = c("census_tract_2020" = "GEOID")) %>%  # or "tract" if the census_tract_2020 is in texts
  mutate(AW_definition = 
           ifelse(county_name %in% "Unknown", "Unknown county",
                  ifelse(catchment_area %in% NA, "Outside of catchment area",
                         ifelse(priority_zone %in% NA, "Non-priority zone",
                                paste0())))) %>% 
  group_by(AW_definition) %>% 
  count(
    race, 
    ethnicity, 
    primary_site_group2
  ) %>% 
  ungroup()

