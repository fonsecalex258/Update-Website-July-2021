#hello world
# adding mre lines of text to chek the push 
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(leaflet)
library(plotly)
library(timevis)
library(tidyverse)
library(DT)
library(readxl)
library(tesseract)
library(magick)

###
img <- image_read("datasets/distiller1.png")
des <- data.frame(image_ocr_data(img))

#distiller <- readr::read_csv("www/PRISMA.csv")

#distiller[5,8] <- des[11,1] 
#distiller[11,8] <- des[26,1]
#distiller[14,8] <- des[31,1]
#distiller[15,8] <- des[38,1]
#distiller[16,8] <- des[49,1]
#distiller[17,8] <- des[56,1]
#distiller[20,8] <- des[65,1]
#distiller[24,8] <- des[83,1]




## read reference from external text file ####
mybib <- readr::read_file("datasets/bib.txt")

## read files ####
#distiller <- readxl::read_xlsx("www/PRISMA.xlsx")
mango <- readxl::read_xlsx("datasets/mango.xlsx")
testtimeline <- readxl::read_xlsx("datasets/testtimeline.xlsx")
cafo <- readxl::read_xlsx("datasets/cafo.xlsx", sheet = "Treatment-Outcome data")
cafo2 <- readxl::read_xlsx("datasets/cafo2.xlsx") %>% 
  select(Refid, `Author(s)`, Year, Country, State, City, lng, lat, Title, `Journal Name`)
cafo2 <- cafo2 %>% mutate(abbr = ifelse (Country == "USA", "us", ifelse(Country == "Netherlands", "nl","de" ) ))
cafo3 <- cafo2 %>% distinct() %>%
  group_by(Country, Title, Year, abbr) %>% summarise(Count = n())
cafo3$Nation <- sprintf ('<img src = "https://flagpedia.net/data/flags/w2560/%s.png" height="25" ></img>', cafo3$abbr)
cafo3 <- cafo3[c(6, 1, 2, 3)]
####For 22 paper updated
cafo4 <- readxl::read_xlsx("datasets/updated_2021_CAFO.xlsx")
cafo4$Nation <- sprintf ('<img src = "https://flagpedia.net/data/flags/w2560/%s.png" height="25" ></img>', cafo4$abbr)
cafo4 <- cafo4[c(6, 1, 2, 3)]
dataset <- readr::read_csv("datasets/CAFO_All_data_new_Aug21.csv")
rob <- readxl::read_xlsx("datasets/cafo.xlsx", sheet = "Risk of bias")
forest <- read_excel("datasets/forest.xlsx")
######### cration of a new dataset based on newest distiller form
forest_cross <- read_excel("datasets/distiller_cross.xlsx")
forest_cross1 <- read_excel("datasets/distiller_cross.xlsx")
forest_cohort <- read_excel("datasets/distiller_cohort_lunes_2.xlsx")
forest_case <- read_excel("datasets/distiller_casecontrol.xlsx")
###with this work only for cross
forest_sabado <- read_excel("datasets/distiller_cross.xlsx")
################
mangos <- forest_cross %>% select(Refid, category)
mangos1 <- forest_cohort %>% select(Refid, category)
mangos2 <- forest_case %>% select(Refid, category)

mangosT <- bind_rows(mangos, mangos1, mangos2)


reyes<- mangosT %>% distinct()
reyes1 <- reyes%>% filter(category=="Lower Respiratory")
reyes2 <- reyes%>% filter(category=="Upper Respiratory")
reyes3 <- reyes%>% filter(category=="Gastrointestinal diseases")
reyes4 <- reyes%>% filter(category=="Antimicrobial resistance")


testtimeline <- testtimeline %>%
  mutate(LR = ifelse(Refid %in% reyes1$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(Gastro = ifelse(Refid %in% reyes3$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(UR = ifelse(Refid %in% reyes2$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(Antimicro = ifelse(Refid %in% reyes4$Refid, "Yes", "No")   )

################
#forest_cross_event <- forest_cross %>% filter(event_state == "Event")
#forest_cross_state <- forest_cross %>% filter(event_state == "State")
forest_cross <- forest_cross %>% filter(rare_outcome == "Yes" | health_event=="Yes")
forest_cross1 <- forest_cross1 %>% filter(is.na(Differential_information_bias)& is.na(Differential_information_bias_1_V2))

########## fOR all cross-sectional


rty <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cross))
forest_cross <-  forest_cross %>% mutate(IDD = c(1:nrow(forest_cross)))
forest_cross <- forest_cross %>% mutate( IDD_2 = paste(forest_cross$IDD, "Cross-sectional" ))

if (sum(rty)==6 ) {
  yis0 <- forest_cross[c(9,13,17,21,25,29)]
  yis <- forest_cross[c(10,14,18,22,26,30)]
  yis1 <- forest_cross[c(11,15,19,23,27,31)]
  yis2 <- forest_cross[c(12,16,20,24,28,32)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross$outcome, each = 6)
  mm <- rep(forest_cross$effect_measure, each = 6)
  Exposure.measure <- rep(forest_cross$exposure, each = 6)
  Categorized.class <- rep(forest_cross$category, each = 6)
  IDD <- rep(forest_cross$IDD, each = 6)
  IDD_2 <- rep(forest_cross$IDD_2, each = 6)
  
  up_forest_melo <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1 <- up_forest_melo[complete.cases(up_forest_melo),]
}else if (sum(rty)==5){
  yis0 <- forest_cross[c(9,13,17,21,25)]
  yis <- forest_cross[c(10,14,18,22,26)]
  yis1 <- forest_cross[c(11,15,19,23,27)]
  yis2 <- forest_cross[c(12,16,20,24,28)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross$outcome, each = 5)
  mm <- rep(forest_cross$effect_measure, each = 5)
  Exposure.measure <- rep(forest_cross$exposure, each = 5)
  Categorized.class <- rep(forest_cross$category, each = 5)
  IDD <- rep(forest_cross$IDD, each = 5)
  IDD_2 <- rep(forest_cross$IDD_2, each = 5)
  
  up_forest_melo <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1 <- up_forest_melo[complete.cases(up_forest_melo),]
}else if (sum(rty)==4){
  yis0 <- forest_cross[c(9,13,17,21)]
  yis <- forest_cross[c(10,14,18,22)]
  yis1 <- forest_cross[c(11,15,19,23)]
  yis2 <- forest_cross[c(12,16,20,24)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross$outcome, each = 4)
  mm <- rep(forest_cross$effect_measure, each = 4)
  Exposure.measure <- rep(forest_cross$exposure, each = 4)
  Categorized.class <- rep(forest_cross$category, each = 4)
  IDD <- rep(forest_cross$IDD, each = 4)
  IDD_2 <- rep(forest_cross$IDD_2, each = 4)
  
  up_forest_melo <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1 <- up_forest_melo[complete.cases(up_forest_melo),]
}else if (sum(rty)==3){
  yis0 <- forest_cross[c(9,13,17)]
  yis <- forest_cross[c(10,14,18)]
  yis1 <- forest_cross[c(11,15,19)]
  yis2 <- forest_cross[c(12,16,20)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross$outcome, each = 3)
  mm <- rep(forest_cross$effect_measure, each = 3)
  Exposure.measure <- rep(forest_cross$exposure, each = 3)
  Categorized.class <- rep(forest_cross$category, each = 3)
  IDD <- rep(forest_cross$IDD, each = 3)
  IDD_2 <- rep(forest_cross$IDD_2, each = 3)
  
  up_forest_melo <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1 <- up_forest_melo[complete.cases(up_forest_melo),]
}
##############
###### For Health States
##########
forest_cross_state <- forest_cross1
rty3 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cross_state))
forest_cross_state <-  forest_cross_state %>% mutate(IDD = c(1:nrow(forest_cross_state)))
forest_cross_state <- forest_cross_state %>% mutate(IDD_2 = paste(forest_cross_state$IDD, "Cross-sectional" ))


if (sum(rty3)==6 ) {
  yis0 <- forest_cross_state[c(9,13,17,21,25,29)]
  yis <- forest_cross_state[c(10,14,18,22,26,30)]
  yis1 <- forest_cross_state[c(11,15,19,23,27,31)]
  yis2 <- forest_cross_state[c(12,16,20,24,28,32)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_state$outcome, each = 6)
  mm <- rep(forest_cross_state$effect_measure, each = 6)
  Exposure.measure <- rep(forest_cross_state$exposure, each = 6)
  Categorized.class <- rep(forest_cross_state$category, each = 6)
  IDD <- rep(forest_cross_state$IDD, each = 6)
  IDD_2 <- rep(forest_cross_state$IDD_2, each = 6)
  
  up_forest_state <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_state1 <- up_forest_state[complete.cases(up_forest_state),]
}else if (sum(rty3)==5){
  yis0 <- forest_cross_state[c(9,13,17,21,25)]
  yis <- forest_cross_state[c(10,14,18,22,26)]
  yis1 <- forest_cross_state[c(11,15,19,23,27)]
  yis2 <- forest_cross_state[c(12,16,20,24,28)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_state$outcome, each = 5)
  mm <- rep(forest_cross_state$effect_measure, each = 5)
  Exposure.measure <- rep(forest_cross_state$exposure, each = 5)
  Categorized.class <- rep(forest_cross_state$category, each = 5)
  IDD <- rep(forest_cross_state$IDD, each = 5)
  IDD_2 <- rep(forest_cross_state$IDD_2, each = 5)
  
  up_forest_state <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_state1 <- up_forest_state[complete.cases(up_forest_state),]
}else if (sum(rty3)==4){
  yis0 <- forest_cross_state[c(9,13,17,21)]
  yis <- forest_cross_state[c(10,14,18,22)]
  yis1 <- forest_cross_state[c(11,15,19,23)]
  yis2 <- forest_cross_state[c(12,16,20,24)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_state$outcome, each = 4)
  mm <- rep(forest_cross_state$effect_measure, each = 4)
  Exposure.measure <- rep(forest_cross_state$exposure, each = 4)
  Categorized.class <- rep(forest_cross_state$category, each = 4)
  IDD <- rep(forest_cross_state$IDD, each = 4)
  IDD_2 <- rep(forest_cross_state$IDD_2, each = 4)
  
  up_forest_state <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_state1 <- up_forest_state[complete.cases(up_forest_state),]
}else if (sum(rty3)==3){
  yis0 <- forest_cross_state[c(9,13,17)]
  yis <- forest_cross_state[c(10,14,18)]
  yis1 <- forest_cross_state[c(11,15,19)]
  yis2 <- forest_cross_state[c(12,16,20)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_state$outcome, each = 3)
  mm <- rep(forest_cross_state$effect_measure, each = 3)
  Exposure.measure <- rep(forest_cross_state$exposure, each = 3)
  Categorized.class <- rep(forest_cross_state$category, each = 3)
  IDD <- rep(forest_cross_state$IDD, each = 3)
  IDD_2 <- rep(forest_cross_state$IDD_2, each = 3)
  
  up_forest_state <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_state1 <- up_forest_state[complete.cases(up_forest_state),]
}


############for case-control
rty1 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_case))
forest_case <-  forest_case%>% mutate(IDD = c(1:nrow(forest_case)))
forest_case <- forest_case %>% mutate( IDD_2 = paste(forest_case$IDD, "C-C" ))
if (sum(rty1)==6 ) {
  yis0 <- forest_case[c(9,13,17,21,25,29)]
  yis <- forest_case[c(10,14,18,22,26,30)]
  yis1 <- forest_case[c(11,15,19,23,27,31)]
  yis2 <- forest_case[c(12,16,20,24,28,32)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_case$outcome, each = 6)
  mm <- rep(forest_case$effect_measure, each = 6)
  Exposure.measure <- rep(forest_case$exposure, each = 6)
  Categorized.class <- rep(forest_case$category, each = 6)
  IDD <- rep(forest_case$IDD, each = 6)
  IDD_2 <- rep(forest_case$IDD_2, each = 6)
  
  up_forest_case <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_case1 <- up_forest_case[complete.cases(up_forest_case),]
}else if (sum(rty1)==5){
  yis0 <- forest_case[c(9,13,17,21,25)]
  yis <- forest_case[c(10,14,18,22,26)]
  yis1 <- forest_case[c(11,15,19,23,27)]
  yis2 <- forest_case[c(12,16,20,24,28)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_case$outcome, each = 5)
  mm <- rep(forest_case$effect_measure, each = 5)
  Exposure.measure <- rep(forest_case$exposure, each = 5)
  Categorized.class <- rep(forest_case$category, each = 5)
  IDD <- rep(forest_case$IDD, each = 5)
  IDD_2 <- rep(forest_case$IDD_2, each = 5)
  
  up_forest_case <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_case1 <- up_forest_case[complete.cases(up_forest_case),]
}else if (sum(rty1)==4){
  yis0 <- forest_case[c(9,13,17,21)]
  yis <- forest_case[c(10,14,18,22)]
  yis1 <- forest_case[c(11,15,19,23)]
  yis2 <- forest_case[c(12,16,20,24)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_case$outcome, each = 4)
  mm <- rep(forest_case$effect_measure, each = 4)
  Exposure.measure <- rep(forest_case$exposure, each = 4)
  Categorized.class <- rep(forest_case$category, each = 4)
  IDD <- rep(forest_case$IDD, each = 4)
  IDD_2 <- rep(forest_case$IDD_2, each = 4)
  
  up_forest_case <- data.frame(
    IDD, IDD_2, Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_case1 <- up_forest_case[complete.cases(up_forest_case),]
}else if (sum(rty1)==3){
  yis0 <- forest_case[c(9,13,17)]
  yis <- forest_case[c(10,14,18)]
  yis1 <- forest_case[c(11,15,19)]
  yis2 <- forest_case[c(12,16,20)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_case$outcome, each = 3)
  mm <- rep(forest_case$effect_measure, each = 3)
  Exposure.measure <- rep(forest_case$exposure, each = 3)
  Categorized.class <- rep(forest_case$category, each = 3)
  IDD <- rep(forest_case$IDD, each = 3)
  IDD_2 <- rep(forest_case$IDD_2, each = 3)
  
  up_forest_case <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_case1 <- up_forest_case[complete.cases(up_forest_case),]
}

##########

############for cohort
rty2 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cohort))
forest_cohort <-  forest_cohort%>% mutate(IDD = c(1:nrow(forest_cohort)))
forest_cohort <- forest_cohort %>% mutate( IDD_2 = paste(forest_cohort$IDD, "Cohort" ))
if (sum(rty2)==6 ) {
  yis0 <- forest_cohort[c(10,14,18,22,26,30)]
  yis <- forest_cohort[c(11,15,19,23,27,31)]
  yis1 <- forest_cohort[c(12,16,20,24,28,32)]
  yis2 <- forest_cohort[c(13,17,21,25,29,33)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cohort$outcome, each = 6)
  mm <- rep(forest_cohort$effect_measure, each = 6)
  Exposure.measure <- rep(forest_cohort$exposure, each = 6)
  Categorized.class <- rep(forest_cohort$category, each = 6)
  IDD <- rep(forest_cohort$IDD, each = 6)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 6)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_cohort1 <- up_forest_cohort[complete.cases(up_forest_cohort),]
}else if (sum(rty2)==5){
  yis0 <- forest_cohort[c(10,14,18,22,26)]
  yis <- forest_cohort[c(11,15,19,23,27)]
  yis1 <- forest_cohort[c(12,16,20,24,28)]
  yis2 <- forest_cohort[c(13,17,21,25,29)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cohort$outcome, each = 5)
  mm <- rep(forest_cohort$effect_measure, each = 5)
  Exposure.measure <- rep(forest_cohort$exposure, each = 5)
  Categorized.class <- rep(forest_cohort$category, each = 5)
  IDD <- rep(forest_cohort$IDD, each = 5)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 5)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_cohort1 <- up_forest_cohort[complete.cases(up_forest_cohort),]
}else if (sum(rty2)==4){
  yis0 <- forest_cohort[c(10,14,18,22)]
  yis <- forest_cohort[c(11,15,19,23)]
  yis1 <- forest_cohort[c(12,16,20,24)]
  yis2 <- forest_cohort[c(13,17,21,25)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cohort$outcome, each = 4)
  mm <- rep(forest_cohort$effect_measure, each = 4)
  Exposure.measure <- rep(forest_cohort$exposure, each = 4)
  Categorized.class <- rep(forest_cohort$category, each = 4)
  IDD <- rep(forest_cohort$IDD, each = 4)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 4)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_cohort1 <- up_forest_cohort[complete.cases(up_forest_cohort),]
}else if (sum(rty2)==3){
  yis0 <- forest_cohort[c(10,14,18)]
  yis <- forest_cohort[c(11,15,19)]
  yis1 <- forest_cohort[c(12,16,20)]
  yis2 <- forest_cohort[c(13,17,21)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cohort$outcome, each = 3)
  mm <- rep(forest_cohort$effect_measure, each = 3)
  Exposure.measure <- rep(forest_cohort$exposure, each = 3)
  Categorized.class <- rep(forest_cohort$category, each = 3)
  IDD <- rep(forest_cohort$IDD, each = 3)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 3)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_cohort1 <- up_forest_cohort[complete.cases(up_forest_cohort),]
}



#up_forest_cohort1$yi=as.numeric(levels(up_forest_cohort1$yi))[up_forest_cohort1$yi]
#up_forest_case1$yi=as.numeric(levels(up_forest_case1$yi))[up_forest_case1$yi]
#up_forest_melo1$yi=as.numeric(levels(up_forest_melo1$yi))[up_forest_melo1$yi]

forest_joint <-  bind_rows(up_forest_cohort1,up_forest_case1, up_forest_melo1)
forest_joint[,8]=as.numeric(forest_joint[,8])
forest_joint[,9]=as.numeric(forest_joint[,9])
forest_joint[,10]=as.numeric(forest_joint[,10])
#####for state
up_forest_state1[,8]=as.numeric(up_forest_state1[,8])
up_forest_state1[,9]=as.numeric(up_forest_state1[,9])
up_forest_state1[,10]=as.numeric(up_forest_state1[,10])

######
forest_joint$short <- ifelse(is.na(word(forest_joint$Outcome.variable, 1, 4)), forest_joint$Outcome.variable, word(forest_joint$Outcome.variable, 1, 4))
forest_joint$numberofwords <- sapply(strsplit(forest_joint$Outcome.variable, " "), length)
forest_joint$short <- ifelse(forest_joint$numberofwords>=5, paste(forest_joint$short, "..." ), forest_joint$short)


forest_joint$shortexpo <- ifelse(is.na(word(forest_joint$Exposure.measure, 1, 5)), forest_joint$Exposure.measure, word(forest_joint$Exposure.measure, 1, 5))
forest_joint$numberofwordsexpo <- sapply(strsplit(forest_joint$Exposure.measure, " "), length)
forest_joint$shortexpo <- ifelse(forest_joint$numberofwords>=6, paste(forest_joint$shortexpo, "..." ), forest_joint$shortexpo)

forest_joint <- forest_joint %>% mutate(inter_95 = ifelse(is.na(lowerci), paste(forest_joint$yi), paste(forest_joint$yi,"[",forest_joint$lowerci,",", forest_joint$upperci,"]")))
######
up_forest_state1$short <- ifelse(is.na(word(up_forest_state1$Outcome.variable, 1, 4)), up_forest_state1$Outcome.variable, word(up_forest_state1$Outcome.variable, 1, 4))
up_forest_state1$numberofwords <- sapply(strsplit(up_forest_state1$Outcome.variable, " "), length)
up_forest_state1$short <- ifelse(up_forest_state1$numberofwords>=5, paste(up_forest_state1$short, "..." ), up_forest_state1$short)


up_forest_state1$shortexpo <- ifelse(is.na(word(up_forest_state1$Exposure.measure, 1, 5)), up_forest_state1$Exposure.measure, word(up_forest_state1$Exposure.measure, 1, 5))
up_forest_state1$numberofwordsexpo <- sapply(strsplit(up_forest_state1$Exposure.measure, " "), length)
up_forest_state1$shortexpo <- ifelse(up_forest_state1$numberofwords>=6, paste(up_forest_state1$shortexpo, "..." ), up_forest_state1$shortexpo)

up_forest_state1 <- up_forest_state1 %>% mutate(inter_95 = ifelse(is.na(lowerci), paste(up_forest_state1$yi), paste(up_forest_state1$yi,"[",up_forest_state1$lowerci,",", up_forest_state1$upperci,"]")))


###### a dataset to ROB
ROB_1 <-  forest_cross %>% filter(health_event== "Yes") %>% select(description_1_V2, description_2_V2, description_3_V2, description_4_V2, description_5_V2,overall_bias_V2,IDD,IDD_2)
ROB_2 <-  forest_cross %>% filter(rare_outcome== "Yes") %>% select(description_1, description_2,description_3, description_4, description_5, overall_bias, IDD, IDD_2)
#ROB_3 <-  forest_cross_event %>% filter(event_state== "State") %>% select(description_1, description_2,description_3, description_4, description_5, overall_bias, IDD, IDD_2)colnames(ROB_1) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")
colnames(ROB_1) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")
colnames(ROB_2) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")

ROB_Cohort <-  forest_cohort %>% select(description_1, description_2,description_3, description_4, description_5,description_6, description_7,overall_bias,IDD, IDD_2)
ROB_Case <-  forest_case %>% select(description_1, description_2,description_3, description_4, description_5, overall_bias, IDD, IDD_2)
colnames(ROB_Case) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")
colnames(ROB_Cohort) <- c("Selection bias", "Differential information", "Differential information 2","Confounding","Differential information 3", "Differential information 4", "Selection bias 2", "Overall bias","IDD", "IDD_2")
ROB_joint <-  bind_rows(ROB_1,ROB_2, ROB_Cohort, ROB_Case)



###### a dataset to ROB for traffic light graphic
ROB_1_event <-  forest_cross %>% filter(health_event== "Yes") %>% select(Differential_information_bias_1_V2, Differential_information_bias2_V2, selection_bias_1_V2, selection_bias_2_V2, confounding_V2,overall_bias_V2,IDD,IDD_2, category)
ROB_2_event <-  forest_cross %>% filter(rare_outcome== "Yes") %>% select(Differential_information_bias, Differential_information_bias2,selection_bias_1, selection_bias_2, confounding, overall_bias, IDD, IDD_2, category)
colnames(ROB_1_event) <- c("Misclassification of exposure", "Misclassification of outcome", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2", "category")
colnames(ROB_2_event) <- c("Misclassification of exposure", "Misclassification of outcome", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2", "category")

ROB_Cohort_tl <-  forest_cohort %>% select(selection_bias, information_bias,information_bias_2, confounding, information_bias_3,information_bias_4, selection_bias_2,overall_bias,IDD, IDD_2,category)
ROB_Case_tl <-  forest_case %>% select(Differential_information_bias, Differential_information_bias2,selection_bias_1, selection_bias_2, confounding, overall_bias, IDD, IDD_2, category)
colnames(ROB_Case_tl) <- c("Misclassification of exposure", "Misclassification of outcome", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2", "category")
colnames(ROB_Cohort_tl) <- c("Selection bias", "Misclassification of exposure", "Misclassification of outcome","Confounding","Differential information 3", "Differential information 4", "Selection_bias 2", "Overall bias","IDD", "IDD_2", "category")
ROB_joint_tl <-  bind_rows(ROB_1_event,ROB_2_event, ROB_Cohort_tl, ROB_Case_tl)

ROB_joint_tl$'Selection bias'[ROB_joint_tl$'Selection bias' == 'Definitely yes (low risk of bias)'] <- 'Low'
ROB_joint_tl$'Selection_bias 2'[ROB_joint_tl$'Selection_bias 2' == 'Definitely yes (low risk of bias)'] <- 'Low'
ROB_joint_tl$'Misclassification of exposure'[ROB_joint_tl$'Misclassification of exposure' == 'Definitely yes (low risk of bias)'] <- 'Low'
ROB_joint_tl$'Misclassification of outcome'[ROB_joint_tl$'Misclassification of outcome' == 'Definitely yes (low risk of bias)'] <- 'Low'
ROB_joint_tl$'Differential information 3'[ROB_joint_tl$'Differential information 3' == 'Definitely yes (low risk of bias)'] <- 'Low'
ROB_joint_tl$'Differential information 4'[ROB_joint_tl$'Differential information 4' == 'Definitely yes (low risk of bias)'] <- 'Low'
ROB_joint_tl$'Confounding'[ROB_joint_tl$'Confounding' == 'Definitely yes (low risk of bias)'] <- 'Low'

ROB_joint_tl$'Selection bias'[ROB_joint_tl$'Selection bias' == 'Probably yes'] <- 'Likely Low'
ROB_joint_tl$'Selection_bias 2'[ROB_joint_tl$'Selection_bias 2' == 'Probably yes'] <- 'Likely Low'
ROB_joint_tl$'Misclassification of exposure'[ROB_joint_tl$'Misclassification of exposure' == 'Probably yes'] <- 'Likely Low'
ROB_joint_tl$'Misclassification of outcome'[ROB_joint_tl$'Misclassification of outcome' == 'Probably yes'] <- 'Likely Low'
ROB_joint_tl$'Differential information 3'[ROB_joint_tl$'Differential information 3' == 'Probably yes'] <- 'Likely Low'
ROB_joint_tl$'Differential information 4'[ROB_joint_tl$'Differential information 4' == 'Probably yes'] <- 'Likely Low'
ROB_joint_tl$'Confounding'[ROB_joint_tl$'Confounding' == 'Probably yes'] <- 'Likely Low'

ROB_joint_tl$'Selection bias'[ROB_joint_tl$'Selection bias' == 'Probably no'] <- 'Likely High'
ROB_joint_tl$'Selection_bias 2'[ROB_joint_tl$'Selection_bias 2' == 'Probably no'] <- 'Likely High'
ROB_joint_tl$'Misclassification of exposure'[ROB_joint_tl$'Misclassification of exposure' == 'Probably no'] <- 'Likely High'
ROB_joint_tl$'Misclassification of outcome'[ROB_joint_tl$'Misclassification of outcome' == 'Probably no'] <- 'Likely High'
ROB_joint_tl$'Differential information 3'[ROB_joint_tl$'Differential information 3' == 'Probably no'] <- 'Likely High'
ROB_joint_tl$'Differential information 4'[ROB_joint_tl$'Differential information 4' == 'Probably no'] <- 'Likely High'
ROB_joint_tl$'Confounding'[ROB_joint_tl$'Confounding' == 'Probably no'] <- 'Likely High'

ROB_joint_tl$'Selection bias'[ROB_joint_tl$'Selection bias' == 'Definitely no (high risk of bias)'] <- 'High'
ROB_joint_tl$'Selection_bias 2'[ROB_joint_tl$'Selection_bias 2' == 'Definitely no (high risk of bias)'] <- 'High'
ROB_joint_tl$'Misclassification of exposure'[ROB_joint_tl$'Misclassification of exposure' == 'Definitely no (high risk of bias)'] <- 'High'
ROB_joint_tl$'Misclassification of outcome'[ROB_joint_tl$'Misclassification of outcome' == 'Definitely no (high risk of bias)'] <- 'High'
ROB_joint_tl$'Differential information 3'[ROB_joint_tl$'Differential information 3' == 'Definitely no (high risk of bias)'] <- 'High'
ROB_joint_tl$'Differential information 4'[ROB_joint_tl$'Differential information 4' == 'Definitely no (high risk of bias)'] <- 'High'
ROB_joint_tl$'Confounding'[ROB_joint_tl$'Confounding' == 'Definitely no (high risk of bias)'] <- 'High'

##########

#g <- nrow(up_forest_melo)

#up_forest_melo$id2 <- c(1:(nrow(up_forest_melo)))
#up_forest_melo[nrow(up_forest_melo)+1,1] <- "OUTCOME"
#up_forest_melo[g+1,2] <- "CATEGORY"
#up_forest_melo[g+1,3] <- "EXPOSURE"
#up_forest_melo[g+1,5] <- "SUBCATEGORY"
#up_forest_melo[g+1,6] <- "EFFECT MEASURE"
#########
forest123 <- read_excel("datasets/forest123.xlsx")
#forest <- forest %>% mutate(inter = ifelse(is.na(lowerci), yi, paste(forest$yi,"[",forest$lowerci, ",", forest$upperci,"]")), Reference = paste(forest$id, ".", forest$study))
forest <- forest %>% mutate(inter = ifelse(is.na(lowerci), paste("Effect size =",forest$yi,"\n Outcome:",forest$Outcome.variable), paste("Effect size (95% CI) =",forest$yi,"[",forest$lowerci, ",", forest$upperci,"]","\n Outcome:",forest$Outcome.variable)), Reference = paste(forest123$study, "(",forest123$id, ")"))
#forest <- forest %>% mutate(inter1 = ifelse(is.na(lowerci), paste("Effect size =",forest$yi,"\n Outcome:",forest$Outcome.variable), paste("Effect size (95% CI) =",forest$yi,"[",forest$lowerci, ",", forest$upperci,"]","\n Outcome:",forest$Outcome.variable)))
forest <- forest %>% mutate(inter1 = ifelse(is.na(lowerci), paste(forest$yi), paste(forest$yi,"[",forest$lowerci,",", forest$upperci,"]")))
#forest123 <- forest123 %>% mutate( Reference = paste(forest123$id, ".", forest123$study))
forest123 <- forest123 %>% mutate( Reference = paste(forest123$study, "(",forest123$id, ")" ))

#### change names
forest$effect_z <- forest$mm
forest$effect_z[forest$effect_z == 'OR'] <- 'Odds Ratio (OR)'
forest$effect_z[forest$effect_z == 'PR'] <- 'Prevalence Ratio (PR)'
forest$effect_z[forest$effect_z == 'beta'] <- 'beta coefficient of the variable'
forest$effect_z[forest$effect_z == 'beta p value'] <- 'p value of the beta coefficient of the variable'
forest$effect_z[forest$effect_z == 'OR p value'] <- 'p value of the Odds Ratio'
#### change names
#forest_joint$effect_z[forest_joint$effect_z == 'OR'] <- 'Odds Ratio (OR)'
#forest$effect_z[forest$effect_z == 'PR'] <- 'Prevalence Ratio (PR)'

forest_joint$effect_z <- forest_joint$mm
up_forest_state1$effect_z_state <- up_forest_state1$mm
forest_joint$effect_z[forest_joint$effect_z == 'PR'] <- 'IDR'
#forest_cross_event$effect_z[forest_cross_event$effect_z == 'OR'] <- 'Odds Ratio (OR)'
#forest_cross_event$effect_z[forest_cross_event$effect_z == 'PR'] <- 'Prevalence Ratio (PR)'
#forest_cross_event$effect_z[forest$effect_z == 'beta'] <- 'beta coefficient of the variable'
#forest_cross_event$effect_z[forest$effect_z == 'beta p value'] <- 'p value of the beta coefficient of the variable'
#forest_cross_event$effect_z[forest$effect_z == 'OR p value'] <- 'p value of the Odds Ratio'


#### change names to capital letters 
forest$narrow[forest$narrow == 'aerosols'] <- 'Aerosols'
forest$narrow[forest$narrow == 'distance'] <- 'Distance'
forest$narrow[forest$narrow == 'other'] <- 'Other'

forest$t_expo <- forest$Type_Exposure
forest$t_expo[forest$t_expo == 'surrogate'] <- 'Indirect measures of exposure'
forest$t_expo[forest$t_expo == 'direct'] <- 'Direct measures of exposure'



## map data ####
cafoo <- cafo %>% 
  mutate(Country = ifelse(Refid %in% c(648, 690, 743, 288), "Germany",
                          ifelse(Refid %in% c(81, 203), "Netherlands", "United States"))) %>% 
  # mutate(`State` = ifelse(Refid %in% c(64, 690, 743, 288), NA,
  #                         ifelse(Refid %in% c(81, 203), NA, "North Carolina"))) %>% 
  mutate(long = ifelse(Country == "Germany", 10.44768,
                       ifelse(Country == "Netherlands", 5.2913, -98.5795))) %>% 
  mutate(lat = ifelse(Country == "Germany", 51.16338,
                      ifelse(Country == "Netherlands", 52.1326, 39.8283))) %>% 
  distinct(Refid, .keep_all = TRUE) %>% 
  group_by(Country, long, lat) %>% 
  summarise(`Number of Studies` = n())

###### alternative dataset for map
cafo_map <- readxl::read_xlsx("datasets/geoloction_cafos.xlsx")

names(cafo_map)[9] <- 'Country'
cafoo_map <- cafo_map %>% group_by(Country) %>% 
  summarise(`Number_of_Studies` = n())%>%
  mutate(long = ifelse(Country == "Germany", 10.44768,
                       ifelse(Country == "Netherlands", 5.2913,ifelse(Country == "USA", -98.5795, ifelse(Country=="Norway", 8.468946, ifelse(Country=="Mexico", -102.552784, ifelse(Country=="Canada", -106.346771, ifelse(Country=="UK", -3.435973, 2.213749))) ))))) %>% 
  mutate(lat = ifelse(Country == "Germany", 51.165691,
                      ifelse(Country == "Netherlands", 52.132633,ifelse(Country == "USA", 37.09024, ifelse(Country=="Norway", 60.472024, ifelse(Country=="Mexico", 23.634501, ifelse(Country=="Canada", 56.130366, ifelse(Country=="UK", 55.378051, 46.227638))) )))))

####### alternative for each health category


cafoo_map_cat <- testtimeline %>%
  mutate(long = ifelse(country == "Germany", 10.44768,
                       ifelse(country == "Netherlands", 5.2913,ifelse(country == "USA", -98.5795, ifelse(country=="Norway", 8.468946, ifelse(country=="Mexico", -102.552784, ifelse(country=="Canada", -106.346771, ifelse(country=="UK", -3.435973, 2.213749))) ))))) %>% 
  mutate(lat = ifelse(country == "Germany", 51.165691,
                      ifelse(country == "Netherlands", 52.132633,ifelse(country == "USA", 37.09024, ifelse(country=="Norway", 60.472024, ifelse(country=="Mexico", 23.634501, ifelse(country=="Canada", 56.130366, ifelse(country=="UK", 55.378051, 46.227638))) )))))

## timeline data ####
## add two columns: author(paperInfo) & published year(paperYear) 
dataset$paperInfo <- rep("", nrow(dataset))
dataset$paperYear <- rep(NA, nrow(dataset))
idx_9 <- which(dataset$Refid == 9)
dataset$paperInfo[idx_9] <- "Schinasi et al. 2014"
dataset$paperYear[idx_9] <- 2014
idx_81 <- which(dataset$Refid == 81)
dataset$paperInfo[idx_81] <- "Smit et al. 2013"
dataset$paperYear[idx_81] <- 2013
idx_203 <- which(dataset$Refid == 203)
dataset$paperInfo[idx_203] <- "Smit et al. 2012"
dataset$paperYear[idx_203] <- 2012
idx_288 <- which(dataset$Refid == 288)
dataset$paperInfo[idx_288] <- "Schulze et al. 2011"
dataset$paperYear[idx_288] <- 2011
idx_327 <- which(dataset$Refid == 327)
dataset$paperInfo[idx_327] <- "Schinasi et al. 2011"
dataset$paperYear[idx_327] <- 2011
idx_452 <- which(dataset$Refid == 452)
dataset$paperInfo[idx_452] <- "Horton et al. 2009"
dataset$paperYear[idx_452] <- 2009
idx_648 <- which(dataset$Refid == 648)
dataset$paperInfo[idx_648] <- "Radon et al. 2007"
dataset$paperYear[idx_648] <- 2007
idx_690 <- which(dataset$Refid == 690)
dataset$paperInfo[idx_690] <- "Hoopmann et al. 2006"
dataset$paperYear[idx_690] <- 2006
idx_713 <- which(dataset$Refid == 713)
dataset$paperInfo[idx_713] <- "Mirabelli et al. 2006"
dataset$paperYear[idx_713] <- 2006
idx_743 <- which(dataset$Refid == 743)
dataset$paperInfo[idx_743] <- "Radon et al. 2005"
dataset$paperYear[idx_743] <- 2005
idx_775 <- which(dataset$Refid == 775)
dataset$paperInfo[idx_775] <- "Schiffman et al. 2005"
dataset$paperYear[idx_775] <- 2005
idx_795 <- which(dataset$Refid == 795)
dataset$paperInfo[idx_795] <- "Avery et al. 2004"
dataset$paperYear[idx_795] <- 2004
idx_1187 <- which(dataset$Refid == 1187)
dataset$paperInfo[idx_1187] <- "Schiffman et al. 1995"
dataset$paperYear[idx_1187] <- 1995
idx_1552 <- which(dataset$Refid == 1552)
dataset$paperInfo[idx_1552] <- "Wing et al. 2013"
dataset$paperYear[idx_1552] <- 2013
idx_2417 <- which(dataset$Refid == 2417)
dataset$paperInfo[idx_2417] <- "Bullers et al. 2005"
dataset$paperYear[idx_2417] <- 2005
idx_4000 <- which(dataset$Refid == 4000)
dataset$paperInfo[idx_4000] <- "Feingold et al. 2012"
dataset$paperYear[idx_4000] <- 2012 



## outcome data ####
dataset <- dataset %>% 
  mutate(paperInfo = factor(paperInfo, levels=names(sort(table(paperInfo), increasing=TRUE))),
         Categorized.class = recode(
           Categorized.class,
           `Dermatologic` =  "Skin",
           Eye = "Eye or Ear", 
           `Live style`= "Other",
           Otologic = "Eye or Ear",
           Psychological = 'Mental Health',
           Stress  = 'Mental Health'))

## forest plot ####
## ROB columns
ROB_cols <- grep("ROB_", names(dataset), value = TRUE)[c(1:7, 9:15)]

## summary - ROB ####


############ summary risk of bias: added monday 08/23/2021
r2 <- ROB_joint_tl# %>%
#select('IDD', 'category','Selection bias' , 'Selection_bias 2', 'Misclassification of exposure', 'Misclassification of outcome' ,'Differential information 3' ,'Differential information 4', 'Confounding') %>% 
#setNames(c("Refid", "categoy",bias_types))
r22 <- r2 %>% gather(key = `Type of Bias`, value = Bias, c('Selection bias' , 'Selection_bias 2', 'Misclassification of exposure', 'Misclassification of outcome' ,'Differential information 3' ,'Differential information 4', 'Confounding')) %>% 
  mutate(Bias = forcats::fct_relevel(Bias, "Low","Likely Low", "Likely High", "High", "Uncertain"),
         `Type of Bias` = forcats::fct_relevel(`Type of Bias`, 'Selection bias' , 'Selection_bias 2', 'Misclassification of exposure', 'Misclassification of outcome' ,'Differential information 3' ,'Differential information 4', 'Confounding')) #%>% 

r22_1 <- r22[complete.cases(r22), ]

color_table <- tibble(
  Bias = c("High", "Likely High", "Likely Low", "Low", "Uncertain"),
  #Color = c(RColorBrewer::brewer.pal(4, "RdYlGn"), "#bdbdbd")
  #Color = c("red", "salmon","lightgreen","forestgreen" , "wheat")
  Color = c("forestgreen", "lightgreen","salmon","red" , "wheat")
)
pal <- color_table$Color
names(pal) <- color_table$Bias

############
## DT ####
forest_dt <- function(forest_data){
  forest_data %>% 
    select(Refid, Outcome.variable:Exposure.measure,
           Effect.measure.1, interval, ROB_overall_paige) %>% 
    mutate_at(vars(Refid:Exposure.measure), funs(as.factor)) %>% 
    datatable(
      colnames = c("Effect size" = 5, "Confidence interval" = 6, "Overall risk of bias" = 7),
      filter = list(position = "top"), class = "display compact",
      options = list(order = list(list(4, "desc")),
                     autoWidth = FALSE,
                     columnDefs = list(
                       list(className = "dt-center", targets = c(1, 4, 5)),
                       list(width = "50px", targets = 1),
                       list(width = "100px", targets = 4),
                       list(width = "150px", targets = 5)
                     )
      )) %>% 
    formatRound("Effect size", 2)
}

## plotly ####
forest_plotly <- function(forest_data, rowIndex){
  forest_data <- forest_data %>% filter(id %in% rowIndex)
  p1 <- plot_ly(
    forest_data,
    x = ~Effect.measure.1, y = ~id,
    color = ~ROB_overall_paige,
    colors = pal,
    type = "scatter", mode = "markers",
    hoverinfo = "text",
    text = ~sprintf("Row index: %s\nEffect size: %.2f (%.2f, %.2f)\nOverall risk of bias: %s",
                    id, Effect.measure.1, Lower, Upper, ROB_overall_paige),
    error_x = ~list(type = "data", color = "black",
                    symmetric = FALSE,
                    array = Upper-Effect.measure.1,
                    arrayminus = Effect.measure.1-Lower)) %>% 
    layout(xaxis = list(title = "Effect size"),
           yaxis = list(title = "Row index"))
  p1$elementId <- NULL
  plotly_data <- forest_data %>% select(id, contains("paige"), -ROB_overall_paige) %>%  
    setNames(c("id", bias_types)) %>% gather(source, rate, -id) %>% 
    replace_na(list(rate = "Uncertain"))
  p2 <- plotly_data %>% plot_ly(
    x = ~source, y = ~id, 
    color = ~rate, colors = pal,
    type = "scatter", mode = "markers",
    marker = list(size = 10, symbol = "square"),
    hoverinfo = "text",
    text = ~sprintf("Row index: %s\nSource of bias: %s\nRating: %s",
                    id, source, rate))
  p2$elementId <- NULL
  subplot(style(p1, showlegend = FALSE), p2, widths = c(0.7, 0.3), shareY = TRUE, titleY = FALSE)
}

######################
#### low_res_map
## map data ####
cafoo_low_res <- cafo %>% 
  mutate(Country = ifelse(Refid %in% c(648, 690), "Germany",
                          ifelse(Refid %in% c(81, 203), "Netherlands", "United States"))) %>% 
  # mutate(`State` = ifelse(Refid %in% c(64, 690, 743, 288), NA,
  #                         ifelse(Refid %in% c(81, 203), NA, "North Carolina"))) %>% 
  mutate(long = ifelse(Country == "Germany", 13.404954,
                       ifelse(Country == "Netherlands", 4.899431, -78.644257))) %>% 
  mutate(lat = ifelse(Country == "Germany", 52.520008,
                      ifelse(Country == "Netherlands", 52.379189, 35.787743))) %>% 
  distinct(Refid, .keep_all = TRUE) %>% 
  group_by(Country, long, lat) %>% 
  summarise(`Number of Studies` = n())
