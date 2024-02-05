forest_cross <- read_excel("datasets/distiller_cross.xlsx")
forest_cross_events <- forest_cross %>% filter(rare_outcome == "Yes" | health_event=="Yes")

exclusion <- read_excel("datasets/exclusion_september.xlsx")

forest_cohort <- read_excel("datasets/distiller_cohort.xlsx")
testtimeline <- read_excel("datasets/testtimeline.xlsx")
forest_cohort <-  forest_cohort %>% mutate(authors = ifelse(Refid%in%testtimeline$Refid, testtimeline$authors, ""))

forest_cohort <- read_excel("datasets/distiller_cohort.xlsx")
forest_case <- read_excel("datasets/distiller_casecontrol.xlsx")

testtimeline <- read_excel("datasets/testtimeline.xlsx")

auxiliar <- forest_cross %>% group_by(Country) %>% summarise(No = n())

ROB_aux <-  bind_rows(forest_cross, forest_cohort, forest_case)

ROB_44 <-  forest_cross %>% select(Refid, category, Country)
ROB_55 <-  forest_cohort %>% select(Refid, category, Country)
ROB_66 <-  forest_case %>% select(Refid, category, Country)

ROB_aux <-  bind_rows(ROB_44, ROB_55, ROB_66)

ROB_aux %>% filter(category=="Lower Respiratory")%>%
  distinct()%>% group_by(Country) %>% summarise(Count = n())


df<-data.frame("Canada","-106.346771", "56.130366", "1")
names(df)<-c("Country","long", "lat","Number of Studies")
as.numeric(df$long)
as.numeric(df$lat)
as.numeric(df$"Number of Studies")
df %>% add_row(
cafoo1  <- data.frame( "Canada","-106.346771", "56.130366")
cafoo2  <- data.frame( "United States","-98.57950", "39.82830")
cafoo3 <- data.frame(cafoo1, cafoo2)
names(cafoo3)<-c("Country", "long", "lat")

cafoo5 <- cafoo
cafoo5[1,1] <- "Canada"
cafoo5[1,2] <- -106.346771
cafoo5[1,3] <- 56.130366
cafoo5 <- cafoo5[-c(2), ]
  

forest_cross <- forest_cross %>% filter(Refid == "5"  | Refid == "7"| Refid == "32507767"| Refid == "32507932" )%>% mutate(Country = "Netherlands")
forest_cross <- forest_cross %>% filter(Refid == "4"  | Refid == "6"| Refid == "32507530"| Refid == "32508669" )%>% mutate(Country = "USA")
forest_cross <- forest_cross %>% filter(Refid == "9"  | Refid == "11"| Refid == "13"| Refid == "15" )%>% mutate(Country = "Germany")                                                                                                                          
forest_cross <- forest_cross %>% filter(Refid == "32508377"  | Refid == "11")%>% mutate(Country = "Mexico")


forest_cross <-  forest_cross %>% mutate(IDD = c(1:nrow(forest_cross)))
forest_cross <- forest_cross %>% mutate( IDD_2 = paste(forest_cross$IDD, "Cross-sectional" ))
as.data.frame(table(forest_cohort$Refid))
as.data.frame(table(forest_cross$Refid))
as.data.frame(table(forest_case$Refid))



!is.na(Reason.Reco)
empty1 <- forest_cross %>% filter(is.na(Differential_information_bias)& is.na(Differential_information_bias_1_V2))
states1 <- forest_cross %>% filter(is.na(Differential_information_bias)& is.na(Differential_information_bias_1_V2))

rty3 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(states1))
states1 <-  states1 %>% mutate(IDD = c(1:nrow(states1)))
states1 <- states1 %>% mutate( IDD_2 = paste(states1$IDD, "Cross-sectional" ))




rty124 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cross_events))
forest_cross_events <-  forest_cross_events %>% mutate(IDD = c(1:nrow(forest_cross_events)))
forest_cross_events <- forest_cross_events %>% mutate( IDD_2 = paste(forest_cross_events$IDD, "Cross-sectional" ))


if (sum(rty124)==6 ) {
  yis0 <- forest_cross_events[c(9,13,17,21,25,29)]
  yis <- forest_cross_events[c(10,14,18,22,26,30)]
  yis1 <- forest_cross_events[c(11,15,19,23,27,31)]
  yis2 <- forest_cross_events[c(12,16,20,24,28,32)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_events$outcome, each = 6)
  mm <- rep(forest_cross_events$effect_measure, each = 6)
  Exposure.measure <- rep(forest_cross_events$exposure, each = 6)
  Categorized.class <- rep(forest_cross_events$category, each = 6)
  IDD <- rep(forest_cross_events$IDD, each = 6)
  IDD_2 <- rep(forest_cross_events$IDD_2, each = 6)
  
  up_forest_melo_124 <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1_124 <- up_forest_melo_124[complete.cases(up_forest_melo_124),]
}else if (sum(rty124)==5){
  yis0 <- forest_cross_events[c(9,13,17,21,25)]
  yis <- forest_cross_events[c(10,14,18,22,26)]
  yis1 <- forest_cross_events[c(11,15,19,23,27)]
  yis2 <- forest_cross_events[c(12,16,20,24,28)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_events$outcome, each = 5)
  mm <- rep(forest_cross_events$effect_measure, each = 5)
  Exposure.measure <- rep(forest_cross_events$exposure, each = 5)
  Categorized.class <- rep(forest_cross_events$category, each = 5)
  IDD <- rep(forest_cross_events$IDD, each = 5)
  IDD_2 <- rep(forest_cross_events$IDD_2, each = 5)
  
  up_forest_melo_124 <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1_124 <- up_forest_melo_124[complete.cases(up_forest_melo_124),]
}else if (sum(rty124)==4){
  yis0 <- forest_cross_events[c(9,13,17,21)]
  yis <- forest_cross_events[c(10,14,18,22)]
  yis1 <- forest_cross_events[c(11,15,19,23)]
  yis2 <- forest_cross_events[c(12,16,20,24)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_events$outcome, each = 4)
  mm <- rep(forest_cross_events$effect_measure, each = 4)
  Exposure.measure <- rep(forest_cross_events$exposure, each = 4)
  Categorized.class <- rep(forest_cross_events$category, each = 4)
  IDD <- rep(forest_cross_events$IDD, each = 4)
  IDD_2 <- rep(forest_cross_events$IDD_2, each = 4)
  
  up_forest_melo_124 <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1_124 <- up_forest_melo_124[complete.cases(up_forest_melo_124),]
}else if (sum(rty124)==3){
  yis0 <- forest_cross_events[c(9,13,17)]
  yis <- forest_cross_events[c(10,14,18)]
  yis1 <- forest_cross_events[c(11,15,19)]
  yis2 <- forest_cross_events[c(12,16,20)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_cross_events$outcome, each = 3)
  mm <- rep(forest_cross_events$effect_measure, each = 3)
  Exposure.measure <- rep(forest_cross_events$exposure, each = 3)
  Categorized.class <- rep(forest_cross_events$category, each = 3)
  IDD <- rep(forest_cross_events$IDD, each = 3)
  IDD_2 <- rep(forest_cross_events$IDD_2, each = 3)
  
  up_forest_melo_124 <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_melo1_124 <- up_forest_melo_124[complete.cases(up_forest_melo_124),]
  
  
  
  paste("Country:","<br>","<b><a href='https://pubmed.ncbi.nlm.nih.gov/21864103/'>Schulze et al. 2011</a></b>","<br>",
        "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17435437/'>Radon et al. 2007</a></b>","<br>",
        "<b><a href='https://pubmed.ncbi.nlm.nih.gov/17039438/'>Hoopmann et al. 2006</a></b>","<br>",
        "<b><a href='https://pubmed.ncbi.nlm.nih.gov/16379061/'>Radon et al. 2005</a></b>","<br>"
  )
}


testtimeline <- readxl::read_xlsx("datasets/testtimeline.xlsx")
webf <- testtimeline %>% filter(country=="USA")%>% select(weblink)
paste(webf[1,])
rosa <- t(webf)
rosa <- as.data.frame(t(webf))

xy.list <- split(webf, seq(nrow(webf)))
xy.list <- split(xy.df, seq(nrow(xy.df)))
paste (webf , sep = ("PERRA"))
k <- webf[1]
webf[1:nrow(webf),]


cafo_map <- readxl::read_xlsx("datasets/geoloction_cafos.xlsx")

names(cafo_map)[9] <- 'Country'
cafoo_map <- cafo_map %>% group_by(Country) %>% 
  summarise(`Number_of_Studies` = n())%>%
  mutate(long = ifelse(Country == "Germany", 10.44768,
                       ifelse(Country == "Netherlands", 5.2913,ifelse(Country == "USA", -98.5795, ifelse(Country=="Norway", 8.468946, ifelse(Country=="Mexico", -102.552784, ifelse(Country=="Canada", -106.346771, ifelse(Country=="UK", -3.435973, 2.213749))) ))))) %>% 
  mutate(lat = ifelse(Country == "Germany", 51.165691,
                      ifelse(Country == "Netherlands", 52.132633,ifelse(Country == "USA", 37.09024, ifelse(Country=="Norway", 60.472024, ifelse(Country=="Mexico", 23.634501, ifelse(Country=="Canada", 56.130366, ifelse(Country=="UK", 55.378051, 46.227638))) )))))

webf1 <- testtimeline %>% filter(country=="USA")%>% select(weblink)
mango1 <- r(webf1)
mango <- readxl::read_xlsx("datasets/mango.xlsx")
mango1 <- data.frame(t(webf1))
mango2 <- data.frame( str_c(mango1, collapse = ", "))

forest_data <- forest_data %>% filter(id %in% rowIndex)

map_data <- testtimeline %>% filter(country %in% "USA")%>% select(weblink)
dt[dt$fct %in% vc,]
testtimeline[testtimeline$country %in% "USA", ]



rv$data[which(rv$data$data == "previous_studies"), "n"]
data.frame(str_c(webf1, collapse = ", "))
cafoo_map$URL <-  cafoo_map%>% filter(cafoo_map$Country %in% testtimeline$country) %>% data.frame(str_c(testtimeline$weblink, collapse = ", "))
timedata2 <- testtimeline  %>% filter(AR=="Yes")

mangos <- forest_cross %>% select(Refid, category)
mangos1<- data.frame(table(mangos$category))
usa1 <- mangos %>% filter(category=="Lower Respiratory")
usa2 <- usa1 %>% group_by(Refid)%>%summarise(No = n())
osmolar <- data.frame(table(usa1))

cafo3 <- cafo2 %>% distinct() %>%
  group_by(Country, Title, Year, abbr) %>% summarise(Count = n())

mangos <- forest_cross %>% select(Refid, category)
mangos1 <- forest_cohort %>% select(Refid, category)
mangos2 <- forest_case %>% select(Refid, category)

mangosT <- bind_rows(mangos, mangos1, mangos2)


reyes<- mangosT %>% distinct()
reyes1 <- reyes%>% filter(category=="Lower Respiratory")
reyes2 <- reyes%>% filter(category=="Upper Respiratory")
reyes3 <- reyes%>% filter(category=="Gastrointestinal diseases")
reyes4 <- reyes%>% filter(category=="Antimicrobial resistance")

cafoo <- cafo %>% 
  mutate(Country = ifelse(Refid %in% c(648, 690, 743, 288), "Germany", x)

testtimeline <- testtimeline %>%
  mutate(LR = ifelse(Refid %in% reyes1$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(Gastro = ifelse(Refid %in% reyes3$Refid, "Yes", "No")   )





cross_categories <- forest_cross %>% select(Refid, category)
cohort_categories <- forest_cohort %>% select(Refid, category)
case_categories <- forest_case %>% select(Refid, category)

total_categories <- bind_rows(cross_categories, cohort_categories, case_categories)


total_categories_1 <- total_categories %>% distinct()
Lower <- total_categories_1%>% filter(category=="Lower Respiratory")
Upper <- total_categories_1%>% filter(category=="Upper Respiratory")
AR1 <- total_categories_1%>% filter(category=="Gastrointestinal condition")
GI1 <- total_categories_1%>% filter(category=="Antimicrobial resistance")



testtimeline <- testtimeline %>%
  mutate(Lower_cat1 = ifelse(Refid %in% Lower$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(Upper_cat = ifelse(Refid %in% Upper$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(AR_cat = ifelse(Refid %in% AR1$Refid, "Yes", "No")   )

testtimeline <- testtimeline %>%
  mutate(GI_cat = ifelse(Refid %in% GI1$Refid, "Yes", "No")   )



ROB_44 <-  forest_cross %>% select(Refid, category, Country)
ROB_55 <-  forest_cohort %>% select(Refid, category, Country)
ROB_66 <-  forest_case %>% select(Refid, category, Country)

ROB_aux <-  bind_rows(ROB_44, ROB_55, ROB_66)


huy <- mangosT %>% filter(category=="Antimicrobial resistance")%>%
  distinct()%>% group_by(Country) %>% summarise(Count = n())




testtimeline %>% filter(LR=="Yes")%>%
  #distinct()%>% group_by(country) %>% summarise(Count = n())%>%
  ggplot(aes(x = country)) +
  geom_bar(aes(fill = country)) +
  scale_fill_brewer(palette = "Set2")+labs(x = "Country", y = "Number of Studies")+theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))



cafoo_map_cat <- testtimeline %>%
  mutate(long = ifelse(country == "Germany", 10.44768,
                       ifelse(country == "Netherlands", 5.2913,ifelse(country == "USA", -98.5795, ifelse(country=="Norway", 8.468946, ifelse(country=="Mexico", -102.552784, ifelse(country=="Canada", -106.346771, ifelse(country=="UK", -3.435973, 2.213749))) ))))) %>% 
  mutate(lat = ifelse(country == "Germany", 51.165691,
                      ifelse(country == "Netherlands", 52.132633,ifelse(country == "USA", 37.09024, ifelse(country=="Norway", 60.472024, ifelse(country=="Mexico", 23.634501, ifelse(country=="Canada", 56.130366, ifelse(country=="UK", 55.378051, 46.227638))) )))))


subset_low <- cafoo_map_cat%>% filter(LR=="Yes")

forest_joint$effect_z <- forest_joint$mm
forest_joint$effect_z[forest_joint$effect_z == 'PR'] <- 'IDR'




testtimeline <- readxl::read_xlsx("datasets/testtimeline.xlsx")


forest_cross <-  forest_cross %>% mutate(IDD = c(1:nrow(forest_cross)))

forest_cross <-  forest_cross %>% mutate(authors = ifelse(Refid%in%testtimeline$Refid, testtimeline$weblink, ""))


cafoo <- cafo %>% 
  mutate(Country = ifelse(Refid %in% c(648, 690, 743, 288), "Germany",
                          ifelse(Refid %in% c(81, 203), "Netherlands", "United States")))

exclusion <- exclusion %>%
  mutate(reason0 = ifelse(`Does the study include more than one unit of measurement of exposure?`=="No", paste("Azul"), "")
          )
exclusion <- exclusion %>%
  mutate(reason1 = ifelse(`Does the study report animal feeding operations that would be reasonably considered either large, concentrated or intensive by modern standards (not nomadic, small holder or pastoral)?`=="No", paste("Amarillo"), "")
  )


exclusion <- exclusion %>%
  filter(`Does the study include more than one unit of measurement of exposure?`=="No")

exclusion$badbonny <-  cbind(exclusion$reason0, exclusion$reason1)

exclusion$badbonny <- merge(exclusion$reason0, exclusion$reason1)
  
write_xlsx(exclusion,"datasets/exclusion1.xlsx")


df$word_count <- rowSums( !is.na( df [,3:5]))

exclusion$count <- rowSums( !is.na( exclusion [,11:18]))

exclusion <- exclusion %>%
  mutate(`Reason for exclusion` = ifelse(count==1, "The study is not written entirely in English",
                         ifelse(count==3, "The study is not primary research",
                                ifelse(count==4, "The study does not report a comparative association between a relevant animal feeding operation and measures of health in surrounding-community members only and include occupational exposure", 
                                       ifelse(count==5, "The study does not analyze the relationship between outcome and exposure at individual human level",
                                       ifelse(count==6, "The study does not report animal feeding operations that would be reasonably considered either large, concentrated or intensive by modern standards",
                                       ifelse(count==7, "The study does not include more than one unit of measurement of exposure", "The study does not include at least one human health outcome measured using either an eligible survey instrument, test, assay or diseases measure obtained from medical records")
                                       
                         ))))))

exclusionF <- exclusion[c(2, 3, 4, 5, 6, 7,8, 21)]











rty2 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cohort))
forest_cohort <-  forest_cohort%>% mutate(IDD = c(1:nrow(forest_cohort)))
forest_cohort <- forest_cohort %>% mutate( IDD_2 = paste(forest_cohort$IDD, "Cohort" ))
if (sum(rty2)==6 ) {
  yis0 <- forest_cohort[c(9,13, 17, 21, 25, 29)] 
  yis <- forest_cohort[c(10, 14, 18, 22, 26, 30)]  
  yis1 <- forest_cohort[c(11, 15, 19, 23, 27, 31)]  
  yis2 <- forest_cohort[c(12, 16, 20, 24,28, 32)] 
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Refid <- rep(forest_cohort$Refid, each = 6)
  Outcome.variable <- rep(forest_cohort$outcome, each = 6)
  authors <- rep(forest_cohort$authors, each = 6)
  mm <- rep(forest_cohort$effect_measure, each = 6)
  Exposure.measure <- rep(forest_cohort$exposure, each = 6)
  Categorized.class <- rep(forest_cohort$category, each = 6)
  IDD <- rep(forest_cohort$IDD, each = 6)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 6)
  
  up_forest_cohort <- data.frame(
    Refid, IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2, authors)
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
  authors <- rep(forest_cohort$authors, each = 5)
  mm <- rep(forest_cohort$effect_measure, each = 5)
  Exposure.measure <- rep(forest_cohort$exposure, each = 5)
  Categorized.class <- rep(forest_cohort$category, each = 5)
  IDD <- rep(forest_cohort$IDD, each = 5)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 5)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2, authors)
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
  authors <- rep(forest_cohort$authors, each = 4)
  mm <- rep(forest_cohort$effect_measure, each = 4)
  Exposure.measure <- rep(forest_cohort$exposure, each = 4)
  Categorized.class <- rep(forest_cohort$category, each = 4)
  IDD <- rep(forest_cohort$IDD, each = 4)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 4)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2, authors)
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
  authors <- rep(forest_cohort$authors, each = 3)
  mm <- rep(forest_cohort$effect_measure, each = 3)
  Exposure.measure <- rep(forest_cohort$exposure, each = 3)
  Categorized.class <- rep(forest_cohort$category, each = 3)
  IDD <- rep(forest_cohort$IDD, each = 3)
  IDD_2 <- rep(forest_cohort$IDD_2, each = 3)
  
  up_forest_cohort <- data.frame(
    IDD, IDD_2,Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2, authors)
  up_forest_cohort1 <- up_forest_cohort[complete.cases(up_forest_cohort),]
}
