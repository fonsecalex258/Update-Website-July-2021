theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0, 0, 0,0), "lines")
)

#use
june$id2 <- c(1:12)
july$id2 <- c(1:23)
nrow(july)
june %>% add_row(id = NA, study= NA)
dat <- data.frame(group = factor(c("A","B","C","D","E","F","G"), levels=c("F","E","D","C","B","A","G"))
june <- data.frame(id2 =factor(c(1:12)) )

df[nrow(df)+1,] <- NA
#use
june[nrow(june)+1,]<- NA
july[nrow(july)+1,]<- NA

p <- ggplot(june,aes(yi,id2)) + 
  geom_point(size=5, shape=18) +
  geom_errorbarh(aes(xmax = upperci, xmin = lowerci), height = 0.15) +
  geom_vline(xintercept = 1, linetype = "longdash") +
  scale_x_continuous(breaks = seq(0,14,1), labels = seq(0,14,1)) +
  labs(x="Adjusted Odds Ratio", y="")

p <- ggplot(july,aes(Effect.measure.1,id2)) + 
  geom_point(size=5, shape=18) +
  geom_errorbarh(aes(xmax = Upper, xmin = Lower), height = 0.15) +
  geom_vline(xintercept = 1, linetype = "longdash") +
  scale_x_continuous(breaks = seq(0,8,1), labels = seq(0,8,1)) +
  labs(x="Adjusted Odds Ratio", y="")

lab <- data.frame(V0 = factor(c("A","Bray","C","D","E","F","G"), levels=c("D","C","Bray","A","F","E","G")),
                  V05 = rep(c(1,2),each=7),
                  V1 = c("Gender","Men","Women","Men","Women","Men","Women","OR",3.1,2.0,1.6,3.2,3.6,7.6)
)


alex123 <- data.frame()

june3 <- data.frame(a = c(june[,"Type_Exposure"], june[,"Outcome.variable"]))
d1 <- data.frame(a=unlist(df, use.names = FALSE))
june3 <- cbind(june$Type_Exposure,june$Outcome.variable)

dat2a <- data.frame(dat[1:2], stack(dat[3:ncol(dat)]))
june3 <- data.frame(c(june$id,june$Type_Exposure))

V0 = rep(c(1:11), times=2)
june4 <- data.frame(V0 = rep(c(1:12), times=2),
                  V05 = rep(c(1,2),each=12),
                  V1 = c(june$Type_Exposure,june$Outcome.variable)
)

june5 <- data.frame(V0 = rep(c(1:23), times=5),
                    V05 = rep(c(1,2,3,4,5),each=23),
                    V1 = c(july$User,july$Outcome.variable, july$Exposure.measure, july$Subcategory, july$inter)
)

june5 <- data.frame(V0 = rep(c(1:23), times=4),
                    V05 = rep(c(1,2,3,4),each=23),
                    V1 = c(july$Outcome.variable, july$Exposure.measure, july$Subcategory, july$inter)
)


june5[23,3] <- "Outcome"
june5[23*2,3] <- "Exposure"
june5[23*3,3] <- "Subcategory"
june5[23*4,3] <- "EM(95% CI)"


june4[12,3]<-"TYPE"
june4[12*2,3]<-"OUTCOME"

forest <- forest %>% mutate(inter = ifelse(is.na(lowerci), paste("Effect size =",forest$yi,"\n Outcome:",forest$Outcome.variable), paste("Effect size (95% CI) =",forest$yi,"[",forest$lowerci, ",", forest$upperci,"]","\n Outcome:",forest$Outcome.variable)), Reference = paste(forest123$study, "(",forest123$id, ")"))
#mutate
july <- july %>% mutate(inter = ifelse(is.na(Lower), paste(july$Effect.measure.1), paste(july$Effect.measure.1,"[",july$Upper,",", july$Lower,"]")))

data_table5 <- ggplot(june5, aes(x = V05, y = V0, label = V1, width=0.1 )) +
  geom_text(size = 2, hjust=0.1, vjust=0) + theme_void() +
  geom_hline(aes(yintercept=c(11.5))) + 
  theme( 
        #legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(hjust = 0.5),#element_blank(),
        axis.text.y = element_blank(), 
        legend.justification = "center",
        legend.text.align = 0,
        axis.ticks = element_line(colour="white")#element_blank(),
        ) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(-0.2,4.5))

data_table5
plot.margin = unit(c(0,0,0,0,0), "lines")
grid.arrange(data_table10,p, ncol = 2, widths=c(2.5,0.9))
####
data_table10 <- ggplot(data = july, aes(y = id2)) +
  #geom_hline(aes(yintercept = Outcome.variable), size = 7) +
  geom_text(aes(x = -0.5,label = Outcome.variable),size= 2.8,hjust = 0) + 
  geom_text(aes(x = 1.5, label = Exposure.measure), size= 2.8)+
  geom_text(aes(x = 2.5, label = Subcategory),size= 2.8, hjust = 1) +
  geom_text(aes(x = 3.1, label = inter), size= 2.8,hjust = 1) +
  #geom_text(aes(x = 3, label = Subcategory), hjust = 1)
  scale_colour_identity() +
  theme_void() + 
  theme(plot.margin = margin(5, 0, 35, 0))
data_table10
##
july$Upper <- as.numeric(july$Upper)
july$Upper <- round(july$Upper ,digit=2)
july$Lower <- as.numeric(july$Lower)
july$Lower <- round(july$Lower ,digit=2)
july$Effect.measure.1 <- as.numeric(july$Effect.measure.1 )
july$Effect.measure.1  <- round(july$Effect.measure.1  ,digit=2)
######
forest_12 <- read_excel("datasets/distiller_7.xlsx")
forest_1_2 <- read_excel("datasets/distiller11.xlsx")
forest_1_2_1 <- read_excel("datasets/distiller_cohort.xlsx")
forest_1_2_2 <- read_excel("datasets/distiller_casecontrol.xlsx")
forest_NEW <- bind_rows(up_forest_V1,up_forest_V2)
forest_NEW2 <- bind_rows(forest_1_2,forest_12)
##########
forest_1_2$Differential_information_bias2 <- paste(forest_1_2$Differential_information_bias2, forest_1_2$Differential_information_bias2_V2, sep = "")

####

rty <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6"  ) %in% names(forest_12))
if (sum(rty)==6 ) {
  yis0 <- forest_12[c(9,13,17,21,25,29)]
  yis <- forest_12[c(10,14,18,22,26,30)]
  yis1 <- forest_12[c(11,15,19,23,27,31)]
  yis2 <- forest_12[c(12,16,20,24,28,32)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_12$outcome, each = 6)
  mm <- rep(forest_12$effect_measure, each = 6)
  Exposure.measure <- rep(forest_12$exposure, each = 6)
  Categorized.class <- rep(forest_12$category, each = 6)
  
  up_forest_1 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V1 <- up_forest_1[complete.cases(up_forest_1),]
}else if (sum(rty)==5){
  yis0 <- forest_12[c(9,13,17,21,25)]
  yis <- forest_12[c(10,14,18,22,26)]
  yis1 <- forest_12[c(11,15,19,23,27)]
  yis2 <- forest_12[c(12,16,20,24,28)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_12$outcome, each = 5)
  mm <- rep(forest_12$effect_measure, each = 5)
  Exposure.measure <- rep(forest_12$exposure, each = 5)
  Categorized.class <- rep(forest_12$category, each = 5)
  
  up_forest_1 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V1 <- up_forest_1[complete.cases(up_forest_1),]
}else if (sum(rty)==4){
  yis0 <- forest_12[c(9,13,17,21)]
  yis <- forest_12[c(10,14,18,22)]
  yis1 <- forest_12[c(11,15,19,23)]
  yis2 <- forest_12[c(12,16,20,24)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_12$outcome, each = 4)
  mm <- rep(forest_12$effect_measure, each = 4)
  Exposure.measure <- rep(forest_12$exposure, each = 4)
  Categorized.class <- rep(forest_12$category, each = 4)
  
  up_forest_1 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V1 <- up_forest_1[complete.cases(up_forest_1),]
}else if (sum(rty)==3){
  yis0 <- forest_12[c(9,13,17)]
  yis <- forest_12[c(10,14,18)]
  yis1 <- forest_12[c(11,15,19)]
  yis2 <- forest_12[c(12,16,20)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_12$outcome, each = 3)
  mm <- rep(forest_12$effect_measure, each = 3)
  Exposure.measure <- rep(forest_12$exposure, each = 3)
  Categorized.class <- rep(forest_12$category, each = 3)
  
  up_forest_1 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V1 <- up_forest_1[complete.cases(up_forest_1),]
}


rty1 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6"  ) %in% names(forest_12))
if (sum(rty1)==6 ) {
  yis0 <- forest_1_2[c(9,13,17,21,25,29)]
  yis <- forest_1_2[c(10,14,18,22,26,30)]
  yis1 <- forest_1_2[c(11,15,19,23,27,31)]
  yis2 <- forest_1_2[c(12,16,20,24,28,32)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_1_2$outcome, each = 6)
  mm <- rep(forest_1_2$effect_measure, each = 6)
  Exposure.measure <- rep(forest_1_2$exposure, each = 6)
  Categorized.class <- rep(forest_1_2$category, each = 6)
  
  up_forest_2 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V2 <- up_forest_2[complete.cases(up_forest_2),]
}else if (sum(rty1)==5){
  yis0 <- forest_1_2[c(9,13,17,21,25)]
  yis <- forest_1_2[c(10,14,18,22,26)]
  yis1 <- forest_1_2[c(11,15,19,23,27)]
  yis2 <- forest_1_2[c(12,16,20,24,28)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_1_2$outcome, each = 5)
  mm <- rep(forest_1_2$effect_measure, each = 5)
  Exposure.measure <- rep(forest_1_2$exposure, each = 5)
  Categorized.class <- rep(forest_1_2$category, each = 5)
  
  up_forest_2 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V2 <- up_forest_2[complete.cases(up_forest_2),]
}else if (sum(rty1)==4){
  yis0 <- forest_1_2[c(9,13,17,21)]
  yis <- forest_1_2[c(10,14,18,22)]
  yis1 <- forest_1_2[c(11,15,19,23)]
  yis2 <- forest_1_2[c(12,16,20,24)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_1_2$outcome, each = 4)
  mm <- rep(forest_1_2$effect_measure, each = 4)
  Exposure.measure <- rep(forest_1_2$exposure, each = 4)
  Categorized.class <- rep(forest_1_2$category, each = 4)
  
  up_forest_2 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V2 <- up_forest_2[complete.cases(up_forest_2),]
}else if (sum(rty1)==3){
  yis0 <- forest_1_2[c(9,13,17)]
  yis <- forest_1_2[c(10,14,18)]
  yis1 <- forest_1_2[c(11,15,19)]
  yis2 <- forest_1_2[c(12,16,20)]
  tinto0 <- data.frame(Subcategory=c(t((yis0))))
  tinto <- data.frame(yi=c(t((yis))))
  tinto1 <- data.frame(upperci=c(t((yis1))))
  tinto2 <- data.frame(lowerci=c(t((yis2))))
  
  Outcome.variable <- rep(forest_1_2$outcome, each = 3)
  mm <- rep(forest_1_2$effect_measure, each = 3)
  Exposure.measure <- rep(forest_1_2$exposure, each = 3)
  Categorized.class <- rep(forest_1_2$category, each = 3)
  
  up_forest_2 <- data.frame(
    Outcome.variable, Categorized.class,Exposure.measure,mm,tinto0, tinto, tinto1, tinto2)
  up_forest_V2 <- up_forest_1[complete.cases(up_forest_2),]
}

###############

forest_sabado <- read_excel("datasets/distiller_7.xlsx")
forest_sabado <-  forest_sabado%>% mutate(ID = c(1:nrow(forest_sabado)))
yis01 <- forest_sabado[c(9,13,17,21,25,29, ncol(forest_sabado))]
tinto01 <- data.frame(Subcategory=c(t((yis01))))
gew1 <- forest_sabado[forest_sabado$ID %in% 5, ]
forest_sabado[c('subcategory3', 'subcategory2')]
select('subcategory3',2)


sum((c("Differential_information_bias_1_V2") %in% names(forest_1_2)))
tuareg <- is.na(forest_1_2$Differential_information_bias_1_V2)
forest_1_2[5,'Differential_information_bias'] <- forest_1_2[5,52]


if ((c("Differential_information_bias_1_V2") %in% names(forest_1_2))==1 ){
  forest_1_2[5,'Differential_information_bias'] <- forest_1_2[5,52]
  
}
gew14<- forest_1_2[c('Differential_information_bias')]




control <- (c("Differential_information_bias_1_V2") %in% names(forest_1_2))
if(sum(control)==1){
  #gew1 <- forest_sabado[forest_sabado$IDD %in% selected_state_up(), ]
  gew1<- gew1[c('Differential_information_bias_1_V2', 'Differential_information_bias2_V2','selection_bias_1_V2', 'selection_bias_2_V2','confounding_V2', 'overall_bias_V2' )]
  t(gew1[1,])
}else {
  gew1 <- forest_1_2[5, ]
  
  gew1 <- gew1 %>% select(41, 43,45, 47,49, 51)
  gew1<- forest_1_2[c('Differential_information_bias', 'Differential_information_bias2','selection_bias_1', 'selection_bias_2','confounding', 'overall_bias' )]
  t(gew1[1,])
  
}

ghj <- which( colnames(forest_1_2)=="Differential_information_bias2_V2" )

#orest %>%
  #filter(mm == selected_state()) %>%
  #pull(mm)
 #### this is to create a ROB for cross sectional studies 

ann <-  forest_1_2 %>% filter(health_event== "Yes") %>% select(description_1_V2, description_2_V2, description_3_V2, description_4_V2, description_5_V2,overall_bias_V2,IDD,IDD_2)
ann1 <-  forest_1_2 %>% filter(rare_outcome== "Yes") %>% select(description_1, description_2,description_3, description_4, description_5, overall_bias, IDD, IDD_2)
colnames(ann) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")
colnames(ann1) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")
ann3 <- bind_rows(ann,ann1)

#select descriptions columns
ann4 <-  forest_1_2_1 %>% select(description_1, description_2,description_3, description_4, description_5,description_6, description_7,overall_bias,IDD, IDD_2)
ann6 <-  forest_1_2_2 %>% select(description_1, description_2,description_3, description_4, description_5, overall_bias, IDD, IDD_2)
colnames(ann6) <- c("Differential information", "Differential information 2", "Selection bias","Selection_bias 2","Confounding", "Overall bias", "IDD", "IDD_2")
colnames(ann4) <- c("Selection bias", "Differential information", "Differential information 2","Confounding","Differential information 3", "Differential information 4", "Selection bias 2", "Overall bias","IDD", "IDD_2")
ann5 <-  bind_rows(ann3,ann4, ann6)

#crete IDD
forest_1_2 <-  forest_1_2%>% mutate(IDD = c(1:nrow(forest_1_2)))
forest_1_2_1 <-  forest_1_2_1%>% mutate(IDD = c(1:nrow(forest_1_2_1)))
forest_1_2_2 <-  forest_1_2_2%>% mutate(IDD = c(1:nrow(forest_1_2_2)))

#crete IDD_2
forest_1_2 <- forest_1_2 %>% mutate( IDD_2 = paste(forest_1_2$IDD, "Cross-sectional" ))
forest_1_2_1 <- forest_1_2_1 %>% mutate( IDD_2 = paste(forest_1_2_1$IDD, "Cohort" ))
forest_1_2_2 <- forest_1_2_2 %>% mutate( IDD_2 = paste(forest_1_2_2$IDD, "C-C" ))


colnames(ann) <- c("Differential", "Overall", "IDD")
colnames(ann1) <- c("Differential", "Overall", "IDD")



#############################################
forest_cross <- read_excel("datasets/distiller_cross.xlsx")

rty <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cross))
forest_cross <-  forest_cross%>% mutate(IDD = c(1:nrow(forest_cross)))
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

#################################

forest_cohort <- read_excel("datasets/distiller_cohort.xlsx")
forest_case <- read_excel("datasets/distiller_casecontrol.xlsx")

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
  yis0 <- forest_cohort[c(9,13,17,21,25,29)]
  yis <- forest_cohort[c(10,14,18,22,26,30)]
  yis1 <- forest_cohort[c(11,15,19,23,27,31)]
  yis2 <- forest_cohort[c(12,16,20,24,28,32)]
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
  yis0 <- forest_cohort[c(9,13,17,21,25)]
  yis <- forest_cohort[c(10,14,18,22,26)]
  yis1 <- forest_cohort[c(11,15,19,23,27)]
  yis2 <- forest_cohort[c(12,16,20,24,28)]
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
  yis0 <- forest_cohort[c(9,13,17,21)]
  yis <- forest_cohort[c(10,14,18,22)]
  yis1 <- forest_cohort[c(11,15,19,23)]
  yis2 <- forest_cohort[c(12,16,20,24)]
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
  yis0 <- forest_cohort[c(9,13,17)]
  yis <- forest_cohort[c(10,14,18)]
  yis1 <- forest_cohort[c(11,15,19)]
  yis2 <- forest_cohort[c(12,16,20)]
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

forest_joint <-  bind_rows(up_forest_cohort1,up_forest_case1, up_forest_melo1)
#####################################
#####################################
#####################################
forest_cross <- read_excel("datasets/distiller_cross.xlsx")
forest_cross_state <- forest_cross %>% filter(event_state == "State")

rty3 <-(c("subcategory1","subcategory2", "subcategory3","subcategory4","subcategory5", "subcategory6") %in% names(forest_cross_state))
forest_cross_state <-  forest_cross_state %>% mutate(IDD = c(1:nrow(forest_cross_state)))
forest_cross_state <- forest_cross_state %>% mutate( IDD_2 = paste(forest_cross_state$IDD, "Cross-sectional" ))



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

