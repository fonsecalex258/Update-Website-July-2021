fw <- read_excel("Data Sheet November 1.xlsx", 
                 sheet = "Data Sheet November 1")
fw1 <- data.frame(fw)

library(readxl)
graph_cs_cs <- read_excel("~/graph_cs_cs.xlsx", sheet = "3")
View(graph_cs_cs) 

graph_cs_cs %>% graph_cs_cs <- mutate(Bias = forcats::fct_relevel(Bias, "Definitely yes","Probably yes", "Probably no", "Definitely no"))

factor(Bias, levels=c("Definitely yes", "Probably yes", "Probably no", "Definitely no"))

graph_cs_cs$Bias <- factor(graph_cs_cs$Bias, levels = c("Definitely yes", "Probably yes", "Probably no", "Definitely no"))

g1 <- graph_cs_cs %>%    
  mutate(Bias = forcats::fct_relevel(Bias, "Definitely yes", "Probably yes", "Probably no", "Definitely no"))
         


graph_cs_cs%>%
ggplot(aes(x = factor(`TBias`, level = c('Question 1', 'Question 2', 'Question 3', 'Question 4')), fill = Bias)) + 
  geom_bar(position = "fill") + 
  scale_x_discrete(labels = label_wrap(50))+
  scale_y_continuous(labels = scales::percent)+
  coord_flip() + 
  scale_fill_manual(values=c("Definitely yes" =  "forestgreen",
                             "Probably yes" = "lightgreen",
                             "Probably no"="salmon",
                             "Definitely no" = "red"))+ labs(x = "", fill = "Question's answer")+ ylab("Percentage")

levels(as.factor(graph_cs_cs$Bias))
"forestgreen", "lightgreen","salmon","red"
  geom_text(
            stat = "count")

gg <- r22_1 %>% filter(category==selected_class()) %>%
  ggplot(aes(x = factor(`Type of Bias`, level = c('Question 10', 'Question 9', 'Question 8', 'Question 7', 'Question 6', 'Question 5', 'Question 4', 'Question 3', 'Question 2', 'Question 1')), fill = Bias)) + 
  
  geom_bar(position = "fill")+ 
  scale_x_discrete(labels = label_wrap(50))+
  scale_y_continuous(labels = scales::percent)+
  coord_flip() + 
  geom_text(aes(label = scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..])),
            position = position_fill(vjust = 0.5),
            stat = "count")+
  
  scale_fill_manual(values = color_table$Color)  +
  ylab("Percentage of Health Outcomes")+
  xlab(" ")+
  
  theme(
    axis.text=element_text(size=13, face= "bold"),
    plot.caption = element_text(hjust = 0, face= "italic", size=16),
    plot.title.position  =  "panel"
  )

ggplotly(gg, tooltip = c("fill")) %>% config(modeBarButtonsToRemove = c('zoom2d','pan2d', 'select2d', 'lasso2d', 'zoomIn', 'zoomOut', 'autoScale', 'resetScale', 'hoverClosestCartesian', 'hoverCompareCartesian'))

#ggplot(gg)`