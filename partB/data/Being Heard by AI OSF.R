
###########################################MAIN ANALYSIS###############################################

all = read.csv('Being Heard by AI OSF.csv')

etaSquared(aov(feelheard~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(accuracy~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(understoodme~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(connection~responseR*labelR,all), type = 2, anova = T)


all %>% group_by(labelR) %>% summarise(feelheard = mean(feelheard,na.rm=T),
                                                 accuracy = mean(accuracy,na.rm=T),
                                                 understoodme = mean(understoodme,na.rm=T),
                                                 connection = mean(connection,na.rm=T))
all %>% group_by(responseR) %>% summarise(feelheard = mean(feelheard,na.rm=T),
                                          accuracy = mean(accuracy,na.rm=T),
                                          understoodme = mean(understoodme,na.rm=T),
                                          connection = mean(connection,na.rm=T))

library(ggpubr)
library(gridExtra)
library(plotrix)
library(ggplot2)
library(lsr)

all$labelRR = ifelse(all$labelR=='ai label',"AI label","human label")
all$responseRR = ifelse(all$responseR=='ai response',"AI response","human response")

dodge = position_dodge(width=0.9)

apatheme=theme_bw()+theme(panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text(family='Helvetica',size=15),
                          axis.text.x = element_text(color="black"))

diff.heard.label =independentSamplesTTest(feelheard~labelR,all)
delta.heard.label=diff.heard.label$mean[1]-diff.heard.label$mean[2]
conf.heard.label=diff.heard.label$conf.int

diff.accuracy.label =independentSamplesTTest(accuracy~labelR,all)
delta.accuracy.label=diff.accuracy.label $mean[1]-diff.accuracy.label $mean[2]
conf.accuracy.label=diff.accuracy.label$conf.int

diff.understoodme.label =independentSamplesTTest(understoodme~labelR,all)
delta.understoodme.label=diff.understoodme.label$mean[1]-diff.understoodme.label$mean[2]
conf.understoodme.label=diff.understoodme.label$conf.int

diff.connection.label =independentSamplesTTest(connection~labelR,all)
delta.connection.label=diff.connection.label$mean[1]-diff.connection.label$mean[2]
conf.connection.label=diff.connection.label$conf.int

diff.heard.response =independentSamplesTTest(feelheard~responseR,all)
delta.heard.response=diff.heard.response$mean[1]-diff.heard.response$mean[2]
conf.heard.response=diff.heard.response$conf.int

diff.accuracy.response =independentSamplesTTest(accuracy~responseR,all)
delta.accuracy.response=diff.accuracy.response $mean[1]-diff.accuracy.response $mean[2]
conf.accuracy.response=diff.accuracy.response$conf.int

diff.understoodme.response =independentSamplesTTest(understoodme~responseR,all)
delta.understoodme.response=diff.understoodme.response$mean[1]-diff.understoodme.response$mean[2]
conf.understoodme.response=diff.understoodme.response$conf.int

diff.connection.response =independentSamplesTTest(connection~responseR,all)
delta.connection.response=diff.connection.response$mean[1]-diff.connection.response$mean[2]
conf.connection.response=diff.connection.response$conf.int

deltaplot = data.frame(effect=c('Label','Label','Label','Label','Response','Response','Response','Response'),
                       dv = c('Feeling\nheard',"Response\naccuracy","Responder\nunderstood me",
                              "Connection\nto responder",'Feeling\nheard',"Response\naccuracy","Responder\nunderstood me",
                              "Connection\nto responder"),
                       delta =c(delta.heard.label,delta.accuracy.label,delta.understoodme.label,delta.connection.label,
                                delta.heard.response,delta.accuracy.response,delta.understoodme.response,delta.connection.response),
                       lower = c(conf.heard.label[1],conf.accuracy.label[1],conf.understoodme.label[1],conf.connection.label[1],
                                 conf.heard.response[1],conf.accuracy.response[1],conf.understoodme.response[1],conf.connection.response[1]),
                       high=c(conf.heard.label[2],conf.accuracy.label[2],conf.understoodme.label[2],conf.connection.label[2],
                              conf.heard.response[2],conf.accuracy.response[2],conf.understoodme.response[2],conf.connection.response[2]))

deltaplot$dv= factor(deltaplot$dv, levels = c("Feeling\nheard", "Response\naccuracy", "Responder\nunderstood me","Connection\nto responder"))

apatheme=theme_bw()+theme(panel.grid.major=element_blank(),
                          panel.grid.minor=element_blank(),
                          panel.border=element_blank(),
                          axis.line=element_line(),
                          text=element_text(family='Helvetica',size=12),
                          axis.text.x = element_text(color="black"))

figure1=deltaplot  %>%
  ggplot(aes(x = dv, y = delta, fill = effect))+
  geom_bar(stat='identity', position=dodge)+
  geom_errorbar(aes(ymin= lower, ymax = high), 
                position = dodge,width = 0.1)+
  ylab('Delta (AI - Human)')+xlab('')+
  apatheme+
  scale_fill_manual(values = c("darkred", "lightgreen"),name="Manipulations")

figure1
########multi group contrast###############

temp1= subset(all,responseR=='ai response'&labelR=='ai label')
temp2 =  subset(all,responseR=='human response'&labelR=='human label')
temp3 =  subset(all,responseR=='human response'&labelR=='ai label')
temp4 =  subset(all,responseR=='ai response'&labelR=='human label')

temp1$cond = 'ai response ai label'
temp2$cond = 'human response human label'
temp3$cond = 'human response ai label'
temp4$cond = 'ai response human label'

all.long = rbind(temp1,temp2,temp3,temp4)
all.long$cond= factor(all.long$cond, levels = c("ai response human label", "ai response ai label", "human response human label","human response ai label"))


library(rempsyc)
table.stats1 <- nice_contrasts(
  response = "feelheard",
  group = "cond",
  data = all.long
)

(my_table1 <- nice_table(table.stats1))

table.stats2 <- nice_contrasts(
  response = "accuracy",
  group = "cond",
  data = all.long
)

(my_table2 <- nice_table(table.stats2))

table.stats3 <- nice_contrasts(
  response = "understoodme",
  group = "cond",
  data = all.long
)

(my_table3 <- nice_table(table.stats3))

table.stats4 <- nice_contrasts(
  response = "connection",
  group = "cond",
  data = all.long
)

(my_table4 <- nice_table(table.stats4))

print(my_table1,my_table2,my_table3,my_table4, preview ="docx")
flextable::save_as_docx(my_table1,my_table2,my_table3,my_table4, path = "contrasts.docx")


## four condition plots

all.long=all.long%>%
  mutate(cond = recode(cond, 
                       "ai response human label" = "AI Response\nHuman Label",
                       "ai response ai label" = "AI Response\nAI Label",
                       "human response human label" = "Human Response\nHuman Label",
                       "human response ai label" = "Human Response\nAI Label"))

apatheme=theme_bw()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        panel.border=element_blank(),
        axis.line=element_line(),
        text=element_text(family='Helvetica',size=10,colour='black'),
        axis.text.x = element_text(color="black"))


heard = all.long %>%
  group_by(cond)%>%
  summarize(ratings = mean(feelheard), se.ratings = std.error(feelheard))%>%
  ggplot(aes(x = cond, y = ratings,fill=cond))+
  geom_bar(stat='identity', position=dodge,color="black",linewidth=1)+
  geom_errorbar(aes(ymin= ratings - se.ratings, ymax = ratings + se.ratings), 
                position = dodge,width = 0.1)+coord_cartesian(ylim = c(1,7))+
  scale_y_continuous(breaks=seq(1,7,1))+
  ylab('Feeling Heard')+xlab('')+  
  scale_fill_manual(values = c("black", "darkgrey","grey","white"))+
  apatheme

accuracy = all.long %>%
  group_by(cond)%>%
  summarize(ratings = mean(accuracy), se.ratings = std.error(accuracy))%>%
  ggplot(aes(x = cond, y = ratings,fill=cond))+
  geom_bar(stat='identity', position=dodge,color="black",linewidth=1)+
  geom_errorbar(aes(ymin= ratings - se.ratings, ymax = ratings + se.ratings), 
                position = dodge,width = 0.1)+coord_cartesian(ylim = c(1,7))+
  scale_y_continuous(breaks=seq(1,7,1))+
  ylab('Response Accuracy')+xlab('')+  
  scale_fill_manual(values = c("black", "darkgrey","grey","white"))+
  apatheme

understood = all.long %>%
  group_by(cond)%>%
  summarize(ratings = mean(understoodme), se.ratings = std.error(understoodme))%>%
  ggplot(aes(x = cond, y = ratings,fill=cond))+
  geom_bar(stat='identity', position=dodge,color="black",linewidth=1)+
  geom_errorbar(aes(ymin= ratings - se.ratings, ymax = ratings + se.ratings), 
                position = dodge,width = 0.1)+coord_cartesian(ylim = c(1,7))+
  scale_y_continuous(breaks=seq(1,7,1))+
  ylab('Responder Understood Me')+xlab('')+  
  scale_fill_manual(values = c("black", "darkgrey","grey","white"))+
  apatheme

connection = all.long %>%
  group_by(cond)%>%
  summarize(ratings = mean(connection), se.ratings = std.error(connection))%>%
  ggplot(aes(x = cond, y = ratings,fill=cond))+
  geom_bar(stat='identity', position=dodge,color="black",linewidth=1)+
  geom_errorbar(aes(ymin= ratings - se.ratings, ymax = ratings + se.ratings), 
                position = dodge,width = 0.1)+coord_cartesian(ylim = c(1,7))+
  scale_y_continuous(breaks=seq(1,7,1))+
  ylab('Connnection to Responder')+xlab('')+  
  scale_fill_manual(values = c("black", "darkgrey","grey","white"))+
  apatheme

figure2=ggarrange(heard, accuracy,understood,connection, ncol=2, nrow=2, common.legend = TRUE, legend="none")
figure2


###################Moderators####################
anova(lm(feelheard~labelR*atti_bing,all))
anova(lm(accuracy~labelR*atti_bing,all))
anova(lm(understoodme~labelR*atti_bing,all))
anova(lm(connection~labelR*atti_bing,all))

anova(lm(feelheard~labelR*experience,all))
anova(lm(accuracy~labelR*experience,all))
anova(lm(understoodme~labelR*experience,all))
anova(lm(connection~labelR*experience,all))

anova(lm(feelheard~labelR*agency,all))
anova(lm(accuracy~labelR*agency,all))
anova(lm(understoodme~labelR*agency,all))
anova(lm(connection~labelR*agency,all))

library(interactions)
library(jtools)
library(ggplot2)
m1=lm(feelheard~labelRR*atti_bing,all)
f1=interact_plot(m1, pred = atti_bing, modx = labelRR,interval=T,y.label = "Feel Heard",x.label = 'Attitude towards Bing Chat',legend.main = c("Label")) + theme_apa()

m2=lm(accuracy~labelRR*atti_bing,all)
f2=interact_plot(m2, pred = atti_bing, modx = labelRR,interval=T,y.label = "Response Accuracy",x.label = 'Attitude towards Bing Chat',legend.main='Label') + theme_apa()

m3=lm(understoodme~labelRR*atti_bing,all)
f3=interact_plot(m3, pred = atti_bing, modx = labelRR,interval=T,y.label = "Responder Understood Me",x.label = 'Attitude towards Bing Chat',legend.main='Label') + theme_apa()

m4=lm(connection~labelRR*atti_bing,all)
f4=interact_plot(m4, pred = atti_bing, modx = labelRR,interval=T,y.label = "Connection to Responder",x.label = 'Attitude towards Bing Chat',legend.main='Label') + theme_apa()

m5=lm(connection~labelRR*agency,all)
f5=interact_plot(m5, pred = agency, modx = labelRR,interval=T,y.label = "Connection to Responder",x.label = 'Mind Perception of Bing Chat \n- Agency',legend.main='Label') + theme_apa()

m6=lm(connection~labelRR*experience,all)
f6=interact_plot(m6, pred = experience, modx = labelRR,interval=T,y.label = "Connection to Responder",x.label = 'Mind Perception of Bing Chat \n- Experience',legend.main='Label') + theme_apa()


library(grid)
library(gridExtra)

figure3=ggarrange(f1, f2,f3,f4,f5,f6,nrow=2,ncol=3,common.legend = TRUE,legend='bottom')
figure3
######################empathic accuracy########################

all$happiness_ai.d = abs(all$happiness_ai-all$happiness_d)
all$sadness_ai.d = abs(all$sadness_ai-all$sadness_d)
all$fear_ai.d = abs(all$fear_ai-all$fear_d)
all$anger_ai.d = abs(all$anger_ai-all$anger_d)
all$surprise_ai.d = abs(all$surprise_ai-all$surprise_d)
all$disgust_ai.d = abs(all$disgust_ai-all$disgust_d)


all$happiness_r.d = abs(all$happiness_r-all$happiness_d)
all$sadness_r.d = abs(all$sadness_r-all$sadness_d)
all$fear_r.d = abs(all$fear_r-all$fear_d)
all$anger_r.d = abs(all$anger_r-all$anger_d)
all$surprise_r.d = abs(all$surprise_r-all$surprise_d)
all$disgust_r.d = abs(all$disgust_r-all$disgust_d)

pairedSamplesTTest( formula= ~happiness_ai.d + happiness_r.d, data=all )
pairedSamplesTTest( formula= ~sadness_ai.d + sadness_r.d, data=all )
pairedSamplesTTest( formula= ~fear_ai.d + fear_r.d, data=all )
pairedSamplesTTest( formula= ~disgust_ai.d + disgust_r.d, data=all )
pairedSamplesTTest( formula= ~surprise_ai.d + surprise_r.d, data=all )
pairedSamplesTTest( formula= ~anger_ai.d + anger_r.d, data=all )

######################follow up study####################################
library(dplyr)
library(lsr)
fu = read.csv('Followup Study OSF.csv')
fu$id = fu$OriginalDiscloserID
all = left_join(all,fu,by='id')

independentSamplesTTest(m_emotional ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_practical ~aiorhuman.response,fu,var.equal = T)

independentSamplesTTest(m_specifics_1 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_2 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_3 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_4 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_5 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_6 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_7 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_8 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_9 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_10 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_11 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_12 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_13 ~aiorhuman.response,fu,var.equal = T)
independentSamplesTTest(m_specifics_14 ~aiorhuman.response,fu,var.equal = T)

independentSamplesTTest(m_motivation ~aiorhuman.response,fu,var.equal = T)
fu$heard = with(fu,apply(data.frame(m_understood,m_validated,m_affirmed,m_seen,m_accepted,m_caredfor),1,mean,na.rm=T))
independentSamplesTTest(heard ~aiorhuman.response,fu,var.equal = T)

######correlation table###########
library(apaTables)
apa.cor.table(all[,c(107,141:156)], filename = "table.all.doc", table.number = 1)

apa.cor.table(all[all$labelR=='ai label',c(107,141:156)], filename = "table.ai label.doc", table.number = 2)
apa.cor.table(all[all$labelR=='human label',c(107,141:156)], filename = "table.human label.doc", table.number = 3)

#no sig moderation by label condition
anova(lm(feelheard~m_emotional*labelR,all))
anova(lm(feelheard~m_practical*labelR,all))
anova(lm(feelheard~m_specifics_1*labelR,all))
anova(lm(feelheard~m_specifics_2*labelR,all))
anova(lm(feelheard~m_specifics_3*labelR,all))
anova(lm(feelheard~m_specifics_4*labelR,all))
anova(lm(feelheard~m_specifics_5*labelR,all))
anova(lm(feelheard~m_specifics_6*labelR,all))
anova(lm(feelheard~m_specifics_7*labelR,all))
anova(lm(feelheard~m_specifics_8*labelR,all))
anova(lm(feelheard~m_specifics_9*labelR,all))
anova(lm(feelheard~m_specifics_10*labelR,all))
anova(lm(feelheard~m_specifics_11*labelR,all))
anova(lm(feelheard~m_specifics_12*labelR,all))
anova(lm(feelheard~m_specifics_13*labelR,all))
anova(lm(feelheard~m_specifics_14*labelR,all))


############### EMOTIONS#####################
etaSquared(aov(hope~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(distress~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(uncomfortable~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(creeped~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(ambivalent~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(happy~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(shame~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(excitement~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(fear~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(surprised~responseR*labelR,all), type = 2, anova = T)
etaSquared(aov(statelonely~responseR*labelR,all), type = 2, anova = T)