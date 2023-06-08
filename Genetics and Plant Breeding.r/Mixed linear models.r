getwd()
setwd("C:/Users/Usuario/Dropbox/Processamento de papers/Paper DS 6/analises-e-figuras")
library(readxl);library(ggplot2);library(dplyr)

#plot 1 - gastropod and damage over time ----
dados=read_xlsx("benjamin.xlsx",1)
head(dados)
names(dados)
dados$CarMed=dados$Car/dados$Planta
dados$LesMed=dados$Les/dados$Planta
summary(dados)

library(tidyr)
names(dados)
names(dados)[6] <- "3FolInjPro"
names(dados)[8] <- "2CarMed"
names(dados)[9] <- "1LesMed"

library(tidyr)
dados2=gather(dados, "variavel", "media", 6,8,9)
dados2$Data2=as.Date(dados2$Data2, format='%b/%y')
dados2$variavel=as.factor(dados2$variavel);levels(dados2$variavel)
names(dados2)
dados3=dados2[,c(1,3,7,8)]
names(dados3)

library(ggplot2)
summary(dados3)
breaks_fun <- function(y) {
  if (max(y)>1.3) {
    seq(0,1.4,0.7)
  } else if (max(y)>0.6) {
    seq(0,0.8,0.4)
  } else {
    seq(0,0.5,0.2)
  }
}
limits_fun <- function(y) {
  if (max(y) >1.3) {
    c(0,1.4)
  } else if (max(y)>0.6) {
    c(0,0.8)
  }else {
    c(0,0.5)
  }
}

colors()
a=ggplot(dados3, aes(x=Data2, y=media))+
  scale_y_continuous(breaks = breaks_fun, limits = limits_fun)+
  scale_linetype_manual(values=c("solid","dashed","f1"), name="",
                        labels = c("Broccoli","Cabbage","Cauliflower"),
                        guide = guide_legend(nrow=1))+
  scale_color_manual(values=c("#999999","#009E73","#D55E00"), name="",
                     labels = c("Broccoli","Cabbage","Cauliflower"),
                     guide = guide_legend(nrow=1))+
  scale_x_date(expand = c(0, 20), date_labels = "%b/%y",
               date_breaks = "2 month",
               limits = as.Date(c("2017-03-15", "2019-04-01")))
li=geom_line(aes(linetype=hospedeiro, color=hospedeiro), size=0.5)
l=labs(y="Mean per plant", x="Month/Year")
fn=c("Slugs", "Snails",
            "Proportion of damaged leaves")#new facet label names
names(fn)=c("1LesMed", "2CarMed", "3FolInjPro")
fw=facet_wrap(~variavel,nrow=3,labeller = labeller(variavel=fn),
              scale="free_y")
t=theme_minimal(base_family="sans",
                base_size=11)
z=theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.minor.y = element_blank(), 
        axis.line.x = element_line(size=0, color = "black"), 
        axis.line.y = element_line(size=0, color = "black"),
        axis.ticks.y = element_line(size=0.2, color = "black"),
        axis.ticks.x = element_line(size=0.2, color = "black"), 
        axis.title.x = element_text(color="black", hjust=0.5, vjust=0),
        #hjust = alinhamento horizontal (left = 0, center = 0.5, right = 1)
        #vjust = alinhamento vertical (top = 1, middle = 0.5, bottom = 0)
        #axis.title.y = element_blank(),
        axis.text.x = element_text(color="black", vjust=0.5),
        axis.text.y = element_text(color="black", vjust=0.5, hjust=1),
        legend.title = element_text(size=9),
        legend.text = element_text(colour="black", size = 9),
        #legend.margin = margin(t=-0.4, r=0, b=0, l=0, "cm"),
        #legend.text.align = 
        #legend.title.align = 
        #legend.key.size=unit(0.6,"cm"),
        #legend.justification = "center",
        #legend.spacing = unit(0.8, "cm"), 
        legend.spacing.x=unit(0.4, "cm"),#Horizontal spacing
        #legend.spacing.y=unit(0.6, "cm")),#Vertical spacing
        legend.position = c(0.5, 0.6),
        #legend.direction = "horizontal",
        #legend.box = "horizontal",
        #legend.box.background=element_rect(size=0.2, linetype="solid", fill="black"),
        plot.margin = unit(c(0.5, 0.2, 0.2, 0.2), "cm"),
        panel.spacing.x = unit(1.5, "lines"),
        strip.text.x = element_text(size = 11, face = "plain"),
        strip.background = element_rect(size=0.2, linetype="solid", color="black", fill="#CCCCCC"),
        panel.background = element_rect(size=0.2, linetype="solid", color="black"))
(f2=a+li+l+fw+t+z)

ggsave("Fig2.tiff", plot = f2, device = "tiff", scale = 1, width = 15,
       height = 10, units = c("cm"), dpi = 300)

#plot 4 - glmm, glm and regression plots ----
dados4=read_xlsx("benjamin.xlsx",3)
names(dados4)
dados4$CarMed=dados4$Car/dados4$Planta
dados4$LesMed=dados4$Les/dados4$Planta
dados4$Gas=dados4$Les+dados4$Car
dados4$GasMed=(dados4$Les+dados4$Car)/dados4$Planta
names(dados4)

#finding family distribution of data
library(fitdistrplus);par(mar=c(5,4,4,4),mfrow=c(1,2))
fitp<-fitdist(dados4$Gas, "pois")
plot(fitp, breaks="default")
fitnb<-fitdist(dados4$Gas, "nbinom")
plot(fitnb, breaks="default")
par(mfrow=c(1,1), mar=c(5,5,2,1))
cdfcomp(list(fitp, fitnb));gofstat(list(fitp, fitnb))
shapiro.test(dados4$Gas)

summary(dados4)
library(lme4);library(lmerTest)
dados4$hospedeiro=as.factor(dados4$hospedeiro)

#mixed model (glmm)
m3_1=glmer.nb(GasMed~hospedeiro+tMed15+pre15+urMed15+DAT+(1|Lavoura),
              weights=Planta,data=dados4)#Planta is number of plants per date
isSingular(m3_1)#test if random variable is significant

#glm model (without random variable)
library(MASS);m3_1=glm.nb(GasMed~hospedeiro+tMed15+pre15+urMed15+DAT,
                          weights=Planta,data=dados4)
library(car);Anova(m3_1)
summary(m3_1)

#checking final model
par(mar=c(5,4,4,4),mfrow=c(1,1))
plot(fitted(m3_1), residuals(m3_1),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, lty = 2)
library(rcompanion);plotNormalHistogram(residuals(m3_1))

#plotting model estimates
#DAT
library(ggeffects);m3_1;(DAT=ggpredict(m3_1, terms = c("DAT", "hospedeiro")))
(DAT2=as.data.frame(DAT))
library(ggplot2);library(ggrepel);library(dplyr)
z5=theme(panel.grid.major.x = element_blank(),
         panel.grid.minor.x = element_blank(), 
         panel.grid.major.y = element_blank(), 
         panel.grid.minor.y = element_blank(), 
         axis.line.x = element_line(size=0.2, color = "black"), 
         axis.line.y = element_line(size=0.2, color = "black"),
         axis.ticks.y = element_line(size=0.2, color = "black"),
         axis.ticks.x = element_line(size=0.2, color = "black"),
         #hjust = alinhamento horizontal (left = 0, center = 0.5, right = 1)
         #vjust = alinhamento vertical (top = 1, middle = 0.5, bottom = 0)
         plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
         #legend.title = element_blank(),
         legend.position = c(.2,.6),
         legend.margin = margin(t=0, r=0, b=0, l=0, "cm"),
         axis.title.x = element_text(color="black", hjust=0.5, vjust=-0.2),
         axis.title.y = element_text(color="black"),
         axis.text.x = element_text(color="black"),
         axis.text.y = element_text(color="black"))
scale=list(
  scale_fill_manual(name=NULL, 
                          breaks = c("broccoli","cabbage","cauliflower"),
                          values = c("#999999","#009E73","#D55E00"),
                          labels = c("Broccoli","Cabbage","Cauliflower")),
  scale_alpha_manual(name=NULL, 
                     breaks = c("broccoli","cabbage","cauliflower"),
                     values = rep(0.6,3),
                     labels = c("Broccoli","Cabbage","Cauliflower")),
  scale_linetype_manual(name=NULL, 
                        breaks = c("broccoli","cabbage","cauliflower"),
                        values = c(3,1,2),
                        labels = c("Broccoli","Cabbage","Cauliflower"))
  )
summary(DAT2)
(f4a=ggplot()+t+z5+
  scale_x_continuous(limits=c(0,90), breaks=seq(0,88,22), expand=c(0,0))+
  scale_y_continuous(limits=c(0,2.4), expand=c(0,0),breaks=seq(0,2.1,0.7))+
  labs(y="Individuals per plant", x="Days after transplanting")+
  annotate("text", x=0, y=2.4, label="a)",
             fontface="bold", size=11/.pt, vjust=1, hjust=-1)+
  geom_ribbon(data=DAT2 %>% filter(group == "broccoli"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="broccoli", alpha="broccoli"),show.legend = T)+
  geom_ribbon(data=DAT2 %>% filter(group == "cabbage"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cabbage", alpha="cabbage"),show.legend = T)+
  geom_ribbon(data=DAT2 %>% filter(group == "cauliflower"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cauliflower", alpha="cauliflower"),show.legend = T)+
  geom_line(data=DAT2 %>% filter(group == "broccoli"),
            aes(x=x, y=predicted,linetype="broccoli"),
            size=0.5,show.legend = T)+
  geom_line(data=DAT2 %>% filter(group == "cabbage"),
            aes(x=x, y=predicted,linetype="cabbage"),
            size=0.5,show.legend = T)+
  geom_line(data=DAT2 %>% filter(group == "cauliflower"),
            aes(x=x, y=predicted,linetype="cauliflower"),
            size=0.5,show.legend = T)+
  scale
  )


#temperatura
(tMed=ggpredict(m3_1, terms = c("tMed15", "hospedeiro")))
(tMed2=as.data.frame(tMed))
summary(tMed2)
(f4b=ggplot()+t+z5+
    scale_x_continuous(limits=c(17,26),expand=c(0,0),breaks=seq(18,26,2))+
    scale_y_continuous(limits=c(0,0.22), expand=c(0,0),breaks=seq(0,0.2,0.1))+
    labs(y="Individuals per plant", x="Temperature (?C)")+
    annotate("text", x=17, y=0.22, label="b)",
             fontface="bold", size=11/.pt, vjust=1, hjust=-1)+
    geom_ribbon(data=tMed2 %>% filter(group == "broccoli"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="broccoli", alpha="broccoli"),show.legend = F)+
    geom_ribbon(data=tMed2 %>% filter(group == "cabbage"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cabbage", alpha="cabbage"),show.legend = F)+
    geom_ribbon(data=tMed2 %>% filter(group == "cauliflower"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cauliflower", alpha="cauliflower"),show.legend = F)+
    geom_line(data=tMed2 %>% filter(group == "broccoli"),
              aes(x=x, y=predicted,linetype="broccoli"),
              size=0.5,show.legend = F)+
    geom_line(data=tMed2 %>% filter(group == "cabbage"),
              aes(x=x, y=predicted,linetype="cabbage"),
              size=0.5,show.legend = F)+
    geom_line(data=tMed2 %>% filter(group == "cauliflower"),
              aes(x=x, y=predicted,linetype="cauliflower"),
              size=0.5,show.legend = F)+
    scale)

#chuva
(pre=ggpredict(m3_1, terms = c("pre15", "hospedeiro")))
(pre2=as.data.frame(pre))
summary(pre2)
(f4c=ggplot()+t+z5+
    scale_x_continuous(limits=c(0,36),expand=c(0,0),breaks=seq(0,36,9))+
    scale_y_continuous(limits=c(0,0.45), expand=c(0,0),breaks=seq(0,0.4,0.1))+
    labs(y="Individuals per plant", x="Rainfall (mm/day)")+
    annotate("text", x=0, y=0.45, label="c)",
             fontface="bold", size=11/.pt, vjust=1, hjust=-1)+
    geom_ribbon(data=pre2 %>% filter(group == "broccoli"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="broccoli", alpha="broccoli"),show.legend = F)+
    geom_ribbon(data=pre2 %>% filter(group == "cabbage"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cabbage", alpha="cabbage"),show.legend = F)+
    geom_ribbon(data=pre2 %>% filter(group == "cauliflower"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cauliflower", alpha="cauliflower"),show.legend = F)+
    geom_line(data=pre2 %>% filter(group == "broccoli"),
              aes(x=x, y=predicted,linetype="broccoli"),
              size=0.5,show.legend = F)+
    geom_line(data=pre2 %>% filter(group == "cabbage"),
              aes(x=x, y=predicted,linetype="cabbage"),
              size=0.5,show.legend = F)+
    geom_line(data=pre2 %>% filter(group == "cauliflower"),
              aes(x=x, y=predicted,linetype="cauliflower"),
              size=0.5,show.legend = F)+
    scale)

#umidade relativa
(urMed=ggpredict(m3_1, terms = c("urMed15", "hospedeiro")))
(urMed2=as.data.frame(urMed))
summary(urMed2)
(f4d=ggplot()+t+z5+
    scale_x_continuous(limits=c(58,90),expand=c(0,0),breaks=seq(58,90,8))+
    scale_y_continuous(limits=c(0,0.58), expand=c(0,0), breaks=seq(0,0.55,0.1))+
    labs(y="Individuals per plant", x="Relative humidity (%)")+
    annotate("text", x=58, y=0.58, label="d)",
             fontface="bold", size=11/.pt, vjust=1, hjust=-1)+
    geom_ribbon(data=urMed2 %>% filter(group == "broccoli"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="broccoli", alpha="broccoli"),show.legend = F)+
    geom_ribbon(data=urMed2 %>% filter(group == "cabbage"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cabbage", alpha="cabbage"),show.legend = F)+
    geom_ribbon(data=urMed2 %>% filter(group == "cauliflower"),
                aes(ymin=conf.low,ymax=conf.high, x=x,
                    fill="cauliflower", alpha="cauliflower"),show.legend = F)+
    geom_line(data=urMed2 %>% filter(group == "broccoli"),
              aes(x=x, y=predicted,linetype="broccoli"),
              size=0.5,show.legend = F)+
    geom_line(data=urMed2 %>% filter(group == "cabbage"),
              aes(x=x, y=predicted,linetype="cabbage"),
              size=0.5,show.legend = F)+
    geom_line(data=urMed2 %>% filter(group == "cauliflower"),
              aes(x=x, y=predicted,linetype="cauliflower"),
              size=0.5,show.legend = F)+
    scale)

#exporting plot
library(gridExtra);(f4_2=grid.arrange(f4a,f4b,f4c,f4d,nrow=2))
ggsave("Fig3.tiff", plot = f4_2, device = "tiff", scale = 1, width = 17.5,
       height = 12, units = c("cm"), dpi = 300)