library(readxl)
library(survival)
library(survminer)
data=read_excel("D:/SMT 6/Analisis Survival/Project/Data Project.xlsx")
head(data)
summary(data)

#Kurva Kaplan Meier
fit1<-survfit(Surv(Time, Status) ~ ecog, data = data)
print(fit1)
ggsurvplot(fit1, data = data, linetype = "strata")

fit2<-survfit(Surv(Time, Status) ~ Sex, data = data)
print(fit2)
ggsurvplot(fit2, data = data, linetype = "strata")

#Log Rank Test
lrank1<-survdiff(Surv(Time, Status) ~ ecog, data = data) 
lrank1

lrank2<-survdiff(Surv(Time, Status) ~ Sex, data = data) 
lrank2

#Regresi Cox PH
rcoxph1 <-coxph(Surv(Time,Status) ~ ecog + Sex + Age + karno , data=data)
rcoxph1
rcoxph2 <-coxph(Surv(Time,Status) ~ ecog + Sex, data=data)
rcoxph2
summary(rcoxph2)

#Uji Asumsi
#Martingle Residual
mresi=resid(rcoxph2,type="martingale")
resi1 <- ggplot(data = data, mapping = aes(x = Sex, y = mresi)) +
  geom_point() +
  geom_smooth() +
  labs(title = "VARIABEL SEX") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme_bw() + theme(legend.key = element_blank())
resi1

mres2=resid(rcoxph2,type="martingale")
resi2 <- ggplot(data = data, mapping = aes(x = ecog, y = mresi)) +
  geom_point() +
  geom_smooth() +
  labs(title = "VARIABEL ECOG") + 
  scale_x_continuous(breaks = scales::pretty_breaks(n = 20)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 20))+
  theme_bw() + theme(legend.key = element_blank())
resi2

#Schoenfeld Residual dan Grambsch-Therneau
test=cox.zph(rcoxph2)
test
plot(test)