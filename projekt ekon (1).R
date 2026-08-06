library(car)
library(MASS)
library(nortest)
install.packages('lmtest')
library(lmtest)
data<-read.csv2("C:/Users/szymo/Downloads/Data_sheet_climbing_performance.csv", header=T)
summary(data)
colnames(data)[-1]<-c(
  "wzrost_cm", 
  "masa_ciala_kg", 
  "plec", 
  "lata_wspinania", 
  "dyscyplina", 
  "lead_onsight", 
  "lead_flash", 
  "boulder_flash", 
  "max_onsight_flash", 
  "boulder_rp", 
  "lead_rp", 
  "max_rp", 
  "boulder_typowa", 
  "lead_typowa", 
  "max_typowa", 
  "poziom", 
  "fds_fdp_px", 
  "fds_fdp_cm", 
  "fds_fdp_cm_wzgl", 
  "fds_fdp_px_wzgl", 
  "mobilnosc_stopnie", 
  "sila_palcow_max_kg", 
  "maf_max_kg", 
  "rfd_max", 
  "rfd_wzgl", 
  "sila_palcow_wzgl", 
  "maf_wzgl", 
  "cf_kg", 
  "cf_wzgl", 
  "praca_w", 
  "praca_wzgl"
)

data$dyscyplina <- as.factor(data$dyscyplina)
data$plec <- as.factor(data$plec)
summary(data)
dane <- data[, colSums(is.na(data)) == 0]
summary(dane)
dane <- dane[, !names(dane) %in% c('rfd_max','cf_kg', 'maf_max_kg','rfd_wzgl','sila_palcow_wzgl','maf_wzgl','cf_wzgl','praca_wzgl','fds_fdp_px','fds_fdp_cm_wzgl', 'fds_fdp_px_wzgl','Participant.nr','max_onsight_flash', 'lead_rp', 'max_rp', 'boulder_typowa', 'lead_typowa', 'max_typowa', 'poziom')]
dane[] <- lapply(dane, function(x) {
  if(is.factor(x)) as.numeric(as.character(x)) else x
})
dane['bouldering']<-as.factor(dane['dyscyplina']==1)
dane['lina']<-as.factor(dane['dyscyplina']==2)
summary(dane)
model<-lm(boulder_rp~.-dyscyplina, data=dane)
vif(model)#wspólliniowość
tescik<-function(model){
  pv<-numeric(5)
  pv[1]<-ks.test(residuals(model), 'pnorm')$p.value#normalność
  pv[2]<-shapiro.test(residuals(model))$p.value
  pv[3]<-bptest(model)$p.value#homo
  pv[4]<-dwtest(model)$p.value#autokorelacja 
  pv[5]<-gqtest(model)$p.value
  cat(pv)
}
#założenia okej
summary(model)#sprawdzamy ze najwyżesz pv ma plec
model1<-lm(boulder_rp~.-dyscyplina-plec, data=dane)#reduklujemy model o plec
anova(model, model1)#anova potwieredza ze plec nie wplywa na model wiec zostajemy przy modelu 1 sprawdzmy jego założenia 
tescik(model1)
#założenia okej
summary(model1)




model2<-lm(boulder_rp~.-dyscyplina-plec-mobilnosc_stopnie, data=dane)#sprawdzamy czy anova pozwala na wywalenie mobliności
anova(model1, model2)#pozwala wiec bierzemy model 2 sprawdzamy założenia
tescik(model2)# założenia dalej dobre 
summary(model2) # praca w kolejna w kolejności

resettest(model6)


model3<-lm(boulder_rp~.-dyscyplina-plec-mobilnosc_stopnie-praca_w, data=dane)
anova(model2, model3)# pozwala na wywalenie pracy w
tescik(model3)#okej
summary(model3)




model4<-lm(boulder_rp~.-dyscyplina-plec-mobilnosc_stopnie-praca_w-bouldering, data=dane)
anova(model3, model4)# pozwala na wywalenie buldering
tescik(model4)#ok
summary(model4)

AIC(model, model1, model2, model3, model4)
BIC(model, model1, model2, model3, model4)#kontorlen dodatkowe kryteria porównawcze: aic, bic czy idziemy w dobrym kierunku?, tak wyniki najniższe nowszych modeli


model5<-lm(boulder_rp~.-dyscyplina-plec-mobilnosc_stopnie-praca_w-bouldering-fds_fdp_cm, data=dane)
anova(model4, model5) # pozwala na wywalenie fds_fdp_cm
tescik(model5)#klasa
summary(model5)




model6<-lm(boulder_rp~.-dyscyplina-plec-mobilnosc_stopnie-praca_w-bouldering-fds_fdp_cm-masa_ciala_kg, data=dane)
anova(model5, model6)#pozwala na masa ciala kg
tescik(model6)#okej
summary(model6)
plot(model6)

AIC(model, model1, model2, model3, model4,model5, model6)#spada ok
BIC(model, model1, model2, model3, model4,model5, model6)
boxcox(model6)# jesteśmy w przedziale wiarygodnosci z nasza 1
resettest(model6)

  