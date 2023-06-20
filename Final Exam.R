library(readr)
library(readxl)
library(dplyr)
library(nortest)
library(ggplot2)
library(gplots)
library(AICcmodavg)
library(nlme)
library(geepack)
library(broom)

#reading data
stroke <- read_excel("stroke.xls")
View(stroke)
stroke$Bart1
#1. Menganalisis perbedaan dari Functional Ability pada pekan pertama (Bart1) berdasarkan grup intervensi (Group) dengan visualisasi boxplot. 
    # Membuat grafik boxplot dan menginterpretasi dan menyimpulkan hasilnya.
boxplot(stroke$Bart1~stroke$Group,xlab= "Group", ylab = "Functional Ability (Week1)",col = c("pink","aquamarine", "blue"), 
        main="Perbedaan Fungsional Ability Score (Week 1) Berdasarkan Group Intervensi")
summary(stroke$Bart1)
#2. Mencek normalitas data dari Functional Ability pekan pertama (Bart1) dengan uji statistik yang sesuai dan menginterpretasikannya.
    #For Small Sample (Saphiro Wilk) 
shapiro.test(stroke$Bart1)

#3. Menghitung perubahan Functional Ability dari pekan pertama (Bart1)sampai pekan terakhir (Bart8) dan membuatnya menjadi variable baru (Bart_diff).

Bart8-Bart1

stroke <- stroke %>%
  mutate(Bart_diff = Bart8-Bart1) %>%
  as.data.frame()

#4. Mencek normalitas data dari perubahan Functional Ability (Bart_diff) dari pekan pertama (Bart1)sampai pekan terakhir (Bart8) dengan uji statistik yang sesuai dan menginterpretasikannya.
#For Small Sample (Saphiro Wilk)
shapiro.test(stroke$Bart_diff) 

#5. Mencek kesamaan variance dari perubahan Functional Ability (Bart_diff) antara grup intervensi (Group) dengan uji statistic yang sesuai dan menginterpretasikannya.
bartlett.test(stroke$Bart_diff, stroke$Group)

#6. Memplot mean dan 95% Confidence Interval dari nilai perubahan Functional Ability (Bart_diff) berdasarkan grup intervensi (Group) dalam 1 grafik.
plotmeans(Bart_diff ~ Group, data = stroke, frame = FALSE,
          mean.labels = TRUE, connect = FALSE)

#7. Melakukan uji anova untuk membandingkan rata-rata(mean) nilai perubahan Functional Ability (Bart_diff) antara 3 grup intervensi (Group) dan menginterpretasikannya
summary(aov(Bart_diff~Group, data = stroke))

#8. Melakukan analisis model linear regresi dengan Functional Ability (Bartlet) sebagai outcome(y) dan explanatory variables meliputi: waktu(time/week), grup intervensi (group), dan interaksi waktu dan grup intervensi.
model1 <- lm(ability ~ as.numeric(time) + Group + as.numeric(time)*Group, data = stroke_long)
summary(model1)

#9. Melakukan ulang Langkah no 8 tanpa variable interaksi di dalam model
model2 <- lm(ability ~ as.numeric(time) + Group, data = stroke_long)
summary(model2)

#10. Menghitung AIC model no 8 dan 9, serta menginterpretasikan perbandingan nilai AIC nya.
model <- list(model1, model2)
mod.names <- c('ability.time.group.timegroup', 'ability.time.group')
aictab(cand.set = model, modnames = mod.names)

#atau

glance(model1)
glance(model2)

#11. Model no 8 dan 9, manakah yang terbaik? Pilih salah satu kemudian interpretasikan hasil dari analisisnya dari model yang dipilih(hubungan antara variable explanatory dengan outcome)

#12. Melakukan analisis mixed model (random intercept) menggunakan package nlme. Functional Ability (Bartlet) sebagai outcome(y) dan explanatory variables meliputi: waktu(time/week), grup intervensi (group), dan Random intercept.
mixmodel <- lme(ability~as.numeric(time) + as.factor(Group), data = stroke_long,
                      random=~1|Subject)
summary(mixmodel)

#13. Melakukan ulang analisis  dengan Functional Ability (Bartlet) sebagai outcome(y) dan explanatory variables meliputi: waktu(time/week), grup intervensi (group) dengan General Estimating Equation (GEE) dengan correlation structure:
#Exchangeable
gee_exch <- geeglm(ability~as.factor(Group)+as.numeric(time)+as.factor(Group)*as.numeric(time),family=gaussian,
                 data=stroke_long,id=as.factor(Subject),wave=as.numeric(time),corst="exchangeable")
exch<-corCompSymm(form = ~ 1 | Subject)
gls.exch<-gls(ability~as.factor(Group)+as.numeric(time)+
                as.factor(Group)*as.numeric(time), data=stroke_long,
              correlation=exch)
summary(gls.exch)
#Auto regressive
gee_ar1 <- geeglm(ability~as.factor(Group)+as.numeric(time)+as.factor(Group)*as.numeric(time),family=gaussian,
                data=stroke_long,id=as.factor(Subject),wave=as.numeric(time),corst="ar1")
ar1<-corAR1(form = ~ 1 | Subject)
gls.ar1<-gls(ability~as.factor(Group)+as.numeric(time)+
               as.factor(Group)*as.numeric(time), data=stroke_long,
             correlation=ar1)
summary(gls.ar1)
#Unstructure
gee_un <- geeglm(ability~as.factor(Group)+as.numeric(time)+as.factor(Group)*as.numeric(time),family=gaussian,
               data=stroke_long,id=as.factor(Subject),wave=as.numeric(time),corst="unstructured")
un<-corSymm(form = ~ 1 | Subject)
gls.un<-gls(ability~as.factor(Group)+as.numeric(time)+
              as.factor(Group)*as.numeric(time), data=stroke_long,
            correlation=un)
summary(gls.un)

#14. Mengingat GEE tidak dapat mengeluarkan AIC, dengan menggunakan statement gls, menghitung AIC dari model GLS dengan ketiga struktur korelasi di atas (Exchangeable, Auto regressive, dan Unstructure).
aic = AIC(gls.exch,gls.ar1,gls.un)

#15. Membuat tabel untuk Membandingkan AIC dari model dengan korelasi struktur Exchangeable, Auto regressive, dan Unstructure, dengan AIC linear regresi model (Model dari instruksi no 9). Interpretasikan dan simpulkan.
write.csv(aic,"aic.csv")


