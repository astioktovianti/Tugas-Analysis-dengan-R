library(readr)
library(readxl)
library(data.table)
library(writexl)
library(ggplot2)
library(tidyverse)

#1. mengakses dan mendownload dataset ke dalam global environment RStudio.
stroke <- read_excel("C:/Users/asus/Downloads/stroke.xls")
names(stroke)


#mengubah data wide to long
stroke_long = stroke %>% select(c(1:6,39:46)) %>%
  pivot_longer(cols=Bart1:Bart8,
               names_to = "time",
               names_prefix = "Bart",
               values_to = "ability")

names(stroke_long) #melihat nama-nama variabel dalam dataset
glimpse(stroke_long) #melihat isi dataset

#2.Membuat visualisasi grafik garis dari perkembangan nilai kemampuan motorik (functional ability score) dari setiap subyek menggunakan variable bart.
#A. Grafik keseluruhan
  ggplot(stroke_long, aes(x = time,
             y = ability)) +
  geom_line(aes(group = Subject)) +
  theme_classic() + labs( y="Functional abiity score", 
                          x="Time", title="Example: Recovery From Stroke")
                                                                                                               
#B. Grafik per group
ggplot(stroke_long, aes(x = time, y = ability)) +
    geom_line(aes(group = Subject)) +
    theme_classic() +
    facet_grid(.~ Group) + labs( y="Functional abiity score", 
                                  x="Time", title="Example: Recovery From Stroke")

     #atau

ggplot(stroke_long, aes(x=time, y=ability, group=Subject)) +geom_line() + 
  facet_wrap(~Group) + labs( y="Functional abiity score", 
                             x="Time", title="Example: Recovery From Stroke")

#3. Membuat grafik nilai rata-rata perkembangan fungsi motorik secara total dan masing-masing yang divisualisasikan pada 1 grafik.

stroke_av <- stroke_long %>% #menambahkan variabel rata-rata perkembangan fungsi motorik
  group_by(Group, time) %>%
  mutate(Average = mean(ability)) %>%
  as.data.frame()

ggplot(stroke_av, aes(x = time, y = Average)) +
  geom_line(aes(group = Group)) +
  theme_classic()+ labs( y="Functional abiity score", 
                         x="Week", title="Average stroke recovery score for groups of patients")

stroke_av %>%
  mutate(label = if_else(time == max(time), as.character(Group), NA_character_)) %>%
  ggplot(aes(x = time, y = Average, group = Group, colour = Group)) + geom_line() + labs( y="Functional abiity score", 
                                                                                     x="Week", title="Average stroke recovery score for groups of patients")
#4. Membuat Matrix Scatter plot dari nilai fungsi motorik antar waktu/pekan.
pairs(~Bart1 + Bart2 +  Bart3 + Bart4 + Bart5 + Bart6 + Bart7 + Bart8, data = stroke) 

#5. Menghitung dan membuat tabel silang koefisien korelasi nilai fungsi motorik antar waktu/pekan. 
cor(stroke$Bart1, stroke$Bart2)
cor(stroke$Bart1, stroke$Bart3)
cor(stroke$Bart1, stroke$Bart4)
cor(stroke$Bart1, stroke$Bart5)
cor(stroke$Bart1, stroke$Bart6)
cor(stroke$Bart1, stroke$Bart7)
cor(stroke$Bart1, stroke$Bart8)

cor(stroke$Bart2, stroke$Bart3)
cor(stroke$Bart2, stroke$Bart4)
cor(stroke$Bart2, stroke$Bart5)
cor(stroke$Bart2, stroke$Bart6)
cor(stroke$Bart2, stroke$Bart7)
cor(stroke$Bart2, stroke$Bart8)

cor(stroke$Bart3, stroke$Bart4)
cor(stroke$Bart3, stroke$Bart5)
cor(stroke$Bart3, stroke$Bart6)
cor(stroke$Bart3, stroke$Bart7)
cor(stroke$Bart3, stroke$Bart8)

cor(stroke$Bart4, stroke$Bart5)
cor(stroke$Bart4, stroke$Bart6)
cor(stroke$Bart4, stroke$Bart7)
cor(stroke$Bart4, stroke$Bart8)

cor(stroke$Bart5, stroke$Bart6)
cor(stroke$Bart5, stroke$Bart7)
cor(stroke$Bart5, stroke$Bart8)

cor(stroke$Bart6, stroke$Bart7)
cor(stroke$Bart6, stroke$Bart8)

cor(stroke$Bart7, stroke$Bart8)


#6. buat intercept dan slopes dari tiap subjek
library(lme4)
stroke_av$time = as.numeric(stroke_av$time)
model <- (lmList(ability ~ time | Subject, data = stroke_av))
summary(model)$coef

intercepts <- sapply(model,coef)[1,]
slope <- sapply(model,coef)[2,]
intercepts
slope


