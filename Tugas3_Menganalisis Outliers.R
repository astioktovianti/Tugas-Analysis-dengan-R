#reading data
pef_final

#1. Mengidentifikasi outlier berdasarkan kriteria dan visualisasi grafik boxplot
summary(pef_final$pef)
boxplot(pef_final$pef,col="aquamarine",main ="PEF Score")

#2. Menentukan cut off outlier (batas atas dan bawah) dari grafik boxplot tersebut.
min(boxplot(pef_final$pef, plot = FALSE)$out)
max(boxplot(pef_final$pef, plot = FALSE)$out)

quartiles <- quantile(pef_final$pef, probs=c(.25, .75), na.rm=FALSE)
quartiles

IQR <- IQR(pef_final$pef)
IQR

Lower <- quartiles[1] - 1.5*IQR
Lower

Upper <- quartiles[2] + 1.5*IQR
Upper

#3. Membuat dataset yang tidak berisi outlier sesuai cut off no 2. 

pef_final_no <- subset(pef_final, pef_final$pef > Lower 
                       & pef_final$pef < Upper)
summary(pef_final_no$pef)
boxplot(pef_final_no$pef, col="aquamarine")

#4. Melakukan tes normalitas pada dataset dengan outlier dan tanpa outlier.

#For Big Sample (Kolmogrov-Smirnov)
library(nortest)
lillie.test(pef_final$pef)
lillie.test(pef_final_no$pef)

#5. Membuat grafik QQ line untuk membandingkan visualisasi nilai pef pada dataset dengan outlier dan tanpa outlier.
qqnorm(pef_final$pef); qqline(pef_final$pef)
qqnorm(pef_final_no$pef); qqline(pef_final_no$pef)

#6. Membuat scatterplot yang memperlihatkan hubungan antara pef dengan height, dengan penambahan garis linear/regresi dan smoothed dengan loes (local regression smoothing).
plot(pef_final_no$pef~pef_final_no$height, xlab = "Tinggi Badan(cm)",
     ylab="Peak Expiratory Flow", main="Sebaran PEF berdasarkan Tinggi Badan")

smoothScatter(pef_final_no$pef~pef_final_no$height, xlab = "Tinggi Badan(cm)",
              ylab="Peak Expiratory Flow", main="Sebaran PEF berdasarkan Tinggi Badan")

abline(lm(pef_final_no$pef~pef_final_no$height, data = pef_final_no), col = "blue")
lines(lowess(pef_final_no$height, pef_final_no$pef), col = "red")
#7. Membuat scatterplot yang memperlihatkan hubungan antara pef dengan umur, dengan penambahan garis linear/regresi dan smoothed dengan loes (local regression smoothing).

plot(pef_final_no$pef~pef_final_no$age, xlab = "Usia(Tahun)",
     ylab="Peak Expiratory Flow", main="Sebaran PEF berdasarkan Usia")

smoothScatter(pef_final_no$pef~pef_final_no$height, xlab = "Usia(Tahun)",
              ylab="Peak Expiratory Flow", main="Sebaran PEF berdasarkan Usia")

abline(lm(pef_final_no$pef~pef_final_no$age, data = pef_final_no), col = "blue")
lines(lowess(pef_final_no$age, pef_final_no$pef), col = "red")
