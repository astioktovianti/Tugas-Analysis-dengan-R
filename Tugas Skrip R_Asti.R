#skrip tugas
#mengaktifkan packages: readr
library(readr)

#reading data
dataasti <- read_csv("https://raw.githubusercontent.com/dwi-agustian/biostat/main/pef.csv")

#install package: janitor
library(janitor)

#cek duplikasi data asti
get_dupes(dataasti, pidlink)

#membuat objek baru tanpa duplikasi
dataxd <- dataasti[!duplicated(dataasti$pidlink),]
