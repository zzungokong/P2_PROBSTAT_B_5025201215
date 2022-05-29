install.packages("BSDA")

library(BSDA)

orang.ke = c(seq(1:9))
oksigen.sebelum = c(78, 75, 67, 77, 70, 72, 78, 74, 77)
oksigen.sesudah = c(100, 95, 70, 90, 90, 90, 89, 90, 100)
data = data.frame(orang.ke,oksigen.sebelum, oksigen.sesudah)
n = 9

selisih = data$oksigen.sesudah - data$oksigen.sebelum
cat("Standar deviasi selisih pasangan pengamatan : ")
standardev = sd(selisih)
standardev

#Menggunakan t karena hanya 9 objek
#Hipotesis 0 : Tidak ada pengaruh signifikan, maka mu = 0
#Hipotesis 1 : Ada pengaruh signifikan, maka mu != 0
mu = 0
#Cari rata rata selisihnya dahulu (x bar)
xbar = mean(selisih)
xbar
#xbar = 16.22222

#Cari nilai t statistik terlebih dahulu menggunaan rumus (xbar - mu / sd/sqrt(n))
tstastistik =( (xbar - mu) / (standardev / sqrt(n)))
tstastistik
#tstastistik = 7.652479
#Cari pvalue, karena merupakan uji 2 arah "two sided" maka dikali 2
pvalue = 2 * pt(-abs(tstastistik), df=n-1)
pvalue
#pvalue = 6.003179e-05


#c
#Hipotesis 0 : Tidak ada pengaruh signifikan, maka mu = 0
#Hipotesis 1 : Ada pengaruh signifikan, maka mu != 0
#Digunakan uji 2 arah sehingga alternative nya adalah "two.sided"
#Signifikannya 5% maka CI nya 95%
t.test(x=data$oksigen.sesudah, y=data$oksigen.sebelum,
       alternative = "two.sided",
       mu = 0, paired = TRUE, var.equal = TRUE,
       conf.level = 0.95)










