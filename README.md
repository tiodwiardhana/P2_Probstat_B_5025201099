# P2_Probstat_B_5025201099


#1
Seorang peneliti melakukan penelitian mengenai pengaruh aktivitas 𝐴 terhadap
kadar saturasi oksigen pada manusia. Peneliti tersebut mengambil sampel
sebanyak 9 responden. Pertama, sebelum melakukan aktivitas 𝐴, peneliti mencatat
kadar saturasi oksigen dari 9 responden tersebut. Kemudian, 9 responden tersebut
diminta melakukan aktivitas 𝐴. Setelah 15 menit, peneliti tersebut mencatat kembali
kadar saturasi oksigen dari 9 responden tersebut. Berikut data dari 9 responden
mengenai kadar saturasi oksigen sebelum dan sesudah melakukan aktivitas 𝐴

![image](https://user-images.githubusercontent.com/73664125/170866930-3a0a5a46-7e46-4e38-9830-256ef02fde9f.png)

Berdasarkan data pada tabel diatas, diketahui kadar saturasi oksigen dari
responden ke-3 ketika belum melakukan aktivitas 𝐴 sebanyak 67, dan setelah
melakukan aktivitas 𝐴 sebanyak 70.

a. Carilah Standar Deviasi dari data selisih pasangan pengamatan tabel
diatas
```
R <- c(1,2,3,4,5,6,7,8,9)
X <- c(78,75,67,77,70,72,78,74,77)
Y <- c(100,95,70,90,90,90,89,90,100)

selisih <- sd(x-y)
print(selisih)
```
maka hasilnya adalah `....`
![image](https://user-images.githubusercontent.com/73664125/170880055-3057cb62-40bb-4ab1-bde4-01a2c85072fa.png)


b. carilah nilai t (p-value)
```
t.test (Y, X, paired = TRUE, var.equal = FALSE)
```
![image](https://user-images.githubusercontent.com/73664125/170880070-1711e394-60a5-4b54-b2e4-0d5fedccabd0.png)

c. tentukanlah apakah terdapat pengaruh yang signifikan secara statistika
dalam hal kadar saturasi oksigen , sebelum dan sesudah melakukan
aktivitas 𝐴 jika diketahui tingkat signifikansi 𝛼 = 5% serta H0 : “tidak ada
pengaruh yang signifikan secara statistika dalam hal kadar saturasi
oksigen , sebelum dan sesudah melakukan aktivitas 𝐴”
```
mu0 = mean(X)
xbar = mean(Y)          
s = sd(Y)
n = length(Y)
t = (xbar-mu0)/(s/sqrt(n)) 

alpha = 0.05 
t.half.alpha = qt(1-alpha/2, df=n-1) 
c(-t.half.alpha, t.half.alpha)

pval <- 2*pt(t, df=n-1)
pval

# karena pvalue > 0,05 atau pvalue > alpha maka keputusan gagal tolak Hl0
# tidak ada pengaruh  secara statistika

```
![image](https://user-images.githubusercontent.com/73664125/170880105-db5dccb6-ecef-4b09-a670-53bb73b67706.png)


#2
Diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun.
Untuk menguji klaim ini, 100 pemilik mobil yang dipilih secara acak diminta untuk
mencatat jarak yang mereka tempuh. Jika sampel acak menunjukkan rata-rata
23.500 kilometer dan standar deviasi 3900 kilometer. (Kerjakan menggunakan 2 library seperti referensi pada modul).
```
library(BSDA)

zsum.test(mean.X = 23500, sigma.X = 3900, n.X = 100,
          alternative = "greater", mu0 = 20000,
          conf.level = 0.95)
```
a. Apakah Anda setuju dengan klaim tersebut?
```
#Setuju
```
b. Jelaskan maksud dari output yang dihasilkan!
```
#Output dari z test adalah hipotesis alternatif
# alternative hypothesis adalah nilai benar mean yaitu lebih dari 20000 atau H1 diterima sehingga klaim benar. 

```
c. Buatlah kesimpulan berdasarkan P-Value yang dihasilkan!
```
# kesimpulan yang didapat yaitu dapat diketahui bahwa mobil dikemudikan rata-rata lebih dari 20.000 kilometer per tahun

```
3. Diketahui perusahaan memiliki seorang data analyst ingin memecahkan
permasalahan pengambilan keputusan dalam perusahaan tersebut. Selanjutnya
didapatkanlah data berikut dari perusahaan saham tersebut.

![image](https://user-images.githubusercontent.com/73664125/170867233-070c35ad-deca-43de-a8de-d460539042fa.png)

Dari data diatas berilah keputusan serta kesimpulan yang didapatkan dari hasil
diatas. Asumsikan nilai variancenya sama, apakah ada perbedaan pada
rata-ratanya (α= 0.05)? Buatlah :
A. H0 dan H1
```
cat("H0 : mu = mu0","\n","mu !=(tidak sama dengan) mu0")

```
B. Hitung Sampel Statistik
```
tsum.test(mean.x = 3.64, s.x = 1.67, n.x = 19, mean.y = 2.79 , s.y = 1.32, n.y = 27, alternative = "greater", var.equal = TRUE)

```
![image](https://user-images.githubusercontent.com/73664125/170880167-e5048535-8c6c-4e44-9dc9-cb9f3fb05e5b.png)

C. Lakukan Uji Statistik (df =2)
```
xbar3 = 2.79
mu03 = 3.64
s3 = 1.32
n3 = 27              
t3 = (xbar3-mu03)/(s3/sqrt(n3)) 
t3  
```
![image](https://user-images.githubusercontent.com/73664125/170880187-070a5af6-9c5e-42ed-9543-0784134f488c.png)

D. Nilai Kritikal
```
alpha3 = 0.05 
t.alpha3 = qt(1-alpha3, df=2) 
t.alpha3 
```
![image](https://user-images.githubusercontent.com/73664125/170880196-7eefe6d5-4216-4df3-a59e-1dbb58b5995d.png)

E. Keputusan
```
cat("Keputusan : Gagal Tolak H0")
```
F. Kesimpulan
```
# Dengan tingkat keyakinan 95%, diyakini bahwa tidak terdapat 
# perbedaan rata-rata saham pada perusahaan di Bandung dan Bali.
```

4. Seorang Peneliti sedang meneliti spesies dari kucing di ITS . Dalam penelitiannya
ia mengumpulkan data tiga spesies kucing yaitu kucing oren, kucing hitam dan
kucing putih dengan panjangnya masing-masing.
Jika :
diketahui dataset https://intip.in/datasetprobstat1
H0 : Tidak ada perbedaan panjang antara ketiga spesies atau rata-rata panjangnya
sama
Maka Kerjakan atau Carilah:
A. Buatlah masing masing jenis spesies menjadi 3 subjek "Grup" (grup 1,grup
2,grup 3). Lalu Gambarkan plot kuantil normal untuk setiap kelompok dan
lihat apakah ada outlier utama dalam homogenitas varians.
```
my_data <- read.delim(file.choose())

my_data$Group <- as.factor(my_data$Group)
my_data$Group = factor(my_data$Group, labels = c("grup1", "grup1", "grup3"))


grup1 <- subset(my_data, Group == "grup1")
grup2 <- subset(my_data, Group == "grup1")
grup3 <- subset(my_data, Group == "grup3")

qqnorm(grup1$Length)

qqnorm(grup2$Length)

qqnorm(grup3$Length)

# berdasarkan plot kuantil normal di atas, tidak ditemukan outlier utama pada homogenitas varians

```
![image](https://user-images.githubusercontent.com/73664125/170880216-18da5c74-2257-490a-b109-2625f32f612c.png)

B. carilah atau periksalah Homogeneity of variances nya , Berapa nilai p yang
didapatkan? , Apa hipotesis dan kesimpulan yang dapat diambil ?
```
bartlett.test(Length ~ Group, data = my_data)

```
![image](https://user-images.githubusercontent.com/73664125/170880224-3278e13d-f56d-44ce-8d31-c0daff35350d.png)

C. Untuk uji ANOVA (satu arah), buatlah model linier dengan Panjang versus
Grup dan beri nama model tersebut model 1.
```
model1 <- aov(Length ~ Group, data = my_data)
summary(model1)
```
![image](https://user-images.githubusercontent.com/73664125/170880275-d122f110-e27b-4454-82b6-7108aa68293d.png)

D. Dari Hasil Poin C, Berapakah nilai-p ? , Apa yang dapat Anda simpulkan
dari H0?
```
# nilai p adalah 0.0013 dimana kurang dari 0.005, sehingga h0 ditolak
```
E. Verifikasilah jawaban model 1 dengan Post-hoc test Tukey HSD, dari nilai p
yang didapatkan apakah satu jenis kucing lebih panjang dari yang lain? Jelaskan.
```
TukeyHSD(model1)
```
![image](https://user-images.githubusercontent.com/73664125/170880304-35264df4-4d4f-4e49-92dd-6579883bd53f.png)

F. Visualisasikan data dengan ggplot2
```
library("ggplot2")

ggplot(my_data, aes(x = Group, y = Length)) + 
  geom_boxplot(fill = "white", colour = "black") + 
  scale_x_discrete() + xlab("Group") + ylab("Length")
```
![image](https://user-images.githubusercontent.com/73664125/170880511-03ef214b-bd8a-415d-b34f-c037a5206bad.png)

5. Data yang digunakan merupakan hasil eksperimen yang dilakukan untuk
mengetahui pengaruh suhu operasi (100˚C, 125˚C dan 150˚C) dan tiga jenis kaca
pelat muka (A, B dan C) pada keluaran cahaya tabung osiloskop. Percobaan
dilakukan sebanyak 27 kali dan didapat data sebagai berikut: Data Hasil
Eksperimen. Dengan data tersebut:
a. Buatlah plot sederhana untuk visualisasi data
```
install.packages("multcompView")
install.packages("ggplot2")
install.packages("readr")
install.packages("dplyr")
library(readr)
library(ggplot2)
library(multcompView)
library(dplyr)

GTL <- read_csv("GTL.CSV")
head(GTL)

str(GTL)

qplot(x = Temp, y = Light, geom = "point", data = GTL) +
  facet_grid(.~Glass, labeller = label_both)

```
b. Lakukan uji ANOVA dua arah
```
GTL$Glass <- as.factor(GTL$Glass)
GTL$Temp_Factor <- as.factor(GTL$Temp)
str(GTL)

anova <- aov(Light ~ Glass*Temp_Factor, data = GTL)
summary(anova)

```
c. Tampilkan tabel dengan mean dan standar deviasi keluaran cahaya untuk
setiap perlakuan (kombinasi kaca pelat muka dan suhu operasi)
```
data_summary <- group_by(GTL, Glass, Temp) %>%
  summarise(mean=mean(Light), sd=sd(Light)) %>%
  arrange(desc(mean))
print(data_summary)

```
d. Lakukan uji Tukey
```
tukey <- TukeyHSD(anova)
print(tukey)

```
e. Gunakan compact letter display untuk menunjukkan perbedaan signifikan
antara uji Anova dan uji Tukey
```
tukey.cld <- multcompLetters4(anova, tukey)
print(tukey.cld)

cld <- as.data.frame.list(tukey.cld$`Glass:Temp_Factor`)
data_summary$Tukey <- cld$Letters
print(data_summary)

write.csv("GTL_summary.csv")
```
Berikut adalah contoh daftar package dan fungsi yang dapat digunakan (dapat pula
menggunakan contoh lainnya)
● Packages: readr, ggplot2, multcompView, dplyr
● Function: aov, TukeyHSD, qplot, group_by, summarise, multcompLetters4


