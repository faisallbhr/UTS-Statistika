#Mengambil data dari csv
data_nilai <- read.csv("E:/[Informatika]/Semester 3/Statistika/Data Nilai Algo E 21.csv",header = FALSE)
data_nilai

#Menghitung banyak data
banyak_data <- nrow(data_nilai)
banyak_data

#Mencari nilai terkecil dan terbesar
nilai_terkecil <- min(data_nilai)
nilai_terbesar <- max(data_nilai)
nilai_terkecil
nilai_terbesar

#Mencari range
range <- nilai_terbesar-nilai_terkecil
range

#Mencari banyak kelas
k <- 1+(3.3*log(range, base=10))
k <- round(k)
k

#Mencari interval kelas
interval <- range/k
interval <- interval
interval <- round(interval)
interval

#Membuat tabel
tabel <- data.frame(kelas1 = c(75,76,77),
                    kelas2 = c(78,79,80),
                    kelas3 = c(81,82,83),
                    kelas4 = c(84,85,86),
                    kelas5 = c(87,88,89))
frekuensi1 <- 4
frekuensi2 <- 33
frekuensi3 <- 106
frekuensi4 <- 55
frekuensi5 <- 2

fixi <- c(median(tabel$kelas1)*frekuensi1,
          median(tabel$kelas2)*frekuensi2,
          median(tabel$kelas3)*frekuensi3,
          median(tabel$kelas4)*frekuensi4,
          median(tabel$kelas5)*frekuensi5)

#Mencari mean
rata2 <- sum(fixi)/banyak_data
rata2

#Mencari median
fkk <- data.frame(kelas1 = 4,
                  kelas2 = 37,
                  kelas3 = 143,
                  kelas4 = 198,
                  kelas5 = 200)

Tb <- 81-0.5

median <- Tb+((0.5*banyak_data-fkk$kelas2)/frekuensi3)*interval
median

#Mencari modus
d1 <- frekuensi3-frekuensi2
d2 <- frekuensi3-frekuensi4

modus <- Tb+(d1/(d1+d2))*interval
modus

#Mencari range
#Cara 1

median_kelas1 <- median(tabel$kelas1)
median_kelas5 <- median(tabel$kelas5)

Range <- median_kelas5 - median_kelas1
Range

#Simpangan rata-rata
xi <- data.frame(xi1 = median(tabel$kelas1),
                 xi2 = median(tabel$kelas2),
                 xi3 = median(tabel$kelas3),
                 xi4 = median(tabel$kelas4),
                 xi5 = median(tabel$kelas5))

xi_xbar <- data.frame(a1 = xi$xi1-rata2,
                      a2 = xi$xi2-rata2,
                      a3 = xi$xi3-rata2,
                      a4 = xi$xi4-rata2,
                      a5 = xi$xi5-rata2)

fi_xi_xbar <- data.frame(b1 = frekuensi1*abs(xi_xbar$a1),
                         b2 = frekuensi2*abs(xi_xbar$a2),
                         b3 = frekuensi3*abs(xi_xbar$a3),
                         b4 = frekuensi4*abs(xi_xbar$a4),
                         b5 = frekuensi5*abs(xi_xbar$a5))

SR <- sum(fi_xi_xbar)/banyak_data
SR

#Simpangan baku
xi_xbar2 <- data.frame(a1 = xi_xbar$a1**2,
                       a2 = xi_xbar$a2**2,
                       a3 = xi_xbar$a3**2,
                       a4 = xi_xbar$a4**2,
                       a5 = xi_xbar$a5**2)

fi_xi_xbar2 <- data.frame(b1 = frekuensi1*xi_xbar2$a1,
                          b2 = frekuensi2*xi_xbar2$a2,
                          b3 = frekuensi3*xi_xbar2$a3,
                          b4 = frekuensi4*xi_xbar2$a4,
                          b5 = frekuensi5*xi_xbar2$a5)

S <- sqrt(sum(fi_xi_xbar2)/banyak_data)
S
