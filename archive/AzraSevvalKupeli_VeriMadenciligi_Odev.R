library(Ecdat)
library(caret)
data(Computers)
str(Computers)
summary(Computers)
attributes(Computers)
anyNA(Computers)

Computers$premium <- as.factor(Computers$premium)

# %70 egitim %30 test 
set.seed(1)
bolme_indis <- createDataPartition(y = Computers$premium, p = .70, list = F)
egitim <- Computers[bolme_indis, ]
test  <- Computers[-bolme_indis, ]


# KARAR AGACI
library(RWeka)
library(rJava)

ka <- J48(premium ~., data = egitim)
plot(ka)
summary(ka)
ka

ongoru_ka <- predict(ka, test)
data.frame(Gercek = test$premium, Tahmin = ongoru_ka)[1:20,]

karisiklikmatrisi_ka <- table(Tahmin = ongoru_ka, Gercek = test$premium,
                              dnn = c("tahmini siniflar", "gercek siniflar"))
karisiklikmatrisi_ka

dogruluk_ka <- sum(diag(karisiklikmatrisi_ka)) / sum(karisiklikmatrisi_ka)
cat("Karar Agaci Dogruluk Orani:", round(dogruluk_ka, 4), "\n")

yeni <- data.frame(price = 1500, speed = 33, hd = 214, ram = 4,
                   screen = 14, cd = "yes", multi = "no", ads = 160, trend = 15)
predict(ka, yeni)



# KNN
library(cluster)
library(class)


egitimnitelikleri  <- scale(egitim[, c("price","speed","hd","ram","screen","ads","trend")])
egitimhedefnitelik <- egitim$premium
testnitelikleri    <- scale(test[,   c("price","speed","hd","ram","screen","ads","trend")])
testhedefnitelik   <- test$premium

k_degeri <- 10
dogruluk <- NULL

for(i in 1:k_degeri)
{
  set.seed(1)
  (tahminisiniflar = knn(egitimnitelikleri, testnitelikleri, egitimhedefnitelik, k = i))
  dogruluk[i] <- mean(tahminisiniflar == testhedefnitelik)
  dogruluk[i] <- round(dogruluk[i], 2)
}

for(i in 1:k_degeri)
  print(paste("k=", i, "icin elde edilen dogruluk=", dogruluk[i]))

en_iyi_k <- which.max(dogruluk)
cat("En iyi k degeri:", en_iyi_k, "| Dogruluk:", dogruluk[en_iyi_k], "\n")

set.seed(1)
tahminisiniflar <- knn(egitimnitelikleri, testnitelikleri, egitimhedefnitelik, k = en_iyi_k)

tablom <- table(tahminisiniflar, testhedefnitelik,
                dnn = c("tahmini siniflar", "gercek siniflar"))
tablom

dogruluk_knn <- sum(diag(tablom)) / sum(tablom)
cat("KNN Dogruluk Orani (k=", en_iyi_k, "):", round(dogruluk_knn, 4), "\n")

set.seed(1234)
ornek_indis        <- sample(1:nrow(testnitelikleri), 30)
orneklem_standart  <- testnitelikleri[ornek_indis, ]
orneklem_etiketler <- testhedefnitelik[ornek_indis]

model_manhattan <- agnes(orneklem_standart, metric = "manhattan", method = "single")
pltree(model_manhattan, main = "Test Verisinden 30 Ornek ile Kumeleme (Manhattan)",labels = orneklem_etiketler)

model_oklid <- agnes(orneklem_standart, metric = "euclidean", method = "single")
pltree(model_oklid, main = "Test Verisinden 30 Ornek ile Kumeleme (Oklid)",labels = orneklem_etiketler)

bannerplot(model_manhattan, main = "Bannerplot Grafigi (Manhattan)", labels = orneklem_etiketler)
bannerplot(model_oklid,     main = "Bannerplot Grafigi (Oklid)",     labels = orneklem_etiketler)


# NAIVE BAYES

library(e1071)

nb_modeli <- naiveBayes(premium ~., data = egitim)
print(nb_modeli)

ongoru_nb <- predict(nb_modeli, test)
print(ongoru_nb)

karisiklikmatrisi_nb <- table(Tahmin = ongoru_nb, Gercek = test$premium,
                              dnn = c("tahmini siniflar", "gercek siniflar"))
karisiklikmatrisi_nb

dogruluk_nb <- sum(diag(karisiklikmatrisi_nb)) / sum(karisiklikmatrisi_nb)
cat("Naive Bayes Dogruluk Orani:", round(dogruluk_nb, 4), "\n")

predict(nb_modeli, yeni)


# K-MEANS
library(fpc)
library(clusterSim)

veri_kmeans <- Computers
veri_kmeans$cd      <- as.numeric(veri_kmeans$cd == "yes")
veri_kmeans$multi   <- as.numeric(veri_kmeans$multi == "yes")
veri_kmeans$premium <- as.numeric(veri_kmeans$premium == "yes")


normalize_computers <- data.Normalization(x = veri_kmeans, type = "n4",
                                          normalization = "column")
summary(normalize_computers)

# 2 kume
model_k2 <- kmeans(x = normalize_computers, 2)
table(model_k2$cluster)

model_k2$totss
model_k2$tot.withinss
model_k2$withinss
model_k2$betweenss

# 3 kume
model_k3 <- kmeans(x = normalize_computers, 3)
table(model_k3$cluster)

model_k3$totss
model_k3$tot.withinss
model_k3$withinss
model_k3$betweenss

# k = 2 icin silhouette 
k <- 2
set.seed(1)
computers_modeli     <- kmeans(normalize_computers, centers = k)
computers_silhouette <- silhouette(computers_modeli$cluster,
                                   dist(normalize_computers, method = "euclidean"))
computers_silhouette

silhouette_degeri <- mean(computers_silhouette[, c("sil_width")])
silhouette_degeri  


k <- 20
silhouette_degeri <- 0

for(i in 2:k) {
  set.seed(1)
  computers_modeli     <- kmeans(normalize_computers, centers = i)
  computers_silhouette <- silhouette(computers_modeli$cluster,
                                     dist(normalize_computers, method = "euclidean"))
  silhouette_degeri[i] <- mean(computers_silhouette[, c("sil_width")])
}

silhouette_degeri


plot(2:k, silhouette_degeri[2:k], col = "blue", pch = 20, cex = 1, lty = "solid",xlab = "kume_sayisi(k)", ylab = "silhouette",xlim = range(2, 20), ylim = range(0, 0.6))

text(2:k, silhouette_degeri[2:k],labels = round(silhouette_degeri[2:k], 2), pos = 3)
lines(2:k, silhouette_degeri[2:k])
axis(1, at = 1:20, labels = c(1:20))
grid(NULL, NULL, lty = 6, col = "red", lwd = 2)


en_iyi_k_kmeans <- which.max(silhouette_degeri)
cat("En iyi k degeri (Silhouette):", en_iyi_k_kmeans, "\n")

set.seed(1)
model_son <- kmeans(normalize_computers, centers = en_iyi_k_kmeans)

plotcluster(normalize_computers, model_son$cluster,
            main = paste("K-Means Kumeleme (k =", en_iyi_k_kmeans, ")"))

clusplot(normalize_computers, model_son$cluster,
         color = T, shade = T, labels = 2,
         main = paste("Clusplot - k =", en_iyi_k_kmeans))


# SONUC KARSILASTIRMASI
cat("Karar Agaci (J48) :", round(dogruluk_ka,  4), "\n")
cat("KNN (k =", en_iyi_k, ")       :", round(dogruluk_knn, 4), "\n")
cat("Naive Bayes        :", round(dogruluk_nb,  4), "\n")
cat("K-Means            : Tanimlayici yontem - dogruluk orani hesaplanmaz\n")
