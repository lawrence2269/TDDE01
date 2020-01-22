options(scipen=999) #To avoid scientific notations
set.seed(12345)
RNGversion('3.5.1')
nirSpectra = read.csv2("NIRspectra.csv",stringsAsFactors = FALSE)

#1. Applying PCA
drops = c("Viscosity")
nirSpectra = nirSpectra[,!(names(nirSpectra) %in% drops)]
nirSpectra$Viscosity = c()
res = prcomp(nirSpectra)
lambda = res$sdev^2
sprintf("%2.3f",lambda/sum(lambda)*100)
screeplot(res)

plot(res$x[,1],res$x[,2],xlab = "PC1",ylab = "PC2")

#2. Traceplots
u = res$rotation
plot(u[,1],main="Traceplot, PC1",ylab = "u[,1]")
plot(u[,2],main="Traceplot PC2",ylab = "u[,2]")

#3. fastICA
set.seed(12345)
ica = fastICA::fastICA(nirSpectra,2)
lambda_2 = ica$K%*%ica$W
plot(lambda_2[,1],main="ICA 1")
plot(lambda_2[,2],main="ICA 2")

plot(ica$S[,1],ica$S[,2],main="ICA Latent Features",xlab="1",ylab="2")





