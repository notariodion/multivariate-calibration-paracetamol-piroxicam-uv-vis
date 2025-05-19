setwd("D:\\UNIKA ATMA JAYA\\PENELITIAN\\Proposal Internal UAJ\\Pengembangan paket metode analisis multivariat\\data spektro\\DOK PA DION\\analisis kalibrasi jamu\\calibration")


# library
library(pls)
library(readxl)

# input data
df1<- as.data.frame(read_excel("kaljam.xlsx"))
head(df1)


#visualisasi raw data
abs <- df1[,-c(1:4)]
conc <- df1[,c(3:4)]
df2 <- data.frame(conc, I(abs))


wl <- c(200:500)

# one by one
matplot(wl, t(df2$abs[c(22, # drug-free herbs
                        19,  # drug-free herbs + piroxicam 
                        18   # drug-free herbs + paracetamol 
                        ),]), type="l",
        col=c(1,1,1),lty=c(1,2,3),
        xlab="wavelength (nm)", ylab="absorbance (a.u)")
legend("topright", c("drug-free herbs", "drug-free herbs + piroxicam", 
                    "drug-free herbs + paracetamol"),
       col=c(1,1,1),lty=c(1,2,3),
       bty="n")

# all spectra
matplot(wl, t(df2$abs), type="l",
xlab="wavelength (nm)", ylab="absorbance (a.u)")

# partial least square regression of piroxicam 


pls_prx <- plsr(Y2~., data=df1[-c(2,4,13,16,20),-c(1,2,4)], 
                validation = "LOO")
## select number of component
plot(RMSEP(pls_prx), main='')
rmsep_result <- RMSEP(pls_prx)
min_comp <- which.min(rmsep_result$val[1,,-1])

predplot(pls_prx,ncomp=min_comp, line=T, 
         newdata = df1[c(2,4,13,16,20),-c(1,2,4)], 
         which=c("train","validation", "test"))

# partial least square regression of paracetamol
pls_pct <- plsr(Y3~., data=df1[-c(2,4,13,16,20),-c(1,2,3)], 
                validation = "LOO")
## select number of component
plot(RMSEP(pls_pct), main='')
rmsep_result <- RMSEP(pls_pct)
min_comp <- which.min(rmsep_result$val[1,,-1])

predplot(pls_pct,ncomp=min_comp, line=T, 
         newdata = df1[c(2,4,13,16,20),-c(1,2,3)], 
         which=c("train","validation", "test"))

