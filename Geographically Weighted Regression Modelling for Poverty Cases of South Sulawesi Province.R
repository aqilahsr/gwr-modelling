# Data Kemiskinan #
library('readxl')
library(writexl)
dataset <- read_excel("E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Latihan/New/2019_Kemiskinan_Var.xlsx")


attach(dataset)
x0 <- array(1, dim = length(y))                  ##Intercecpt##
X  <- cbind(x0,x1,x2,x3,x4,x5,x6,x7,x8,x9)
k  <- 9
n  <- length(y)

library('sp')
Y  <- scale(y)
X1 <- scale(x1)
X2 <- scale(x2)
X3 <- scale(x3)
X4 <- scale(x4)
X5 <- scale(x5)
X6 <- scale(x6)
X7 <- scale(x7)
X8 <- scale(x8)
X9 <- scale(x9)

#regresi parametrik
reg=lm(y~x1+x2+x3+x4+x5+x6+x7+x8+x9)
summary(reg)



#longlat
library('rgdal')
Koordinat    <- cbind(b,l)
Z<-as.matrix(Koordinat)

#konversi lat-lot ke UTM
coord=SpatialPoints(cbind(dataset$b,dataset$l),proj4string=CRS("+proj=longlat"))
coord.UTM=spTransform(coord,CRS("+init=epsg:32748"))
coords=cbind(coord.UTM$coords.x1,coord.UTM$coords.x2)
Cordinat=coords/1000

#data
library('GWmodel')
dtgab <- data.frame(y,x1,x2,x3,x4,x5,x6,x7,x8,x9)
m     <- matrix(1,nrow(dataset),1)
Xm    <- cbind(m,x1,x2,x3,x4,x5,x6,x7,x8,x9)
X     <- cbind(x1,x2,x3,x4,x5,x6,x7,x8,x9)
Y     <- cbind(Y)
MatriksKoordinat     <- as.matrix(Cordinat)
d.mat <- gw.dist(dp.locat=MatriksKoordinat,p=2, focus=0,theta=0,longlat=T)
datapakaiutm <- SpatialPointsDataFrame(d.mat,data=dtgab, match.ID=TRUE)
dtnew<-SpatialPointsDataFrame(Z,as.data.frame(dtgab))

Matriksdij <- read_excel("E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Latihan/dij.xlsx")
Matriksdij<-as.matrix(Matriksdij)
datapakailonglat <- SpatialPointsDataFrame(Matriksdij,data=dtgab, match.ID=TRUE)



########################################################
####----Model GEOGRAPHICALLY WEIGHTED REGRESSION----####
########################################################

##Model GWR Global##
##Inv
bw.FI <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="boxcar",dMat=Matriksdij,adaptive=F)
bw.AI <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="boxcar",dMat=Matriksdij,adaptive=T)
##Gaussian
bw.FG <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="gaussian",dMat=Matriksdij,adaptive=F)
bw.AG <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="gaussian",dMat=Matriksdij,adaptive=T)
##Exp
bw.FE <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="exponential",dMat=Matriksdij,adaptive=F)
bw.AE <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="exponential",dMat=Matriksdij,adaptive=T)
##BisquareLonglat
bw.FB <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="bisquare",dMat=Matriksdij,adaptive=F)
bw.AB <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="bisquare",dMat=Matriksdij,adaptive=T)
##Tricube
bw.FT <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="tricube",dMat=Matriksdij,adaptive=F)
bw.AT <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,kernel="tricube",dMat=Matriksdij,adaptive=T)

##
##BisquareUTM
bw.ABUTM <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=datapakaiutm,kernel="bisquare",dMat=d.mat,adaptive=T)
bw.VBUTM <- bw.gwr(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=datapakaiutm,kernel="bisquare",dMat=d.mat,adaptive=F)
##

###MODEL GWR###
w          <- gw.weight(Matriksdij,bw=bw.AB,kernel="bisquare",adaptive=T)
W.GWR.AB <- as.data.frame(w)
library(writexl)
write_xlsx(W.GWR, "E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/BobotGWR.xlsx")
gwr_invA  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.FI,kernel="boxcar",dMat=Matriksdij,adaptive = F)
gwr_invB  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.AI,kernel="boxcar",dMat=Matriksdij,adaptive=T)
gwr_gaussA <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.FG,kernel="gaussian",dMat=Matriksdij, adaptive = F)
gwr_gaussB <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.AG,kernel="gaussian",dMat=Matriksdij,adaptive=T)
gwr_expA  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.FE,kernel="exponential",dMat=Matriksdij,adaptive = F)
gwr_expB  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.AE,kernel="exponential",dMat=Matriksdij,adaptive=T)
gwr_bisqA  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.FB,kernel="bisquare",dMat=Matriksdij,adaptive = F)
gwr_bisqB  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.AB,kernel="bisquare",dMat=Matriksdij,adaptive=T)
gwr_tricA  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.FT,kernel="tricube",dMat=Matriksdij,adaptive = F)
gwr_tricB  <- gwr.basic(y~x1+x2+x3+x4+x5+x6+x7+x8+x9,data=dtnew,bw=bw.AT,kernel="tricube",dMat=Matriksdij,adaptive=T)

###Model GWR Lokal###
##F_Invers
SDFGWRLokal_FI<-gwr_invA$SDF
GWRLokal_FI<-as.data.frame(cbind(SDFGWRLokal_FI$Intercept,SDFGWRLokal_FI$x1,SDFGWRLokal_FI$x2,SDFGWRLokal_FI$x3,SDFGWRLokal_FI$x4,SDFGWRLokal_FI$x5,SDFGWRLokal_FI$x6,SDFGWRLokal_FI$x7,SDFGWRLokal_FI$x8,SDFGWRLokal_FI$x9))
write_xlsx(GWRLokal_FI,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_FI.xlsx")
##A_Invers
SDFGWRLokal_AI<-gwr_invB$SDF
GWRLokal_AI<-as.data.frame(cbind(SDFGWRLokal_AI$Intercept,SDFGWRLokal_AI$x1,SDFGWRLokal_AI$x2,SDFGWRLokal_AI$x3,SDFGWRLokal_AI$x4,SDFGWRLokal_AI$x5,SDFGWRLokal_AI$x6,SDFGWRLokal_AI$x7,SDFGWRLokal_AI$x8,SDFGWRLokal_AI$x9))
write_xlsx(GWRLokal_AI,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_AI.xlsx")
##F_Gauss
SDFGWRLokal_FG<-gwr_gaussA$SDF
GWRLokal_FG<-as.data.frame(cbind(SDFGWRLokal_FG$Intercept,SDFGWRLokal_FG$x1,SDFGWRLokal_FG$x2,SDFGWRLokal_FG$x3,SDFGWRLokal_FG$x4,SDFGWRLokal_FG$x5,SDFGWRLokal_FG$x6,SDFGWRLokal_FG$x7,SDFGWRLokal_FG$x8,SDFGWRLokal_FG$x9))
write_xlsx(GWRLokal_FG,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_FG.xlsx")
##A_Gauss
SDFGWRLokal_AG<-gwr_gaussB$SDF
GWRLokal_AG<-as.data.frame(cbind(SDFGWRLokal_AG$Intercept,SDFGWRLokal_AG$x1,SDFGWRLokal_AG$x2,SDFGWRLokal_AG$x3,SDFGWRLokal_AG$x4,SDFGWRLokal_AG$x5,SDFGWRLokal_AG$x6,SDFGWRLokal_AG$x7,SDFGWRLokal_AG$x8,SDFGWRLokal_AG$x9))
write_xlsx(GWRLokal_AG,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_AG.xlsx")
##F_Exp
SDFGWRLokal_FE<-gwr_expA$SDF
GWRLokal_FE<-as.data.frame(cbind(SDFGWRLokal_FE$Intercept,SDFGWRLokal_FE$x1,SDFGWRLokal_FE$x2,SDFGWRLokal_FE$x3,SDFGWRLokal_FE$x4,SDFGWRLokal_FE$x5,SDFGWRLokal_FE$x6,SDFGWRLokal_FE$x7,SDFGWRLokal_FE$x8,SDFGWRLokal_FE$x9))
write_xlsx(GWRLokal_FE,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_FE.xlsx")
##A_Exp
SDFGWRLokal_AE<-gwr_expB$SDF
GWRLokal_AE<-as.data.frame(cbind(SDFGWRLokal_AE$Intercept,SDFGWRLokal_AE$x1,SDFGWRLokal_AE$x2,SDFGWRLokal_AE$x3,SDFGWRLokal_AE$x4,SDFGWRLokal_AE$x5,SDFGWRLokal_AE$x6,SDFGWRLokal_AE$x7,SDFGWRLokal_AE$x8,SDFGWRLokal_AE$x9))
write_xlsx(GWRLokal_AE,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_AE.xlsx")
##F_Bisq
SDFGWRLokal_FB<-gwr_bisqA$SDF
GWRLokal_FB<-as.data.frame(cbind(SDFGWRLokal_FB$Intercept,SDFGWRLokal_FB$x1,SDFGWRLokal_FB$x2,SDFGWRLokal_FB$x3,SDFGWRLokal_FB$x4,SDFGWRLokal_FB$x5,SDFGWRLokal_FB$x6,SDFGWRLokal_FB$x7,SDFGWRLokal_FB$x8,SDFGWRLokal_FB$x9))
write_xlsx(GWRLokal_FB,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_FB.xlsx")
##A_Bisq
SDFGWRLokal_AB<-gwr_bisqB$SDF
GWRLokal_AB<-as.data.frame(cbind(SDFGWRLokal_AB$Intercept,SDFGWRLokal_AB$x1,SDFGWRLokal_AB$x2,SDFGWRLokal_AB$x3,SDFGWRLokal_AB$x4,SDFGWRLokal_AB$x5,SDFGWRLokal_AB$x6,SDFGWRLokal_AB$x7,SDFGWRLokal_AB$x8,SDFGWRLokal_AB$x9))
write_xlsx(GWRLokal_AB,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_AB.xlsx")
##F_Tric
SDFGWRLokal_FT<-gwr_tricA$SDF
GWRLokal_FT<-as.data.frame(cbind(SDFGWRLokal_FT$Intercept,SDFGWRLokal_FT$x1,SDFGWRLokal_FT$x2,SDFGWRLokal_FT$x3,SDFGWRLokal_FT$x4,SDFGWRLokal_FT$x5,SDFGWRLokal_FT$x6,SDFGWRLokal_FT$x7,SDFGWRLokal_FT$x8,SDFGWRLokal_FT$x9))
write_xlsx(GWRLokal_FT,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_FT.xlsx")
##A_Tric
SDFGWRLokal_AT<-gwr_tricB$SDF
GWRLokal_AT<-as.data.frame(cbind(SDFGWRLokal_AT$Intercept,SDFGWRLokal_AT$x1,SDFGWRLokal_AT$x2,SDFGWRLokal_AT$x3,SDFGWRLokal_AT$x4,SDFGWRLokal_AT$x5,SDFGWRLokal_AT$x6,SDFGWRLokal_AT$x7,SDFGWRLokal_AT$x8,SDFGWRLokal_AT$x9))
write_xlsx(GWRLokal_AT,"E:/Dokumenku/Perkuliahanku (Universitas Hasanuddin)/2020/Semester 7/Bismilah/Seminar Statistika 2/SDF/GWRLokal_AT.xlsx")
