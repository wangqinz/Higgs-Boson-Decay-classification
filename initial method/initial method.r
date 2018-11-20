###########################
##  Higgs Boson Decay Classification

###########################
higgs<-read.csv("higgs_challenge_subset.csv")
require(MASS)
require(ggplot2)
library(gridExtra)

original_data <- read.csv("higgs_challenge_subset.csv")
data <- original_data[,2:33]
data <- data[,-c(31)]

DER <- data[,2:13]
PRI <- data[,14:30]
LABEL <- data[,31]


##############################################################
##############################################################
################ HISTOGRAM OF SOME FEATURES ##################
##############################################################
##############################################################

### for loop needed

p1 <- ggplot(DER, aes(x=DER$DER_mass_transverse_met_lep, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p2 <- ggplot(DER, aes(x=DER$DER_mass_vis, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p3 <- ggplot(DER, aes(x=DER$DER_pt_h, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p4 <- ggplot(DER, aes(x=DER$DER_deltaeta_jet_jet, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p5 <- ggplot(DER, aes(x=DER$DER_mass_jet_jet, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p6 <- ggplot(DER, aes(x=DER$DER_prodeta_jet_jet, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p7 <- ggplot(DER, aes(x=DER$DER_deltar_tau_lep, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p8 <- ggplot(DER, aes(x=DER$DER_pt_tot, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p9 <- ggplot(DER, aes(x=DER$DER_sum_pt, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p10 <- ggplot(DER, aes(x=DER$DER_pt_ratio_lep_tau, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p11 <- ggplot(DER, aes(x=DER$DER_met_phi_centrality, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p12 <- ggplot(DER, aes(x=DER$DER_lep_eta_centrality, colour=LABEL, fill = LABEL)) + geom_density(alpha=.3)
p13 <- ggplot(PRI, aes(x=PRI[,1], colour = LABEL, fill = LABEL)) + xlab("PRI_tau_pt") + geom_density(alpha=.3)
p14 <- ggplot(PRI, aes(x=PRI[,2], colour = LABEL, fill = LABEL)) + xlab("PRI_tau_eta") + geom_density(alpha=.3)
p15 <- ggplot(PRI, aes(x=PRI[,3], colour = LABEL, fill = LABEL)) + xlab("PRI_tau_phi") + geom_density(alpha=.3)
p16 <- ggplot(PRI, aes(x=PRI[,4], colour = LABEL, fill = LABEL)) + xlab("PRI_lep_pt") + geom_density(alpha=.3)
p17 <- ggplot(PRI, aes(x=PRI[,5], colour = LABEL, fill = LABEL)) + xlab("PRI_lep_eta") + geom_density(alpha=.3)
p18 <- ggplot(PRI, aes(x=PRI[,6], colour = LABEL, fill = LABEL)) + xlab("PRI_lep_phi") + geom_density(alpha=.3)
p19 <- ggplot(PRI, aes(x=PRI[,7], colour = LABEL, fill = LABEL)) + xlab("PRI_met") + geom_density(alpha=.3)
p20 <- ggplot(PRI, aes(x=PRI[,8], colour = LABEL, fill = LABEL)) + xlab("PRI_met_phi") + geom_density(alpha=.3)
p21 <- ggplot(PRI, aes(x=PRI[,9], colour = LABEL, fill = LABEL)) + xlab("PRI_met_sumet") + geom_density(alpha=.3)
p22 <- ggplot(PRI, aes(x=PRI[,10], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_num") + geom_density(alpha=.3)
p23 <- ggplot(PRI, aes(x=PRI[,11], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_leading_pt") + geom_density(alpha=.3)
p24 <- ggplot(PRI, aes(x=PRI[,12], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_leading_eta") + geom_density(alpha=.3)
p25 <- ggplot(PRI, aes(x=PRI[,13], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_leading_phi") + geom_density(alpha=.3)
p26 <- ggplot(PRI, aes(x=PRI[,14], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_subleading_pt") + geom_density(alpha=.3)
p27 <- ggplot(PRI, aes(x=PRI[,15], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_subleading_eta") + geom_density(alpha=.3)
p28 <-ggplot(PRI, aes(x=PRI[,16], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_subleading_phi") + geom_density(alpha=.3)
p29 <-ggplot(PRI, aes(x=PRI[,17], colour = LABEL, fill = LABEL)) + xlab("PRI_jet_all_pt") + geom_density(alpha=.3)

pdf(file = "Density Plots",width = 12,height = 12)
grid.arrange(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29, ncol = 5, nrow =6)
#dev.off()


################
## PCA   #######
################
require(devtools)
install_github("ggbiplot", "vqv")
library(ggbiplot)
library(randomForest)


# apply PCA, remember to scale and center to work with normalized data
tt10.pca <- prcomp(matrix.x, center = TRUE, scale. = TRUE)

# plot the variances along each PC
plot(tt10.pca, type = "l", main = "Principal Variances")

summary(tt10.pca)   ###take pc 8

reduced0<-(tt10.pca$x)[,1:11]

View(tt10.pca$x)

summary(tt10.pca)

rotation<-tt10.pca$rotation
center<-function(matrix){
  col<-ncol(matrix)
  row<-nrow(matrix)
  for(i in 1:col){
    matrix[,i]<-matrix[,i]-mean(matrix[,i])
  }
  return(matrix)
}
center_mxx<-center(matrix.x)
c<-as.matrix(center_mxx)%*%as.matrix(rotation)

reduced_dimension<-c[,1:8]

write.csv(reduced_dimension,file = "reduced")  ###for knn



# Fancier Biplot
p <- ggbiplot(tt10.pca, obs.scale = 1, var.scale = 1, groups = Y_10, ellipse = TRUE, circle = TRUE)
p <- p + scale_color_discrete(name = '')
p <- p + theme(legend.direction = 'horizontal', legend.position = 'top')
p

# 3d visualization, red is the signal
library(rgl)

plot3d(c[,1:3],col =as.integer(Y_10))
x<-c[,1]
y<-c[,2]
z<-c[,3]
ellips <- ellipse3d(cov(cbind(x,y,z)), 
                    centre=c(mean(x), mean(y), mean(z)), level = 0.95)
plot3d(ellips, col = "#D95F02", alpha = 0.3, add = TRUE, type = "wire")



####  featurlization
####missing percentage
######################

library(DataComputing)
library(ggplot2)
ttt_gather<-ttt[,-32]%>%gather(key,value,-Weight)
ggplot(aes(x=key),data = ttt_gather)+geom_hist()+facet_wrap(~key)


miss_pertg<-function(col){
  a<-(col==999|col==-999)
  p<-sum(a)/length(col)
  return(p)
}
missing_percentage<-data.frame(variables=colnames(ttt))
missing_pc<-c()
for (i in 1:ncol(ttt)) {
  append<-miss_pertg(ttt[,i])
  missing_pc<-c(missing_pc,append)
}
table<-cbind(missing_percentage,missing_pc)