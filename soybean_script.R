####ANOVA####
rm(list = ls());ls ()
dados<- read.table("soybean_data.txt", header = T)
summary(dados)
attach(dados)
dados$genotype<- as.factor(dados$genotype)
dados$block<- as.factor(dados$block)
dados$group<-as.factor(dados$group)
dados$gen<- as.factor(dados$gen)
parent<-(dados$parent)
hybrid<- (dados$hybrid)
str(dados)

fm1 <- lm(hsw ~ block + gen + group + genotype:parent + genotype:hybrid, data = dados)
anova(fm1)

#ANOVA for all traits
formula <- as.formula(paste0("cbind(", paste(names(dados)[9:ncol(dados)], collapse = ","), ") ~ block + gen + (group+ genotype:parent + genotype:hybrid)^2"))
fit <- aov(formula, data=dados)
summary(fit)
#capture.output(summary(fit),file="anovas.txt")

####MEANS####

rm(list = ls());ls ()
dados<- read.table("soybean_data.txt", header = T)

#combined means
genotype<- dados$genotype
traits <- names(dados)[9:18]
traits
dados.p<-data.frame()
for (i in traits) {
  Y<-dados[,i]
  dados.p <-tapply(Y, genotype, mean)
    print(i)
  print(dados.p)
}

#separate means
dados$separa <- dados$gen > 3
dadosf3<- dados[dados$separa == FALSE,]
dadosf4<- dados[dados$separa == TRUE,]

#means f3
rm(list = ls());ls ()
dados<- dadosf3
parent<- dados$parent
for (i in traits) {
  Y<-dados[,i]
  dados.p <-tapply(Y, parent, mean)
  print(i)
  print(dados.p)
}
#means f4
rm(list = ls());ls ()
dados<- dadosf4
parent<- dados$parent
for (i in traits) {
  Y<-dados[,i]
  dados.p <-tapply(Y, parent, mean)
  print(i)
  print(dados.p)
}

####Diallel - Griffing's Aproach Method 2 & Model 1 ####
rm(list = ls());ls ()
dados<- read.table("diallel_f3.txt", header = T)##do with diallel_f4.txt file
summary(dados)
str(dados)
require(DiallelAnalysisR)
Griffing1Data2 <-
  Griffing(
    y      = hsw
    , Rep    = Rep
    , Cross1 = Cross1
    , Cross2 = Cross2
    , data   = dados
    , Method = 2
    , Model  = 2
  )
names(Griffing1Data2)
Griffing1Data2


####Correlation####

rm(list = ls());ls ()
mydata<- read.table("soybean_data.txt", header = T)
summary(mydata)
str(mydata)
mydata$ndr6<-as.numeric(mydata$ndr6)
mydata$sgr6<-as.numeric(mydata$sgr6)
mydata$ndm<-as.numeric(mydata$ndm)
mydata<-mydata[9:ncol(mydata)]
mydata<-as.matrix(mydata)
cormat <- round(cor(mydata),2)
correlation1<- rcorr(as.matrix(mydata), type= "pearson")


####Genotype by trait####

rm(list = ls());ls ()
dados<- read.table("means_pca.txt", header = T)
groups<-as.factor(dados$group)
library(ggplot2)
library(ggord)
ord <- prcomp(dados[, 2:10], scale. = TRUE)
row.names(dados)<-dados$genotype
tiff("gen-by-trait-new5.jpg", width = 40, height = 40, res = 300, units = "cm")
ggord(ord, groups, obslab=TRUE, arrow = 0.3, size =8, txt=8,
      vec_ext = 5, veclsz = 0.9, ellipse = TRUE, repel= TRUE) + theme(panel.grid = element_blank()) +
  theme(axis.title = element_text(size = 26)) + theme(axis.text = element_text(size = 24)) +
  theme(legend.text = element_text(size = 24)) + theme(legend.title = element_text(size = 26))
dev.off()
summary(ord)
