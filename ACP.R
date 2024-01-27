library(readr)
library(tidyverse)
library(summarytools)
library(FactoMineR)
library(Factoshiny)
library(summarytools)
library(cowplot)

rm(list=ls())


waterQuality <- read_csv("Master ISN/Méthode Apprentissage/Projet/waterQuality.csv")%>%
  na.omit()%>%
  mutate(is_safe=as.factor(is_safe))

X=waterQuality%>%select(-"is_safe")
y=waterQuality%>%select("is_safe")

# Matrice de corrélation ------------
library(corrplot)
corrplot(cor(X), type="lower", order="hclust", tl.col="black", tl.srt=45)
View(cor(X))

# histogrammes -------------------
ggplot(waterQuality)+
  geom_histogram(aes(aluminium),col="darkgray", fill="gray", bins=30)+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))+
  theme_minimal()


ggplot(waterQuality)+
  geom_histogram(aes(arsenic),col="darkgray", fill="gray", bins=30)+
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12))+
  theme_minimal()



# ACP ---------------

res.PCA<-PCA(waterQuality,quali.sup=c(21),graph=FALSE)

plot.PCA(res.PCA,choix='var')

coord = data.frame(res.PCA$ind$coord, is_safe=waterQuality$is_safe)

## graphe indiv
all=ggplot(coord)+
  geom_point(aes(Dim.1,Dim.2,color=is_safe))+
  scale_color_manual(values = c("black","red"))+
  labs(x = "Dim 1 (20.61%)", y = "Dim 2 (8.39%)") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12),
        legend.position = "bottom") +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()

## graphe indiv is_safe
is_safe=ggplot(coord%>%filter(is_safe==1))+
  geom_point(aes(Dim.1,Dim.2,color=is_safe))+
  scale_color_manual(values = c("red"))+
  labs(x = "Dim 1 (20.61%)", y = "Dim 2 (8.39%)") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()


## graphe indiv no_safe
no_safe=ggplot(coord%>%filter(is_safe==0))+
  geom_point(aes(Dim.1,Dim.2,color=is_safe))+
  scale_color_manual(values = c("black"))+
  labs(x = "Dim 1 (20.61%)", y = "Dim 2 (8.39%)") +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 12)) +
  geom_vline(xintercept = 0, color = "black", linetype = "dashed")+
  geom_hline(yintercept = 0, color = "black", linetype = "dashed")+
  theme_minimal()
