library(lme4)
library(readxl)
library(car)
library(emmeans)
data <- read_excel("harpo.xlsx")
data$Usuario<-as.factor(data$Usuario)
data$parrafo<-as.factor(data$parrafo)
data$modelo<-as.factor(data$modelo)
str(data)
M1 <- glmer(preferecnia ~  modelo+
             (1 |parrafo)+
              (1 |Usuario)+
             (1 |parrafo:modelo)+
              (1 |parrafo:Usuario)+
              (1 |Usuario:modelo), data = data, family = binomial)
summary(M1)
Anova(M1, type=3)
lsm<-emmeans(M1, pairwise~ modelo, type="response")
confint(lsm)
