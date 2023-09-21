rm(list=ls) # Supprimer toutes les variables stockées précédemment
library(Hmisc) # importation

data <- read.csv("D:/Data analytics projects/covid_R/COVID19_line_list_data.csv")
describe(data) # Hmisc command

# nettoyée la colonne de "death"
data$death_dummy <- as.integer(data$death != 0)
# death rate
sum(data$death_dummy) / nrow(data)

# ÂGE
# Allégation : Les personnes qui meurent sont plus âgées
dead = subset(data, death_dummy == 1)
alive = subset(data, death_dummy == 0)
mean(dead$age, na.rm = TRUE) #48.1%
mean(alive$age, na.rm = TRUE) #68.6%
# Est-ce statistiquement significatif?
t.test(alive$age, dead$age, alternative= "two.sided", conf.level = 0.99)
# Normalement, si la valeur de p < 0,05, nous rejetons l’hypothèse nulle
# Ici, la valeur de p ~ 0, nous rejetons donc l’hypothèse nulle et
# conclure que cela est statistiquement significatif

# SEXE
# Allégation : le sexe n’a aucun effet
men = subset(data, gender == "male")
women = subset(data, gender == "female")
mean(men$death_dummy, na.rm = TRUE) #8.5%
mean(women$death_dummy, na.rm = TRUE)#3.7%
# Est-ce statistiquement significatif?
t.test(men$death_dummy, women$death_dummy, alternative= "two.sided", conf.level = 0.99)
# 99% de confiance: les hommes ont de 0,8% à 8,8% plus de chances
# de mourir.
# Valeur de p = 0,002 < 0,05, ce qui est statistiquement 
# significatif

