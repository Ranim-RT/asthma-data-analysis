##############################################################
#          PROJET R (MP-DS1 Group A) : Ranim Tobji 
##############################################################


## Nettoyage de l'environnement :
rm(list = ls())


##  Chargement des packages nécessaires :

install.packages("tidyverse")
library(tidyverse)

install.packages("dplyr")
library(dplyr)

install.packages("ggplot2")    # ggplot2 → visualisation graphique
library(ggplot2)

install.packages("GGally")     # GGally → matrice de graphiques (ggpairs)
library(GGally) 

install.packages("reshape2")   # reshape2 → transformer les matrices en format long (melt)
library(reshape2)

install.packages("scales")     # scales → formater les axes et les labels (ex : pourcentages)
library(scales)


##    Importation du Dataset :
df_asthma <- read_csv("C:\\Users\\ranim\\Downloads\\asthma_disease_data.csv")



##############################################################
#          PARTIE 1 : EXPLORATION DES DONNÉES :
##############################################################


# -- Aperçu des données 

view(df_asthma)        # Affichage du data
head(df_asthma)        # Premières lignes
tail(df_asthma)        # Dernières lignes


# -- Dimensions du dataset

dim(df_asthma)   # lignes + colonnes
nrow(df_asthma)  # nombre de lignes
ncol(df_asthma)  # nombre de colonnes


# -- Structure du dataset
str(df_asthma)      


# -- Résumé statistique du dataset
summary(df_asthma)


# -- Vérifier les valeurs manquantes (NA)
colSums(is.na(df_asthma))   # Nbre de NA par colonne


# -- Vérifier les doublons
sum(duplicated(df_asthma))




##############################################################
#            PARTIE 2 : TRANSFORMATION DES DONNÉES :
##############################################################


asthma_new <- df_asthma      # Copie du dataset pour appliquer les modifications


# 1. Supprimer les colonnes inutiles ( PatientID - DoctorInCharge )

asthma_new <- asthma_new %>% select(-PatientID, -DoctorInCharge)   


# 2. Recodage des variables 0/1 en labels descriptifs 

asthma_new <- asthma_new %>%
  mutate(
    Gender = factor(Gender, levels = c(0,1), labels = c("Male", "Female")),
    Smoking = factor(Smoking, levels = c(0,1), labels = c("No", "Yes")),
    PetAllergy = factor(PetAllergy , levels = c(0,1), labels = c("No", "Yes")),
    FamilyHistoryAsthma = factor(FamilyHistoryAsthma, levels = c(0,1), labels = c("No", "Yes")),
    HistoryOfAllergies = factor(HistoryOfAllergies, levels = c(0,1), labels = c("No", "Yes")),
    Eczema = factor(Eczema, levels = c(0,1), labels = c("No", "Yes")),
    HayFever = factor(HayFever, levels = c(0,1), labels = c("No", "Yes")),
    GastroesophagealReflux = factor(GastroesophagealReflux, levels = c(0,1), labels = c("No", "Yes")),
    Wheezing = factor(Wheezing, levels = c(0,1), labels = c("No", "Yes")),
    ShortnessOfBreath = factor(ShortnessOfBreath, levels = c(0,1), labels = c("No", "Yes")),
    ChestTightness = factor(ChestTightness, levels = c(0,1), labels = c("No", "Yes")),
    Coughing = factor(Coughing, levels = c(0,1), labels = c("No", "Yes")),
    NighttimeSymptoms = factor(NighttimeSymptoms, levels = c(0,1), labels = c("No", "Yes")),
    ExerciseInduced = factor(ExerciseInduced, levels = c(0,1), labels = c("No", "Yes")),
    Diagnosis = factor(Diagnosis, levels = c(0,1), labels = c("No", "Yes"))
  )



# 3. Recodage des colonnes Ethnicity et EducationLevel 

asthma_new <- asthma_new %>%
  mutate(
    Ethnicity = factor(Ethnicity, levels = c(0,1,2,3), labels = c("Caucasian", "African American", "Asian", "Other")),
    EducationLevel = factor(EducationLevel, levels = c(0,1,2,3), labels = c("None", "High School", "Bachelor's", "Higher"))
  )



# 4. Creation des tranches d'ages 

asthma_new <- asthma_new %>%
  mutate(
    age_group = case_when(
      Age < 13 ~ "Child",
      Age >= 13 & Age < 18 ~ "Teen",
      Age >= 18 & Age < 40 ~ "Young Adult",
      Age >= 40 & Age < 60 ~ "Adult",
      Age >= 60 ~ "Senior"
    )
  )



# 5. Creation des categories du BMI

asthma_new <- asthma_new %>%
  mutate(
    bmi_category = case_when(
      BMI < 18.5 ~ "Underweight",
      BMI >= 18.5 & BMI < 25 ~ "Normal",
      BMI >= 25 & BMI < 30 ~ "Overweight",
      BMI >= 30 ~ "Obese"
    )
  )



# 6. Changer les noms de colonnes 

asthma_new <- asthma_new %>%
  rename(
    PhysicalActivity_hour.week = PhysicalActivity,
    FEV1_L.sec = LungFunctionFEV1,
    FVC_L.sec = LungFunctionFVC    
  )



# 7. Creation d'une variable qui calcule le risque 

asthma_new <- asthma_new %>%
  mutate(
    risk_col = ifelse(BMI >= 30 & Smoking == "Yes", "High Risk", "Normal Risk")
  )



# 8. BOUCLE FOR : Calcul automatique des moyennes pour plusieurs colonnes

moy_cols <- c("Age", "BMI", "FVC_L.sec", "FEV1_L.sec")

for (col in moy_cols){
  print(paste("Moyenne de", col, "=", mean(asthma_new[[col]], na.rm = TRUE)))
}



# 9. Fonction personnalisée pour afficher les proportions des valeurs d'une colonne

proportions <- function(df, variable){
  print(prop.table(table(df[[variable]])))
}

# Exemple Application
proportions(asthma_new, "Smoking")
proportions(asthma_new, "age_group")



# 10. Fonction appliquée sur chaque groupe d'âge (affiche les proportions de quelques critères)

for(group in unique(asthma_new$age_group)){
  
  print(paste("Statistiques pour le groupe d'âge :", group))
  
  subset_age <- asthma_new %>% filter(age_group == group)
  
  proportions(subset_age, "Smoking")
  proportions(subset_age, "Diagnosis")
}



# 11. Trier le dataset selon l'âge de manière croissante

asthma_new <- asthma_new %>% arrange(Age)






##########################################################################
#        PARTIE 3 : ANALYSES STATISTIQUES & VISUALISATIONS :
##########################################################################


data <- asthma_new


#####  A. ANALYSES DEMOGRAPHIQUES  #####


# 1. Asthme par tranche d'âge

ggplot(data, aes(x = age_group, fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(title = "Prévalence de l'asthme par tranche d'âge",
       x = "Tranche d'âge", y = "Proportion", fill = "Asthme") +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()


# 2. Asthme par genre

ggplot(data, aes(x = Gender, fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(title = "Prévalence de l'asthme selon le genre",
       x = "Genre", y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal()


# 3. Asthme par ethnie

ggplot(data, aes(Ethnicity, fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(title = "Prévalence de l'asthme selon l'ethnie",
       x = "Ethnie", y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal()



#####  B. FACTEURS DE RISQUE & MODE DE VIE  #####


# 4. BMI categories vs Asthma

ggplot(data, aes(bmi_category, fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(title = "Asthme selon la catégorie de BMI",
       x = "Catégorie BMI", y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal()


# 5. Tabagisme vs Asthma

ggplot(data, aes(Smoking, fill = Diagnosis)) +
  geom_bar(position = "fill") +
  labs(title = "Impact du tabagisme sur l'asthme",
       x = "Fumeur ?", y = "Proportion") +
  scale_y_continuous(labels = percent) +
  theme_minimal()


# 6. Activité physique vs Asthma

ggplot(data, aes(Diagnosis, PhysicalActivity_hour.week, fill = Diagnosis)) +
  geom_boxplot() +
  labs(title = "Activité physique hebdomadaire selon diagnostic",
       x = "Asthme", y = "Heures / semaine") +
  theme_minimal()



#####  C. ALLERGIES & ANTÉCÉDENTS FAMILIAUX  #####  

allergy_vars <- data %>%
  select(HistoryOfAllergies, PetAllergy, HayFever, Eczema, 
         FamilyHistoryAsthma, Diagnosis)


# 7. Heatmap de toutes les allergies vs Asthma  : une carte thermique qui montre la relation entre toutes les allergies et le diagnostic d’asthme

tab_allergy <- apply(allergy_vars, 2, function(x) table(x, allergy_vars$Diagnosis))
heat_allergy <- melt(as.matrix(tab_allergy))

ggplot(heat_allergy, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title = "Relation allergies / antécédents avec l'asthme",
       x = "Variable", y = "Diagnostic") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal()



#####   D. SYMPTÔMES ASSOCIÉS À L’ASTHME   #####  

symptoms <- data %>%
  select(Wheezing, ShortnessOfBreath, ChestTightness,
         Coughing, NighttimeSymptoms, ExerciseInduced, Diagnosis)


# 8. Graphique multi-symptômes
tab_symptoms <- apply(symptoms, 2, function(x) table(x, symptoms$Diagnosis))
heat_symptoms <- melt(as.matrix(tab_symptoms))

ggplot(heat_symptoms, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  labs(title = "Symptômes les plus associés à l'asthme",
       x = "Symptômes", y = "Diagnostic") +
  scale_fill_gradient(low = "white", high = "darkred") +
  theme_minimal()



#####  E. FONCTION RESPIRATOIRE (FEV1, FVC)  #####  


# 9. FEV1 vs Asthma
ggplot(data, aes(Diagnosis, FEV1_L.sec, fill = Diagnosis)) +
  geom_boxplot() +
  labs(title = "FEV1 selon diagnostic d'asthme",
       x = "Asthme", y = "FEV1 (L)") +
  theme_minimal()

# 10. FVC vs Asthma
ggplot(data, aes(Diagnosis, FVC_L.sec, fill = Diagnosis)) +
  geom_boxplot() +
  labs(title = "FVC selon diagnostic d'asthme",
       x = "Asthme", y = "FVC (L)") +
  theme_minimal()

