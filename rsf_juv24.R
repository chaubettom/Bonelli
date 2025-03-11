setwd("C:/Users/Samovar/Documents/Salsepareille/Bonelli/Analyse juv avr 23")
data_rsf<-read.csv("combined_data_df.csv")
#Ici on regarde d'abord les données brutes
#on crée des variables discrètes les variable altitude est distance cours d'eau. 
data_rsf$distance_river_min_discr <- (cut(data_rsf$distance_river_min,
                                                    breaks=seq(min(data_rsf$distance_river_min),
                                                               max(data_rsf$distance_river_min),100),labels=F)*100)-50
data_rsf$distance_zh_min_discr <- (cut(data_rsf$distance_zh_min,
                                          breaks=seq(min(data_rsf$distance_zh_min,na.rm = T),
                                                     max(data_rsf$distance_zh_min,na.rm = T),100),labels=F)*100)-50
data_rsf$altitude_discr <- (cut(data_rsf$altitude,
                                       breaks=seq(min(data_rsf$altitude,na.rm = T),
                                                  max(data_rsf$altitude,na.rm = T),50),labels=F)*50)-25
# On calcul les proportions d'utilisé/pas utilisé par habitat
prop_habitat <- data_rsf %>%
  group_by(habitat, Utilise) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()

# On calcul les proportions d'utilisé/pas utilisé par region
prop_region <- data_rsf %>%
  group_by(region, Utilise) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count)) %>%
  ungroup()
#On regarde les proportions de utilisé/pas utilisé pour chaque variable habitat
ggplot(prop_habitat, aes(x = habitat, y = proportion, fill = Utilise)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Proportion", x = "Habitat", fill = "Utilise") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
ggplot(prop_region, aes(x = region, y = proportion, fill = Utilise)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(y = "Proportion", x = "Region", fill = "Utilise") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

ggplot(data_rsf,aes(x=distance_river_min_discr,fill=Utilise))+
  geom_bar(position="fill",width = 80)+ylab("Proportion")
ggplot(data_rsf,aes(x=distance_zh_min_discr,fill=Utilise))+
  geom_bar(position="fill",width = 80)+ylab("Proportion")
ggplot(data_rsf,aes(x=altitude_discr,fill=Utilise))+
  geom_bar(position="fill",width = 80)+ylab("Proportion")

# modèle nul 
rsf_null <- glm(Utilise_binary ~ 1, family = binomial, data = data_rsf)
#Modèle altitude distance cours d'eau habitat. 
model_test <- glm(Utilise_binary ~ habitat+distance_zh_min_discr+altitude, family = binomial, data = data_rsf)

#autres modèles
rsf_model_1 <- glm(Utilise_binary ~ habitat + scale(altitude) + scale(distance_zh_min) * region + scale(distance_zh_min2) * region, 
                   family = binomial, data = data_rsf)
rsf_model_2 <- glm(Utilise_binary ~ scale(altitude) + scale(distance_zh_min) * region + scale(distance_zh_min2) * region, 
                   family = binomial, data = data_rsf)
rsf_model_3 <- glm(Utilise_binary ~ scale(distance_zh_min) + scale(distance_zh_min2), 
                   family = binomial, data = data_rsf)
rsf_model_4 <- glm(Utilise_binary ~ scale(distance_zh_min) * region + scale(distance_zh_min2) * region, 
                   family = binomial, data = data_rsf)
rsf_model_5 <- glm(Utilise_binary ~ habitat, 
                   family = binomial, data = data_rsf)
rsf_model_6 <- glm(Utilise_binary ~ scale(distance_zh_min), 
                   family = binomial, data = data_rsf)
rsf_model_7 <- glm(Utilise_binary ~ scale(altitude), 
                   family = binomial, data = data_rsf)
rsf_model_8 <- glm(Utilise_binary ~ habitat + scale(altitude), 
                   family = binomial, data = data_rsf)

# AIC, log-vraisemblance, paramètres
models <- list(rsf_null, rsf_model_1, rsf_model_2, rsf_model_3, rsf_model_4, rsf_model_5, rsf_model_6, rsf_model_7, rsf_model_8)
model_names <- c("Modèle Nul", "Modèle 1", "Modèle 2", "Modèle 3", "Modèle 4", "Modèle 5", "Modèle 6", "Modèle 7", "Modèle 8")
aic_values <- sapply(models, AIC)
logLik_values <- sapply(models, logLik)
k_values <- sapply(models, function(m) length(coef(m)))  # Nombre de paramètres estimés (k)
# delta AIC et des poids d'Akaike
min_aic <- min(aic_values)
delta_aic <- aic_values - min_aic  # Delta AIC
aic_weights <- exp(-0.5 * delta_aic) / sum(exp(-0.5 * delta_aic))  # Poids d'Akaike

results <- data.frame(
  Model = model_names,
  AIC = aic_values,
  Delta_AIC = delta_aic,
  AIC_Weight = aic_weights,
  LogLik = logLik_values,
  Parameters = k_values
)

# AIC croissant
results <- results[order(results$AIC), ]
print(results)

# AIC corrigé (AICc)
aic_c <- function(aic, n, k) {
  return(aic + (2 * k * (k + 1)) / (n - k - 1))
}
aic_values <- sapply(models, AIC)
n <- nrow(data_rsf)  # Nombre d'observations
aicc_values <- sapply(1:length(models), function(i) aic_c(aic_values[i], n, k_values[i]))
results$AICc <- aicc_values
print(results)

# odds ratios pour le modèle 8
odds_ratios <- exp(coef(rsf_model_8))
confint_values <- exp(confint(rsf_model_8))
odds_ratio_table <- data.frame(
  Coefficient = names(odds_ratios),
  OddsRatio = odds_ratios,
  CI_Lower = confint_values[, 1],  # Limite inférieure de l'intervalle de confiance
  CI_Upper = confint_values[, 2]   # Limite supérieure de l'intervalle de confiance
)
print(odds_ratio_table) # intercept > urbain dense ; habitat_14 > vergers ; habitat_16 > forets de feuillus ; habitat_18 > pelouses ; 
# habitat_19 > landes

#corrélation distance mini rivières (cours d'eau.shp) et distance mini surf_elemen (surf_elem.shp : cours d'eau + étangs)
correlation <- cor(data_rsf$distance_river_min, data_rsf$distance_zh_min, use = "complete.obs", method = "pearson")
print(paste("Corrélation Pearson:", correlation)) #à revoir!
