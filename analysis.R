return <- read.csv("path_to_your_data.csv")

# Kategorik de??i??kenleri fakt??r olarak tan??mlay??n
return$gender <- as.factor(return$gender)
return$education <- as.factor(return$education)
return$new_marital_status <- as.factor(return$new_marital_status)
return$legal_status <- as.factor(return$legal_status)
return$job <- as.factor(return$job)
return$health <- as.factor(return$health)
return$disability <- as.factor(return$disability)
return$care_need <- as.factor(return$care_need)
return$language_skill <- as.factor(return$language_skill)
return$social_work <- as.factor(return$social_work)
return$kizilay_card <- as.factor(return$kizilay_card)
return$return <- as.factor(return$return)
return$nursing_home <- as.factor(return$nursing_home)


result_model <- svyglm(nursing_home ~ gender + age + education + new_marital_status + 
                         legal_status + financial_syria + financial_turkiye + job + 
                         health + disability + care_need + language_skill + 
                         social_work + kizilay_card + return + districts + 
                         stigma + discrimination + medicine, 
                       design=design_ps)

# Sonu??lar?? g??r??nt??leyin
summary(result_model)
# new_marital_status'da 5 olan sat??rlar?? ????kar??n
return <- return[return$new_marital_status != 5, ]
# disability'de 7 olan sat??rlar?? ????kar??n
return <- return[return$disability != 7, ]


# Temel model: Demografik de??i??kenler
model1 <- svyglm(nursing_home ~ gender + age + new_marital_status, design = design_ps)
summary(model1)

# new_marital_status'ta 5 olan sat??rlar?? g??r??nt??leyin
subset(return, new_marital_status == 5)

# new_marital_status'taki benzersiz de??erleri kontrol edin
unique(return$new_marital_status)

# new_marital_status'ta 5 olan sat??rlar?? ????kar??n
return <- return[return$new_marital_status != 5, ]
# Gereksiz fakt??r seviyelerini kald??r??n
return$new_marital_status <- droplevels(return$new_marital_status)

# De??i??iklikleri kontrol edin
unique(return$new_marital_status)



# Fakt??r seviyelerini yeniden s??ralama (iste??e ba??l??)
return$new_marital_status <- factor(return$new_marital_status, levels = c(1, 2, 3))


# NA olan sat??rlar?? veri setinden kald??r??n
return <- return[!is.na(return$new_marital_status), ]
# Fakt??r seviyelerini yeniden d??zenleyin
return$new_marital_status <- factor(return$new_marital_status, levels = c(1, 2, 3)
                                                                          
summary(return)                                                                         
                                                                      
rm(return)


summary(return)


return$new_marital_status <- ifelse(return$marital_status == 1, 1, 2)  # ??rnek bir atama

rm(return)




# Temel model: Demografik de??i??kenler
model1 <- svyglm(nursing_home ~ gender + age + marital_status, design = design_ps)
summary(model1)
# Model 2: Demografik ve sosyoekonomik de??i??kenler
model2 <- svyglm(nursing_home ~ gender + age + marital_status + education + financial_syria + financial_turkiye + job, design = design_ps)
summary(model2)

# Model 3: Demografik, sosyoekonomik, sa??l??k ve sosyal destek de??i??kenleri
model3 <- svyglm(nursing_home ~ gender + age + marital_status + education + financial_syria + financial_turkiye + job + health + disability + care_need + social_work + kizilay_card, design = design_ps)
summary(model3)
# Model 4: T??m de??i??kenler
model4 <- svyglm(nursing_home ~ gender + age + marital_status + education + financial_syria + financial_turkiye + job + health + disability + care_need + social_work + kizilay_card + stigma + discrimination + medicine, design = design_ps)
summary(model4)
# Temel model: Demografik de??i??kenler
model1_return <- svyglm(return ~ gender + age + marital_status, design = design_ps)
summary(model1_return)



# return de??i??kenini numeric hale getirin
return$return <- as.numeric(as.character(return$return))




# NA/NaN de??erlerini kontrol edin
sum(is.na(return$return))
# Eksik de??erleri i??eren sat??rlar?? ????kar??n
return <- return[!is.na(return$return), ]
# return de??i??keninin s??n??f??n?? kontrol edin
class(return$return)
# return de??i??kenini say??sal hale getirin
return$return <- as.numeric(as.character(return$return))
# Temel model: Demografik de??i??kenler
model1_return <- svyglm(return ~ gender + age + marital_status, design = design_ps)
summary(model1_return)


# Veri setinin ismini de??i??tirme
mydata <- return
# return de??i??kenini say??sal hale getirin
mydata$return <- as.numeric(as.character(mydata$return))
# NA/NaN de??erlerini kontrol edin
sum(is.na(mydata$return))
# Temel model: Demografik de??i??kenler
design_ps_new <- svydesign(ids=~1, weights=~weights, data=mydata)

model1_return <- svyglm(return ~ gender + age + marital_status, design = design_ps_new)
summary(model1_return)
summary(mydata)

library(MatchIt)
ps_model <- glm(return ~ age + gender + education + legal_status + financial_turkiye + language_skill + discrimination, family = binomial(link = "logit"), data = mydata)
summary(ps_model)

match <- matchit(return ~ age_arranged + gender + education + marital_status + legal_status + financial_turkiye + health + care_need + language_skill + social_work + kizilay_card + discrimination, method = "nearest", data = mydata)
summary(match)

matched_data <- match.data(match)
model_psm <- glm(return ~ age_arranged + gender + education + marital_status + legal_status + financial_turkiye + health + care_need + language_skill + social_work + kizilay_card + discrimination, family = binomial(link = "logit"), data = matched_data)
summary(model_psm)

matched_data <- match.data(match, weights = "psm_weights")
model_psm <- glm(return ~ age_arranged + gender + education + marital_status + legal_status + financial_turkiye + health + care_need + language_skill + social_work + kizilay_card + discrimination, family = binomial(link = "logit"), data = matched_data)
summary(model_psm)

plot(summary(match))

library(car)
vif(model_psm)
model_psm_interaction <- glm(return ~ age_arranged * gender + education * financial_turkiye + health * care_need, family = binomial(link = "logit"), data = matched_data)
summary(model_psm_interaction)

library(logistf)
model_psm_firth <- logistf(return ~ age_arranged * gender + education * financial_turkiye + health * care_need, data = matched_data)
summary(model_psm_firth)
library(haven)
write.csv(mydata, "return.csv", row.names = FALSE)



# Gerekli k??t??phaneyi y??kleyelim
library(pscl)  # Pseudo R-squared and Log-likelihood measures
library(DescTools)  # Goodness-of-fit tests

# Modeli olu??turmak i??in glm fonksiyonunu kullan??yoruz
model <- glm(nursing_home ~ gender + age_arranged + new_marital_status + new_arrival + legal_status + return + discrimination, 
             data = mydata, 
             family = binomial)


model_4 <- glm(nursing_home ~ gender + age_arranged + new_marital_status + new_arrival + legal_status + return + discrimination, 
               data = mydata, 
               family = binomial)


model_3 <- glm(nursing_home ~ return + discrimination, 
               data = mydata, 
               family = binomial)


model_2 <- glm(nursing_home ~ new_arrival + legal_status, 
               data = mydata, 
               family = binomial)


model_1 <- glm(nursing_home ~ gender + age_arranged + new_marital_status, 
               data = mydata, 
               family = binomial)
library(pROC)



# ROC e??rilerini olu??turma
roc_model_1 <- roc(mydata$nursing_home, fitted(model_1))
roc_model_2 <- roc(mydata$nursing_home, fitted(model_2))
roc_model_3 <- roc(mydata$nursing_home, fitted(model_3))
roc_model_4 <- roc(mydata$nursing_home, fitted(model_4))








# Grafiklerin yerle??imini ayarlama
par(mfrow = c(2, 2))  # 2x2 grid layout (4 panel)

# Her bir model i??in ROC e??risini ??izme
plot(roc_model_1, main="Model I", col="blue", lwd=2)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_1), 3)))

plot(roc_model_2, main="Model II", col="green", lwd=2)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_2), 3)))

plot(roc_model_3, main="Model III", col="purple", lwd=2)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_3), 3)))

plot(roc_model_4, main="Model IV", col="orange", lwd=2)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_4), 3)))

# Eski grafik ayarlar??na geri d??nme
par(mfrow = c(1, 1))

# ROC e??rilerini olu??turma
roc_model_1 <- roc(mydata$nursing_home, fitted(model_1))
roc_model_2 <- roc(mydata$nursing_home, fitted(model_2))
roc_model_3 <- roc(mydata$nursing_home, fitted(model_3))
roc_model_4 <- roc(mydata$nursing_home, fitted(model_4))

# Grafiklerin yerle??imini ayarlama
par(mfrow = c(2, 2))  # 2x2 grid layout (4 panel)

# Her bir model i??in ROC e??risini ??izme
plot(roc_model_1, main="Model I", col="blue", lwd=2, legacy.axes = TRUE)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_1), 3)))

plot(roc_model_2, main="Model II", col="green", lwd=2, legacy.axes = TRUE)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_2), 3)))

plot(roc_model_3, main="Model III", col="purple", lwd=2, legacy.axes = TRUE)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_3), 3)))

plot(roc_model_4, main="Model IV", col="orange", lwd=2, legacy.axes = TRUE)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_model_4), 3)))
















# Model ??zetini alal??m
summary(model)

# Pseudo R-squared de??erlerini ve Log-likelihood hesaplama
pR2(model)

# Odds Ratio'yu hesaplayal??m
odds_ratios <- exp(coef(model))
odds_ratios

# Model ??zeti
model_summary <- summary(model)

# Wald Ki-kare istatisti??ini hesaplama
wald_statistic <- (model_summary$coefficients[, "Estimate"] / model_summary$coefficients[, "Std. Error"])^2
wald_statistic

# Odds Ratio'lar?? ve Wald Ki-kare de??erlerini bir tablo olarak g??sterme
results <- data.frame(
  Variable = rownames(model_summary$coefficients),
  Odds_Ratio = odds_ratios,
  Wald_Chi_Square = wald_statistic,
  p_value = model_summary$coefficients[, "Pr(>|z|)"]
)

print(results)








# AIC ve BIC de??erlerini hesaplayal??m
AIC_value <- AIC(model)
BIC_value <- BIC(model)

# Log-Likelihood de??erini hesaplayal??m
logLik_value <- logLik(model)

# Sonu??lar?? yazd??rma
cat("AIC: ", AIC_value, "\n")
cat("BIC: ", BIC_value, "\n")
cat("Log-Likelihood: ", logLik_value, "\n")





# Paketi y??kleme
library(ResourceSelection)

# Hosmer-Lemeshow Testi'ni uygulama
HL_test <- hoslem.test(model$y, fitted(model), g=10)

# Sonu??lar?? yazd??rma
print(HL_test)



# Hosmer-Lemeshow Uyum ??yili??i Testi
HL_test <- HoslemTest(model$y, fitted(model))
HL_test
library(pROC)
roc_curve <- roc(mydata$nursing_home, fitted(model))



jpeg("ROC_curve.jpg", width = 800, height = 600)
plot(roc_curve, main="ROC E??risi", col="blue", lwd=2)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_curve), 3)))




dev.off()





library(broom)
tidy_model <- tidy(model, conf.int = TRUE)
tidy_model <- tidy_model[!is.na(tidy_model$estimate), ]  # NA de??erleri kald??r??yoruz

ggplot(tidy_model, aes(x = term, y = exp(estimate))) +
  geom_point() +
  geom_errorbar(aes(ymin = exp(conf.low), ymax = exp(conf.high)), width = 0.2) +
  coord_flip() +
  labs(y = "Odds Ratios", x = "De??i??kenler", 
       title = "Lojistik Regresyon Sonu??lar?? - Odds Oranlar??") +
  theme_minimal()
HL_test <- HoslemTest(model$y, fitted(model))

# Plotting Observed vs Predicted
observed <- HL_test$observed
expected <- HL_test$expected

plot(observed, expected, pch=16, col="blue", 
     main="Observed vs. Predicted",
     xlab="Observed", ylab="Predicted")
abline(a=0, b=1, col="red", lwd=2)



r2_values <- data.frame(
  Metric = c("McFadden", "r2ML", "r2CU"),
  Value = c(0.04191208, 0.05620320, 0.07509221)
)

library(ggplot2)
ggplot(r2_values, aes(x = Metric, y = Value)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  theme_minimal() +
  labs(title = "Pseudo R-squared De??erleri",
       x = "Metrik",
       y = "De??er")



model <- glm(nursing_home ~ gender + age_arranged + new_marital_status, 
             data = mydata, 
             family = binomial)



library(pROC)
roc_curve <- roc(mydata$nursing_home, fitted(model))

plot(roc_curve, main="ROC E??risi", col="blue", lwd=2)
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_curve), 3)))


# ROC e??risini olu??turma
roc_curve <- roc(mydata$nursing_home, fitted(model))

# ROC e??risini ??izme ve eksen etiketlerini ??zelle??tirme
plot(roc_curve, main="ROC E??risi", col="blue", lwd=2, 
     xlab="1 - Specificity (??zg??ll??k)", ylab="Sensitivity (Duyarl??l??k)")
abline(a=0, b=1, lty=2, col="red")
text(0.5, 0.4, paste("AUC = ", round(auc(roc_curve), 3)))
Sys.setlocale("LC_ALL", "Turkish")


devtools::install_github("unhcr-dataviz/unhcrthemes")
library(ggplot2)
library(unhcrthemes)


autoplot(Refugee)

