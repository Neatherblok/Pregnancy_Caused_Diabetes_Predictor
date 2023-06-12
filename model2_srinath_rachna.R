Model2 <- glm(Diagnosis ~ Pregnancies + Age + Glucose + Insulin + BMI + SkinThickness + BloodPressure + Pedigree, data = final_data)
y_pred2 <- ifelse(predict(Model2, type = "response") > 0.5, 1, 0)
confusionMatrix2 <- table(Actual = final_data$Diagnosis, Predicted = y_pred2
precision2 <- confusionMatrix2[2, 2] / sum(confusionMatrix2[, 2])
recall2 <- confusionMatrix2[2, 2] / sum(confusionMatrix2[2, ])
f1_score2 <- 2 * (precision2 * recall2) / (precision2 + recall2)
support2 <- sum(confusionMatrix2)
summary(Model2)