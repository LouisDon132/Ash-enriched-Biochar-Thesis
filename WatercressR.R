##WATERCRESS

install.packages("car")
install.packages("multcomp")
install.packages("ggplot2")
install.packages("dplyr")
library(car)
library(multcomp)
library(ggplot2)
library(dplyr)
watercress <- data.frame(
  
# olumn 1: Treatment names 

  Treatment = c(
    "Control",          "Control",          "Control",
    "Biochar_450",      "Biochar_450",      "Biochar_450",
    "AshEnriched_450",  "AshEnriched_450",  "AshEnriched_450",
    "Biochar_600",      "Biochar_600",      "Biochar_600",
    "AshEnriched_600",  "AshEnriched_600",  "AshEnriched_600",
    "Biochar_700",      "Biochar_700",      "Biochar_700",
    "AshEnriched_700",  "AshEnriched_700",  "AshEnriched_700"
  ),
  
#Column 2:Germination Rate (%)
#
  GR = c(
    60, 70, 60,         # Control:           Sand 1, Sand 2, Sand 3
    100, 70, 90,        # Biochar 450:       450N1, 450N2, 450N3
    50, 90, 70,         # Ash-enriched 450:  450C1, 450C2, 450C3
    60, 70, 80,         # Biochar 600:       600N1, 600N2, 600N3
    60, 60, 60,         # Ash-enriched 600:  600C1, 600C2, 600C3
    80, 70, 50,         # Biochar 700:       700N1, 700N2, 700N3
    80, 70, 80          # Ash-enriched 700:  700C1, 700C2, 700C3
  ),
  
#c olumn 3: Mean shoot length per dish (mm)
  # Each value is the average shoot length of germinated seeds in that dish
  Shoot = c(
    1.959,    1.4495,   1.0205, # Control
    8.2276,   13.6,     12.12811, # Biochar 450
    15.4604,  8.852111, 15.95657, # Ash-enriched 450
    10.82067, 8.306,    16.61738, # Biochar 600
    15.18033, 12.50367, 14.077,   # Ash-enriched 600
    11.167,   18.00357, 4.6325,   # Biochar 700
    16.40043, 19.60638, 16.35117  # Ash-enriched 700
  ),
  
#Column 4: Mean root length per dish (mm)
  Root = c(
    3.243333, 2.928429, 3.252,        # Control
    4.7484,   4.375714, 4.426778,     # Biochar 450
    5.696,    4.664556, 7.585571,     # Ash-enriched 450
    2.561333, 4.970571, 4.205375,     # Biochar 600
    6.381714, 3.678,    3.590429,     # Ash-enriched 600
    4.0165,   4.476,    2.1268,       # Biochar 700
    4.708857, 6.162625, 4.778286      # Ash-enriched 700
  )
)
watercress$Treatment <- factor(watercress$Treatment)
print(watercress)
watercress %>%
  group_by(Treatment) %>%
  summarise(
    n          = n(),
    Mean_GR    = round(mean(GR), 1),
    SD_GR      = round(sd(GR), 1),
    Mean_Shoot = round(mean(Shoot), 2),
    SD_Shoot   = round(sd(Shoot), 2),
    Mean_Root  = round(mean(Root), 2),
    SD_Root    = round(sd(Root), 2)
  )
#PLOT DATA - shoot

ggplot(watercress, aes(x = Treatment, y = Shoot, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Watercress Shoot Length by Treatment",
    y = "Mean Shoot Length (mm)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )
#Boxplot of ROOT LENGTH

ggplot(watercress, aes(x = Treatment, y = Root, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Watercress Root Length by Treatment",
    y = "Mean Root Length (mm)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )


#Boxplot of GERMINATION RATE

ggplot(watercress, aes(x = Treatment, y = GR, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Watercress Germination Rate by Treatment",
    y = "Germination Rate (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )




model_shoot <- aov(Shoot ~ Treatment, data = watercress)
shapiro.test(residuals(model_shoot))

#Visual check: Q-Q plot
  qqnorm(residuals(model_shoot), main = "Q-Q Plot: Shoot Length Residuals")
qqline(residuals(model_shoot), col = "red", lwd = 2)
# --- 6d. Test equality of variances (Levene's test) ---
leveneTest(Shoot ~ Treatment, data = watercress)

#View the ANOVA results
summary(model_shoot)
# If Pr(>F) < 0.05:
#significant difference between at least two treatments.
#Proceed to post-hoc tests.
#    watercress shoot length (F(6, 14) = 5.554, p = 0.00393).

#Post-hoc Test OPTION A: Tukey's HSD
tukey_shoot <- TukeyHSD(model_shoot)
print(tukey_shoot)
plot(tukey_shoot, las = 1, cex.axis = 0.6)

# Post-hoc Test OPTION B: Dunnett's test
watercress$Treatment <- relevel(watercress$Treatment, ref = "Control")
# Re-fit the model becausechanged the reference level

model_shoot_dunnett <- aov(Shoot ~ Treatment, data = watercress)
# Run Dunnett's test:
dunnett_shoot <- summary(glht(model_shoot_dunnett,
                              linfct = mcp(Treatment = "Dunnett")))
print(dunnett_shoot)


#Now exact same sequence for Root Length.

model_root <- aov(Root ~ Treatment, data = watercress)

#Check normality
shapiro.test(residuals(model_root))
qqnorm(residuals(model_root), main = "Q-Q Plot: Root Length Residuals")
qqline(residuals(model_root), col = "red", lwd = 2)

#Check equal variance
leveneTest(Root ~ Treatment, data = watercress)

#If assumptions met → ANOVA
summary(model_root)

#Post-hoc: Tukey
TukeyHSD(model_root)

#Post-hoc: Dunnett (vs control)
dunnett_root <- summary(glht(
  aov(Root ~ Treatment, data = watercress),
  linfct = mcp(Treatment = "Dunnett")
))
print(dunnett_root)

#GERMINATION RATE
# GLM with binomial family

#GLM FOR GERMINATION RATE
#number germinated and not germinated per dish
watercress$Germinated     <- (watercress$GR / 100) * 10
watercress$Not_germinated <- 10 - watercress$Germinated
#Fit the GLM
model_gr_glm <- glm(
  cbind(Germinated, Not_germinated) ~ Treatment,
  family = binomial(link = "logit"),
  data = watercress
)
summary(model_gr_glm)
#Test overall significance of Treatment
anova(model_gr_glm, test = "Chisq")

#        Df Deviance Resid. Df Resid. Dev Pr(>Chi)
NULL                         20     20.019         
Treatment  6   7.3841        14     12.635   0.2868
#The "Pr(>Chi)" value tells if Treatment is significant overall.
#The summary() output shows each treatment compared to Control with p-values for each.
#Pairwise comparisons
summary(glht(model_gr_glm, linfct = mcp(Treatment = "Tukey")))

