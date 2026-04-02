#   CAT-GRASS 

library(car)
library(multcomp)
library(ggplot2)
library(dplyr)
catgrass <- data.frame(
  
  Treatment = c(
    "Control",          "Control",          "Control",
    "Biochar_450",      "Biochar_450",      "Biochar_450",
    "AshEnriched_450",  "AshEnriched_450",  "AshEnriched_450",
    "Biochar_600",      "Biochar_600",      "Biochar_600",
    "AshEnriched_600",  "AshEnriched_600",  "AshEnriched_600",
    "Biochar_700",      "Biochar_700",      "Biochar_700",
    "AshEnriched_700",  "AshEnriched_700",  "AshEnriched_700"
  ),
  
  GR = c(
    20, 30, 60,         # Control:           Sand 1, Sand 2, Sand 3
    40, 50, 50,         # Biochar 450:       450N1, 450N2, 450N3
    20, 30, 60,         # Ash-enriched 450:  450C1, 450C2, 450C3
    60, 40, 20,         # Biochar 600:       600N1, 600N2, 600N3
    50, 50, 50,         # Ash-enriched 600:  600C1, 600C2, 600C3
    80, 40, 70,         # Biochar 700:       700N1, 700N2, 700N3
    80, 60, 30          # Ash-enriched 700:  700C1, 700C2, 700C3
  ),
  
  Shoot = c(
    52.3715,  13.94533, 25.94217,     # Control
    17.524,   25.9394,  33.987,       # Biochar 450
    32.176,   50.238,   25.58717,     # Ash-enriched 450
    29.26617, 64.78675, 50.222,       # Biochar 600
    6.08325,  60.12175, 42.1078,      # Ash-enriched 600
    46.45975, 26.78325, 36.11629,     # Biochar 700
    21.772,   28.8514,  9.9475        # Ash-enriched 700
  ),
  
  Root = c(
    22.661,   3.491,    19.6344,      # Control
    46.552,   28.58967, 42.99025,     # Biochar 450
    33.8055,  37.044,   17.352,       # Ash-enriched 450
    40.0845,  55.53,    31.6065,      # Biochar 600
    16.175,   49.3164,  49.1105,      # Ash-enriched 600
    52.74314, 26.319,   47.25475,     # Biochar 700
    23.09,    27.89623, 5.623         # Ash-enriched 700
  )
)

catgrass$Treatment <- factor(catgrass$Treatment)
print(catgrass)

catgrass %>%
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
# PLOT Shoot Length
ggplot(catgrass, aes(x = Treatment, y = Shoot, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Cat-grass Shoot Length by Treatment",
    y = "Mean Shoot Length (mm)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )

#Root Length
ggplot(catgrass, aes(x = Treatment, y = Root, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Cat-grass Root Length by Treatment",
    y = "Mean Root Length (mm)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )

#Germination Rate

ggplot(catgrass, aes(x = Treatment, y = GR, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Cat-grass Germination Rate by Treatment",
    y = "Germination Rate (%)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )


#CHECK ASSUMPTIONS
#Fit the model
model_shoot <- aov(Shoot ~ Treatment, data = catgrass)

#Normality of residuals
shapiro.test(residuals(model_shoot))
#Shapiro-Wilk p = 0.9896


#Q-Q plot (visual check)
qqnorm(residuals(model_shoot), main = "Q-Q Plot: Cat-grass Shoot Length")
qqline(residuals(model_shoot), col = "red", lwd = 2)

#Homogeneity of variance
leveneTest(Shoot ~ Treatment, data = catgrass)
#Levene's p = 0.7608

# Both p > 0.05

#ONE-WAY ANOVA + POST-HOC — SHOOT LENGTH
#ANOVA results
summary(model_shoot)
#F(6, 14) = 0.89 p = 0.528

#Tukey's HSD all pairwise comparisons
tukey_shoot <- TukeyHSD(model_shoot)
print(tukey_shoot)
plot(tukey_shoot, las = 1, cex.axis = 0.6)

# 7c. Dunnett's test (each treatment vs control)
catgrass$Treatment <- relevel(catgrass$Treatment, ref = "Control")
model_shoot_dunnett <- aov(Shoot ~ Treatment, data = catgrass)
dunnett_shoot <- summary(glht(model_shoot_dunnett,
                              linfct = mcp(Treatment = "Dunnett")))
print(dunnett_shoot)


#ROOT LENGTH ANALYSIS

#Reset Treatment factor level 
catgrass$Treatment <- factor(catgrass$Treatment)

model_root <- aov(Root ~ Treatment, data = catgrass)

#Normality
shapiro.test(residuals(model_root))
#p-value = 0.0124
qqnorm(residuals(model_root), main = "Q-Q Plot: Cat-grass Root Length")
qqline(residuals(model_root), col = "red", lwd = 2)

# Equal variance
leveneTest(Root ~ Treatment, data = catgrass)
#0.9975

catgrass$Root_log <- log(catgrass$Root + 1)
model_root_log <- aov(Root_log ~ Treatment, data = catgrass)
shapiro.test(residuals(model_root_log))
# p = 0.0317
kruskal.test(Root ~ Treatment, data = catgrass)
p-value = 0.1429

##GERMINATION RATE ANALYSIS

#Reset Treatment factor
catgrass$Treatment <- factor(catgrass$Treatment)
catgrass$Treatment <- relevel(catgrass$Treatment, ref = "Control")

#Calculate counts
catgrass$Germinated     <- (catgrass$GR / 100) * 10
catgrass$Not_germinated <- 10 - catgrass$Germinated

#Fit the GLM
model_gr_glm <- glm(
  cbind(Germinated, Not_germinated) ~ Treatment,
  family = binomial(link = "logit"),
  data = catgrass
)

#View results
summary(model_gr_glm)

#Overall significance
anova(model_gr_glm, test = "Chisq")
# Pr (>Chi) = 0.2634

#Pairwise comparisons
summary(glht(model_gr_glm, linfct = mcp(Treatment = "Tukey")))

