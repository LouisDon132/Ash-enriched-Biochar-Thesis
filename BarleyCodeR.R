#Barley 
#Germination rate was 100% across all treatments and replicates.
# No statistical tes for GR
# shoot length and root length only.
install.packages("readxl")
library(car)
library(multcomp)
library(ggplot2)
library(dplyr)
library(readxl)
list.files()
barley_raw <- read_excel("Barley.xlsx", skip = 2)
barley <- data.frame(
  Treatment = barley_raw$Treatment,
  GR        = barley_raw$`GR (%)`,
  Shoot     = barley_raw$`Shoot (mm)`,
  Root      = barley_raw$`Root (mm)`
)
barley$Treatment <- gsub("Ash-enriched Biochar ", "AshEnriched_", barley$Treatment)
barley$Treatment <- gsub("Biochar ", "Biochar_", barley$Treatment)
barley$Treatment <- gsub("°C", "", barley$Treatment)
barley$Treatment <- factor(barley$Treatment)
print(barley)

barley %>%
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

# PLOT DATA

#### a. Shoot Length 
ggplot(barley, aes(x = Treatment, y = Shoot, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Barley Shoot Length by Treatment",
    y = "Mean Shoot Length (mm)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )

#b. Root Length 
ggplot(barley, aes(x = Treatment, y = Root, fill = Treatment)) +
  geom_boxplot(alpha = 0.7) +
  geom_point(size = 2) +
  labs(
    title = "Barley Root Length by Treatment",
    y = "Mean Root Length (mm)",
    x = ""
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    legend.position = "none"
  )


# CHECK ASSUMPTIONS SHOOT LENGTH
model_shoot <- aov(Shoot ~ Treatment, data = barley)
#b normality of residuals
shapiro.test(residuals(model_shoot))
qqnorm(residuals(model_shoot), main = "Q-Q Plot: Barley Shoot Length")
qqline(residuals(model_shoot), col = "red", lwd = 2)
#d. Homogeneity of variance
leveneTest(Shoot ~ Treatment, data = barley)

##ONE-WAY ANOVA + POST-HOCSHOOT LENGTH

summary(model_shoot)

# Tukey's HSD (Every treatment compared to every other treatment)
tukey_shoot <- TukeyHSD(model_shoot)
print(tukey_shoot)
plot(tukey_shoot, las = 1, cex.axis = 0.6)


#Dunnett's test (each treatment vs control)
barley$Treatment <- relevel(barley$Treatment, ref = "Control")
model_shoot_dunnett <- aov(Shoot ~ Treatment, data = barley)
dunnett_shoot <- summary(glht(model_shoot_dunnett,
                              linfct = mcp(Treatment = "Dunnett")))
print(dunnett_shoot)




#ROOT LENGTH ANALYSIS
barley$Treatment <- factor(barley$Treatment)

model_root <- aov(Root ~ Treatment, data = barley)

#Normality
shapiro.test(residuals(model_root))
qqnorm(residuals(model_root), main = "Q-Q Plot: Barley Root Length")
qqline(residuals(model_root), col = "red", lwd = 2)

#Equal variance
leveneTest(Root ~ Treatment, data = barley)

#ONE-WAY ANOVA + POST-HOC ROOT LENGTH


#ANOVA
summary(model_root)

#Tukey's HSD
tukey_root <- TukeyHSD(model_root)
print(tukey_root)
plot(tukey_root, las = 1, cex.axis = 0.6)

#Dunnett's test
barley$Treatment <- relevel(barley$Treatment, ref = "Control")
model_root_dunnett <- aov(Root ~ Treatment, data = barley)
dunnett_root <- summary(glht(model_root_dunnett,
                             linfct = mcp(Treatment = "Dunnett")))
print(dunnett_root)

