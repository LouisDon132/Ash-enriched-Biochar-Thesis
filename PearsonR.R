##Pearson Heatmap

library(ggplot2)
library(reshape2)
install.packages("reshape2")
library(reshape2)
#All biochar properties (from Tables 2, Appendix I, Appendix J)
biochar_props <- data.frame(
  Biochar = c("450N", "600N", "700N", "450A", "600A", "700A"),
#Elemental analysis (Table 2)
  C  = c(66.0, 72.0, 74.0, 60.0, 66.0, 65.0),
  H  = c(3.60, 2.70, 1.70, 3.30, 2.50, 1.60),
  N  = c(1.00, 0.90, 0.60, 0.90, 0.80, 0.50),
  S  = c(0.15, 0.12, 0.08, 0.15, 0.12, 0.08),
  O  = c(14.25, 6.28, 1.62, 12.65, 2.58, 1.74),
#Proximate analysis (Appendix I)
  VM  = c(35.0, 22.0, 10.0, 32.0, 20.0, 9.0),
  FC  = c(50.0, 60.0, 68.0, 45.0, 52.0, 60.0),
  Ash = c(15.0, 18.0, 22.0, 23.0, 28.0, 31.0),
#Molar ratios 
  HC_ratio = c(0.65, 0.45, 0.27, 0.66, 0.45, 0.29),
  OC_ratio = c(0.16, 0.07, 0.02, 0.16, 0.03, 0.02),
#ICP-OES (Appendix J) — ALL elements
  K  = c(25, 30, 35, 30, 35, 40),
  Ca = c(8, 10, 14, 25, 32, 40),
  Mg = c(3.0, 3.5, 4.2, 8.0, 10.0, 12.0),
  Na = c(1.0, 1.2, 1.5, 1.5, 1.8, 2.0),
  P  = c(4.0, 5.0, 6.0, 7.0, 9.0, 10.0),
  Si = c(20.0, 25.0, 32.0, 25.0, 30.0, 38.0),
  Al = c(1.0, 1.3, 1.8, 2.0, 2.5, 3.0),
  Fe = c(0.80, 1.00, 1.40, 1.50, 1.80, 2.20)
)

#Biological responses from Table 3 using means
bio_response <- data.frame(
  Biochar = c("450N", "600N", "700N", "450A", "600A", "700A"),
  WC_Germ  = c(86.7, 70.0, 66.7, 70.0, 60.0, 76.7),
  WC_Shoot = c(11.32, 11.91, 11.27, 13.42, 13.92, 17.45),
  WC_Root  = c(4.52, 3.91, 3.54, 5.98, 4.55, 5.22),
  CG_Germ  = c(46.7, 56.7, 63.3, 36.7, 46.7, 43.3),
  CG_Shoot = c(29.53, 48.09, 33.11, 25.25, 27.12, 20.19),
  CG_Root  = c(24.83, 42.41, 25.85, 19.29, 24.50, 18.87),
  BL_Shoot = c(60.97, 58.10, 79.01, 60.69, 74.09, 78.02),
  BL_Root  = c(57.87, 63.33, 101.37, 61.15, 98.91, 91.81)
)

#Merge and compute correlations 
merged <- merge(biochar_props, bio_response, by = "Biochar")

props <- c("C", "H", "N", "S", "O", "VM", "FC", "Ash",
           "HC_ratio", "OC_ratio",
           "K", "Ca", "Mg", "Na", "P", "Si", "Al", "Fe")

responses <- c("WC_Germ", "WC_Shoot", "WC_Root",
               "CG_Germ", "CG_Shoot", "CG_Root",
               "BL_Shoot", "BL_Root")

cor_matrix <- cor(merged[, props], merged[, responses], method = "pearson")

#Melt and label
cor_melted <- melt(cor_matrix)
names(cor_melted) <- c("Property", "Response", "r")

#Clean labels
prop_labels <- c(
  "C" = "Carbon (%)", "H" = "Hydrogen (%)", "N" = "Nitrogen (%)",
  "S" = "Sulphur (%)", "O" = "Oxygen (%)",
  "VM" = "Volatile Matter (%)", "FC" = "Fixed Carbon (%)", 
  "Ash" = "Ash Content (%)",
  "HC_ratio" = "H/C Ratio", "OC_ratio" = "O/C Ratio",
  "K" = "K (g/kg)", "Ca" = "Ca (g/kg)", "Mg" = "Mg (g/kg)",
  "Na" = "Na (g/kg)", "P" = "P (g/kg)", "Si" = "Si (g/kg)",
  "Al" = "Al (g/kg)", "Fe" = "Fe (g/kg)"
)

resp_labels <- c(
  "WC_Germ" = "WC Germ.", "WC_Shoot" = "WC Shoot", "WC_Root" = "WC Root",
  "CG_Germ" = "CG Germ.", "CG_Shoot" = "CG Shoot", "CG_Root" = "CG Root",
  "BL_Shoot" = "BL Shoot", "BL_Root" = "BL Root"
)

cor_melted$Property <- prop_labels[as.character(cor_melted$Property)]
cor_melted$Response <- resp_labels[as.character(cor_melted$Response)]

#group by analysis type
cor_melted$Property <- factor(cor_melted$Property,
                              levels = rev(c(
                                # Elemental
                                "Carbon (%)", "Hydrogen (%)", "Nitrogen (%)", "Sulphur (%)", "Oxygen (%)",
                                # Proximate
                                "Volatile Matter (%)", "Fixed Carbon (%)", "Ash Content (%)",
                                # Molar ratios
                                "H/C Ratio", "O/C Ratio",
                                # ICP-OES
                                "K (g/kg)", "Ca (g/kg)", "Mg (g/kg)", "Na (g/kg)",
                                "P (g/kg)", "Si (g/kg)", "Al (g/kg)", "Fe (g/kg)"
                              )))

cor_melted$Response <- factor(cor_melted$Response,
                              levels = c("WC Germ.", "WC Shoot", "WC Root",
                                         "CG Germ.", "CG Shoot", "CG Root",
                                         "BL Shoot", "BL Root"))

# Plot 
ggplot(cor_melted, aes(x = Response, y = Property, fill = r)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = sprintf("%.2f", r)), size = 2.5) +
  scale_fill_gradient2(
    low = "#C0392B", mid = "white", high = "#27AE60",
    midpoint = 0, limits = c(-1, 1),
    name = "Pearson r"
  ) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 8),
    panel.grid = element_blank(),
    legend.position = "right"
  )
 