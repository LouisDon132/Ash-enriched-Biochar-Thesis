#Biochar Characteristion Figures

library(ggplot2)
library(dplyr)
library(tidyr)

#CHARACTERISATION DATA
#Proximate Analysis
proximate <- data.frame(
  Temperature = rep(c("450", "600", "700"), each = 2),
  Biochar_Type = rep(c("Standard", "Ash-enriched"), 3),
  VM      = c(35.0, 32.0, 22.0, 20.0, 10.0, 9.0),
  VM_sd   = c(1.5, 1.4, 1.2, 1.1, 0.8, 0.7),
  Ash     = c(15.0, 23.0, 18.0, 28.0, 22.0, 31.0),
  Ash_sd  = c(0.8, 0.9, 0.8, 1.0, 0.9, 1.1),
  FC      = c(50.0, 45.0, 60.0, 52.0, 68.0, 60.0),
  FC_sd   = c(1.7, 1.7, 1.4, 1.5, 1.2, 1.3)
)

proximate$Temperature <- factor(proximate$Temperature,
                                levels = c("450", "600", "700"))


#Elemental Analysis
elemental <- data.frame(
  Temperature = rep(c("450", "600", "700"), each = 2),
  Biochar_Type = rep(c("Standard", "Ash-enriched"), 3),
  C       = c(66.0, 60.0, 72.0, 66.0, 74.0, 65.0),
  C_sd    = c(0.8, 0.9, 0.8, 0.9, 0.9, 1.0),
  H       = c(3.60, 3.30, 2.70, 2.50, 1.70, 1.60),
  H_sd    = c(0.12, 0.12, 0.10, 0.10, 0.08, 0.08),
  N       = c(1.00, 0.90, 0.90, 0.80, 0.60, 0.50),
  N_sd    = c(0.06, 0.06, 0.05, 0.05, 0.05, 0.05),
  O       = c(14.25, 12.65, 6.28, 2.58, 1.62, 1.74),
  O_sd    = c(1.2, 1.4, 1.2, 1.5, 1.3, 1.7)
)

elemental$Temperature <- factor(elemental$Temperature,
                                levels = c("450", "600", "700"))
biochar_colours <- c("Standard" = "#E67E22",        # orange
                     "Ash-enriched" = "#27AE60")     # green
dissertation_theme <- theme_minimal() +
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 11, face = "bold")
  )

#FIGURE 1: PROXIMATE ANALYSIS GROUPED BAR CHART

prox_long <- proximate %>%
  pivot_longer(
    cols = c(VM, Ash, FC),
    names_to = "Component",
    values_to = "Value"
  )
#atching SD values
prox_sd <- proximate %>%
  pivot_longer(
    cols = c(VM_sd, Ash_sd, FC_sd),
    names_to = "Component_sd",
    values_to = "SD"
  ) %>%
  mutate(Component = gsub("_sd", "", Component_sd))
# Combine
prox_plot <- prox_long %>%
  left_join(
    prox_sd %>% select(Temperature, Biochar_Type, Component, SD),
    by = c("Temperature", "Biochar_Type", "Component")
  )

# Set component order
prox_plot$Component <- factor(prox_plot$Component,
                              levels = c("VM", "Ash", "FC"),
                              labels = c("Volatile Matter", "Ash", "Fixed Carbon"))

# Create the plot
fig1 <- ggplot(prox_plot, aes(x = Temperature, y = Value,
                              fill = Biochar_Type)) +
  geom_bar(stat = "identity", position = position_dodge(0.7),
           width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.3) +
  facet_wrap(~ Component) +
  scale_fill_manual(values = biochar_colours, name = "Biochar Type") +
  labs(
    x = expression("Pyrolysis Temperature ("*degree*"C)"),
    y = "Content (wt%, dry basis)"
  ) +
  dissertation_theme

##PRINT 

print(fig1)






#FIGURE 2: ELEMENTAL ANALYSIS GROUPED BAR CHART
elem_long <- elemental %>%
  pivot_longer(
    cols = c(C, H, N, O),
    names_to = "Element",
    values_to = "Value"
  )
elem_sd <- elemental %>%
  pivot_longer(
    cols = c(C_sd, H_sd, N_sd, O_sd),
    names_to = "Element_sd",
    values_to = "SD"
  ) %>%
  mutate(Element = gsub("_sd", "", Element_sd))

elem_plot <- elem_long %>%
  left_join(
    elem_sd %>% select(Temperature, Biochar_Type, Element, SD),
    by = c("Temperature", "Biochar_Type", "Element")
  )

elem_plot$Element <- factor(elem_plot$Element,
                            levels = c("C", "H", "N", "O"))

fig2 <- ggplot(elem_plot, aes(x = Temperature, y = Value,
                              fill = Biochar_Type)) +
  geom_bar(stat = "identity", position = position_dodge(0.7),
           width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.3) +
  facet_wrap(~ Element, scales = "free_y") +
  scale_fill_manual(values = biochar_colours, name = "Biochar Type") +
  labs(
    x = expression("Pyrolysis Temperature ("*degree*"C)"),
    y = "Content (wt%, dry basis)"
  ) +
  dissertation_theme

print(fig2)







#2c. ICP-OES Data
# 1 corrected to 6 
icp <- data.frame(
  Temperature = rep(c("450", "600", "700"), each = 2),
  Biochar_Type = rep(c("Standard", "Ash-enriched"), 3),
  K       = c(25.0, 30.0, 30.0, 35.0, 35.0, 40.0),
  K_sd    = c(2.0, 2.4, 2.4, 2.8, 2.8, 3.2),
  Ca      = c(8.0, 25.0, 10.0, 32.0, 14.0, 40.0),
  Ca_sd   = c(0.7, 2.0, 0.8, 2.6, 1.1, 3.2),
  Mg      = c(3.0, 8.0, 3.5, 10.0, 4.2, 12.0),
  Mg_sd   = c(0.3, 0.8, 0.4, 1.0, 0.5, 1.2),
  P       = c(4.0, 7.0, 5.0, 9.0, 6.0, 10.0),
  P_sd    = c(0.4, 0.7, 0.5, 0.9, 0.6, 1.0)
)

icp$Temperature <- factor(icp$Temperature,
                          levels = c("450", "600", "700"))
icp_long <- icp %>%
  pivot_longer(
    cols = c(K, Ca, Mg, P),
    names_to = "Element",
    values_to = "Value"
  )
icp_sd_long <- icp %>%
  pivot_longer(
    cols = c(K_sd, Ca_sd, Mg_sd, P_sd),
    names_to = "Element_sd",
    values_to = "SD"
  ) %>%
  mutate(Element = gsub("_sd", "", Element_sd))

icp_plot <- icp_long %>%
  left_join(
    icp_sd_long %>% select(Temperature, Biochar_Type, Element, SD),
    by = c("Temperature", "Biochar_Type", "Element")
  )

icp_plot$Element <- factor(icp_plot$Element,
                           levels = c("K", "Ca", "Mg", "P"))

fig3 <- ggplot(icp_plot, aes(x = Temperature, y = Value,
                             fill = Biochar_Type)) +
  geom_bar(stat = "identity", position = position_dodge(0.7),
           width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(0.7), width = 0.2, linewidth = 0.3) +
  facet_wrap(~ Element, scales = "free_y") +
  scale_fill_manual(values = biochar_colours, name = "Biochar Type") +
  labs(
    x = expression("Pyrolysis Temperature ("*degree*"C)"),
    y = "Concentration (mg/kg)"
  ) +
  dissertation_theme

print(fig3)



#FIGURE: ICP-OES Na, Si, Al, Fe 



# Reuse colour scheme and theme
biochar_colours <- c("Standard" = "#E67E22",
                     "Ash-enriched" = "#27AE60")

dissertation_theme <- theme_minimal() +
  theme(
    text = element_text(size = 11),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "top",
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text = element_text(size = 11, face = "bold")
  )

icp_other <- data.frame(
  Temperature  = rep(c("450", "600", "700"), each = 2),
  Biochar_Type = rep(c("Standard", "Ash-enriched"), 3),
  Na      = c(1.0, 1.5, 1.2, 1.8, 1.5, 2.0),
  Na_sd   = c(0.1, 0.2, 0.1, 0.2, 0.2, 0.2),
  Si      = c(20.0, 25.0, 25.0, 30.0, 32.0, 38.0),
  Si_sd   = c(3.0, 4.0, 3.5, 4.5, 4.0, 5.0),
  Al      = c(1.0, 2.0, 1.3, 2.5, 1.8, 3.0),
  Al_sd   = c(0.2, 0.3, 0.2, 0.4, 0.3, 0.5),
  Fe      = c(0.80, 1.50, 1.00, 1.80, 1.40, 2.20),
  Fe_sd   = c(0.10, 0.18, 0.12, 0.20, 0.16, 0.25)
)

icp_other$Temperature <- factor(icp_other$Temperature,
                                levels = c("450", "600", "700"))

# Pivot values to long format
icp_other_long <- icp_other %>%
  pivot_longer(
    cols = c(Na, Si, Al, Fe),
    names_to = "Element",
    values_to = "Value"
  )


icp_other_sd <- icp_other %>%
  pivot_longer(
    cols = c(Na_sd, Si_sd, Al_sd, Fe_sd),
    names_to = "Element_sd",
    values_to = "SD"
  ) %>%
  mutate(Element = gsub("_sd", "", Element_sd))

# Join values and SDs
icp_other_plot <- icp_other_long %>%
  left_join(
    icp_other_sd %>% select(Temperature, Biochar_Type, Element, SD),
    by = c("Temperature", "Biochar_Type", "Element")
  )

# Set element order
icp_other_plot$Element <- factor(icp_other_plot$Element,
                                 levels = c("Si", "Na", "Al", "Fe"))

# Plot
fig_icp_other <- ggplot(icp_other_plot,
                        aes(x = Temperature, y = Value,
                            fill = Biochar_Type)) +
  geom_bar(stat = "identity", position = position_dodge(0.7),
           width = 0.6, colour = "black", linewidth = 0.3) +
  geom_errorbar(aes(ymin = Value - SD, ymax = Value + SD),
                position = position_dodge(0.7), width = 0.2,
                linewidth = 0.3) +
  facet_wrap(~ Element, scales = "free_y") +
  scale_fill_manual(values = biochar_colours, name = "Biochar Type") +
  labs(
    x = expression("Pyrolysis Temperature ("*degree*"C)"),
    y = "Concentration (mg/kg)"
  ) +
  dissertation_theme

print(fig_icp_other)




#ELEMENTAL DATA AND CALCULATE RATIOS
# Atomic masses
mass_C <- 12.011
mass_H <- 1.008
mass_O <- 15.999

# elemental analysis data (wt%, dry basis)
ratios <- data.frame(
  Temperature  = c("450", "600", "700", "450", "600", "700"),
  Biochar_Type = c("Standard", "Standard", "Standard",
                   "Ash-enriched", "Ash-enriched", "Ash-enriched"),
  C_wt = c(66.0, 72.0, 74.0, 60.0, 66.0, 65.0),
  H_wt = c(3.60, 2.70, 1.70, 3.30, 2.50, 1.60),
  O_wt = c(14.25, 6.28, 1.62, 12.65, 2.58, 1.74)
)
#wt% to moles
# Divide each elements wt% by its atomic mass
#number of moles per 100g of biochar
ratios$C_mol <- ratios$C_wt / mass_C
ratios$H_mol <- ratios$H_wt / mass_H
ratios$O_mol <- ratios$O_wt / mass_O

#molar ratios
ratios$HC_ratio <- ratios$H_mol / ratios$C_mol
ratios$OC_ratio <- ratios$O_mol / ratios$C_mol

#results
ratios %>%
  select(Temperature, Biochar_Type, HC_ratio, OC_ratio) %>%
  mutate(
    HC_ratio = round(HC_ratio, 3),
    OC_ratio = round(OC_ratio, 3)
  )

#H/C decreasing with temperature (less hydrogen = more aromatic)
#O/C decreasing with temperature (less oxygen = fewer functional groups)
#Ash-enriched biochars may have slightly different ratios due to
#lower C content (diluted by ash)


# PART 3: VAN KREVELEN DIAGRAM

# The position on the diagram tells the degree of carbonisation.
# elemental analysis it show biochar chemistry.

# Set up colours 
biochar_colours <- c("Standard" = "#E67E22",        # orange
                     "Ash-enriched" = "#27AE60")     # green

#Set up shapes
temp_shapes <- c("450" = 16,    # filled circle
                 "600" = 17,    # filled triangle
                 "700" = 15)    # filled square

ratios$Temperature <- factor(ratios$Temperature,
                             levels = c("450", "600", "700"))

fig_vk <- ggplot(ratios, aes(x = OC_ratio, y = HC_ratio,
                             colour = Biochar_Type,
                             shape = Temperature)) +
  geom_point(size = 4, stroke = 1) +
  scale_colour_manual(values = biochar_colours, name = "Biochar Type") +
  scale_shape_manual(values = temp_shapes,
                     name = expression("Temperature ("*degree*"C)")) +
  
  # Add reference zones 
  annotate("rect", xmin = 0, xmax = 0.4, ymin = 0, ymax = 0.6,
           fill = "green", alpha = 0.08) +
  annotate("text", x = 0.10, y = 0.55, label = "Suitable biochar",
           size = 3, colour = "grey40", fontface = "italic") +
  

  
  # arrow showing the direction of increasing carbonisation
  annotate("segment", x = 0.55, y = 1.0, xend = 0.15, yend = 0.25,
           arrow = arrow(length = unit(0.25, "cm")),
           colour = "grey50", linewidth = 0.5) +
  annotate("text", x = 0.42, y = 0.75,
           label = "Increasing\ncarbonisation", size = 3,
           colour = "grey50", fontface = "italic") +
  
  labs(
    x = "O/C molar ratio",
    y = "H/C molar ratio"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 20),
    axis.title = element_text(size = 11),
    axis.text = element_text(size = 10, colour = "black"),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    legend.position = "right",
    panel.grid.minor = element_blank()
  ) +
  # Set axis limits to give some breathing room
  xlim(0, 0.65) +
  ylim(0, 1.3)

print(fig_vk)

