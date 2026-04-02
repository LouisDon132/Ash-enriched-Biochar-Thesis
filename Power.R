
install.packages("pwr")
library(pwr)

#7 groups (treatments), n = 3 per group
#effect size Cohen's f could detect
#at α = 0.05 with power = 0.80

#What was the smallest effect I could reliably detect
pwr.anova.test(
  k = 7,           # number of groups
  n = 3,           # observations per group
  sig.level = 0.05,
  power = 0.80,
  f = NULL          #R will solve fo
)

#Interpret the output
#Cohen's conventions: f = 0.10 small, 0.25 medium, 0.40 large
# f = 0.995
#f > 0.80 meansonly detect VERY large effects

#actual effect sizes? 
# Convert eta-squared values to Cohen's f:
# f = sqrt(eta_squared / (1 - eta_squared))

# Barley shoot (η² = 0.83)
f_barley_shoot <- sqrt(0.83 / (1 - 0.83))
cat("Barley shoot Cohen's f:", round(f_barley_shoot, 2), "\n")
# f: 2.21

# Barley root (η² = 0.92)
f_barley_root <- sqrt(0.92 / (1 - 0.92))
cat("Barley root Cohen's f:", round(f_barley_root, 2), "\n")
# f:3.39

# Watercress shoot (η² = 0.70)
f_watercress_shoot <- sqrt(0.70 / (1 - 0.70))
cat("Watercress shoot Cohen's f:", round(f_watercress_shoot, 2), "\n")
# f 1.53 

# Cat-grass shoot (η² = 0.28)
f_catgrass_shoot <- sqrt(0.28 / (1 - 0.28))
cat("Cat-grass shoot Cohen's f:", round(f_catgrass_shoot, 2), "\n")
# f: 0.62 

# ── How many replicates would cat-grass have needed? ───
pwr.anova.test(
  k = 7,
  f = f_catgrass_shoot,   # use the actual cat-grass effect size
  sig.level = 0.05,
  power = 0.80,
  n = NULL                # solve for n
)
# n = 5.968
#Balanced one-way analysis of variance power calculation 

#k = 7
#n = 5.968189
#f = 0.6236096
#sig.level = 0.05
#power = 0.8


