### ================================
###  POTENCIAL FECUNDITY ESTIMATION 
### ================================

# Gravimetric method (Murua et al 2003)

# Total weight of the individual: 2907 g
# Eviscerated weight: 2504 g
# Gonad weight: 325 g

#       sub-sample weight	  number of oocytes(a)	number of oocytes(b)  TOTAL
# sub1	0.046 g	            53              	    34                    87
# sub2	0.042 g	            42              	    43                    85
# sub3	0.020 g	            162             	    35                    197
# sub4	0.029 g	            171	                  23                    194


packages <- c(
  "ggplot2", "gridExtra", "mgcv", "multcomp", "multcompView", "sizeMat",
  "nlme", "scales", "MASS", "carData", "TH.data", "car", "FSA", "magrittr",
  "dplyr", "lubridate", "psych", "nlstools", "RColorBrewer", "readr",
  "emmeans", "lme4", "tidyverse", "stats", "leaflet", "weights", "corrplot",
  "rcompanion", "lsr", "FactoMineR", "factoextra", "GGally", "ggfortify", "reshape2",
  "ggthemes", "ggridges", "rlang", "moments", "purrr", "lmtest", "sizeMat", "FSA", "randomForest", "caret"
)

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

# -------------------------------------------------------
# 1. Load file
# -------------------------------------------------------
setwd("D:/Usuarios/mgonzalez/Desktop/Fec_pez_globo")

data <- read.csv("fecundidad_pezglobo.csv", header = TRUE, dec = ".", sep = ";")

names(data)
str(data)

cols_to_factor <- c("numero", "sub", "tamaño", "Label")

missing <- setdiff(cols_to_factor, names(data))
if (length(missing) > 0) stop(paste("Missing columns:", paste(missing, collapse = ", ")))
data[cols_to_factor] <- lapply(data[cols_to_factor], as.factor)

# -------------------------------------------------------
# 2. A table is created with the weight of each subsample
# -------------------------------------------------------
conteos <- data %>%
  group_by(sub) %>%
  summarise(N_ovocitos = n(), .groups = "drop") 


pesos <- data.frame(
  sub = c("2", "3", "4", "5"),
  Wsub = c(0.046, 0.042, 0.020, 0.029),   
  Wgonada = c(325,325,325,325 ),    
  Weviscerado = c(2504, 2504, 2504, 2504))  

# -------------------------------------------------------
# 3. The counts are merged with the weights
# -------------------------------------------------------
datos_completos <- conteos %>%
  left_join(pesos, by="sub") %>%
  mutate(
    densidad = N_ovocitos / Wsub,       # Number of oocytes per gram of subsample
    PF_fraccion = densidad * Wgonada    # Estimated potential fecundity of each fraction
  )


# -------------------------------------------------------
# 4. Estimate the potential fecundity 
# -------------------------------------------------------

PF_total <- sum(datos_completos$PF_fraccion) # All fractions are summed to obtain total fecundity
PF_relativa <- PF_total / unique(datos_completos$Weviscerado) # Fecundity normalized by the fish’s eviscerated weight

# -------------------------------------------------------
# 5. Final results
# -------------------------------------------------------

print(PF_total) # Estimated total number of oocytes in the gonad
print(PF_relativa) # Total fecundity adjusted for fish size (reproductive investment)


cat("Overall potential fecundity:", round(PF_total,0), "\n")
#Overall potential fecundity: 6647800 
cat("Relative fecundity (oocytes g⁻¹ eviscerated weight):", round(PF_relativa,2), "\n")
#Relative fecundity (oocytes g⁻¹ eviscerated weight): 2654.87

# -------------------------------------------------------
# 6. Graphs
# -------------------------------------------------------

# 1. Histogram showing the number of oocytes per Feret diameter
ggplot(data, aes(x = Feret)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") +
  geom_vline(xintercept = 500, linetype = "dashed", color = "red") +
  labs(
    title = "Red line - 500 µm sieve",
    x = "Oocyte diameter (µm)",
    y = "Number of oocytes"
  ) +
  theme_minimal()

# 2. Subsample-specific density plot
ggplot(data, aes(x = Feret, color = sub, fill = sub)) +
  geom_density(alpha = 0.3) +
  labs(
    x = "Diameter (µm)",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 26) +
  theme(legend.position = "none")

    # 2.1 Overall density plot
    ggplot(data, aes(x = Feret)) +
      geom_density(alpha = 0.3) +
      labs(
      x = "Diameter (µm)",
      y = "Frequency"
        ) +
      theme_minimal(base_size = 26)

# 3.Subsample variability in Feret diameter
ggplot(data, aes(x = sub, y = Feret)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Diameter (Feret) by subsample", x = "Subsample", y = "Feret (µm)") +
  theme_minimal()




