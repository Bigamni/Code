#install.packages("readxl")
#install.packages("xtable")
#install.packages("dplyr")
#install.packages("forcats")
#install.packages("broom")

library(broom)
library(dplyr) 
library(forcats)
library(readxl)
library(ggplot2)
library(xtable)

dataall <- read_excel("anonym", sheet = 2)

# Erster Plot
plot(dataall$C_SOLL[-73670],dataall$C_IST[-73670], xlab = "Sollwert Kohlenstoff",
     ylab = "Istwert Kohlenstoff", main = "Vergleich der Soll- und Istwerte",
     pch = 16, col = rgb(0.2, 0.2, 0.8, 0.3), cex = 0.6)
abline(a = 0, b = 1, col = "red", lty = 2, lwd = 2)

# Erstes Modell schaetzen
model <- lm(C_IST ~ C_SOLL, data = dataall)
summary(model)


# Erster Plot aber nach (sinvollen) Farben
# Kleine Gruppen rausfiltern
data_clean <- dataall %>%
  filter(!is.na(STAHLGRUPPEN) & STAHLGRUPPEN != "Keine definierte Stahlgruppe")

gruppenanteile <- data_clean %>%
  count(STAHLGRUPPEN) %>%
  mutate(anteil = n / sum(n))

relevante_gruppen <- gruppenanteile %>%
  filter(anteil >= 0.01) %>%
  pull(STAHLGRUPPE)
data_clean$STAHLGRUPPEN <- ifelse(data_clean$STAHLGRUPPE %in% relevante_gruppen,
                                  data_clean$STAHLGRUPPE, "Sonstige")
data_clean$STAHLGRUPPEN <- fct_infreq(data_clean$STAHLGRUPPEN)

farben <- c("red", "blue", "brown", "orange", "purple", "pink",
            "cyan", "lightgreen", "darkgreen", "gold")
ggplot(data_clean, aes(x = C_SOLL, y = C_IST, color = STAHLGRUPPEN)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "black") +
  scale_color_manual(values = farben)
labs(title = "Soll- Istwerte nach Stahlgruppe",
     x = "C-Soll", y = "C-Ist", color = "STAHLGRUPPEN") +
  theme_minimal()+
  guides(color = guide_legend(override.aes = list(size = 4)))


# gruppenspezifische regressionen
gruppen <- unique(data_clean$STAHLGRUPPEN) 
ergebnisse <- data.frame()
for(g in gruppen){
  dat <- data_clean %>% filter(STAHLGRUPPEN == g)
  model <- lm(C_IST ~ C_SOLL, data = dat)
  tidy_res <- tidy(model)
  glance_res <- glance(model)
  
  ergebnisse <- rbind(ergebnisse, data.frame(
    Gruppe = g,
    Intercept = tidy_res$estimate[1],
    Intercept_p = tidy_res$p.value[1],
    Slope = tidy_res$estimate[2],
    Slope_p = tidy_res$p.value[2],
    Adj_R2 = glance_res$adj.r.squared,
    N = nrow(dat)
  ))
}
ergebnisse <- ergebnisse %>% arrange(Gruppe)
print(ergebnisse)
xtable(print(ergebnisse), digits = c(4))


# Plot der  vier auffaelligen Gruppen
auffaellig <- data_clean %>%
  filter(STAHLGRUPPEN %in% c("Nb-IF", "Ti-IF", "Si-legierter Baustahl", "Weichstahl unlegiert"))
# Scatterplot
ggplot(auffaellig, aes(x = C_SOLL, y = C_IST, color = STAHLGRUPPEN)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  labs(
    x = "Sollwert Kohlenstoff (C)",
    y = "Istwert Kohlenstoff (C)",
    color = "Stahlgruppe"
  ) +
  theme_minimal() +
  theme(legend.position = "right")


# Multiples Regressionsmodell mit Gruppenindikatoren
data_clean$STAHLGRUPPEN <- as.factor(data_clean$STAHLGRUPPEN)
multi_model <- lm(C_IST ~ C_SOLL * STAHLGRUPPEN, data = data_clean)
summary(multi_model)
model_summary <- tidy(multi_model)
glance_summary <- glance(multi_model)
xtable(model_summary, digits = c(4))

# linearität überprüfen
# Liste der auffälligen Gruppen
auffaellig <- c("Nb-IF", "Si-legierter Baustahl", "Weichstahl unlegiert")
daten_subset <- data_clean %>% filter(STAHLGRUPPEN %in% auffaellig)
#Filtere in Ti-IF die extremwerte
ti_if_data <- subset(data_clean, STAHLGRUPPEN == "Ti_IF")
model_TiIF <- lm(C_IST ~ C_SOLL, data = ti_if_data)
res <- residuals(model)
filtered_ind <- which(abs(res) <= 0.2)

windows(width = 8, height = 7)
par(mfrow = c(2,2))
plot(fitted(model)[filtered_ind], res[filtered_ind], cex = 0.5,
     main = paste("Residuenplot:", gruppe),
     xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
for (gruppe in auffaellig) {
  model <- lm(C_IST ~ C_SOLL, data = daten_subset %>% filter(STAHLGRUPPEN == gruppe))
  plot(model$fitted.values, model$residuals, cex = 0.5,
       main = paste("Residuenplot:", gruppe),
       xlab = "Fitted Values", ylab = "Residuals")
    abline(h = 0, col = "red")
}
par(mfrow = c(1,1))



# Anova zw normalem und quadratischem
results <- data.frame(STAHLGRUPPEN = character(),
                      F_Wert = numeric(),
                      p_Wert = numeric(),
                      Signifikant = character(),
                      stringsAsFactors = FALSE)
for (gruppe in auffaellig) {
  data_group <- daten_subset %>% filter(STAHLGRUPPEN == gruppe)
  model_linear <- lm(C_IST ~ C_SOLL, data = data_group)
  model_quadratic <- lm(C_IST ~ C_SOLL + I(C_SOLL^2), data = data_group)
  
  anova_result <- anova(model_linear, model_quadratic)
  F_value <- anova_result$F[2]
  p_value <- anova_result$`Pr(>F)`[2]
  results <- rbind(results, data.frame(
    STAHLGRUPPEN = gruppe,
    F_Wert = round(F_value, 4),
    p_Wert = format.pval(p_value, digits = 4),
    Signifikant = ifelse(p_value < 0.05, "ja", "nein")
  ))
}
xtable(results, digits = 4)
