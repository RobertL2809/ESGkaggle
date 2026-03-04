# 1. Lae vajalikud paketid
library(ggplot2)
library(tidyverse)
library(scales) 


df <- read.csv("company_esg_financial_dataset.csv", sep = ",")



# GRAAFIK 1: ESG_Overall vs ProfitMargin tööstusharude lõikes

p1 <- ggplot(df, aes(x = ESG_Overall, y = ProfitMargin, color = Industry)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm", linewidth = 0.5, color = "black") + # Lisab lineaarsed trendijooned
  facet_wrap(~Industry) + # Jagab graafikud tööstusharude kaupa
  labs(title = "ESG skoori ja kasumimarginaali seos", 
       subtitle = "Kas jätkusuutlikkus toob parema finantsvõimekuse?",
       x = "Üldine ESG skoor (0-100)", 
       y = "Kasumimarginaal (%)") +
  theme_minimal() +
  theme(legend.position = "none")

print(p1)



# GRAAFIK 2: Süsinikuheitmete trendid ajas regioonide lõikes

# Arvutame regioonide keskmised iga aasta kohta
df_trend <- df %>%
  group_by(Year, Region) %>%
  summarise(AvgCarbon = mean(CarbonEmissions, na.rm = TRUE),
            AvgEnvScore = mean(ESG_Environmental, na.rm = TRUE),
            .groups = "drop")

p2 <- ggplot(df_trend, aes(x = Year, y = AvgCarbon, color = Region)) +
  geom_line(linewidth = 0.6) +
  geom_point(aes(size = AvgEnvScore), alpha = 0.7) + # Punktide suurus näitab ESG_enviromental skoori
  scale_x_continuous(breaks = 2015:2025) +
  scale_y_continuous(labels = comma) +
  labs(title = "Keskmine süsinikuheide ajas (2015-2025)",
    subtitle = "Punkti suurus tähistab keskmist keskkonna skoori (ESG_Environmental)",
    x = "Aasta",
    y = "Keskmine süsinikuheide (CO2 tonni)",
    color = "Regioon",
    size = "Keskkonna skoor") +
  theme_light()

print(p2)



# GRAAFIK 3: Energiaintensiivsus vs Tulu (Revenue) tööstusharude lõikes

p3 <- ggplot(df, aes(x = Revenue, y = EnergyConsumption, color = ESG_Environmental)) +
  geom_point(alpha = 0.6) +
  scale_color_viridis_c(option = "mako") +
  facet_wrap(~Industry, scales = "free") + # free scales lubab telgedel kohanduda andmetega
  scale_x_continuous(labels = dollar_format(prefix = "$", suffix = "M")) +
  scale_y_continuous(labels = comma) +
  labs(title = "Energiatarbimine vs Teenitud Tulu",
    subtitle = "Värv näitab keskkonna skoori (heledam = parem skoor)",
    x = "Aastane Tulu (miljonites USD)",
    y = "Energiatarbimine (MWh)",
    color = "ESG Env Skoor") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# GRAAFIK 4: Süsiniku intensiivsus ajas, regiooni põhjal. 

# Lisame andmestikku uue veeru: Süsiniku intensiivsus
df <- df %>%
  mutate(CarbonIntensity = CarbonEmissions / Revenue)

# Arvutame keskmise süsiniku intensiivsuse aastate ja regioonide lõikes
df_intensity_trend <- df %>%
  group_by(Year, Region) %>%
  summarise(AvgCarbonIntensity = mean(CarbonIntensity, na.rm = TRUE),
            .groups = "drop")

# Loome uue graafiku (p4)
p4 <- ggplot(df_intensity_trend, aes(x = Year, y = AvgCarbonIntensity, color = Region)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.8) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(title = "Süsiniku intensiivsus ajas (2015-2025)",
    subtitle = "Mitu tonni CO2 paisatakse õhku iga 1 miljoni USD tulu kohta?",
    x = "Aasta",
    y = "Keskmine süsiniku intensiivsus (CO2 tonni / miljon USD)",
    color = "Regioon") +
  theme_minimal()

# Kuvame graafiku
print(p4)

# GRAAFIK 5: Süsiniku intensiivsus ajas, sektori põhjal. 

# Arvutame keskmise süsiniku intensiivsuse aastate ja sektorite lõikes
df_intensity_trend_industry <- df %>%
  group_by(Year, Industry) %>%
  summarise(AvgCarbonIntensity = mean(CarbonIntensity, na.rm = TRUE),
            .groups = "drop")

# Loome uue graafiku (p5)
p5 <- ggplot(df_intensity_trend_industry, aes(x = Year, y = AvgCarbonIntensity, color = Industry)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3, alpha = 0.8) +
  scale_x_continuous(breaks = 2015:2025) +
  labs(title = "Süsiniku intensiivsus ajas (2015-2025)",
       subtitle = "Mitu tonni CO2 paisatakse õhku iga 1 miljoni USD tulu kohta?",
       x = "Aasta",
       y = "Keskmine süsiniku intensiivsus (CO2 tonni / miljon USD)",
       color = "Tööstusharu") +
  theme_minimal()

# Kuvame graafiku
print(p5)



# Regressioonimudel: Turuväärtuse (MarketCap) ennustamine


# Koostame lineaarse regressiooni mudeli
# Valem: MarketCap sõltub tulust, kasumimarginaalist, üldisest ESG skoorist ja süsinikuheitmetest.
mudel <- lm(MarketCap ~ Revenue + ProfitMargin + ESG_Overall + CarbonEmissions, data = df)

# mudeli statistiline kokkuvõte
#koefitsendid (Estimate), p-väärtuseid (Pr(>|t|)) ja R-ruutu (Multiple R-squared)
print(summary(mudel))
#ESG_overall p -väärtus 0.08, esg väärtus ei ole statistiliselt oluline. R ruudus 0.74, mudel seletab 74% juhtudest minu andmete põhjal

#Lisame mudeli tehtud ennustused oma andmestikku, et neid reaalsusega võrrelda
df <- df %>%
  # drop_na() eemaldab read, kus mõni muutuja on puudu (nt 2015 aasta GrowthRate vms), 
  # et predict() funktsioon töötaks tõrgeteta
  drop_na(MarketCap, Revenue, ProfitMargin, ESG_Overall, CarbonEmissions) %>% 
  mutate(PredictedMarketCap = predict(mudel, newdata = .))

# 4. Loome graafiku: Tegelik turuväärtus vs Ennustatud turuväärtus
p6 <- ggplot(df, aes(x = MarketCap, y = PredictedMarketCap, color = ESG_Overall)) +
  geom_point(alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linewidth = 0.6) + #ennustusjoon
  scale_color_viridis_c(option = "mako") + 
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma) +
  labs(
    title = "Regressioonimudel: Tegelik vs Ennustatud Turuväärtus",
    subtitle = "Punane joon tähistab ideaalset ennustust. Värv näitab ESG skoori.",
    x = "Tegelik Turuväärtus (MarketCap)",
    y = "Mudeli ennustatud Turuväärtus",
    color = "ESG Skoor"
  ) +
  theme_minimal()

print(p6)
