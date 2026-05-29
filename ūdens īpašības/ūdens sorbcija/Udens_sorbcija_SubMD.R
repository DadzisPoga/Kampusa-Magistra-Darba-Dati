#pievieno paplašinājumus----
library(qqplotr)
library(readxl)
library(writexl)
library(rstatix)
library(ggplot2)
library(emmeans)
library(dplyr)
library(nlme)
library(tidyr)
library(effects)
library(janitor)

#Norāda darba direktoriju (tā, kurā atrodas R fails)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Datu importesana ----
#Norāda excel faila nosaukumu un lapu kurā atrodas tabula
dati<-read_excel("udens absorbcija paraugs.xlsx", sheet = 2)

dati <- clean_names(dati)
dati$layers <- as.factor(dati$layers)
dati$time <- as.factor(dati$time)
dati$substrate <- as.factor(dati$substrate)
dati$id <- as.factor(dati$id)
datiSub<-read_excel("parauguNr2025 22.10.xlsx", sheet = 13)
datiSub <- clean_names(datiSub)
datiSub$id <- as.factor(datiSub$id)
#PreProcesing----
control <- filter(dati, layers == 0)
hito <- filter(dati, layers == 2)
control <- select(control, -layers)
hito <- select(hito, -layers)

control$apstrade <- "control"
control$hito_g_g_mk <- 0
control$sub_sk_g_g_mk <- 0
control$delta_m_mo <- control$delta_m
control$delt_m_kond <- control$delta_m

hito$apstrade <- "hito"
hito$hito_g_g_mk <- 0.082
hito$sub_sk_g_g_mk <- 0
hito$delta_m_mo <- hito$delta_m
hito$delt_m_kond <- hito$delta_m

control0 <- filter(control, time == 0)
hito0 <- filter(hito, time == 0)

control$udens <- control$masa - control0$masa
hito$udens <- hito$masa - hito0$masa

control1 <- control %>%
  group_by(time) %>%
  summarise(
    m2 = control0$masa
  )

control$m2 <- control1$m2

hito1 <- hito %>%
  group_by(time) %>%
  summarise(
    m2 = hito0$masa
  )

hito$m2 <- hito1$m2

control$time <- as.factor(control$time)
datiSub$time <- as.factor(datiSub$time)
hito$time <- as.factor(hito$time)

udens_sub_full <- bind_rows(datiSub, control)
udens_sub_full <- bind_rows(udens_sub_full, hito)

udens_sub_full$pos_delta_v <- udens_sub_full$delta_v
udens_sub_full$pos_delta_v[udens_sub_full$pos_delta_v < -0.2] <- ""

udens_sub_full$pos_delta_v <- as.numeric(as.character(udens_sub_full$pos_delta_v))

udens_sub_full$udensgg <- udens_sub_full$udens / udens_sub_full$m2

udens_sub_full <- udens_sub_full[udens_sub_full$substrate != "WSz3", ]


udens_sub_full$apstrade <- factor(
  udens_sub_full$apstrade,
  levels = c("control", "hito", "sub", "hito_sub"),  # Desired order
  labels = c("Kontrole", "Hitozāns", "Suberīnskābes", "Hitozāns + Suberīnskābes")  # Desired names
)

udens_sub_full$substrate <- factor(
  udens_sub_full$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)

udens_sub_summary <- udens_sub_full %>%
  group_by(time, apstrade, substrate) %>%
  summarise(
    mean_delta_m = mean(delta_m, na.rm = TRUE),
    sd_delta_m = sd(delta_m, na.rm = TRUE),
    mean_delta_v = mean(delta_v, na.rm = TRUE),
    sd_delta_v = sd(delta_v, na.rm = TRUE),
    mean_pos_delta_v = mean(pos_delta_v, na.rm = TRUE),
    sd_pos_delta_v = sd(pos_delta_v, na.rm = TRUE),
    mean_delta_m_kond = mean(delt_m_kond, na.rm = TRUE),
    sd_delta_m_kond = sd(delt_m_kond, na.rm = TRUE),
    mean_udens = mean(udens, na.rm = TRUE),
    sd_udens = sd(udens, na.rm = TRUE),
    mean_udensgg = mean(udensgg, na.rm = TRUE),
    sd_udensgg = sd(udensgg, na.rm = TRUE),
    mean_tilppr = mean(tilpumprocenti, na.rm = TRUE),
    sd_tilppr = sd(tilpumprocenti, na.rm = TRUE)
  )

udens_sub_summary <- filter(udens_sub_summary, time != 0)
udens_sub_full <- filter(udens_sub_full, time != 0)

write_xlsx(udens_sub_summary, "udens_sub_summary.xlsx")

#Masas izmaiņa----
##Normalitāte----
# QQ plot with facets
qq_plot <- ggplot(udens_sub_full, aes(sample = delta_m)) +
  facet_grid(time ~ substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

#nenoraidu normalitāti


ggplot(udens_sub_full, aes(x = substrate, y = delta_m)) + 
  geom_boxplot() +
  labs(x = "substrate", y = "delta m", fill = "apstrade")+facet_grid(time ~ substrate ~ apstrade)
#izlecējvērtības - Noraidu normalitāti

##Paraugkopu salīdzināšana----
lmeModelm <- lme(delta_m ~ apstrade + substrate + time, random = ~1 | id, data = udens_sub_full)

plot(residuals(lmeModelm))
qqnorm(residuals(lmeModelm))
qqline(residuals(lmeModelm))
#heteroscedasticity

glsModel <- gls(
  delta_m ~ apstrade + substrate + time,
  data = udens_sub_full
)

plot(residuals(glsModel))
qqnorm(residuals(glsModel))
qqline(residuals(glsModel))
#average distribution

glm <-glm(delta_m ~ apstrade + substrate + time,
    data = udens_sub_full,family = gaussian(link = "identity"))
glm1 <-glm(delta_m ~ apstrade + substrate + time,
          data = udens_sub_full,family = Gamma(link = "inverse"))
glm2 <-glm(delta_m ~ apstrade + substrate + time,
           data = udens_sub_full,family = Gamma(link = "log"))
glm3 <-glm(delta_m ~ apstrade + substrate + time,
           data = udens_sub_full,family = poisson(link = "log"))
glm4 <-glm(delta_m ~ apstrade + substrate + time,
           data = udens_sub_full,family = quasipoisson(link = "log"))

AIC(glm,glm1,glm2,glm3,glm4)

plot(residuals(glm1))
qqnorm(residuals(glm1))
qqline(residuals(glm1))
#Iepriekšējas labāks
#final model
summary(glsModel)
#Gan apstrādei gan substrātam, gan laikam būtiska ietekme

emmSubstr <- emmeans(glsModel, ~ substrate)  
pairs(emmSubstr)
pairs_subs <- pairs(emmSubstr, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subs)
sig_pairs_subs <- subset(pairs_df, p.value < 0.05)
sig_pairs_subs
#BSD2 and WS2 has significantly higher water absorbtion than all other substrates (p<0.05).
#WS3 is lower than WS1 and WS2.
emmapstrade <- emmeans(glsModel, ~ apstrade)  
pairs(emmapstrade)
#Suberinic acids + Chitosan had the lowest absorbtion of all (p<0.0001).
#Control and Chitosan had the higher absorbtion than others (p<0.05)
emmTime <- emmeans(glsModel, ~ time)  
pairs(emmTime)
#after 24h had higher absorbtionthan 2h (p<0.0001).

plot(allEffects(glsModel))

#corected----
glsModel1 <- gls(
  tilpumprocenti ~ apstrade + substrate + time,
  data = udens_sub_full
)

plot(residuals(glsModel1))
qqnorm(residuals(glsModel1))
qqline(residuals(glsModel1))

summary(glsModel1)


emmSubstr <- emmeans(glsModel1, ~ substrate)  
pairs(emmSubstr)
pairs_subs <- pairs(emmSubstr, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subs)
sig_pairs_subs <- subset(pairs_df, p.value < 0.05)
sig_pairs_subs
#BSD2 and WS2 has significantly higher water absorbtion than all other substrates (p<0.05).
#WS3 is lower than WS1 and WS2.
emmapstrade <- emmeans(glsModel1, ~ apstrade)  
pairs(emmapstrade)
#Suberinic acids + Chitosan had the lowest absorbtion of all (p<0.0001).
#Control and Chitosan had the higher absorbtion than others (p<0.05)
emmTime <- emmeans(glsModel1, ~ time)  
pairs(emmTime)
#after 24h had higher absorbtionthan 2h (p<0.0001).

plot(allEffects(glsModel1))
#tilpuma izmaiņa----
##Normalitāte----
# QQ plot with facets
qq_plot <- ggplot(udens_sub_full, aes(sample = delta_v)) +
  facet_grid(time ~ substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

#Nenoraidu normalitāti

ggplot(udens_sub_full, aes(x = substrate, y = delta_v)) + 
  geom_boxplot() +
  labs(x = "substrate", y = "delta m", fill = "apstrade")+facet_grid(time ~ substrate ~ apstrade)
#izlecējvērtības - Noraidu normalitāti

##Paraugkopu salīdzināšana----
vlmeModelm <- lme(delta_v ~ apstrade + substrate + time, random = ~1 | id, data = udens_sub_full)
vlmeModelm1 <- lme(delta_v ~ apstrade + substrate + time+ sub_sk_g_g_mk + hito_g_g_mk, random = ~1 | id, data = udens_sub_full)
vlmeModelm2 <- lme(delta_v ~ substrate + time+ sub_sk_g_g_mk + hito_g_g_mk, random = ~1 | id, data = udens_sub_full)


AIC(vlmeModelm, vlmeModelm1, vlmeModelm2)

plot(residuals(vlmeModelm))
qqnorm(residuals(vlmeModelm))
qqline(residuals(vlmeModelm))
#good enaugh

#final model
summary(vlmeModelm)
#Būtiska ietekme ganapstrādei, gansubstrātam
emmSubstr <- emmeans(vlmeModelm, ~ substrate)  
pairs(emmSubstr)
pairs_subs <- pairs(emmSubstr, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subs)
sig_pairs_subs <- subset(pairs_df, p.value < 0.05)
sig_pairs_subs
#BSD1bija būtiski augstāka uzbriešana tilpumā, nekā WS1,WS2 un WS4 (p<0.05).
emmApstrade <- emmeans(vlmeModelm, ~ apstrade)  
pairs(emmApstrade)
#hitozāna apstrāde izsauca būtiski augstāku uzbriešanu tilpumā,nekā pārējāsapstrādes.
emmTime <- emmeans(vlmeModelm, ~ time)  
pairs(emmTime)
#Pēc 2h bija augstāka uzbriešana nekā pēc 24h (p=0.0002).(nav loģiski - neuzticami dati)
plot(allEffects(vlmeModelm))
#Attēli----

mass1 <- ggplot(udens_sub_summary, aes(x = substrate, y = mean_delta_m_kond, fill = apstrade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_delta_m_kond - sd_delta_m_kond, ymax = mean_delta_m_kond + sd_delta_m_kond), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  facet_grid(~time, labeller = labeller(time = function(x) paste0(x, " h"))) + # Add this line
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    panel.spacing = unit(1, "lines"), 
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top",  
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10),  
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    plot.margin = margin(6, 10, 9.8, 10)
  ) +
  labs(
    x = NULL,
    y = "Ūdens absorbcija (%)", 
    fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))+
theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


mass1

ggsave("udensSorbcMasa1_2025.png", plot = mass1, width = 8, height = 4, units = "in", dpi = 200)


laiks24 <- filter(udens_sub_summary, time == 24)

Tiplpr <- ggplot(laiks24, aes(x = substrate, y = mean_tilppr, fill = apstrade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_tilppr - sd_tilppr, ymax = mean_tilppr + sd_tilppr), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    panel.spacing = unit(1, "lines"), 
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top",  
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10),  
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    plot.margin = margin(6, 10, 9.8, 10)
  ) +
  labs(
    x = NULL,
    y = "Ūdens absorbcija (% no tilpuma)", 
    fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


Tiplpr

ggsave("udensSorbcTilpumprocenti_2025.png", plot = Tiplpr, width = 8, height = 4, units = "in", dpi = 200)



udens <- ggplot(udens_sub_summary, aes(x = substrate, y = mean_udens, fill = apstrade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_udens - sd_udens, ymax = mean_udens + sd_udens), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  facet_grid(~time, labeller = labeller(time = function(x) paste0(x, " h"))) + # Add this line
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    panel.spacing = unit(1, "lines"), 
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top",  
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10),  
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    plot.margin = margin(6, 10, 9.8, 10)
  ) +
  labs(
    x = NULL,
    y = "Water amount (g)", 
    fill = NULL
  )

udens

udensgg <- ggplot(udens_sub_summary, aes(x = substrate, y = mean_udensgg, fill = apstrade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_udensgg - sd_udensgg, ymax = mean_udensgg + sd_udensgg), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  facet_grid(~time, labeller = labeller(time = function(x) paste0(x, " h"))) + # Add this line
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    panel.spacing = unit(1, "lines"), 
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top",  
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10),  
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    plot.margin = margin(6, 10, 9.8, 10)
  ) +
  labs(
    x = NULL,
    y = "Water amount (g)", 
    fill = NULL
  )

udensgg


#Mass----
mass <- ggplot(laiks24, aes(x = substrate, y = mean_delta_m, fill = apstrade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_delta_m - sd_delta_m, ymax = mean_delta_m + sd_delta_m), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    panel.spacing = unit(1, "lines"), 
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top",  
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10),  
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    plot.margin = margin(6, 10, 9.8, 10)
  ) +
  labs(
    x = NULL,
    y = "Ūdens absorbcija (% pret sāk. masu)", 
    fill = NULL
  ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  scale_fill_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 


mass


ggsave("udensSorbcMasa2025.png", plot = mass, width = 8, height = 4, units = "in", dpi = 200)



swelling <- ggplot(udens_sub_summary, aes(x = substrate, y = mean_pos_delta_v, fill = apstrade)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +
  geom_errorbar(aes(ymin = mean_pos_delta_v - sd_pos_delta_v, ymax = mean_pos_delta_v + sd_pos_delta_v), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  facet_grid(~time, labeller = labeller(time = function(x) paste0(x, " h"))) + 
  theme(
    axis.text.x = element_text(size = 10), 
    axis.text.y = element_text(size = 10), 
    panel.spacing = unit(1, "lines"), 
    panel.grid.major = element_line(color = "grey90"), 
    panel.grid.minor = element_blank(),
    legend.position = "top",  
    legend.justification = c(0, 1),
    legend.text = element_text(size = 10),  
    legend.title = element_text(size = 10),  
    legend.background = element_rect(fill = NA),
    legend.direction = "horizontal",
    plot.margin = margin(6, 10, 9.8, 10)
  ) +
  labs(
    x = NULL,
    y = "Volumetric swelling (%)", 
    fill = NULL
  )+
  scale_fill_manual(values = c("Control" = "#e04","Chitosan" = "#fd0","Suberinic acids" = "#00a","Chitosan + Suberinic acids" = "#0ed"))


swelling

ggsave("udensSorbcTilpums2025.png", plot = swelling, width = 10, height = 5, units = "in", dpi = 120)

ggplot(udens_sub_full, aes(x = substrate, y = delta_m, fill = apstrade))+
  geom_boxplot()+
  facet_grid(~time)

ggplot(udens_sub_full, aes(x = sub_sk_g_g_mk, y = udens, colour = substrate, shape = apstrade))+
  geom_point()+
  facet_grid(~time)

ggplot(udens_sub_full, aes(x = sub_sk_g_g_mk, y = udensgg, colour = substrate, shape = apstrade))+
  geom_point()+
  facet_grid(~time)

ggplot(udens_sub_full, aes(x = sub_sk_g_g_mk, y = delta_m, colour = substrate, shape = apstrade))+
  geom_point()+
  facet_grid(~time)

ggplot(udens_sub_full, aes(x = sub_sk_g_g_mk, y = tilpumprocenti, colour = substrate, shape = apstrade))+
  geom_point()+
  facet_grid(~time)

ggplot(udens_sub_full, aes(x = sub_sk_g_g_mk, y = delt_m_kond, colour = substrate, shape = apstrade))+
  geom_point()+
  facet_grid(~time)

ggplot(udens_sub_full, aes(x = tilpums, y = udens, colour = substrate, shape = apstrade))+
  geom_point()+
  facet_grid(~time)

cor.test(udens_sub_full$delt_m_kond,udens_sub_full$sub_sk_g_g_mk,method="spearman")




library(patchwork)
mass <- mass + 
  annotate("text", x = -Inf, y = Inf, label = "A", hjust = -0.5, vjust = 1.5, size = 5, fontface = "bold")

Tiplpr <- Tiplpr + 
  annotate("text", x = -Inf, y = Inf, label = "B", hjust = -0.5, vjust = 1.5, size = 5, fontface = "bold")

# 2. Combine them normally (without plot_annotation)
UdensCombined <- (mass / Tiplpr) + 
  plot_layout(guides = "collect", axes = "collect") & 
  theme(legend.position = "top",
        legend.text = element_text(size = 12))

UdensCombined

ggsave("UdensSorbc.png", plot = UdensCombined, width = 7.5, height = 7, units = "in", dpi = 96)
