#pievieno paplašinājumus----
library(readxl)
library(writexl)
library(ggplot2)
library(emmeans)
library(dplyr)
library(janitor)
library(ggrepel)
library(gridExtra)
library(tidyr)
library(lme4)
library(stringr)


#Norāda darba direktoriju (tā, kurā atrodas R fails)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Datu importesana ----
#Norāda excel faila nosaukumu un lapu kurā atrodas tabula
dati<-read_excel("MK  higroskopija 2024.xlsx", sheet = 3)
datisub <- read_excel("MK  higroskopija 2025.xlsx", sheet = 3)
str(dati)
str(datisub)

#Sagatavo datus----
dati=clean_names(dati)
datisub=clean_names(datisub)

dati$slanu_skaits <- as.factor(dati$slanu_skaits)
dati$substrate <- as.factor(dati$substrate)
dati$rh <- as.factor(dati$rh)

datisub$substrate <- as.factor(datisub$substrate)
datisub$rh <- as.factor(datisub$rh)
datisub$apstrade <- as.factor(datisub$apstrade)

dati$date <- as.Date(dati$date)
datisub$date <- as.Date(datisub$date)
# Find the minimum date (this will be day 0)
min_date <- min(dati$date)
min_datesub <- min(datisub$date)
# Calculate the difference in days from the minimum date
dati$laiks <- as.numeric(dati$date - min_date)
dati$laiks <- as.factor(dati$laiks)
datisub$laiks <- as.numeric(datisub$date - min_datesub+2)
datisub$laiks <- as.factor(datisub$laiks)
# Exclude rows where time is 34 because mold growth may have affected the results
dati <- dati[dati$laiks != 34, ]
dati <- dati[dati$laiks != 0, ]

control <- dati[dati$slanu_skaits == 0, ]
datihito <- dati[dati$slanu_skaits == 2, ]
control$apstrade <- "control"
datihito$apstrade <- "hito"
control$slanu_skaits <- NULL
datihito$slanu_skaits <- NULL
control$apstrade <- as.factor(control$apstrade)
control$id <- as.factor(control$id)
datisub$id <- as.factor(datisub$id)
datihito$id <- as.factor(datihito$id)
str(control)
str(datisub)

datifull <- full_join(control,datisub)
datifull <- full_join(datifull,datihito)

datifull <- datifull[datifull$substrate != "WSz3", ]

reference <- datifull[datifull$apstrade == "ref", ]
datifull <- datifull[datifull$apstrade != "ref", ]
reference$apstrade <- "control"

datifull <- full_join(datifull,reference)

#Mean SD----
dati_summary <- datifull %>%
  group_by(laiks, apstrade, substrate, rh) %>%
  summarise(
    mean_udens_sat = mean(humidity_percent, na.rm = TRUE),
    sd_udens_sat = sd(humidity_percent, na.rm = TRUE)
  )

plato <- datifull %>%
  group_by(rh, id, substrate, apstrade) %>%
  filter(humidity_percent == max(humidity_percent)) %>%  # Keep rows with max udens_saturs
  slice_max(laiks, n = 1) %>%  # If tie, choose the one with greatest laiks
  ungroup() %>%
  filter(laiks != 0) %>%  # Exclude rows where laiks is 0
  mutate(apstrade = as.factor(apstrade))

summary_table <- plato %>%
  group_by(apstrade, substrate, rh) %>%
  summarise(
    mean_udens_sat = mean(humidity_percent, na.rm = TRUE),
    sd_udens_sat = sd(humidity_percent, na.rm = TRUE)
  )

write_xlsx(summary_table, "higroskopija_summary.xlsx")

#Normalitāte----

datifull$laiks <- as.numeric(as.character(datifull$laiks))
plato$laiks <- as.numeric(as.character(plato$laiks))

shapiro_results <- plato %>%
  group_by(substrate, apstrade, rh) %>%
  summarise(
    n = n(),
    p_value = if (n >= 3) shapiro.test(humidity_percent)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    normality = case_when(
      n < 3 ~ "Not applicable (n < 3)",
      p_value < 0.05 ~ "Not normal",
      TRUE ~ "Normal"
    )
  )


# View the results
print(shapiro_results)


# noraidu normalitāti.

#Paraugkopu salīdzināšana---- 
plato$rh <- factor(plato$rh)

lmerModel <- lmer(humidity_percent ~ rh + apstrade + substrate + 
                    (1 | id), data = plato)
# Log-transform the response variable
lmerModel1 <- lmer(log(humidity_percent) ~ rh + apstrade + substrate + 
                    (1 | id), data = plato)
# Include a quadratic or higher-order term
lmerModel2 <- lmer(humidity_percent ~ poly(rh, 3) + apstrade + substrate + 
                    (1 | id), data = plato)

AIC(lmerModel,lmerModel1,lmerModel2)
summary(lmerModel1)


plot(residuals(lmerModel1))
qqnorm(residuals(lmerModel1))
qqline(residuals(lmerModel1))


emmSubstr <- emmeans(lmerModel1, ~ substrate)  
pairs(emmSubstr)

pairs_subs <- pairs(emmSubstr, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subs)
sig_pairs_subs <- subset(pairs_df, p.value < 0.05)
sig_pairs_subs

#Visiem references paraugiem, izņemot BP bija būtiski zemāks higroskopiskums, nekā kontroles MK (p<0.05)
#WS4 bija būtiski zemāks higroskopiskums, nekā BSD2 un BSD3 (p<0.05).
emmApstrader <- emmeans(lmerModel1, ~ apstrade)  
pairs(emmApstrader)
#Būtiskas atšķirības pastāv starp visiem apstrādes veidiem, izņemot Hitozāns + suberīnskābes : suberīnskābes (p<0.001).
#Viszemākais higroskopiskums ir sub un sub_hito paraugiem, tad kontroles grupai un visaugstākais hito paraugiem.
emmHumid <- emmeans(lmerModel1, ~ rh)  
pairs(emmHumid)

emmSubsAps <- emmeans(lmerModel1, ~ apstrade + substrate)
pairs_subsaps <- pairs(emmSubsAps, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subsaps)
sig_pairs_subsaps <- subset(pairs_df, p.value < 0.05)
sig_pairs_subsaps

#Tilpumprocenti----
  #Normalitāte----

datifull$laiks <- as.numeric(as.character(datifull$laiks))
plato$laiks <- as.numeric(as.character(plato$laiks))

shapiro_results <- plato %>%
  group_by(substrate, apstrade, rh) %>%
  summarise(
    n = n(),
    p_value = if (n >= 3) shapiro.test(tilpumprocenti)$p.value else NA_real_,
    .groups = "drop"
  ) %>%
  mutate(
    normality = case_when(
      n < 3 ~ "Not applicable (n < 3)",
      p_value < 0.05 ~ "Not normal",
      TRUE ~ "Normal"
    )
  )


# View the results
print(shapiro_results)


# noraidu normalitāti.

#Paraugkopu salīdzināšana---- 
plato$rh <- factor(plato$rh)

lmerModel <- lmer(tilpumprocenti ~ rh + apstrade + substrate + 
                    (1 | id), data = plato)
# Log-transform the response variable
lmerModel1 <- lmer(log(tilpumprocenti) ~ rh + apstrade + substrate + 
                     (1 | id), data = plato)
# Include a quadratic or higher-order term
lmerModel2 <- lmer(tilpumprocenti ~ poly(rh, 3) + apstrade + substrate + 
                     (1 | id), data = plato)

AIC(lmerModel,lmerModel1,lmerModel2)
summary(lmerModel1)


plot(residuals(lmerModel1))
qqnorm(residuals(lmerModel1))
qqline(residuals(lmerModel1))


emmSubstr <- emmeans(lmerModel1, ~ substrate)  
pairs(emmSubstr)

pairs_subs <- pairs(emmSubstr, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subs)
sig_pairs_subs <- subset(pairs_df, p.value < 0.05)
sig_pairs_subs

#Visiem references paraugiem, izņemot BP bija būtiski zemāks higroskopiskums, nekā kontroles MK (p<0.05)
#WS4 bija būtiski zemāks higroskopiskums, nekā BSD2 un BSD3 (p<0.05).
emmApstrader <- emmeans(lmerModel1, ~ apstrade)  
pairs(emmApstrader)
#Būtiskas atšķirības pastāv starp visiem apstrādes veidiem, izņemot Hitozāns + suberīnskābes : suberīnskābes (p<0.001).
#Viszemākais higroskopiskums ir sub un sub_hito paraugiem, tad kontroles grupai un visaugstākais hito paraugiem.
emmHumid <- emmeans(lmerModel1, ~ rh)  
pairs(emmHumid)

emmSubsAps <- emmeans(lmerModel1, ~ apstrade + substrate)
pairs_subsaps <- pairs(emmSubsAps, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subsaps)
sig_pairs_subsaps <- subset(pairs_df, p.value < 0.05)
sig_pairs_subsaps

#Attēls----
plato$laiks <- as.numeric(as.character(plato$laiks))
plato$rh <- as.numeric(as.character(plato$rh))


plato$apstrade <- factor(
  plato$apstrade,
  levels = c("control", "hito", "sub", "hito_sub"),  # Desired order
  labels = c("Kontrole", "Hitozāns", "Suberīnskābes", "Hitozāns + Suberīnskābes")  # Desired names
)

plato$substrate <- factor(
  plato$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4", "BP", "mMDF", "LPB", "MRLPB", "LMDF", "MDF"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S", "BP", "mMDF", "LPB", "MRLPB", "LMDF", "MDF")  # Desired names
)

plato <- plato[plato$rh != 95, ]

Sl <- plato[plato$substrate %in% c("B–Kl", "B–Kl–K", "B–Kl–Z", "B"), ]
Sa <- plato[plato$substrate %in% c("S–Kl", "S–Kl–K", "S–Kl–Z", "S"), ]
Ref <- plato[plato$substrate %in% c("BP", "mMDF", "LPB", "MRLPB", "LMDF", "MDF"), ]


# ~ gaisa mitrums



# Find common y-axis limits


min_y <- min(c(Sl$humidity_percent, Sa$humidity_percent))
max_y <- max(c(Sl$humidity_percent, Sa$humidity_percent))

Sllabel_data <- Sl %>%
  group_by(substrate, apstrade) %>%
  filter(rh == max(rh)) %>%
  summarise(rh = first(rh),   # Keep the max x-value
            humidity_percent = mean(humidity_percent),
            .groups = "drop")  # Avoid warnings

Salabel_data <- Sa %>%
  group_by(substrate, apstrade) %>%
  filter(rh == max(rh)) %>%
  summarise(rh = first(rh),   # Keep the max x-value
            humidity_percent = mean(humidity_percent),      
            .groups = "drop")  # Avoid warnings

Reflabel_data <- Ref %>%
  group_by(substrate, apstrade) %>%
  filter(rh == max(rh)) %>%
  summarise(rh = first(rh),   # Keep the max x-value
            humidity_percent = mean(humidity_percent),    
            .groups = "drop")  # Avoid warnings


plot1 <- ggplot(Sl, aes(x = rh, y = humidity_percent, shape = substrate, colour = apstrade)) +
  geom_smooth(method = "loess", se = FALSE) +  
  geom_text_repel(
    data = Sllabel_data, aes(label = substrate), 
    nudge_x = 7, direction = "y", size = 3, fontface = "bold",
    segment.color = "grey40", segment.size = 0.2, 
    box.padding = 0.1, point.padding = 0.1
  ) +  
  theme_bw() +
  theme(
    legend.position = c(0.4, 0.9),    # Move legend to the right for better visibility
    legend.text = element_text(size = 11),  # Make legend text bigger
    legend.title = element_text(size = 11),  # Increase legend title size
    legend.background = element_rect(fill = NA),
    axis.title = element_text(size = 15)
  ) +
  labs(colour = NULL, x = NULL, y = NULL) +
  coord_cartesian(ylim = c(min_y, max_y))+
  scale_colour_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))

plot2 <- ggplot(Sa, aes(x = rh, y = humidity_percent, shape = substrate, colour = apstrade)) +
  geom_smooth(method = "loess", se = FALSE) +  
  geom_text_repel(
    data = Salabel_data, aes(label = substrate), 
    nudge_x = 7, direction = "y", size = 3, fontface = "bold",
    segment.color = "grey40", segment.size = 0.2, 
    box.padding = 0.1, point.padding = 0.1
  ) +  
  theme_bw() +
  theme(
    legend.position = c(0.4, 0.9),  
    legend.text = element_text(size = 11),  
    legend.title = element_text(size = 11),  
    legend.background = element_rect(fill = NA),
    axis.title = element_text(size = 15)
  ) +
  labs(colour = NULL, x = NULL, y = NULL) +
  coord_cartesian(ylim = c(min_y, max_y))+
  scale_colour_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))



plot3 <- ggplot(Ref, aes(x = rh, y = humidity_percent, shape = substrate)) +
  geom_smooth(
    aes(colour = "References"),
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  geom_text_repel(
    data = Reflabel_data,
    aes(label = substrate),
    nudge_x = 7,
    direction = "y",
    size = 3,
    fontface = "bold",
    segment.color = "grey40",
    segment.size = 0.2,
    box.padding = 0.1,
    point.padding = 0.1
  ) +
  scale_colour_manual(
    name = NULL,               # legend title
    values = c("References" = "#0a3")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.2, 0.9),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.background = element_rect(fill = NA)
  ) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(min_y, max_y))


p = (grid.arrange(plot1, plot2, plot3, ncol = 3))+
  labs(x = "Relatīvais mitrums (%)", y = "Mitruma saturs (%)")

library(ggpubr)

p <- ggarrange(plot1, plot2, plot3, ncol = 3)

d <- annotate_figure(
  p,
  bottom = text_grob("Relatīvais mitrums (%)", size = 15),
  left   = text_grob("Mitruma saturs (%)", rot = 90, size = 15)
)
d
ggsave("Higroskopija2025.png",
       plot = d,
       width = 10,
       height = 6,
       units = "in",
       dpi = 120)

#Tilpumprocenti----
min_y <- 0
max_y <- max(c(Sl$tilpumprocenti,Sa$tilpumprocenti))

Sllabel_data <- Sl %>%
  group_by(substrate, apstrade) %>%
  filter(rh == max(rh)) %>%
  summarise(rh = first(rh),   # Keep the max x-value
            tilpumprocenti = mean(tilpumprocenti),
            .groups = "drop")  # Avoid warnings

Salabel_data <- Sa %>%
  group_by(substrate, apstrade) %>%
  filter(rh == max(rh)) %>%
  summarise(rh = first(rh),   # Keep the max x-value
            tilpumprocenti = mean(tilpumprocenti),      
            .groups = "drop")  # Avoid warnings

Reflabel_data <- Ref %>%
  group_by(substrate, apstrade) %>%
  filter(rh == max(rh)) %>%
  summarise(rh = first(rh),   # Keep the max x-value
            tilpumprocenti = mean(tilpumprocenti),    
            .groups = "drop")  # Avoid warnings

plot1 <- ggplot(Sl, aes(x = rh, y = tilpumprocenti, shape = substrate, colour = apstrade)) +
  geom_smooth(method = "loess", se = FALSE) +  
  geom_text_repel(
    data = Sllabel_data, aes(label = substrate), 
    nudge_x = 7, direction = "y", size = 3, fontface = "bold",
    segment.color = "grey40", segment.size = 0.2, 
    box.padding = 0.1, point.padding = 0.1
  ) +  
  theme_bw() +
  theme(
    legend.position = c(0.4, 0.9),    # Move legend to the right for better visibility
    legend.text = element_text(size = 11),  # Make legend text bigger
    legend.title = element_text(size = 11),  # Increase legend title size
    legend.background = element_rect(fill = NA),
    axis.title = element_text(size = 15)
  ) +
  labs(colour = NULL, x = NULL, y = NULL) +
  coord_cartesian(ylim = c(min_y, max_y))+
  scale_colour_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))

plot2 <- ggplot(Sa, aes(x = rh, y = tilpumprocenti, shape = substrate, colour = apstrade)) +
  geom_smooth(method = "loess", se = FALSE) +  
  geom_text_repel(
    data = Salabel_data, aes(label = substrate), 
    nudge_x = 7, direction = "y", size = 3, fontface = "bold",
    segment.color = "grey40", segment.size = 0.2, 
    box.padding = 0.1, point.padding = 0.1
  ) +  
  theme_bw() +
  theme(
    legend.position = c(0.4, 0.9),  
    legend.text = element_text(size = 11),  
    legend.title = element_text(size = 11),  
    legend.background = element_rect(fill = NA),
    axis.title = element_text(size = 15)
  ) +
  labs(colour = NULL, x = NULL, y = NULL) +
  coord_cartesian(ylim = c(min_y, max_y))+
  scale_colour_manual(values = c("Kontrole" = "#e04","Hitozāns" = "#eb0","Suberīnskābes" = "#00a","Hitozāns + Suberīnskābes" = "#0dd"))



plot3 <- ggplot(Ref, aes(x = rh, y = tilpumprocenti, shape = substrate)) +
  geom_smooth(
    aes(colour = "References"),
    method = "loess",
    se = FALSE,
    linewidth = 1
  ) +
  geom_text_repel(
    data = Reflabel_data,
    aes(label = substrate),
    nudge_x = 7,
    direction = "y",
    size = 3,
    fontface = "bold",
    segment.color = "grey40",
    segment.size = 0.2,
    box.padding = 0.1,
    point.padding = 0.1
  ) +
  scale_colour_manual(
    name = NULL,               # legend title
    values = c("References" = "#0a3")
  ) +
  theme_bw() +
  theme(
    legend.position = c(0.2, 0.9),
    legend.text = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.background = element_rect(fill = NA)
  ) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(ylim = c(min_y, max_y))


p = (grid.arrange(plot1, plot2, plot3, ncol = 3))+
  labs(x = "Relatīvais mitrums (%)", y = "Mitruma saturs (%)")

library(ggpubr)

p <- ggarrange(plot1, plot2, ncol = 2)

d <- annotate_figure(
  p,
  bottom = text_grob("Relatīvais mitrums (%)", size = 15),
  left   = text_grob("Mitruma saturs (%)", rot = 90, size = 15)
)
d
ggsave("Higroskopija2025.png",
       plot = d,
       width = 10,
       height = 6,
       units = "in",
       dpi = 120)
