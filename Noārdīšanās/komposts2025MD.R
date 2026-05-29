library(readxl)
library(writexl)
library(janitor)
library(ggplot2)
library(dplyr)
library(qqplotr)
library(rstatix)
library(effects)
library(gridExtra)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

dati <- read_excel("komposts2025.xlsx", sheet = 4)
viz <- read_excel("komposts2025.xlsx", sheet = 3)
chnsctrlpirms <- read_excel("komposts flpp 2024.xlsx", sheet = 6)
chnsctrlpec <- read_excel("komposts flpp 2024.xlsx", sheet = 7)
chnspirms <- read_excel("komposts2025.xlsx", sheet = 5)
chnspec <- read_excel("komposts2025.xlsx", sheet = 6)
vizcontr <- read_excel("komposts ERAF 29.06.23.xlsx", sheet = 4)
vizcontr2 <- read_excel("komposts flpp 2024.xlsx", sheet = 5)

#Preprocesing----
##Vizuālais----
viz<- clean_names(viz)
vizcontr <- clean_names(vizcontr)
vizcontr2 <- clean_names(vizcontr2)
vizcontr$apstrade <- "control"
viz <- viz %>%
  group_by(id) %>%
  mutate(
    days_since_start = as.numeric(difftime(date, min(date), units = "days"))
  ) %>%
  ungroup()

vizcontr <- vizcontr %>%
  group_by(id) %>%
  mutate(
    days_since_start = as.numeric(difftime(date, min(date), units = "days"))
  ) %>%
  ungroup()
vizcontr$id <-as.factor(vizcontr$id)
viz$id <-as.factor(viz$id)
remove(Viz)
Viz <- full_join(viz,vizcontr)

vizcontr2 <- vizcontr2 %>%
  group_by(id) %>%
  mutate(
    days_since_start = as.numeric(difftime(date, min(date), units = "days"))
  ) %>%
  ungroup()

vizcontr2 <- vizcontr2 %>% 
  filter(concentration == "0")

vizcontr2$apstrade <- "control"
vizcontr2$id <-as.factor(vizcontr2$id)
Viz <- full_join(Viz,vizcontr2)
Viz$concentration <- NULL


dati=clean_names(dati)

##CHNS----
chnspirms=clean_names(chnspirms)
chnspec=clean_names(chnspec)

chnsctrlpirms=clean_names(chnsctrlpirms)
chnsctrlpec=clean_names(chnsctrlpec)

chnsblank <- chnspec[chnspec$substrate == "Blank", ]
chnspec <- chnspec[chnspec$substrate != "Blank", ]

chnsctrlblank <- chnsctrlpec[chnsctrlpec$substrate == "Blank", ]
chnsctrlpec <- chnsctrlpec[chnsctrlpec$substrate != "Blank", ]

chnsctrlpec <- chnsctrlpec[chnsctrlpec$concentration == 0, ]
chnsctrlpirms <- chnsctrlpirms[chnsctrlpirms$concentration == 0, ]
chnsctrlpec$apstrade <- "control"
chnsctrlpirms$apstrade <- "control"
chnsctrlpec$concentration <- NULL
chnsctrlpirms$concentration <- NULL

chnspirms$apstrade <- as.factor(chnspirms$apstrade)
chnspec$apstrade <- as.factor(chnspec$apstrade)
chnspirms$id <- as.factor(chnspirms$id)
chnspec$id <- as.factor(chnspec$id)

chnsctrlpirms$apstrade <- as.factor(chnsctrlpirms$apstrade)
chnsctrlpec$apstrade <- as.factor(chnsctrlpec$apstrade)
chnsctrlpirms$id <- as.factor(chnsctrlpirms$id)
chnsctrlpec$id <- as.factor(chnsctrlpec$id)

###Average tehnical repeats----
chnspirms1 <- chnspirms %>%
  group_by(substrate, apstrade, id) %>%
  summarise(
    weight = mean(weight, na.rm = TRUE),
    n_percent = mean(n_percent, na.rm = TRUE),
    c_percent = mean(c_percent, na.rm = TRUE),
    h_percent = mean(h_percent, na.rm = TRUE),
    o_percent = mean(o_percent, na.rm = TRUE),
    s_percent = mean(s_percent, na.rm = TRUE),
    n = mean(n, na.rm = TRUE),
    c = mean(c, na.rm = TRUE),
    h = mean(h, na.rm = TRUE),
    o = mean(o, na.rm = TRUE),
    c_n = mean(c_n, na.rm = TRUE),
    h_c = mean(h_c, na.rm = TRUE),
    o_c = mean(o_c, na.rm = TRUE)
  )

chnspec1 <- chnspec %>%
  group_by(substrate,apstrade, id) %>%
  summarise(
    weight = mean(weight, na.rm = TRUE),
    n_percent = mean(n_percent, na.rm = TRUE),
    c_percent = mean(c_percent, na.rm = TRUE),
    h_percent = mean(h_percent, na.rm = TRUE),
    o_percent = mean(o_percent, na.rm = TRUE),
    s_percent = mean(s_percent, na.rm = TRUE),
    n = mean(n, na.rm = TRUE),
    c = mean(c, na.rm = TRUE),
    h = mean(h, na.rm = TRUE),
    o = mean(o, na.rm = TRUE),
    c_n = mean(c_n, na.rm = TRUE),
    h_c = mean(h_c, na.rm = TRUE),
    o_c = mean(o_c, na.rm = TRUE)
  )

chnsblank1 <- chnsblank %>%
  group_by(substrate, apstrade, id) %>%
  summarise(
    weight = mean(weight, na.rm = TRUE),
    n_percent = mean(n_percent, na.rm = TRUE),
    c_percent = mean(c_percent, na.rm = TRUE),
    h_percent = mean(h_percent, na.rm = TRUE),
    o_percent = mean(o_percent, na.rm = TRUE),
    s_percent = mean(s_percent, na.rm = TRUE),
    n = mean(n, na.rm = TRUE),
    c = mean(c, na.rm = TRUE),
    h = mean(h, na.rm = TRUE),
    o = mean(o, na.rm = TRUE),
    c_n = mean(c_n, na.rm = TRUE),
    h_c = mean(h_c, na.rm = TRUE),
    o_c = mean(o_c, na.rm = TRUE)
  )
ungroup(chnsblank1)
ungroup(chnspec1)
ungroup(chnspirms1)

chnsctrlpirms1 <- chnsctrlpirms %>%
  group_by(substrate, apstrade, id) %>%
  summarise(
    weight = mean(weight, na.rm = TRUE),
    n_percent = mean(n_percent, na.rm = TRUE),
    c_percent = mean(c_percent, na.rm = TRUE),
    h_percent = mean(h_percent, na.rm = TRUE),
    o_percent = mean(o_percent, na.rm = TRUE),
    s_percent = mean(s_percent, na.rm = TRUE),
    n = mean(n, na.rm = TRUE),
    c = mean(c, na.rm = TRUE),
    h = mean(h, na.rm = TRUE),
    o = mean(o, na.rm = TRUE),
    c_n = mean(c_n, na.rm = TRUE),
    h_c = mean(h_c, na.rm = TRUE),
    o_c = mean(o_c, na.rm = TRUE)
  )

chnsctrlpec1 <- chnsctrlpec %>%
  group_by(substrate, apstrade, id) %>%
  summarise(
    weight = mean(weight, na.rm = TRUE),
    n_percent = mean(n_percent, na.rm = TRUE),
    c_percent = mean(c_percent, na.rm = TRUE),
    h_percent = mean(h_percent, na.rm = TRUE),
    o_percent = mean(o_percent, na.rm = TRUE),
    s_percent = mean(s_percent, na.rm = TRUE),
    n = mean(n, na.rm = TRUE),
    c = mean(c, na.rm = TRUE),
    h = mean(h, na.rm = TRUE),
    o = mean(o, na.rm = TRUE),
    c_n = mean(c_n, na.rm = TRUE),
    h_c = mean(h_c, na.rm = TRUE),
    o_c = mean(o_c, na.rm = TRUE)
  )

chnsctrlblank1 <- chnsctrlblank %>%
  group_by(substrate, id) %>%
  summarise(
    weight = mean(weight, na.rm = TRUE),
    n_percent = mean(n_percent, na.rm = TRUE),
    c_percent = mean(c_percent, na.rm = TRUE),
    h_percent = mean(h_percent, na.rm = TRUE),
    o_percent = mean(o_percent, na.rm = TRUE),
    s_percent = mean(s_percent, na.rm = TRUE),
    n = mean(n, na.rm = TRUE),
    c = mean(c, na.rm = TRUE),
    h = mean(h, na.rm = TRUE),
    o = mean(o, na.rm = TRUE),
    c_n = mean(c_n, na.rm = TRUE),
    h_c = mean(h_c, na.rm = TRUE),
    o_c = mean(o_c, na.rm = TRUE)
  )
ungroup(chnsctrlblank1)
ungroup(chnsctrlpec1)
ungroup(chnsctrlpirms1)

#Combine
chnspirms1$expstate <- "Before experiment"
chnspec1$expstate <- "After experiment"

chnsctrlpirms1$expstate <- "Before experiment"
chnsctrlpec1$expstate <- "After experiment"

chns <- bind_rows(chnspirms1, chnspec1)
chnsblank1$apstrade <- as.factor(chnsblank1$apstrade)
chnsblank1$id <- as.factor(chnsblank1$id)
chnsblank1$expstate <- "Blank"

chnsfull <- bind_rows(chns, chnsblank1)

chnsctrl <- bind_rows(chnsctrlpirms1, chnsctrlpec1)
chnsctrlblank1$id <- as.factor(chnsctrlblank1$id)
chnsctrlblank1$expstate <- "Blank"
chnsctrlblank1$apstrade <- "Blank"
chnsctrlfull <- bind_rows(chnsctrl, chnsctrlblank1)

#Blank tests
chnsblpir <- chnsfull[chnsfull$expstate != "After experiment", ]
t.test(c_percent ~ expstate, data = chnsblpir)
t.test(n_percent ~ expstate, data = chnsblpir)
t.test(h_percent ~ expstate, data = chnsblpir)
t.test(c_n ~ expstate, data = chnsblpir)

ggplot(chnsfull, aes(x = expstate, y = c_percent, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
ggplot(chnsfull, aes(x = expstate, y = n_percent, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
ggplot(chnsfull, aes(x = expstate, y = h_percent, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
ggplot(chnsfull, aes(x = expstate, y = c_n, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")


ggplot(chnsctrlfull, aes(x = expstate, y = c_percent, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
ggplot(chnsctrlfull, aes(x = expstate, y = n_percent, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
ggplot(chnsctrlfull, aes(x = expstate, y = h_percent, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
ggplot(chnsctrlfull, aes(x = expstate, y = c_n, fill = substrate)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "substrate")
###Blank corection----

meanblank <- chnsblank1 %>%
  group_by(expstate) %>%
  summarise(
    avg_n_percent = mean(n_percent, na.rm = TRUE),
    avg_c_percent = mean(c_percent, na.rm = TRUE),
    avg_h_percent = mean(h_percent, na.rm = TRUE),
    avg_o_percent = mean(o_percent, na.rm = TRUE),
    avg_s_percent = mean(s_percent, na.rm = TRUE),
    avg_c_n = mean(c_n, na.rm = TRUE),
    avg_h_c = mean(h_c, na.rm = TRUE),
    avg_o_c = mean(o_c, na.rm = TRUE),
    )

meanpirms <- chnspirms1 %>%
  group_by(expstate) %>%
  summarise(
    avg_n_percent = mean(n_percent, na.rm = TRUE),
    avg_c_percent = mean(c_percent, na.rm = TRUE),
    avg_h_percent = mean(h_percent, na.rm = TRUE),
    avg_o_percent = mean(o_percent, na.rm = TRUE),
    avg_s_percent = mean(s_percent, na.rm = TRUE),
    avg_c_n = mean(c_n, na.rm = TRUE),
    avg_h_c = mean(h_c, na.rm = TRUE),
    avg_o_c = mean(o_c, na.rm = TRUE),
  )

differenceC <- (meanblank$avg_c_percent - meanpirms$avg_c_percent)
differenceN <- (meanblank$avg_n_percent - meanpirms$avg_n_percent)
differenceH <- (meanblank$avg_h_percent - meanpirms$avg_h_percent)
differenceCN <- (meanblank$avg_c_n - meanpirms$avg_c_n)

chnsnoncor <- chns

chnspec1$c_percent =chnspec1$c_percent - differenceC
chnspec1$n_percent =chnspec1$n_percent - differenceN
chnspec1$h_percent =chnspec1$h_percent - differenceH
chnspec1$c_n =chnspec1$c_n - differenceCN

chns <- bind_rows(chnspirms1, chnspec1)



meanblankctrl <- chnsctrlblank1 %>%
  group_by(expstate) %>%
  summarise(
    avg_n_percent = mean(n_percent, na.rm = TRUE),
    avg_c_percent = mean(c_percent, na.rm = TRUE),
    avg_h_percent = mean(h_percent, na.rm = TRUE),
    avg_o_percent = mean(o_percent, na.rm = TRUE),
    avg_s_percent = mean(s_percent, na.rm = TRUE),
    avg_c_n = mean(c_n, na.rm = TRUE),
    avg_h_c = mean(h_c, na.rm = TRUE),
    avg_o_c = mean(o_c, na.rm = TRUE),
  )

meanpirmsctrl <- chnsctrlpirms1 %>%
  group_by(expstate) %>%
  summarise(
    avg_n_percent = mean(n_percent, na.rm = TRUE),
    avg_c_percent = mean(c_percent, na.rm = TRUE),
    avg_h_percent = mean(h_percent, na.rm = TRUE),
    avg_o_percent = mean(o_percent, na.rm = TRUE),
    avg_s_percent = mean(s_percent, na.rm = TRUE),
    avg_c_n = mean(c_n, na.rm = TRUE),
    avg_h_c = mean(h_c, na.rm = TRUE),
    avg_o_c = mean(o_c, na.rm = TRUE),
  )

differenceCctrl <- (meanblankctrl$avg_c_percent - meanpirmsctrl$avg_c_percent)
differenceNctrl <- (meanblankctrl$avg_n_percent - meanpirmsctrl$avg_n_percent)
differenceHctrl <- (meanblankctrl$avg_h_percent - meanpirmsctrl$avg_h_percent)
differenceCNctrl <- (meanblankctrl$avg_c_n - meanpirmsctrl$avg_c_n)

chnsctrlnoncor <- chnsctrl

chnsctrlpec1$c_percent =chnsctrlpec1$c_percent - differenceCctrl
chnsctrlpec1$n_percent =chnsctrlpec1$n_percent - differenceNctrl
chnsctrlpec1$h_percent =chnsctrlpec1$h_percent - differenceHctrl
chnsctrlpec1$c_n =chnsctrlpec1$c_n - differenceCNctrl

chnsctrl <- bind_rows(chnsctrlpirms1, chnsctrlpec1)

chns <- bind_rows(chns, chnsctrl)

chnspec2 <- bind_rows(chnspec1, chnsctrlpec1)
chnspirms2 <- bind_rows(chnspirms1, chnsctrlpirms1)
chnspec2$diffC <- chnspec2$c_percent - chnspirms2$c_percent
chnspec2$diffN <- chnspec2$n_percent - chnspirms2$n_percent
chnspec2$diffH <- chnspec2$h_percent - chnspirms2$h_percent
chnspec2$diffS <- chnspec2$s_percent - chnspirms2$s_percent
chnspec2$diffO <- chnspec2$o_percent - chnspirms2$o_percent
chnspec2$diffCN <- chnspec2$c_n - chnspirms2$c_n

##Mean SD----

chns_summary <- chns %>%
  group_by(substrate,apstrade,expstate) %>%
  summarise(
    avg_n_percent = mean(n_percent, na.rm = TRUE),
    sd_n_percent = sd(n_percent, na.rm = TRUE),
    avg_c_percent = mean(c_percent, na.rm = TRUE),
    sd_c_percent = sd(c_percent, na.rm = TRUE),
    avg_h_percent = mean(h_percent, na.rm = TRUE),
    sd_h_percent = sd(h_percent, na.rm = TRUE),
    avg_o_percent = mean(o_percent, na.rm = TRUE),
    sd_o_percent = sd(o_percent, na.rm = TRUE),
    avg_s_percent = mean(s_percent, na.rm = TRUE),
    sd_s_percent = sd(s_percent, na.rm = TRUE),
    avg_c_n = mean(c_n, na.rm = TRUE),
    sd_c_n = sd(c_n, na.rm = TRUE),
    avg_h_c = mean(h_c, na.rm = TRUE),
    sd_h_c = sd(h_c, na.rm = TRUE),
    avg_o_c = mean(o_c, na.rm = TRUE),
    sd_o_c = sd(o_c, na.rm = TRUE)
  )

write_xlsx(chns_summary, "chns_komposts.xlsx")

dati_summary <- dati %>%
  group_by(substrate,apstrade) %>%
  summarise(
    avg_ph = mean(p_h, na.rm = TRUE),
    sd_ph = sd(p_h, na.rm = TRUE),
    avg_end_mass_percent = mean(end_mass_percent_corected, na.rm = TRUE),
    sd_end_mass_percent = sd(end_mass_percent_corected, na.rm = TRUE)
  )

write_xlsx(dati_summary, "ph_mass_komposts.xlsx")

chns$apstrade <- as.factor(chns$apstrade)
chns$expstate <- as.factor(chns$expstate)
chns$substrate <- as.factor(chns$substrate)

chns <- chns %>% ungroup()  # Remove grouping

chnspec2$apstrade <- factor(
  chnspec2$apstrade,
  levels = c("control", "sub"),
  labels = c("Kontrole","Suberīnskābes")
)

dati$apstrade <- factor(
  dati$apstrade,
  levels = c("control", "sub"),
  labels = c("Kontrole","Suberīnskābes")
)

Viz$apstrade <- factor(
  Viz$apstrade,
  levels = c("control", "sub"),
  labels = c("Kontrole","Suberīnskābes")
)

dati$substrate <- factor(
  dati$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)

chns$substrate <- factor(
  chns$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)

chnspec2$substrate <- factor(
  chnspec2$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)

Viz$substrate <- factor(
  Viz$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)
#Statics----
##pH----
###Normality----

qq_plot <- ggplot(dati, aes(sample = p_h)) +
  facet_grid(substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

#Nenoraidu normalitāti

ggplot(dati, aes(x = substrate, y = p_h, fill=apstrade)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")
#nenoraidu normalitāti

levene_test(p_h ~ interaction(substrate, apstrade), data = dati)

#Dispersija homogēna

###comparisons----
pHaov <- aov(p_h ~ substrate + apstrade, data = dati)
pHaov1 <- aov(p_h ~ substrate * apstrade, data = dati)

AIC(pHaov,pHaov1)
summary(pHaov)
#BUtiskaietekme ir apstrādei (p = 0.006) un substrātam (p=0.035).
TukeyHSD(pHaov)
#Nav identificētas būtiskas pāru atšķirības
plot(allEffects(pHaov))
###Attēls----

pH <- ggplot(dati, aes(x = substrate, y = p_h, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrāts", y = "pH vērtība", fill = NULL)+
  theme_bw()+
  theme(
    legend.position = c(0.22, 0.8))+
  scale_fill_manual(values = c("Kontrole" = "#e04","Suberīnskābes" = "#00a"))+
  geom_hline(yintercept = 6.93, color = "black", linetype = "dashed") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  annotate("text", x = 0.5, y = 6.94, 
           label = "Pirms eksperimenta", color = "black", hjust = 0, size = 3)
pH
ggsave("KompostspH.png", plot = pH, width = 4, height = 3.5, units = "in", dpi = 120)

##End mass----
dati$apstrade <- as.factor(dati$apstrade)
endMass <- dati %>% 
  filter(end_mass_percent_corected != 0)


###Normality----

qq_plot <- ggplot(dati, aes(sample = end_mass_percent_corected)) +
  facet_grid(substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(dati, aes(x = substrate, y = end_mass_percent_corected, fill=apstrade)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")

#nenoraidu normalitāti

levene_test(end_mass_percent_corected ~ interaction(substrate, apstrade), data = dati)

#Dispersija homogēna

###comparisons----
maov <- aov(end_mass_percent_corected ~ substrate + apstrade, data = dati)
maov1 <- aov(end_mass_percent_corected ~ substrate * apstrade, data = dati)
AIC(maov,maov1)
summary(maov1)
#BUtiska apstrādes un substrāta ietekme, kā ari abu kombinācija (p<0.001).

TukeyHSD(maov1)

plot(allEffects(maov1))
###Attēls----
ggplot(dati, aes(x = substrate, y = end_mass_percent_corected, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrāts", y = "sadalīšanās pakāpe (% sāk m)", fill = "apstrāde")+
  theme_bw()

endMass <- ggplot(dati, aes(x = substrate, y = end_mass_percent_corected, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrāts", y = "Beigu masa (%)", fill = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        axis.title = element_text(size = 15), 
        legend.background = element_blank()
        )+
  theme(
    legend.position = c(0.8, 0.85))+
  scale_fill_manual(values = c("Kontrole" = "#e04","Suberīnskābes" = "#00a"))
endMass
ggsave("KompostsEndMass.png", plot = endMass, width = 4, height = 3.5, units = "in", dpi = 120)

p = grid.arrange(endMass, pH, ncol = 2)
ggsave("KompostsEndMasspH.png", plot = p, width = 8, height = 3.5, units = "in", dpi = 120)

##C%----
###Normality----
qq_plot <- ggplot(chns, aes(sample = c_percent)) +
  facet_grid(substrate ~ apstrade ~ expstate) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chns, aes(x = substrate, y = c_percent)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(apstrade ~ expstate)

#nenoraidu normalitāti


levene_test(c_percent ~ interaction(apstrade, expstate, substrate), data = chns)

###comparisons----

c <- aov(c_percent ~ substrate + apstrade + expstate + Error(id/expstate), data = chns)

summary(c)
#Nav būtisku atšķirību C saturā augnē pirms un pēc pētījuma (12 nedēļas)
###Attēls----

 chns_summary_interaction <- chns_summary %>%
  mutate(
    expstate = factor(expstate, levels = c("Before experiment", "After experiment"))
     ) %>%
  mutate(
     sub_exp_interaction = interaction(substrate, expstate, sep = ":", drop = TRUE)
     )



C <- ggplot(chns_summary_interaction,
            aes(x = apstrade,
                y = avg_c_percent,           # Map y to average C%
                fill = expstate,             # Fill still based on expstate
                group = sub_exp_interaction)) + # Group by the interaction term for dodging
  
  # Bar layer - matching N plot's dodging width and bar width
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8), # Use same dodge width as N plot
    color = "black",
    width = 0.7                            # Use same bar width as N plot
  ) +
  
  # Error bar layer - using C% standard deviation and matching N plot's style
  geom_errorbar(
    aes(ymin = avg_c_percent - sd_c_percent, # Use C% sd
        ymax = avg_c_percent + sd_c_percent), # Use C% sd
    width = 0.25,                          # Match N plot error bar cap width
    position = position_dodge(width = 0.8)   # Match N plot dodge width
  ) +
  
  # Text layer - adding substrate label, matching N plot's dodging and style
  # **NOTE:** Adjust the 'y' value if needed based on your typical C% values
  # Setting y = -2 assuming C% values are positive and not extremely close to zero.
  geom_text(
    aes(y = -2, label = substrate),        # Adjust y position for C scale
    position = position_dodge(width = 0.8), # Match N plot dodge width
    vjust = 1, size = 3, check_overlap = TRUE # Match N plot text style
  ) +
  
  # Theme - Apply the same theme settings as the N plot for consistency
  theme_bw() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5), # Center x-axis labels
    axis.text.y = element_text(size = 10),
    legend.position = "top",               # Match N plot legend position
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  
  # Labels - Match N plot style, just update the y-axis label
  labs(
    x = "Chitosan apstrade (%)",
    y = "C content (%)",                 # Updated y-axis title
    fill = " "  
  )+ 
  scale_fill_manual(values = c("Before experiment" = "#0099FF",  
                               "After experiment" = "#FF4444"))

# Display the plot
print(C)

# Save the plot - using dimensions similar to N plot for consistency (adjust if needed)
#ggsave("KompostsC.png", plot = C, width = 8, height = 5, units = "in", dpi = 96)

###Diff----
###Normality----
qq_plot <- ggplot(chnspec2, aes(sample = diffC)) +
  facet_grid(substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chnspec2, aes(x = substrate, y = diffC)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(~apstrade)

#nenoraidu normalitāti


levene_test(chnspec2$diffC ~ interaction(chnspec2$apstrade, chnspec2$substrate), data = chnspec2)

###comparisons----

c <- aov(diffC ~ substrate + apstrade, data = chnspec2)

summary(c)
#Nav būtiskua tšķirību starp apstrādēm un substrātiem.
plot(allEffects(c))

CDiff <- ggplot(chnspec2, aes(x = substrate, y = diffC, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrāts", y = "C satura izmaiņa (%)", fill = NULL)+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  theme(
  legend.position = c(0.22, 0.15))+
  scale_fill_manual(values = c("Kontrole" = "#e04","Suberīnskābes" = "#00a"))
CDiff
ggsave("KompostsC.png", plot = CDiff, width = 4, height = 3.5, units = "in", dpi = 120)


##N%----
###Normality----
qq_plot <- ggplot(chns, aes(sample = n_percent)) +
  facet_grid(substrate ~ apstrade ~ expstate) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chns, aes(x = substrate, y = n_percent)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(apstrade ~ expstate)

#nenoraidu normalitāti

levene_test(n_percent ~ interaction(apstrade, expstate, substrate), data = chns)

###comparisons----

n <- aov(n_percent ~ substrate + apstrade + expstate + Error(id/expstate), data = chns)

summary(n)
#Nav būtisku atšķirību pirms un pēc eksperimenta
###Attēls----

chns_summary_interaction <- chns_summary %>%

  mutate(
    expstate = factor(expstate, levels = c("Before experiment", "After experiment"))
  ) %>%
  mutate(
    sub_exp_interaction = interaction(substrate, expstate, sep = ":", drop = TRUE)
  )

N_interaction <- ggplot(chns_summary_interaction,
                        aes(x = apstrade,
                            y = avg_n_percent,
                            fill = expstate, # Color by expstate
                            group = sub_exp_interaction)) + # Group (for dodging) by the interaction
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8), # Adjust width as needed
    color = "black",
    width = 0.7 # Width of individual bars (adjust relative to dodge width)
  ) +
  geom_errorbar(
    aes(ymin = avg_n_percent - sd_n_percent, ymax = avg_n_percent + sd_n_percent),
    width = 0.25, # Adjust width of error bar caps
    position = position_dodge(width = 0.8) # MUST match geom_bar dodge width
  ) +
  geom_text(
    aes(y = -0.1, label = substrate), # Example: Placing substrate label below bar
    position = position_dodge(width = 0.8), # MUST match geom_bar dodge width
    vjust = 1, size = 3, check_overlap = TRUE # Adjust positioning and size
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12),
    axis.text.x = element_text(size = 10, angle = 0, hjust = 0.5),
    axis.text.y = element_text(size = 10),
    legend.position = "top",
    legend.justification = c(0, 1),
    legend.direction = "horizontal",
    strip.background = element_blank(),
    panel.spacing = unit(1, "lines"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  labs(
    x = "Chitosan apstrade (%)",
    y = "N content (%)",
    fill = ""
  ) + 
  scale_fill_manual(values = c("Before experiment" = "#0099FF",  
                               "After experiment" = "#FF4444"))

print(N_interaction)

#ggsave("KompostsN.png", plot = N_interaction, width = 8, height = 5, units = "in", dpi = 96)

###Diff----

###Normality----
qq_plot <- ggplot(chnspec2, aes(sample = diffN)) +
  facet_grid(substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chnspec2, aes(x = substrate, y = diffN)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(~apstrade)

#nenoraidu normalitāti


levene_test(chnspec2$diffN ~ interaction(chnspec2$apstrade, chnspec2$substrate), data = chnspec2)

###comparisons----

N <- aov(diffN ~ substrate + apstrade, data = chnspec2)

summary(N)

#Nav būtisku atšķirību starp substrātiem un apstrādēm

plot(allEffects(N))

NDiff <- ggplot(chnspec2, aes(x = substrate, y = diffN, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrate", y = "Change in N content (%)", fill = NULL)+
  theme_bw()+
  theme(
    legend.position = c(0.22, 0.12))+
  scale_fill_manual(values = c("Control" = "#e04","Suberinic acids" = "#00a"))
NDiff
ggsave("KompostsN.png", plot = NDiff, width = 4, height = 3.5, units = "in", dpi = 120)

##H%----
###Normality----
qq_plot <- ggplot(chns, aes(sample = h_percent)) +
  facet_grid(substrate ~ apstrade ~ expstate) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chns, aes(x = substrate, y = h_percent)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(apstrade ~ expstate)

#nenoraidu normalitāti

levene_test(h_percent ~ interaction(apstrade, expstate, substrate), data = chns)

###comparisons----

h <- aov(h_percent ~ substrate + apstrade + expstate + Error(id/expstate), data = chns)

summary(h)

#H saturs pēc eksperimenta būtiski augstāks (p<0.001).

###Attēls----
H <- ggplot(chns_summary, aes(x = apstrade, y = avg_h_percent, fill = expstate, group = substrate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +  # Thinner bars
  geom_errorbar(aes(ymin = avg_h_percent - sd_h_percent, ymax = avg_h_percent + sd_h_percent), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12), # Adjust facet label size
    axis.text.x = element_text(size = 10, angle = 0, hjust = 1), # Rotate x-axis labels
    axis.text.y = element_text(size = 10), # Adjust y-axis text
    legend.position = "bottom", # Place legend at the bottom
    strip.text.y.left = element_text(angle = 0), # Adjust left facet labels orientation
    strip.background = element_blank(), # Remove background around facet labels
    panel.spacing = unit(1, "lines"), # Increase spacing between facets
    panel.grid.major = element_line(color = "grey90"), # Subtle gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    # Ensure x-axis labels appear under both upper and lower facets
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    axis.text.x.bottom = element_text(margin = margin(t = 10)) # Add space below x-axis labels
  ) +                                  
  theme(legend.position = "top", # Position legend in top-left corner
        legend.justification = c(0, 1),
        legend.direction = "horizontal")  +
  
  # Add substrate labels below bars
  geom_text(aes(y = 1, label = substrate), position = position_dodge(width = 0.7), vjust = 2, size = 4) + 
  labs(x = "Chitosan apstrade (%)", y = "H content (%)", fill = "")

H

#ggsave("KompostsH.png", plot = H, width = 7.5, height = 5, units = "in", dpi = 96)


###Diff----

###Normality----
qq_plot <- ggplot(chnspec2, aes(sample = diffH)) +
  facet_grid(substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chnspec2, aes(x = substrate, y = diffH)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(~apstrade)

#nenoraidu normalitāti


levene_test(chnspec2$diffH ~ interaction(chnspec2$apstrade, chnspec2$substrate), data = chnspec2)

###comparisons----

H <- aov(diffH ~ substrate + apstrade, data = chnspec2)

summary(H)
#Būtiskas H atšķirības ir starp substrātiem (0.010) un apstrādēm (p<0.001).

plot(allEffects(H))
HDiff <- ggplot(chnspec2, aes(x = substrate, y = diffH, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrate", y = "Change in H content (%)", fill = NULL)+
  theme_bw()+
  theme(
    legend.position = c(0.22, 0.8))+
  scale_fill_manual(values = c("Control" = "#e04","Suberinic acids" = "#00a"))
HDiff
ggsave("KompostsH.png", plot = HDiff, width = 4, height = 3.5, units = "in", dpi = 120)




##C/N----
###Normality----
qq_plot <- ggplot(chns, aes(sample = c_n)) +
  facet_grid(substrate ~ apstrade ~ expstate) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chns, aes(x = substrate, y = c_n)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(apstrade ~ expstate)

#nenoraidu normalitāti

levene_test(c_n ~ interaction(apstrade, expstate, substrate), data = chns)

###comparisons----

c_n <- aov(c_n ~ substrate + apstrade + expstate + Error(id/expstate), data = chns)

summary(c_n)
#Nav būtisku C:N atšķirību pirms un pēc eksperimenta
###Attēls----
CN <- ggplot(chns_summary, aes(x = apstrade, y = avg_c_n, fill = expstate, group = substrate)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.7), color = "black", width = 0.7) +  # Thinner bars
  geom_errorbar(aes(ymin = avg_c_n - sd_c_n, ymax = avg_c_n + sd_c_n), 
                width = 0.2, position = position_dodge(0.7)) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12), # Adjust facet label size
    axis.text.x = element_text(size = 10, angle = 0, hjust = 1), # Rotate x-axis labels
    axis.text.y = element_text(size = 10), # Adjust y-axis text
    legend.position = "bottom", # Place legend at the bottom
    strip.text.y.left = element_text(angle = 0), # Adjust left facet labels orientation
    strip.background = element_blank(), # Remove background around facet labels
    panel.spacing = unit(1, "lines"), # Increase spacing between facets
    panel.grid.major = element_line(color = "grey90"), # Subtle gridlines
    panel.grid.minor = element_blank(), # Remove minor gridlines
    # Ensure x-axis labels appear under both upper and lower facets
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    axis.text.x.bottom = element_text(margin = margin(t = 10)) # Add space below x-axis labels
  ) +                                  
  theme(legend.position = "top", # Position legend in top-left corner
        legend.justification = c(0, 1),
        legend.direction = "horizontal")  +
  
  # Add substrate labels below bars
  geom_text(aes(y = 2, label = substrate), position = position_dodge(width = 0.7), vjust = 2, size = 4) + 
  labs(x = "Chitosan apstrade (%)", y = "C:N", fill = "Time")

CN

#ggsave("KompostsCN.png", plot = CN, width = 7.5, height = 5, units = "in", dpi = 96)


###Diff----

###Normality----
qq_plot <- ggplot(chnspec2, aes(sample = diffCN)) +
  facet_grid(substrate ~ apstrade) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot


ggplot(chnspec2, aes(x = substrate, y = diffCN)) + 
  geom_boxplot() +
  labs(x = "x", y = "y", fill = "apstrade")+
  facet_grid(~apstrade)

#nenoraidu normalitāti


levene_test(chnspec2$diffCN ~ interaction(chnspec2$apstrade, chnspec2$substrate), data = chnspec2)

###comparisons----

CN <- aov(diffCN ~ substrate + apstrade, data = chnspec2)

summary(CN)
#Nav būtisku atšķirībustarp substrātiem vai apstrādēm.
plot(allEffects(CN))

CNDiff <- ggplot(chnspec2, aes(x = substrate, y = diffCN, fill = apstrade)) + 
  geom_boxplot() +
  labs(x = "Substrate", y = "Change in C:N ratio", fill = NULL)+
  theme_bw()+
  theme(
    legend.position = c(0.4, 0.07),
    legend.direction = "horizontal")+
  scale_fill_manual(values = c("Control" = "#e04","Suberinic acids" = "#00a"))
CNDiff
ggsave("KompostsCN.png", plot = CNDiff, width = 4, height = 3.5, units = "in", dpi = 120)




#Vizuālais----
##Comparisons----
library(ordinal)

Viz$substrate <- factor(Viz$substrate)
Viz$apstrade  <- factor(Viz$apstrade)
Viz$id        <- factor(Viz$id)
Viz$vertejums <- factor(Viz$vertejums)

m1 <- clmm(
  vertejums ~ days_since_start * substrate * apstrade +
    (1 | id),
  data = Viz,
  link = "logit"
)

library(splines)

m2 <- clmm(
  vertejums ~ ns(days_since_start, df = 3) * substrate * apstrade +
    (1 | id),
  data = Viz
)

m3 <- clmm(
  vertejums ~ days_since_start * substrate + days_since_start * apstrade +
    (1 | id),
  data = Viz,
  link = "logit"
)

m4 <- clmm(
  vertejums ~ ns(days_since_start, df = 3) * substrate + ns(days_since_start, df = 3) * apstrade +
    (1 | id),
  data = Viz
)
AIC(m1,m2,m3,m4)

summary(m4)
#Suberīnskābes apstrāde būtiski palēnina MK noārdīšanās ātrumu (p<0.001), Būtisi atšķirās noārdīšanās ātrums starp substrātiem (p<0.05).
library(emmeans)

emmsubs <- emmeans(
  m4,
  ~substrate,
  mode = "latent"
)
emmsubs
pairs_subs <- pairs(emmsubs, adjust = "tukey")
pairs_df <- as.data.frame(pairs_subs)
sig_pairs <- subset(pairs_df, p.value < 0.05)
sig_pairs

emmApst <- emmeans(
  m4,
  ~apstrade,
  mode = "latent"
)
emmApst
pairs_aps <- pairs(emmApst, adjust = "tukey")
pairs_df <- as.data.frame(pairs_aps)
sig_pairs <- subset(pairs_df, p.value < 0.05)
sig_pairs
#WS1 un WS4 noārdās būtiski ātrāk par BSD3, WS2 un WS3.
##Attēls----
Viz$vertejums <- as.numeric(as.character(Viz$vertejums))
Vert<-ggplot( Viz, aes(
  x = days_since_start, 
  y = vertejums, 
  colour = substrate, 
  shape = apstrade, 
  linetype = apstrade 
  )) +
  geom_smooth(se = FALSE) + 
  scale_linetype_manual(values = c("22", "solid")) + 
  scale_color_manual( values = c(
    "#c00","#fc0","#9b0",
    "#f45","#009","#09f",
    "#b0f","#0f9"
    ))+ 
  labs( 
    x = "Laiks (dienās)", 
    y = "Sadalīšanās pakāpe (balles)", 
    colour = "Substrāts", 
    shape = "Apstrāde", 
    linetype = "Apstrāde" ) + 
  theme_bw()+ 
  theme( 
    axis.title = element_text(size = 15), 
    legend.background = element_blank(), 
    legend.position = c(0.1, 0.39)
    )
Vert
ggsave("KompostsVizuālais.png", plot = Vert, width = 7.5, height = 5, units = "in", dpi = 120)


#Combined----
library(cowplot)
library(stringr)

# Example: wrap both to 2 lines (adjust width as needed)
Vert <- Vert +
  scale_x_continuous(
    labels = function(x) paste0(x, "\n\n\n\n\n.")
  ) +
  theme(axis.text.x = element_text(lineheight = 0.4))
Vert <- Vert +
  annotate("text", x = -Inf, y = Inf, label = "A",
           hjust = -0.2, vjust = 1.5, size = 6)

endMass <- endMass +
  annotate("text", x = -Inf, y = Inf, label = "B",
           hjust = -0.2, vjust = 1.5, size = 6)

p <- plot_grid(
  Vert, endMass,
  ncol = 2,
  rel_widths = c(2, 1),
  align = "v",
  axis = "b"
)

p

ggsave("KompostsVizMass.png", plot = p,
       width = 11, height = 5,
       units = "in", dpi = 120)
