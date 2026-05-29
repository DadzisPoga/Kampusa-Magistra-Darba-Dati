library(readxl)
library(writexl)
library(dplyr)
library(ggplot2)
library(qqplotr)
library(janitor)
library(rstatix)

# Datu importesana ----
#Norāda darba direktoriju (tā, kurā atrodas R fails)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

#Norāda excel faila nosaukumu un lapu kurā atrodas tabula
myc<-read_excel("LactoStaining.xlsx", sheet = 2)
meh<-read_excel("Mehanika2025.xlsx", sheet = 6)
meh2<-read_excel("MehanikaAlg2026.xlsx", sheet = 3)
uden<-read_excel("udens absorbcija paraugs.xlsx", sheet = 3)
higr<-read_excel("MK  higroskopija 2024.xlsx", sheet = 4)
mycfull<-read_excel("LactoStaining.xlsx", sheet = 1)

#Preprocesing----
liece_control <- filter(meh, apstrade == "control")
spiede_control <- filter(meh2, apstrade == "control")
uden <- filter(uden, layers == 0)
uden <- filter(uden, time == 24)
higr <- filter(higr, slanu_skaits == 0)
higr <- filter(higr, gaisa_mitrums == 95)
mycfull <- clean_names(mycfull)

mycfull$substrate <- factor(
  mycfull$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)

myc$substrate <- factor(
  myc$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)

meh$substrate <- factor(
  meh$substrate,
  levels = c("BSD1", "BSD2", "BSD3", "BSD4", "WS1", "WS2", "WS3", "WS4"),  # Desired order
  labels = c("B–Kl", "B–Kl–K", "B–Kl–Z", "B", "S–Kl", "S–Kl–K", "S–Kl–Z", "S")  # Desired names
)
#Myc : Lieces stipr----
##Priekšnosacījumi----
# QQ plot with facets
qq_plot <- ggplot(myc, aes(sample = area_percent)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(myc, aes(x = NULL, y = area_percent))+
  geom_boxplot()
#Normāls
# QQ plot with facets
qq_plot <- ggplot(liece_control, aes(sample = mean_max_stipr_σf_m)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(liece_control, aes(x = NULL, y = mean_max_stipr_σf_m))+
  geom_boxplot()
#Normāls

library(ggrepel)  # optional but recommended

res <- cor.test(myc$area_percent, liece_control$mean_max_stipr_σf_m, method = "pearson")

label_txt <- sprintf("r = %.2f\np = %.3f", res$estimate, res$p.value)

p <- ggplot(myc, aes(x = area_percent, y = liece_control$mean_max_stipr_σf_m)) +
  geom_smooth(method = "lm", colour = "#000") +
  geom_point() +
  geom_text_repel(aes(label = substrate), colour = "#666") +
  annotate("text",
           x = -Inf, y = Inf,
           label = label_txt,
           hjust = -0.1, vjust = 1.2,  # nudges inside the panel
           size = 3.5) +
  theme_bw() +
  labs(x = "Kolonizācijas pakāpe (%)",
       y = expression("Lieces stiprība, " ~ sigma[fM] ~ "(MPa)"))

p

ggsave("MicLiece.png", plot = p, width = 4, height = 3, units = "in", dpi = 180)
#Lineārs

##Korelācija----
cor.test(myc$area_percent,liece_control$mean_max_stipr_σf_m,method = "pearson")
#pastāv statistiski būtiska pozitīva korelācija starp micēlija blīvumu un Lieces stiprību (R=0.8788756, p=0.004049)

#Myc : Ef_liece----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(liece_control, aes(sample = mean_ef)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(liece_control, aes(x = NULL, y = mean_ef))+
  geom_boxplot()
#Normāls

ggplot(myc, aes(x = area_percent, y = liece_control$mean_ef))+
  geom_point()+
  geom_smooth(method = "lm")
#apmēram Lineārs

##Korelācija----
cor.test(myc$area_percent,liece_control$mean_ef,method = "pearson")
#nepastāv statistiski būtiska korelācija starp micēlija blīvumu un elastības moduli (R=0.5124383, p=0.1941)

#Myc : Spiedes stipr----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(spiede_control, aes(sample = mean_stipriba_σ10)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(spiede_control, aes(x = NULL, y = mean_stipriba_σ10))+
  geom_boxplot()
#Normāls


res <- cor.test(myc$area_percent, spiede_control$mean_stipriba_σ10, method = "pearson")

label_txt <- sprintf("r = %.2f\np = %.3f", res$estimate, res$p.value)

p <- ggplot(myc, aes(x = area_percent, y = spiede_control$mean_stipriba_σ10)) +
  geom_smooth(method = "lm", colour = "#000") +
  geom_point() +
  geom_text_repel(aes(label = substrate), colour = "#666") +
  annotate("text",
           x = -Inf, y = Inf,
           label = label_txt,
           hjust = -0.1, vjust = 1.2,  # nudges inside the panel
           size = 3.5) +
  theme_bw() +
  labs(x = "Kolonizācijas pakāpe (%)",
       y = expression("Spiedes stiprība, " ~ sigma[10] ~ "(MPa)"))

p

ggsave("MicSpiede.png", plot = p, width = 4, height = 3, units = "in", dpi = 180)
#Lineārs

#Lineārs

##Korelācija----
cor.test(myc$area_percent,spiede_control$mean_stipriba_σ10,method = "pearson")
#nepastāv statistiski būtiska korelācija starp micēlija blīvumu un spiedes stiprību (R=0.4919545, p=0.2156)

#Myc : Ef_spiede----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(spiede_control, aes(sample = mean_ef)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(spiede_control, aes(x = NULL, y = mean_ef))+
  geom_boxplot()
#Normāls

ggplot(myc, aes(x = area_percent, y = spiede_control$mean_ef))+
  geom_point()+
  geom_smooth(method = "lm")
#apmēram Lineārs

##Korelācija----
cor.test(myc$area_percent,spiede_control$mean_ef,method = "pearson")
#nepastāv statistiski būtiska korelācija starp micēlija blīvumu un elastības moduli (R=0.3718785, p=0.3643)

#Myc : udens----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(uden, aes(sample = mean_delta_m)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(uden, aes(x = NULL, y = mean_delta_m))+
  geom_boxplot()
#Normāls

ggplot(myc, aes(x = area_percent, y = uden$mean_delta_m))+
  geom_point()+
  geom_smooth(method = "lm")
#apmēram Lineārs

##Korelācija----
cor.test(myc$area_percent,uden$mean_delta_m,method = "pearson")
#nepastāv statistiski būtiska korelācija starp micēlija blīvumu un masas izmaiņu (R=0.1708658 , p=0.6858)

#Myc : swelling----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(uden, aes(sample = mean_delta_v)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(uden, aes(x = NULL, y = mean_delta_v))+
  geom_boxplot()
#Normāls

ggplot(myc, aes(x = area_percent, y = uden$mean_delta_v))+
  geom_point()+
  geom_smooth(method = "lm")
#apmēram Lineārs

##Korelācija----
cor.test(myc$area_percent,uden$mean_delta_v,method = "pearson")
#nepastāv statistiski būtiska korelācija starp micēlija blīvumu un tilpuma izmaiņu (R=0.5225283  , p=0.184)

#Myc : higroscopy----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(higr, aes(sample = mean_udens_sat)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(higr, aes(x = NULL, y = mean_udens_sat))+
  geom_boxplot()
#NeNormāls

ggplot(myc, aes(x = area_percent, y = higr$mean_udens_sat))+
  geom_point()+
  geom_smooth(method = "lm")
#NeLineārs

##Korelācija----
cor.test(myc$area_percent,higr$mean_udens_sat,method = "spearman")
#nepastāv statistiski būtiska korelācija starp micēlija blīvumu un Ūdens saturu (R=0.04761905, p=0.9349)

#Myc : blīvums ----
##Priekšnosacījumi----

# QQ plot with facets
qq_plot <- ggplot(spiede_control, aes(sample = mean_density)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")

qq_plot

ggplot(spiede_control, aes(x = NULL, y = mean_density))+
  geom_boxplot()
#Normāls

ggplot(myc, aes(x = area_percent, y = spiede_control$mean_density))+
  geom_smooth(method = "lm",colour="#000")+
  theme_bw()+
  geom_point()+
  labs(x = "Mycelial area (%)", y = expression(Density ~ (g ~ cm^-3)))
#nelineārs

##Korelācija----
cor.test(myc$area_percent,spiede_control$mean_density,method = "spearman")
#nepastāv statistiski korelācija starp micēlija blīvumu un MK Blīvumu (R=0.2142857, p=0.6191)

#Myc : Substrats----

# QQ plot with facets
qq_plot <- ggplot(mycfull, aes(sample = area_percent)) +
  geom_qq_band() + 
  stat_qq_line() + 
  stat_qq_point() +
  labs(x = "Teorētiskās kvantiles", y = "Paraugkopas kvantiles")
qq_plot

ggplot(mycfull, aes(x = substrate, y = area_percent))+
  geom_boxplot()

#Nenormāls


kruskal.test(area_percent ~ substrate, data = mycfull)
Dunn <- dunn_test(area_percent ~ substrate, data = mycfull, p.adjust.method = "holm")

library(multcompView)

# create named vector of adjusted p-values
pvals <- Dunn$p.adj
names(pvals) <- paste(Dunn$group1, Dunn$group2, sep = "-")

letters <- multcompLetters(pvals)$Letters
letters

letters_df <- data.frame(
  substrate = names(letters),
  letter = letters
)
letters_df <- letters_df %>%
  left_join(
    mycfull %>%
      group_by(substrate) %>%
      summarise(y = max(area_percent)),
    by = "substrate"
  )

p <- ggplot(mycfull, aes(x = substrate, y = area_percent)) +
  geom_boxplot(fill = "#0dd", outlier.shape = NA) +
  geom_text(data = letters_df,
            aes(x = substrate, y = y + 4, label = letter),
            size = 5) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = "Substrāts", y = "Kolonizācijas pakāpe (%)")+
  #geom_point(size = 1.4)+
  geom_jitter(width = 0.2, colour = "#888", size = 0.8)
p
ggsave("MicSubs.png", plot = p, width = 6, height = 3, units = "in", dpi = 180)

write_xlsx(myc, "miceijs.xlsx")
