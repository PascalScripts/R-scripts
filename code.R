rm(list = ls())

packages <- c("vars","tidyr","dplyr","ggplot2","mFilter","zoo","seasonal","readxl")

for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}

setwd("C:\\Users\\mario\\Desktop\\SVAR_with_R\\")

# Real GDP ----------------------------------------------------------------
rgdp <- read_excel("data_FP_Italy//real_gdp_sa.xlsx","Sheet 1")
rgdp <- rgdp[11:211,1:2]
names(rgdp)<-c("time","rgdp")
rgdp$time <- as.yearqtr(rgdp$time, format = "%Y-Q%q")
rgdp$rgdp <- as.numeric(rgdp$rgdp)
rgdp <- na.omit(rgdp)

ggplot(rgdp, aes(x = time, y = rgdp)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") + 
  labs(title = "PIL",
       x = "Anno", y = "Milioni") +
  theme_minimal() +
  theme(legend.position = "bottom")

#deriva pil potenziale tramite filtro HP
rgdp$log_rgdp <- log(rgdp$rgdp)
hp_result <- hpfilter(rgdp$log_rgdp, freq = 1600, type = "lambda")
rgdp$log_trend <- hp_result$trend
rgdp$output_gap <- (rgdp$log_rgdp - rgdp$log_trend)*100

plot_data <- rgdp[, c("time", "log_rgdp", "log_trend")]
plot_data <- tidyr::pivot_longer(plot_data, cols = c("log_rgdp", "log_trend"),
                                 names_to = "Serie", values_to = "Valore")

ggplot(plot_data, aes(x = time, y = Valore, color = Serie)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") +
  labs(title = "PIL osservato e potenziale (log)",
       x = "Anno", y = "log(PIL)") +
  theme_minimal() +
  theme(legend.position = "bottom")

ggplot(rgdp, aes(x = time, y = output_gap)) +
  geom_line(size = 1, color = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_x_yearqtr(n = 15, format = "%Y") +
  labs(title = "Output Gap (log)",
       x = "Anno", y = "Deviazione dal PIL potenziale") +
  theme_minimal()


# Nominal GDP -------------------------------------------------------------
nom_gdp <- read_excel("data_FP_Italy//nominal_gdp_sa.xlsx","Sheet 1")
nom_gdp <- nom_gdp[11:211,1:2]
names(nom_gdp)<-c("time","nom_gdp")
nom_gdp$time <- as.yearqtr(nom_gdp$time, format = "%Y-Q%q")
nom_gdp$nom_gdp <- as.numeric(nom_gdp$nom_gdp)
nom_gdp <- na.omit(nom_gdp)

# Gov.Cons -------------------------------------------------------------
govcons <- read_excel("data_FP_Italy//Italy_govcons_sa.xlsx","DATA(GFS)")
govcons <- govcons %>% select("TIME PERIOD", "OBS.VALUE")
names(govcons)<-c("time","govcons")
govcons$time <- as.yearqtr(govcons$time, format = "Q%q %Y")
govcons$govcons <- as.numeric(govcons$govcons)
govcons <- na.omit(govcons)

# Gov.Inv -------------------------------------------------------------
govinv <- read_excel("data_FP_Italy//Italy_govinv_notsa.xlsx","DATA(GFS)")
govinv <- govinv %>% select("TIME PERIOD", "OBS.VALUE")
names(govinv)<-c("time","govinv_notsa")
govinv$time <- as.yearqtr(govinv$time, format = "Q%q %Y")
govinv$govinv_notsa <- as.numeric(govinv$govinv_notsa)
govinv <- na.omit(govinv)

#grafico
ggplot(govinv, aes(x = time, y = govinv_notsa)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") + 
  labs(title = "Gov.Inv.",
       x = "Anno", y = "Milioni") +
  theme_minimal() +
  theme(legend.position = "bottom")

#converti in formato ts
start_year <- as.numeric(format(min(govinv$time), "%Y"))
start_quarter <- as.numeric(cycle(govinv$time)[1])
govinv_ts <- ts(govinv$govinv_notsa, start = c(start_year, start_quarter), frequency = 4)
#destagionalizza
govinv_sa <- seas(govinv_ts)
govinv_sa <- final(govinv_sa)
govinv$govinv_sa <- as.numeric(govinv_sa)

govinv_long <- govinv %>%
  pivot_longer(cols = c("govinv_notsa", "govinv_sa"),
               names_to = "Serie", values_to = "Valore") %>%
  mutate(Serie = recode(Serie,
                        "govinv_notsa" = "Non destagionalizzata",
                        "govinv_sa" = "Destagionalizzata"))

# Grafico
ggplot(govinv_long, aes(x = time, y = Valore, color = Serie)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") +
  labs(title = "Spesa pubblica in investimenti",
       x = "Anno", y = "Milioni di euro") +
  theme_minimal() +
  theme(legend.position = "bottom")


# TASSE INDIRETTE ---------------------------------------------------------
indtax <- read_excel("data_FP_Italy//Italy_indirect_taxes_notsa.xlsx","DATA(GFS)")
indtax <- indtax %>% select("TIME PERIOD", "OBS.VALUE")
names(indtax)<-c("time","indtax_notsa")
indtax$time <- as.yearqtr(indtax$time, format = "Q%q %Y")
indtax$indtax_notsa <- as.numeric(indtax$indtax_notsa)
indtax <- na.omit(indtax)

#grafico
ggplot(indtax, aes(x = time, y = indtax_notsa)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") + 
  labs(title = "Tasse Indirette",
       x = "Anno", y = "Milioni") +
  theme_minimal() +
  theme(legend.position = "bottom")

#converti in formato ts
start_year <- as.numeric(format(min(indtax$time), "%Y"))
start_quarter <- as.numeric(cycle(indtax$time)[1])
indtax_ts <- ts(indtax$indtax_notsa, start = c(start_year, start_quarter), frequency = 4)
#destagionalizza
indtax_sa <- seas(indtax_ts)
indtax_sa <- final(indtax_sa)
indtax$indtax_sa <- as.numeric(indtax_sa)

indtax_long <- indtax %>%
  pivot_longer(cols = c("indtax_notsa", "indtax_sa"),
               names_to = "Serie", values_to = "Valore") %>%
  mutate(Serie = recode(Serie,
                        "indtax_notsa" = "Non destagionalizzata",
                        "indtax_sa" = "Destagionalizzata"))

# Grafico
ggplot(indtax_long, aes(x = time, y = Valore, color = Serie)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") +
  labs(title = "Tasse Indirette",
       x = "Anno", y = "Milioni di euro") +
  theme_minimal() +
  theme(legend.position = "bottom")

# TASSE DIRETTE -------------------------------------------------------------
dirtax <- read_excel("data_FP_Italy//Italy_direct_taxes_notsa.xlsx","DATA(GFS)")
dirtax <- dirtax %>% select("TIME PERIOD", "OBS.VALUE")
names(dirtax)<-c("time","dirtax_notsa")
dirtax$time <- as.yearqtr(dirtax$time, format = "Q%q %Y")
dirtax$dirtax_notsa <- as.numeric(dirtax$dirtax_notsa)
dirtax <- na.omit(dirtax)

#grafico
ggplot(dirtax, aes(x = time, y = dirtax_notsa)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") + 
  labs(title = "Tasse dirette",
       x = "Anno", y = "Milioni") +
  theme_minimal() +
  theme(legend.position = "bottom")

#converti in formato ts
start_year <- as.numeric(format(min(dirtax$time), "%Y"))
start_quarter <- as.numeric(cycle(dirtax$time)[1])
dirtax_ts <- ts(dirtax$dirtax_notsa, start = c(start_year, start_quarter), frequency = 4)
#destagionalizza
dirtax_sa <- seas(dirtax_ts)
dirtax_sa <- final(dirtax_sa)
dirtax$dirtax_sa <- as.numeric(dirtax_sa)

dirtax_long <- dirtax %>%
  pivot_longer(cols = c("dirtax_notsa", "dirtax_sa"),
               names_to = "Serie", values_to = "Valore") %>%
  mutate(Serie = recode(Serie,
                        "dirtax_notsa" = "Non destagionalizzata",
                        "dirtax_sa" = "Destagionalizzata"))

# Grafico
ggplot(dirtax_long, aes(x = time, y = Valore, color = Serie)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") +
  labs(title = "Tasse dirette",
       x = "Anno", y = "Milioni di euro") +
  theme_minimal() +
  theme(legend.position = "bottom")


# CAPITAL TAXES -----------------------------------------------------------

captax <- read_excel("data_FP_Italy//Italy_capital_taxes_notsa.xlsx","DATA(GFS)")
captax <- captax %>% select("TIME PERIOD", "OBS.VALUE")
names(captax)<-c("time","captax_notsa")
captax$time <- as.yearqtr(captax$time, format = "Q%q %Y")
captax$captax_notsa <- as.numeric(captax$captax_notsa)
captax <- na.omit(captax)

#grafico
ggplot(captax, aes(x = time, y = captax_notsa)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") + 
  labs(title = "Imposte in conto capitale",
       x = "Anno", y = "Milioni") +
  theme_minimal() +
  theme(legend.position = "bottom")

#converti in formato ts
start_year <- as.numeric(format(min(captax$time), "%Y"))
start_quarter <- as.numeric(cycle(captax$time)[1])
captax_ts <- ts(captax$captax_notsa, start = c(start_year, start_quarter), frequency = 4)
#verifica se c'è stagionalità
captax_sa <- seas(captax_ts)
captax_sa <- final(captax_sa)
captax$captax_sa <- as.numeric(captax_sa)

captax_long <- captax %>%
  pivot_longer(cols = c("captax_notsa", "captax_sa"),
               names_to = "Serie", values_to = "Valore") %>%
  mutate(Serie = recode(Serie,
                        "captax_notsa" = "Non destagionalizzata",
                        "captax_sa" = "Destagionalizzata"))

# Grafico
ggplot(captax_long, aes(x = time, y = Valore, color = Serie)) +
  geom_line(size = 1) +
  scale_x_yearqtr(n = 15, format = "%Y") +
  labs(title = "Imposte in conto capitale",
       x = "Anno", y = "Milioni di euro") +
  theme_minimal() +
  theme(legend.position = "bottom")
#NON C'E' STAGIONALITA'

# COSTRUZIONE DATASET -----------------------------------------------------
df_list <- list(rgdp, nom_gdp, govcons, govinv, captax, dirtax, indtax)

data <- Reduce(function(x, y) merge(x, y, by= "time", all=TRUE), df_list)

data <- data %>% mutate(defl = (nom_gdp/rgdp)*100,
                        rgovcons = (govcons/defl)*100,
                        rgovinv = (govinv_sa/defl)*100,
                        rtaxes = ((dirtax_sa + indtax_sa + captax_notsa)/defl)*100,
                        rgov = rgovcons + rgovinv
                        ) %>%
                select(time,rgdp,rgov,rtaxes) %>%
                filter(time >= "1999 Q1" & time <= "2019 Q4")

data <- data %>%
  mutate(across(.cols = -time, .fns = ~ log(.) * 100))


# SELEZIONA LAG -----------------------------------------------------------
var_data <- data %>% select(rgov,rtaxes,rgdp)

lag_selection <- VARselect(var_data, lag.max = 8, type = "const")
lag_selection

var_model <- VAR(var_data, p = 3, type = "const") 

a.mat <- diag(3)
diag(a.mat) <- NA
a.mat[2, 1] <- NA
a.mat[3, 1] <- NA
a.mat[3, 2] <- NA
print(a.mat)


svar_model  <- SVAR(var_model, Amat = a.mat)

govresp <- irf(svar_model, response = "rgov", impulse = "rgov", 
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.90, runs = 1000)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(govresp)

taxresp <- irf(svar_model, response = "rtaxes", impulse = "rgov", 
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.90, runs = 1000)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(taxresp)

gdpresp <- irf(svar_model, response = "rgdp", impulse = "rgov", 
               n.ahead = 20, ortho = TRUE, boot = TRUE, ci = 0.90, runs = 1000)
par(mfrow = c(1, 1), mar = c(2.2, 2.2, 1, 1), cex = 0.6)
plot(gdpresp)
