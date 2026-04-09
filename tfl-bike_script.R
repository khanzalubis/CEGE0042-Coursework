# ---------------------------------------------------
# CEGE0042 Coursework
# TfL Santander Cycles (London Bike Sharing) 2025
# ---------------------------------------------------

# --- 0. install/load packages ---
packages <- c("tidyverse", "lubridate", "forecast", "spdep", 
              "ggplot2", "sf", "sp", "reshape2", "httr",
              "starma", "viridis", "tseries", "gstat")

for (pkg in packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# --- 1. DATA LOADING & PREPROCESSING ---

# --- 1.1 data folder path ---
data_folder <- "C:/Users/Khanza/Downloads/STDM/"

# --- 1.2 load csv files ---
csv_files <- list.files(data_folder, pattern = "\\.csv$", full.names = TRUE)
cat("Found", length(csv_files), "CSV files:\n")
print(basename(csv_files))

raw_data <- bind_rows(lapply(csv_files, function(f) {
  tryCatch({
    df <- read_csv(f, show_col_types = FALSE)
    names(df) <- tolower(trimws(gsub("[^a-zA-Z0-9]", "_", names(df))))
    df <- mutate(df, across(everything(), as.character))
    df
  }, error = function(e) { cat("Error reading:", basename(f), "\n"); NULL })
}))

cat("Total rows loaded:", nrow(raw_data), "\n")

# --- 1.3 rename and parse ---
raw_data <- raw_data %>%
  rename(
    start_time     = start_date,
    start_stn_id   = start_station_number,
    start_stn_name = start_station,
    end_time       = end_date
  ) %>%
  mutate(
    start_stn_id = as.integer(start_stn_id),
    start_time   = parse_date_time(start_time,
                                   orders = c("dmy HM", "dmy HMS",
                                              "ymd HM", "ymd HMS")),
    hour    = hour(start_time),
    date    = as_date(start_time),
    weekday = wday(start_time, label = TRUE)
  ) %>%
  filter(!is.na(start_time), !is.na(start_stn_id))

cat("Rows after parsing:", nrow(raw_data), "\n")
cat("Date range:", format(min(raw_data$date)),
    "to", format(max(raw_data$date)), "\n")

# --- 1.4 get station locations ---
cat("Fetching station locations from TfL API...\n")
tryCatch({
  response <- GET("https://api.tfl.gov.uk/BikePoint/")
  stn_raw  <- content(response, as = "parsed")
  stations <- tibble(
    station_id_num = as.integer(gsub("BikePoints_", "",
                                     sapply(stn_raw, `[[`, "id"))),
    station_name   = sapply(stn_raw, `[[`, "commonName"),
    lat = as.numeric(sapply(stn_raw, `[[`, "lat")),
    lon = as.numeric(sapply(stn_raw, `[[`, "lon"))
  )
  cat("Loaded", nrow(stations), "stations from TfL API\n")
}, error = function(e) {
  cat("API unavailable - using fallback stations\n")
  stations <<- tibble(
    station_id_num = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
                       16,17,18,19,20,21,22,23,24,25),
    station_name = c("River Street, Clerkenwell",
                     "Phillimore Gardens, Kensington",
                     "Christopher Street, Liverpool Street",
                     "St. Chad's Street, King's Cross",
                     "Sedding Street, Sloane Square",
                     "Broadcasting House, Marylebone",
                     "Charlbert Street, St. John's Wood",
                     "Gower Street, Bloomsbury",
                     "New Inn Yard, Shoreditch",
                     "Park Street, Bankside",
                     "Brunswick Square, Bloomsbury",
                     "Malet Street, Bloomsbury",
                     "Scala Street, Fitzrovia",
                     "Belgrove Street, King's Cross",
                     "Drury Lane, Covent Garden",
                     "Hatton Wall, Holborn",
                     "Tavistock Place, Bloomsbury",
                     "Bayley Street, Bloomsbury",
                     "Flower & Dean Walk, Stepney",
                     "Greenland Road, Camden Town",
                     "Hampstead Road, Euston",
                     "Eversholt Street, Euston",
                     "Pentonville Road, Angel",
                     "Gray's Inn Road, Holborn",
                     "Moorfields, Moorgate"),
    lat = c(51.529163,51.499606,51.521284,51.530317,51.493130,
            51.520331,51.534616,51.522341,51.524826,51.505974,
            51.523951,51.521890,51.519509,51.530052,51.513052,
            51.520131,51.525941,51.521086,51.516204,51.539099,
            51.528130,51.527600,51.531240,51.521980,51.518760),
    lon = c(-0.109771,-0.197575,-0.084606,-0.120498,-0.156876,
            -0.144664,-0.168641,-0.134765,-0.077040,-0.093080,
            -0.123208,-0.130504,-0.135208,-0.123478,-0.120298,
            -0.112168,-0.122983,-0.131507,-0.055312,-0.144404,
            -0.136420,-0.134280,-0.113450,-0.115670,-0.088760)
  )
})

# --- 1.5 match stations ---
raw_data <- raw_data %>%
  mutate(name_clean = tolower(trimws(start_stn_name)))

stations <- stations %>%
  mutate(name_clean = tolower(trimws(station_name)))

raw_data <- raw_data %>%
  left_join(stations %>% select(name_clean, lat, lon),
            by = "name_clean") %>%
  filter(!is.na(lat))

cat("Journeys with matched coordinates:", nrow(raw_data), "\n")
cat("Unique stations matched:", n_distinct(raw_data$start_stn_id), "\n")


# --- 2. AGGREGATE TO HOURLY DEPARTURES PER STATION ---

hourly <- raw_data %>%
  mutate(datetime = floor_date(start_time, "hour")) %>%
  group_by(start_stn_id, lat, lon, start_stn_name, datetime) %>%
  summarise(departures = n(), .groups = "drop")

all_stns      <- unique(hourly$start_stn_id)
all_datetimes <- seq(min(hourly$datetime), max(hourly$datetime), by = "hour")

complete_grid <- expand.grid(
  start_stn_id = all_stns,
  datetime     = all_datetimes,
  stringsAsFactors = FALSE
) %>%
  left_join(hourly %>% select(start_stn_id, datetime, departures,
                              lat, lon, start_stn_name),
            by = c("start_stn_id", "datetime")) %>%
  mutate(departures = replace_na(departures, 0),
         date = as_date(datetime),
         hour = hour(datetime)) %>%
  group_by(start_stn_id) %>%
  fill(lat, lon, start_stn_name, .direction = "downup") %>%
  ungroup()

# Top 50 busiest stations
top_stns <- complete_grid %>%
  group_by(start_stn_id) %>%
  summarise(total = sum(departures), .groups = "drop") %>%
  arrange(desc(total)) %>%
  slice(1:50) %>%
  pull(start_stn_id)

hourly_top   <- complete_grid %>% filter(start_stn_id %in% top_stns)
stations_top <- complete_grid %>%
  filter(start_stn_id %in% top_stns) %>%
  select(start_stn_id, start_stn_name, lat, lon) %>%
  distinct()

cat("Top stations:", nrow(stations_top), "\n")
cat("Time points:", length(all_datetimes), "\n")
cat("Grid rows:", nrow(hourly_top), "\n")


# --- 3. EXPLORATORY SPATIO-TEMPORAL DATA ANALYSIS ---

# --- 3.1 mean hourly time series ---
mean_hourly <- complete_grid %>%
  group_by(datetime) %>%
  summarise(mean_dep = mean(departures), .groups = "drop")

p1 <- ggplot(mean_hourly, aes(x = datetime, y = mean_dep)) +
  geom_line(colour = "#2166ac", linewidth = 0.5) +
  labs(title = "Mean Hourly Departures Across All Stations",
       x = "Date", y = "Mean Departures") +
  theme_minimal()
print(p1)
ggsave("fig1_mean_ts.png", p1, width = 10, height = 4, dpi = 150)

# --- 3.2 daily pattern ---
p2 <- ggplot(complete_grid %>%
               group_by(hour) %>%
               summarise(mean_dep = mean(departures), .groups = "drop"),
             aes(x = hour, y = mean_dep)) +
  geom_line(colour = "#d6604d", linewidth = 1) +
  geom_point(colour = "#d6604d", size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(title = "Average Departures by Hour of Day",
       x = "Hour", y = "Mean Departures") +
  theme_minimal()
print(p2)
ggsave("fig2_daily_pattern.png", p2, width = 8, height = 4, dpi = 150)


# --- 3.3 weekday vs weekend ---
p3 <- ggplot(complete_grid %>%
               mutate(day_type = ifelse(wday(date) %in% c(1,7),
                                        "Weekend", "Weekday")) %>%
               group_by(hour, day_type) %>%
               summarise(mean_dep = mean(departures), .groups = "drop"),
             aes(x = hour, y = mean_dep, colour = day_type)) +
  geom_line(linewidth = 1) +
  scale_x_continuous(breaks = seq(0, 23, 2)) +
  labs(title = "Weekday vs Weekend Departure Patterns",
       x = "Hour", y = "Mean Departures", colour = "") +
  theme_minimal()
print(p3)
ggsave("fig3_weekday_weekend.png", p3, width = 8, height = 4, dpi = 150)


# --- 3.4 heatmap ---
p4 <- ggplot(hourly_top %>% filter(date <= min(date) + 13),
             aes(x = datetime, y = factor(start_stn_id), fill = departures)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  labs(title = "Heatmap: Hourly Departures by Station (First 2 Weeks)",
       x = "Date", y = "Station ID", fill = "Departures") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_blank())
print(p4)
ggsave("fig4_heatmap.png", p4, width = 12, height = 6, dpi = 150)


# --- 3.5 station map ---
p5 <- ggplot(stations_top, aes(x = lon, y = lat)) +
  geom_point(colour = "#1a9641", size = 3, alpha = 0.7) +
  labs(title = "Top 50 TfL Bike Stations Used in Analysis",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
print(p5)
ggsave("fig5_map.png", p5, width = 7, height = 6, dpi = 150)


# --- 3.6 ACF / PACF ---
mean_ts <- ts(mean_hourly$mean_dep, frequency = 24)

png("fig6_acf_pacf.png", width = 900, height = 400)
par(mfrow = c(1, 2))
acf(mean_ts,  lag.max = 72, main = "ACF - Mean Hourly Departures")
pacf(mean_ts, lag.max = 72, main = "PACF - Mean Hourly Departures")
dev.off()
par(mfrow = c(1, 2))
acf(mean_ts,  lag.max = 72, main = "ACF - Mean Hourly Departures")
pacf(mean_ts, lag.max = 72, main = "PACF - Mean Hourly Departures")
par(mfrow = c(1, 1))

# --- 3.7 ADF stationarity test ---
cat("\n--- ADF Test ---\n")
print(adf.test(mean_ts, alternative = "stationary"))


# --- 3.8 moran's i ---
stn_avg <- hourly_top %>%
  group_by(start_stn_id) %>%
  summarise(avg_dep = mean(departures), .groups = "drop") %>%
  left_join(stations_top, by = "start_stn_id") %>%
  filter(!is.na(lat), !is.na(lon))

coords <- cbind(stn_avg$lon, stn_avg$lat)
wt4    <- nb2listw(knn2nb(knearneigh(coords, k = 4)), style = "W")

cat("\n--- Moran's I ---\n")
print(moran.test(stn_avg$avg_dep, wt4))

png("fig7_moran.png", width = 600, height = 600)
moran.plot(stn_avg$avg_dep, wt4,
           main = "Moran's I - Average Departures per Station",
           xlab = "Average Departures",
           ylab = "Spatially Lagged Departures")
dev.off()
moran.plot(stn_avg$avg_dep, wt4,
           main = "Moran's I - Average Departures per Station",
           xlab = "Average Departures",
           ylab = "Spatially Lagged Departures")

# --- 3.9 semivariogram ---
library(gstat)

stn_avg <- hourly_top %>%
  group_by(start_stn_id) %>%
  summarise(avg_dep = mean(departures), .groups = "drop") %>%
  left_join(stations_top, by = "start_stn_id") %>%
  filter(!is.na(lat), !is.na(lon))

stn_avg_sp <- as.data.frame(stn_avg)
coordinates(stn_avg_sp) <- ~lon+lat
proj4string(stn_avg_sp) <- CRS("+proj=longlat +datum=WGS84")

vgm_result <- variogram(avg_dep ~ 1, stn_avg_sp)

png("fig_semivariogram.png", width = 700, height = 500)
plot(vgm_result,
     main = "Semivariogram of Average Departures per Station",
     xlab = "Distance (degrees)",
     ylab = "Semivariance",
     pch  = 19, col = "steelblue")
dev.off()
plot(vgm_result,
     main = "Semivariogram of Average Departures per Station",
     xlab = "Distance (degrees)",
     ylab = "Semivariance",
     pch  = 19, col = "steelblue")
cat("Semivariogram saved.\n")



# --- 4. MODEL 1 — SARIMA ---

target_id   <- top_stns[1]
target_name <- stations_top %>%
  filter(start_stn_id == target_id) %>%
  pull(start_stn_name) %>% first()
cat("\nTarget station:", target_name, "(ID:", target_id, ")\n")

target_ts <- ts(
  hourly_top %>%
    filter(start_stn_id == target_id) %>%
    arrange(datetime) %>%
    pull(departures),
  frequency = 24
)

n_total <- length(target_ts)
n_train <- floor(0.75 * n_total)
n_test  <- n_total - n_train
train_ts <- head(target_ts, n_train)
test_ts  <- tail(target_ts, n_test)
cat("Train:", n_train, "hours | Test:", n_test, "hours\n")

cat("\nFitting SARIMA...\n")
sarima_model <- auto.arima(train_ts, seasonal = TRUE,
                           stepwise = TRUE, approximation = TRUE,
                           trace = FALSE)
print(summary(sarima_model))

sarima_fc <- forecast(sarima_model, h = n_test)

png("fig8_sarima_forecast.png", width = 1000, height = 500)
plot(sarima_fc,
     main = paste("SARIMA Forecast -", target_name),
     xlab = "Time (hours)", ylab = "Departures")
lines(ts(c(rep(NA, n_train), as.numeric(test_ts)), frequency = 24),
      col = "red", lwd = 2)
legend("topleft", c("Actual", "Forecast"),
       col = c("red", "blue"), lty = 1, bty = "n")
dev.off()
plot(sarima_fc,
     main = paste("SARIMA Forecast -", target_name),
     xlab = "Time (hours)", ylab = "Departures")
lines(ts(c(rep(NA, n_train), as.numeric(test_ts)), frequency = 24),
      col = "red", lwd = 2)
legend("topleft", c("Actual", "Forecast"),
       col = c("red", "blue"), lty = 1, bty = "n")

png("fig9_sarima_residuals.png", width = 800, height = 500)
checkresiduals(sarima_model)
dev.off()
checkresiduals(sarima_model)

sarima_pred <- as.numeric(sarima_fc$mean)
sarima_rmse <- sqrt(mean((as.numeric(test_ts) - sarima_pred)^2))
sarima_mae  <- mean(abs(as.numeric(test_ts)  - sarima_pred))
cat("\n--- SARIMA Errors ---\n")
cat("RMSE:", round(sarima_rmse, 4), "\n")
cat("MAE: ", round(sarima_mae,  4), "\n")


# --- 5. MODEL 2 — ST-ARIMA ---

# --- 5.1 wide matrix ---
vol_matrix <- hourly_top %>%
  select(start_stn_id, datetime, departures) %>%
  pivot_wider(names_from  = start_stn_id,
              values_from = departures,
              values_fill = 0) %>%
  arrange(datetime) %>%
  select(-datetime) %>%
  as.matrix()

cat("\nVolume matrix:", nrow(vol_matrix), "x", ncol(vol_matrix), "\n")

# --- 5.2 centre and scale ---
vol_scaled <- stcenter(vol_matrix)
                       
# --- 5.3 spatial weight matrix ---
stn_order <- stations_top %>%
  filter(start_stn_id %in% as.integer(colnames(vol_matrix))) %>%
  arrange(match(start_stn_id, as.integer(colnames(vol_matrix))))

coords_st <- cbind(stn_order$lon, stn_order$lat)
nb3       <- knn2nb(knearneigh(coords_st, k = 3))
max_d     <- max(unlist(nbdists(nb3, coords_st)))
nb_d      <- dnearneigh(coords_st, 0, max_d)
wt_mat    <- nb2mat(nb_d, style = "W", zero.policy = TRUE)

# blist: identity matrix (space lag 0) + weight matrix (space lag 1)
n_stn  <- ncol(vol_scaled)
blist  <- list(diag(n_stn), wt_mat)

# Connectivity plot
png("fig10_connectivity.png", width = 600, height = 600)
plot(nb_d, coords_st,
     main = "Spatial Connectivity Graph",
     pch = 19, cex = 0.6)
dev.off()
plot(nb_d, coords_st,
     main = "Spatial Connectivity Graph",
     pch = 19, cex = 0.6)


# --- 5.4 train/test split ---
n_rows  <- nrow(vol_scaled)
n_tr    <- floor(0.75 * n_rows)
train_m <- vol_scaled[1:n_tr, ]
test_m  <- vol_scaled[(n_tr + 1):n_rows, ]

# --- 5.5 ST-ACF / ST-PACF ---
cat("Computing ST-ACF and ST-PACF...\n")
png("fig11_stacf.png",  width = 800, height = 400)
stacf(train_m,  blist, tlag.max = 48)
dev.off()
stacf(train_m,  blist, tlag.max = 48)

png("fig12_stpacf.png", width = 800, height = 400)
stpacf(train_m, blist, tlag.max = 48)
dev.off()
stpacf(train_m, blist, tlag.max = 48)

# --- 5.6 fit starma model ---
ar <- matrix(0, 2, 2)
ar[1, 1] <- 1   # phi10
ar[1, 2] <- 1   # phi11
ar[2, 1] <- 1   # phi20
ma <- matrix(c(0, 1), 1, 2)  # theta11

cat("\nFitting ST-ARIMA (STARMA) model...\n")
fit_star <- starma(train_m, blist, ar = ar, ma = ma)
cat("Model fitted.\n")
summary(fit_star)

# --- 5.7 residual diagnostics ---
png("fig13_starima_res_acf.png", width = 800, height = 400)
stacf(fit_star$residuals, blist, tlag.max = 48)
dev.off()
stacf(fit_star$residuals, blist, tlag.max = 48)

# ljung-box test
lb_test <- stcor.test(fit_star$residuals, blist,
                      fitdf = sum(ar) + sum(ma))
cat("\n--- Ljung-Box Test on Residuals ---\n")
print(lb_test)

# --- 5.8 predict  ---
n_ahead <- n_rows - n_tr

phi   <- fit_star$phi
theta <- fit_star$theta
phi[is.na(phi)]     <- 0
theta[is.na(theta)] <- 0

res <- fit_star$residuals

pre_star <- matrix(NA, nrow = n_ahead, ncol = ncol(vol_scaled))

for (t in 1:n_ahead) {
  t_idx <- n_tr + t
  pred  <- rep(0, ncol(vol_scaled))
  
  # AR part
  for (k in 1:nrow(phi)) {
    for (l in 1:ncol(phi)) {
      if (phi[k, l] != 0 && (t_idx - k) > 0) {
        pred <- pred + phi[k, l] * blist[[l]] %*% vol_scaled[t_idx - k, ]
      }
    }
  }
  
  # MA part
  for (k in 1:nrow(theta)) {
    for (l in 1:ncol(theta)) {
      if (theta[k, l] != 0 && (t_idx - k) > 0 && (t_idx - k) <= n_tr) {
        pred <- pred + theta[k, l] * blist[[l]] %*% res[t_idx - k, ]
      }
    }
  }
  
  pre_star[t, ] <- pred
}

starima_rmse <- sqrt(mean((test_m - pre_star)^2, na.rm = TRUE))
starima_mae  <- mean(abs(test_m  - pre_star),    na.rm = TRUE)
cat("\n--- ST-ARIMA Errors ---\n")
cat("RMSE:", round(starima_rmse, 4), "\n")
cat("MAE: ", round(starima_mae,  4), "\n")

# --- 5.9 forecast plot for target station ---
target_col <- which(colnames(vol_scaled) == as.character(target_id))
if (length(target_col) > 0) {
  png("fig14_starima_forecast.png", width = 1000, height = 500)
  matplot(
    cbind(test_m[, target_col], pre_star[, target_col]),
    type = "l", lty = 1, lwd = 1.5, col = c("black", "red"),
    main = paste("ST-ARIMA Actual vs Predicted -", target_name),
    xlab = "Time (hours)", ylab = "Scaled Departures"
  )
  legend("topright", c("Actual", "Predicted"),
         col = c("black", "red"), lty = 1, bty = "n")
  dev.off()
  matplot(
    cbind(test_m[, target_col], pre_star[, target_col]),
    type = "l", lty = 1, lwd = 1.5, col = c("black", "red"),
    main = paste("ST-ARIMA Actual vs Predicted -", target_name),
    xlab = "Time (hours)", ylab = "Scaled Departures"
  )
  legend("topright", c("Actual", "Predicted"),
         col = c("black", "red"), lty = 1, bty = "n")
}

# --- 6. MODEL COMPARISON ---
results <- data.frame(
  Model = c("SARIMA", "ST-ARIMA"),
  RMSE  = round(c(sarima_rmse,  starima_rmse), 4),
  MAE   = round(c(sarima_mae,   starima_mae),  4)
)

print(results)

write.csv(results, "model_comparison.csv", row.names = FALSE)
cat("\nDone! All figures saved to:", getwd(), "\n")

