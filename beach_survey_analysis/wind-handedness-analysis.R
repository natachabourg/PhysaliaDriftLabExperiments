# 1. Housekeeping ------------------------------------------------------------ #
# Load libraries 
library(tidyverse)     # data manipulation
library(mgcv)          # fitting gams
library(gratia)        # convenience functions for working with {mgcv} objects
library(patchwork)     # pretty multi pnael plots
library(zoo)           # timeseries manipiluations
library(broom)         # tidying up model outputs into tibbles
library(metR)          # labelled contours
library(janitor)
library(statmod)

# Load custom functions
source("R/rqresiduals.R")
source("R/plot_defaults.R")
plot_defaults()

# Load the bluebottle data
data <- read_csv(
  file = "data-processed/bluebottle-observations/beach-survey-samples.csv"
  )

# Load the wind data
wind <- read_csv(
  file = "data-processed/bom-wind-kurnell-begin-2022-09-01-end-2024-02-21.csv"
  )
# ---------------------------------------------------------------------------- #

# 2. Calculate the handedness ratio ------------------------------------------ #
# Remove the NAs for handedness
data <- data %>% filter(!is.na(handedness))

# Do the calculation
data <- data %>%
  group_by(date, time, handedness) %>%
  summarise(n = n()) %>%
  ungroup() %>% 
  mutate(datetime = ymd_hms(paste0(date, " ", time))) %>%
  pivot_wider(names_from = handedness, values_from = n, values_fill = 0) %>%
  clean_names() %>%
  mutate(hand_ratio = left / (left + right),
         n = left + right)
# ---------------------------------------------------------------------------- #

# 3. Format some covariates -------------------------------------------------- #
# Time-related covariates
data <- data %>% mutate(woy = factor(week(date)), month = factor(month(date)))

# for NA add the most recent previous value
wind <- wind %>%
  arrange(datetime) %>%
  mutate(wind_speed_km_h_bom = na.locf(wind_speed_km_h_bom),
         wind_direction_deg_bom = na.locf(wind_direction_deg_bom))

# summarise wind measurements
wind <- wind %>%
  group_by(date) %>%
  mutate(mean_wind_speed_km_h_bom = mean(wind_speed_km_h_bom, na.rm = TRUE),
         mean_wind_direction_deg_bom = mean(wind_direction_deg_bom, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(wind_time = time,
         wind_date = date,
         wind_datetime = datetime) %>%
  select(-time, -date, -datetime)

# join the wind and bb obs together - this returns every possible combination
data <- data %>% cross_join(wind)

# now filter it
data <- data %>% 
  # calculate the difference in time between observations and measurements
  mutate(time_diff = datetime - wind_datetime) %>%
  # only keep those where the wind measurement happened before the bb obs
  filter(time_diff > 0) %>%
  # for each datetime
  group_by(datetime) %>%
  # retain the observation that was the closest before
  filter(time_diff == min(time_diff)) %>%
  ungroup() %>%
  arrange(date)
# ---------------------------------------------------------------------------- #

# 4. Fit the model ----------------------------------------------------------- #
# Fit a simple GAM
m1 <- mgcv::gam(
  formula = hand_ratio ~
    s(wind_direction_deg_bom, bs = "cc"),
  knots = list(wind_direction_deg_bom = c(0.5, 360.5)),
  family = binomial(link = "logit"),
  weights = n / mean(n),
  method = "REML",
  select = TRUE,
  data = data
)
# ---------------------------------------------------------------------------- #

# 5. Assess the model -------------------------------------------------------- #
# Check the model is flexbile enough (i.e., k is high enough)
k.check(m1) # all good

# Check the residuals
appraise(m1, point_alpha = 0.4) # looks ok

# Check autocorrelation
acf(m1$residuals) # some autocorrelation

# Try to address this by adding a month of year random effect
m2 <- m1 %>% update(
  formula. = hand_ratio ~
    s(wind_direction_deg_bom, bs = "cc") + 
    s(month, bs = "re"))

# Check the residuals
appraise(m2, point_alpha = 0.4) # looks worse

# Check autocorrelation
acf(m2$residuals) # looks better

m3 <- m2 %>% update(
  formula. = hand_ratio ~
    s(wind_direction_deg_bom, bs = "cc") + 
    s(woy, bs = "re"))

# Check the residuals
appraise(m3, point_alpha = 0.4) # looks ok

# save the residuals plot
ggsave(filename = "figures/wind-direction-handedness-residuals.jpeg",
       width = 15,
       height = 15,
       units = "cm")

# Check autocorrelation
acf(m3$residuals) # looks best

# Create an ACF plot with {ggplot2}...

# ---------------------------------------------------------------------------- #

# 6. Plot the model estimates ------------------------------------------------ #
# Quickly plot the estimated effects
draw(m3, fun = inv_link(m3))

# Have a look at the model summary
summary(m3)

# Create a tidy version to be saved
table <- m3 %>% tidy()

# How long before the wind observation usually is from (in hours)
median(as.numeric(data$time_diff) / 60 / 60)
IQR(as.numeric(data$time_diff) / 60/ 60)

# get the estimates from the model
effects <- m3 %>% 
  smooth_estimates() %>% 
  add_confint() %>%
  mutate(est = plogis(est),
         lower_ci = plogis(lower_ci),
         upper_ci = plogis(upper_ci))

# list all smooths included in the model
smooths <- effects$smooth %>% unique()

# labels/breaks for the plot
wind_breaks <- seq(0, 360, 45)
wind_labels <- c("N" , "NE", "E", "SE", "S", "SW", "W", "NW", "N")

# plot the effects from this model
effects %>%
  filter(smooth == smooths[1]) %>%
  ggplot(data = .) +
  geom_ribbon(aes(x = wind_direction_deg_bom, 
                  ymin = lower_ci, 
                  ymax = upper_ci), 
              fill = "light grey") +
  geom_line(aes(x = wind_direction_deg_bom, 
                y = est)) +
  geom_point(data = data, 
             aes(x = wind_direction_deg_bom, 
                 y = hand_ratio, 
                 size = n / mean(n)), 
             alpha = 0.25) +
  scale_size(breaks = seq(0, 2.5, 0.5)) +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, labels = wind_labels, name = "Compass wind direction")) +
  labs(y = "Handedness ratio",
       x = "Wind direction (°)",
       size = "Model weight") +
  geom_hline(yintercept = c(0, 0.5, 1), linetype = "dashed") +
  annotate(geom = "text", x = 0, y = 0.5, label = "↑ more left-handed", vjust = -1, hjust = 0) +
  annotate(geom = "text", x = 0, y = 0.49, label = "↓ more right-handed", vjust = 1, hjust = 0) +
  annotate(geom = "text", x = 0, y = 0.98, label = "All left-handed", vjust = -1, hjust = 0) +
  annotate(geom = "text", x = 0, y = -0.01, label = "All right-handed", vjust = 1, hjust = 0) 

# save it
ggsave(filename = "figures/wind-direction-handedness-effects.jpeg",
       height = 12,
       width = 12,
       units = "cm")



# save it
#ggsave(filename = "figures/wind-interaction-handedness-effects.jpeg",
 #      height = 10,
  #     width = 30,
   #    units = "cm")


# make a histogram of the number of bluebottles sampled each day
ggplot(data = data, aes(x = n)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(limits = c(min(data$n), max(data$n) + 1),
                     breaks = seq(0, max(data$n)+1, 5)) +
  labs(x = "Bluebottle colonies sampled",
       y = "Frequency")

ggsave(filename = "figures/bluebottle-colonies-sampled.jpeg",
       height = 12,
       width = 12,
       units = "cm")

# plot of wind speeds
ggplot(data = data, aes(x = wind_speed_km_h_bom)) +
  geom_histogram(binwidth = 2) +
  geom_vline(xintercept = 28.8, linetype = "dashed") +
  labs(x = expression(paste("Wind speed (km h"^-1, ")")),
       y = "Frequency") +
  scale_x_continuous(breaks = seq(0, 80, 4),
                     sec.axis = sec_axis(trans = ~ . / 3.6,
                                         breaks = seq(0, 22, 2),
                                         name = expression(paste("Wind speed (m s"^-1, ")"))))

length(which(data$wind_speed_km_h_bom > 28.8))


ggsave(filename = "figures/observed-wind-speeds.jpeg",
       height = 12,
       width = 12,
       units = "cm")


# plot of wind directions
ggplot(data = data, aes(x = wind_direction_deg_bom)) +
  geom_histogram(binwidth = 2) +
  labs(x = "Wind direction (°)",
       y = "Frequency") +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, labels = wind_labels, name = "Compass wind direction"))

ggsave(filename = "figures/observed-wind-directions.jpeg",
       height = 12,
       width = 12,
       units = "cm")

ggplot(data = data, aes(x = wind_direction_deg_bom, y = wind_speed_km_h_bom)) +
  stat_density_2d(geom = "raster", aes(fill = after_stat(density)), contour = FALSE) +
  scale_fill_viridis_c(breaks = c(0, 0.00008, 0.00016)) +
  #geom_density_2d_filled() +
  geom_point(colour = "white", position = position_jitter()) +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, labels = wind_labels, name = "Compass wind direction")) +
  scale_y_continuous(breaks = seq(0, 80, 4),
                     sec.axis = sec_axis(trans = ~ . / 3.6,
                                         breaks = seq(0, 22, 2),
                                         name = expression(paste("Wind speed (m s"^-1, ")")))) +
  labs(x = "Wind direction (°)",
       y = expression(paste("Wind speed (km h"^-1, ")")),
       fill = "Density") +
  #guides(fill = guide_legend(keywidth = unit(2, "cm"))) +
  theme(legend.key.width = unit(1, "cm"))

ggsave(filename = "figures/observed-wind-speed-and-direction.jpeg",
       height = 12,
       width = 12,
       units = "cm")

ggplot() + 
  geom_point(data = data,
            aes(x = wind_direction_deg_bom,
                y = wind_speed_km_h_bom,
                colour = hand_ratio),
            size = 2) +
  scale_colour_viridis_c(breaks = c(0, 0.5, 1)) + 
  labs(x = "Wind direction (°)",
       y = expression(paste("Wind speed (km h"^-1, ")")),
       colour = "Handedness ratio") +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, 
                                         labels = wind_labels, 
                                         name = "Compass wind direction")) +
  scale_y_continuous(breaks = seq(0, 80, 4),
                     sec.axis = sec_axis(trans = ~ . / 3.6,
                                         breaks = seq(0, 22, 2),
                                         name = expression(paste("Wind speed (m s"^-1, ")"))))





# plot interactive effects
b <- effects %>%
  filter(smooth == smooths[3]) %>%
  mutate(est = inv(est),
         lower_ci = inv(lower_ci),
         upper_ci = inv(upper_ci)) %>%
  ggplot(data = .) +
  geom_tile(aes(x = wind_direction_deg_bom,
                y = wind_speed_km_h_bom,
                colour = est,
                fill = est)) +
  geom_hline(yintercept = 28.8, linetype = "dashed", colour = "white", linewidth = 1) +
  geom_contour(aes(x = wind_direction_deg_bom,
                   y = wind_speed_km_h_bom,
                   z = est),
               colour = "white") +
  geom_text_contour(aes(x = wind_direction_deg_bom,
                        y = wind_speed_km_h_bom,
                        z = est),
                    size = 5,
                    stroke = 0.15) +
  scale_colour_viridis_c(limits = c(0, 1)) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(x = "Wind direction (°)") +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, labels = wind_labels, name = "Compass wind direction")) +
  scale_y_continuous(breaks = seq(0, 80, 4),
                     sec.axis = sec_axis(trans = ~ . / 3.6,
                                         breaks = seq(0, 22, 2))) +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank())

# plot interactive errors
a <- effects %>%
  filter(smooth == smooths[3]) %>%
  mutate(lower_ci = inv(lower_ci)) %>%
  ggplot(data = .) +
  geom_tile(aes(x = wind_direction_deg_bom,
                y = wind_speed_km_h_bom,
                colour = lower_ci,
                fill = lower_ci)) +
  geom_hline(yintercept = 28.8, linetype = "dashed", colour = "white", linewidth = 1) +
  geom_contour(aes(x = wind_direction_deg_bom,
                   y = wind_speed_km_h_bom,
                   z = lower_ci),
               breaks = seq(0, 1, 0.1),
               colour = "white") +
  geom_text_contour(aes(x = wind_direction_deg_bom,
                        y = wind_speed_km_h_bom,
                        z = lower_ci),
                    breaks = seq(0, 1, 0.1),
                    size = 5,
                    stroke = 0.15) +
  scale_colour_viridis_c(limits = c(0, 1)) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(y = expression(paste("Wind speed (km h"^-1, ")")),
       x = "Wind direction (°)",
       fill = "Standard error",
       colour = "Standard error") +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, labels = wind_labels, name = "Compass wind direction")) +
  scale_y_continuous(breaks = seq(0, 80, 4),
                     sec.axis = sec_axis(trans = ~ . / 3.6,
                                         breaks = seq(0, 22, 2))) +
  theme(axis.title.y.right = element_blank(),
        axis.text.y.right = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none") 

c <- effects %>%
  filter(smooth == smooths[3]) %>%
  mutate(upper_ci = inv(upper_ci)) %>%
  ggplot(data = .) +
  geom_tile(aes(x = wind_direction_deg_bom,
                y = wind_speed_km_h_bom,
                colour = upper_ci,
                fill = upper_ci)) +
  geom_hline(yintercept = 28.8, linetype = "dashed", colour = "white", linewidth = 1) +
  geom_contour(aes(x = wind_direction_deg_bom,
                   y = wind_speed_km_h_bom,
                   z = upper_ci),
               breaks = seq(0, 1, 0.1),
               colour = "white") +
  geom_text_contour(aes(x = wind_direction_deg_bom,
                        y = wind_speed_km_h_bom,
                        z = upper_ci),
                    breaks = seq(0, 1, 0.1),
                    size = 5,
                    stroke = 0.15) +
  scale_colour_viridis_c(limits = c(0, 1)) +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(y = expression(paste("Wind speed (km h"^-1, ")")),
       x = "Wind direction (°)",
       fill = "Standard error",
       colour = "Standard error") +
  scale_x_continuous(breaks = wind_breaks,
                     sec.axis = sec_axis(trans = ~ ., breaks = wind_breaks, labels = wind_labels, name = "Compass wind direction")) +
  scale_y_continuous(breaks = seq(0, 80, 4),
                     sec.axis = sec_axis(trans = ~ . / 3.6,
                                         breaks = seq(0, 22, 2),
                                         name = expression(paste("Wind speed (m s"^-1, ")")))) +
  theme(axis.title.y.left = element_blank(),
        axis.text.y.left = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "none")

# combine two plots
(a|b|c) + plot_annotation(tag_levels = "a")
