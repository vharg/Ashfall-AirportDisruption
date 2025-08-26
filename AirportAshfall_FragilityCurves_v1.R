## This script generates fragility curves from an airport closure and ashfall dataset 
# Created 1 August 2025

# Load packages
library(readxl) 
library(tidyverse)
library(patchwork)
library(ordinal) # needed for clm() 
library(reshape) # needed for the melt() 
library(cowplot) # needed for plotgrid()
library(patchwork) # needed for arranging plots  
library(dplyr) 

# Load airport closure and ashfall dataset
raw_data <- read.csv("DataFolder/AirportDataNew1.csv")

### EDIT / CALCULATE INFORMATION REQUIRED FOR PLOTS ####

# Rename ashfall thickness to 'him' (hazard intensity metric)
data <- raw_data %>%
  dplyr::rename(him=Ash_thickness)

# Create count of each ash thickness to plot as bars
him_count <- data %>%
  group_by(him) %>%
  summarise(count=n())

# Calculate frequency of each point
data_freq_df <- data %>%
  group_by(him, ds) %>%
  mutate(count=n()) %>%
  ungroup()

# Transform damage states to categories for plotting (instead of continuous values)
data_freq_df$ds <- as.factor(data_freq_df$ds) 

#### FITTING DATA WITH CLM ####

# Turn ds variable into an ordered factor data type for CLM
# ds 0<1<2<3<4<5
data$ds <- as.ordered(data$ds)

# Fitting fragility curves with CLM - IS 0-5 and log(ashfall)
clmmod_log_IS5 <- clm(ds ~ log(him), data=data, link="probit")

#### PREDICTING Y-VALUE TO PLOT FRAGILITY CURVES ####

# As plot uses log scale, points cannot be at equal intervals
# Otherwise there are too few points at small values e.g. 0.01, 0.1
# Create points between each break on the log scale, excluding 0
him_plot1 <- seq(0.01, 0.09, length=500) # for 0.01 to 0.1
him_plot2 <- seq(0.1, 0.9, length=500) # for 0.1 to 1
him_plot3 <- seq(1, 9, length = 500) # for 1 to 10 
him_plot4 <- seq(10, max(data$him)*2, length=1500) # for 10 to 1000 
him_plot <- c(him_plot1, him_plot2, him_plot3, him_plot4) # combine all values

# Delete variables to tidy environment, keeping him_plot only
rm(him_plot1, him_plot2, him_plot3, him_plot4)

# Predicted values from CLM model - IS 0-5 and log(ashfall)
clmpred_log_IS5 <- predict(clmmod_log_IS5, newdata=data.frame(him=him_plot), 
                           type = "linear.predictor", se.fit=TRUE)

#### CALCULATE PROBABILITIES ####

# For damage state 1
pred_log_IS5_DS1=data.frame(mean=1-pnorm(clmpred_log_IS5$eta2[,2]),
                            lwr= 1-pnorm(clmpred_log_IS5$eta2[,2] - clmpred_log_IS5$se.eta2[,2]),
                            upr= 1-pnorm(clmpred_log_IS5$eta2[,2] + clmpred_log_IS5$se.eta2[,2]),
                            him=him_plot) %>%
  mutate(DS="1", model="CLM_log_IS5")

# For damage state 2
pred_log_IS5_DS2=data.frame(mean=1-pnorm(clmpred_log_IS5$eta2[,3]),
                            lwr= 1-pnorm(clmpred_log_IS5$eta2[,3] - clmpred_log_IS5$se.eta2[,3]),
                            upr= 1-pnorm(clmpred_log_IS5$eta2[,3] + clmpred_log_IS5$se.eta2[,3]),
                            him=him_plot) %>%
  mutate(DS="2", model="CLM_log_IS5")

# For damage state 3
pred_log_IS5_DS3=data.frame(mean=1-pnorm(clmpred_log_IS5$eta2[,4]),
                            lwr= 1-pnorm(clmpred_log_IS5$eta2[,4] - clmpred_log_IS5$se.eta2[,4]),
                            upr= 1-pnorm(clmpred_log_IS5$eta2[,4] + clmpred_log_IS5$se.eta2[,4]),
                            him=him_plot) %>%
  mutate(DS="3", model="CLM_log_IS5")

# For damage state 4
pred_log_IS5_DS4=data.frame(mean=1-pnorm(clmpred_log_IS5$eta2[,5]),
                            lwr= 1-pnorm(clmpred_log_IS5$eta2[,5] - clmpred_log_IS5$se.eta2[,5]),
                            upr= 1-pnorm(clmpred_log_IS5$eta2[,5] + clmpred_log_IS5$se.eta2[,5]),
                            him=him_plot) %>%
  mutate(DS="4", model="CLM_log_IS5")

# For damage state 5
pred_log_IS5_DS5=data.frame(mean=1-pnorm(clmpred_log_IS5$eta2[,6]),
                            lwr= 1-pnorm(clmpred_log_IS5$eta2[,6] - clmpred_log_IS5$se.eta2[,6]),
                            upr= 1-pnorm(clmpred_log_IS5$eta2[,6] + clmpred_log_IS5$se.eta2[,6]),
                            him=him_plot) %>%
  mutate(DS="5", model="CLM_log_IS5")

# Combine probabilities for all impact states in single dataframe
pred_log_IS5_all <- rbind(pred_log_IS5_DS1, 
                          pred_log_IS5_DS2, 
                          pred_log_IS5_DS3, 
                          pred_log_IS5_DS4,
                          pred_log_IS5_DS5)

# Delete individual DS probability variables to tidy environment
rm(pred_log_IS5_DS1, pred_log_IS5_DS2, pred_log_IS5_DS3, pred_log_IS5_DS4, pred_log_IS5_DS5)

#### PLOT FRAGILITY CURVES ####

# Custom labeling function to handle decimals based on value of number
custom_label <- function(x) {
  ifelse(x >= 1, scales::number(x, accuracy = 1), scales::number(x, accuracy = 0.01))
}

# From viridis inferno palette
color_palette <- c(
  "0" = "#f1f179",
  "1" = "#fbb61a", 
  "2" = "#ed6925", 
  "3" = "#bc3754", 
  "4" = "#781c6d",
  "5" = "#320a5e")

# Plot of fragility curves
FC_log_IS5_plot <- ggplot(data=pred_log_IS5_all) +
  # Bar plot of data points (divide by 32 to scale secondary axis)
  geom_col(data=him_count, aes(x=him, y=(count/32)), fill="#D3D3D3") + 
  # Secondary axis for bar plot
  scale_y_continuous(sec.axis = sec_axis(name="No. of Points", transform = ~.*32,
                                         breaks=seq(0,32,8))) +
  # Uncertainty range
  geom_ribbon(aes(x=him, ymax=lwr, ymin=upr, fill=DS), alpha=0.3) +
  # Curve for each damage state
  geom_line(aes(x=him, y=mean, colour=DS), linewidth=1) +
  # Ensure limits match the raw data plot
  scale_x_log10(labels = custom_label, limits = c(0.01, 1000)) +  
  xlab("Ashfall Thickness (mm)") +
  ylab(expression(P(Impact >= IS))) +
  scale_colour_manual(name = "Impact State (Closure length)", 
                      labels = c("IS1 (<1 day)", "IS2 (1-2 days)", 
                                 "IS3 (>2-7 days)", "IS4 (>1 week [Finite])", "IS5 (Permanent)"),
                      values = color_palette[2:6], aesthetics = c("colour")) + 
  scale_fill_manual(name = "Impact State (Closure length)", 
                    labels = c("IS1 (<1 day)", "IS2 (1-2 days)", 
                               "IS3 (>2-7 days)", "IS4 (>1 week [Finite])", "IS5 (Permanent)"),
                    values = color_palette[2:6], aesthetics = c("fill")) +
  theme_bw() +
  theme(
    legend.position = "right",  # Position the legend on the right
    legend.title = element_text(size = 12),  # Customize legend title size
    legend.text = element_text(size = 10),  # Customize legend text size
    axis.text = element_text(size=10),  # Customize axis label size
    axis.title = element_text(size=12)  # Customize axis title size
  )

# Plot of raw data - number of points per ashfall thickness and impact state
# Logarithmic x-axis and size of point = count
raw_data_plot <- ggplot(data_freq_df) +
  geom_point(aes(x = him, y = ds, color = ds, size = count)) +
  # Adjust point size range
  scale_size_continuous(range = c(1.5, 5)) +  
  # Set limits from 0.1 to 1000 (check that it matches fragility curve plot limits)
  scale_x_log10(labels = custom_label, limits = c(0.01, 1000)) + 
  # Select colours for IS0 to IS5 from palette (all 6 colours in palette)
  scale_color_manual(values = color_palette, 
                     breaks = c("0", "1", "2", "3", "4", "5")) +
  labs(
    y = "Impact State"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_blank(), # Removed x-axis title
    axis.text.x = element_blank(),  # Removed x-axis labels
    axis.ticks.x = element_blank(), # Removed x-axis ticks
    legend.position = "none"        # Removed legend
  )

# Combining fragility curve and data frequency plot
combplot_log_IS5 <- (raw_data_plot / FC_log_IS5_plot) + 
  plot_layout(heights = c(1, 6))

# Display plot
combplot_log_IS5
