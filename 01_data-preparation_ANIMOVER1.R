# Code for preprocessing ANIMOV1 simulation data.



df_DARs_synth <- read.csv(file=".../2KernelMovement.csv", skip=6)
# First few rows have explanatory text, hence skipped.
df_DARs_synth <- na.omit(df_DARs_synth)

df_DARs_synth <- transform(df_DARs_synth, Delta=Delta+99*(Day-1))
# Time parameter in ANIMOV1 resets at the turn of each day.

colnames(df_DARs_synth)[6] <- "Heading"  # Changing name of column 6 to reflect angle of heading.
df_DARs_synth$Heading <- with(df_DARs_synth, pmin(df_DARs_synth$Heading, 360-df_DARs_synth$Heading))
Turning_angle_deg <- with(df_DARs_synth, diff(df_DARs_synth$Heading))  # Turning angle
Turning_angle_deg <- abs(Turning_angle_deg)  # To ensure turning angle is in [0,180]

df_DARs_synth <- df_DARs_synth[-1, ]  # Time parameters in the ANIMOVER\_1 RAMP to produce $99$ points
# for each of our "nominal days" other than for the first, which has $100$ points.
df_DARs_synth$Heading <- Turning_angle_deg
rm(Turning_angle_deg)
colnames(df_DARs_synth)[6] <- "Turning_angle_deg"

df_DARs_synth <- transform(df_DARs_synth, Turning_angle_deg=Turning_angle_deg*pi/180)
colnames(df_DARs_synth)[6] <- "Turning_angle_rad"


saveRDS(df_DARs_indiv, file=".../merged-DARs_ANIMOV.Rds")


df_DARs_synth <- df_DARs_synth[, c('X', 'Y', 'Delta', 'Distance', 'Turning_angle_rad')]
colnames(df_DARs_synth)[1] <- "x"
colnames(df_DARs_synth)[2] <- "y"
colnames(df_DARs_synth)[3] <- "delta"
colnames(df_DARs_synth)[4] <- "speed"
colnames(df_DARs_synth)[5] <- "turning_angle"


saveRDS(df_DARs_synth, file="merged-DARs_ANIMOV.Rds")