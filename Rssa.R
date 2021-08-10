#Package for SSA
install.packages("Rssa")

#library(lubricate)
library(Rssa)

#Package for importing excel sheet
install.packages("xlsx")
library("xlsx")

#Package for plotting
install.packages("ggplot")
library(ggplot2)

setwd("C:/Users/Lenovo/Dropbox/My PC (DESKTOP-S5BBDSI)/Desktop/Ayodeji Thesis/new_piezometric/453_18")
data <- read.xlsx("453_18.xlsx",1, header = TRUE)
date <- as.Date(data$Date)
PiezometricSeries <- data$Piezometric.Level
charttitle<- basename(getwd())

# Piezometric deconstrction. The window length was taken as
# 10% of the number of observations. This really did not affect the result
# Other factors are used as default from the RSSA package

# piezo_SSA <- ssa(PiezometricSeries, L = (0.5* (length(PiezometricSeries))), svd.method = "auto")
piezo_SSA <- ssa(PiezometricSeries)
jpeg("eigenval.jpg")
plot(piezo_SSA)
dev.off()

#Reconstructing the series
#The number of components taken as trend really did not affect the trend output
#But others affect the seasonality
r1 = reconstruct(piezo_SSA, groups = list(Trend=c(1:3),  Seasonality=c(3:12)))

#r1 = reconstruct(piezo_SSA, groups = list(Trend=c(1,4), Seasonality=c(2:3,5:6)))

#Plotting with ggplot
PiezometricTrend <- r1$Trend
Seasonality <- r1$Seasonality
Residuals <- attr(r1,"residuals")
df<- data.frame(date, PiezometricSeries, PiezometricTrend, Seasonality, Residuals)
colors <- c("Piezometric Series" = "black", "Piezometric Trend" = "orange", "Seasonality" = "red", "Residuals" = "green")


jpeg("outputplot.jpg",units="cm", width=25, height=15, res=300)
ggplot(df, aes(date)) +                    # basic graphical object
  geom_line(aes(y = PiezometricSeries, color ="Piezometric Series")) +  # series plot
  geom_line(aes(y=PiezometricTrend,color="Piezometric Trend")) + # trend plot
  geom_line(aes(y=Seasonality,color = "Seasonality")) + #seasonality plot
  geom_line(aes(y=Residuals,color = "Residuals")) + #residual plot
  labs(y= "Piezometric Level(m)", x = "Year" ,colour = " ") +
  scale_color_manual(values=colors) +
  theme_update(plot.title = element_text(hjust = 0.5), legend.position = "bottom") + #update legend and other positions
  ggtitle(charttitle) + #title for the plot
  scale_x_date(date_breaks = "1 year", date_labels =  "%Y")  #change the x scale
dev.off()

#saving the outputs
sink("summary.txt")
summary(piezo_SSA)
sink()

write.xlsx(PiezometricTrend, file = "RSSAoutput.xlsx", sheetName = "Trend", append = FALSE)
write.xlsx(Residuals, file = "RSSAoutput.xlsx", sheetName = "Residual", append = TRUE)
write.xlsx(PiezometricSeries, file = "RSSAoutput.xlsx", sheetName = "Original_Series", append = TRUE)
