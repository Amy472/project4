na.omit(vgsales)

boxplot(vgsales$JP_Sales)
outliers<-boxplot(vgsales$JP_Sales,plot = F)$out
vgsales[-which(vgsales$Global_Sales %in% outliers),]


Q1 <- quantile(vgsales$JP_Sales, 0.25)
Q3 <- quantile(vgsales$JP_Sales, 0.75)
IQR <- IQR(vgsales$JP_Sales)

Up <- Q3 + 1.5 * IQR 
Low <- Q1 - 1.5 * IQR 

cleaned_vgsales <- subset(vgsales, vgsales$JP_Sales > Low & vgsales$JP_Sales < Up)
boxplot(cleaned_vgsales$JP_Sales, main="Boxplot of JP Sales (Outliers Removed)", ylab="JP Sales")

vgsales$JP_Sales[is.na(vgsales$JP_Sales)] <- median(vgsales$JP_Sales, na.rm = TRUE)

library(ggplot2)
ggplot(vgsales, aes(x = JP_Sales)) + 
  geom_histogram(binwidth = 0.5, fill = "skyblue", color = "black") + 
  labs(title = "Histogram of JP Sales (Missing Values Replaced with Median)", x = "Global Sales", y = "Frequency") +
  theme_minimal()