# Question 2


#Import excel sheet
library(readxl)
data <- read_excel("/Users/olubunmi/Downloads/DANA4800_HW2_Q2_Data.xlsx")
View(data)
#head(data, n = 5)

#class(data)
x <- data$Expense
#A) Use the summary() function to find the Five-Number Summary. [1 mark]
summary(x)


#B) Use the hist() functions to produce a Histogram with proportion on the y-axis with 6 bars only.
#Specifically, please make sure there are 6 bars (1-10, 11-20, …, 51-60), chart title included and
#axes labelled properly

h <- hist(x,
          breaks = seq(0, 60, by = 10),
          plot = FALSE)

#class(x)
h$density <- h$counts / length(x)

plot(h,
     main = "Weekly Expenses for Langara Students (CAD)",
     xlab = "Weekly Expenses (CAD$)",
     xlim = c(0, 60),
     ylab = "Proportion of Students",
     ylim = c(0,0.4),
     freq = FALSE,
     col = "purple")

#C) Provide a description of the above graph. [2 marks]

# The histogram is roughly symmetrical, with most weekly expenses falling between $21 and $40. 
# The middle 50% of the Langara students spend between about $24 and $36, giving an IQR of $12.50. 
# The tallest bar, $21–$30, reaches 0.38 on the proportion axis (38% of students), and the $31–$40 bar reaches 0.31 (31%), 
# so 69% of the sampled students spend between $21 and $40 a week.

#D) Use the following segment of code to produce a Density Curve. Provide a description of the shape (only). [2+1 marks]

dens <- density(x)
plot(dens, main = "Density Curve of Weekly Expenses",
     xlab = "Weekly Expenses (CAD$)")
polygon(dens,col = "pink")

#Description of Density
#The description of the shape is slightly symmetrical with small bumps at the beginning of the curve near $10 and towards the end of the curve near $55.


#E) Use the boxplot() function to make a horizontal Boxplot. [2 marks]
#Note: Make sure you label your graph and axes appropriately.

boxplot(x, 
        horizontal = TRUE,
        main = "Boxplot of Weekly Expenses",
        xlab = "Weekly Expenses (CAD$)",
        col = "Orange")





















