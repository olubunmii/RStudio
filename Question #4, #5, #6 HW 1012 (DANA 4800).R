# Question 4

#C) Use the plot() function to produce a Scatterplot, based on the roles you defined in the above
# part. [2 marks]

library(readxl)

corrData <- read_excel ('/Users/olubunmi/Downloads/DANA-4800-001 - Data Analysis and Stat Infer - 10112026 - 1022 PM/DANA4800_HW2_Q4_Data.xlsx')
View(corrData)

fat <- corrData$Fat
calories <- corrData$Calories

summary(fat)
summary(calories)

plot(calories,
     fat,
     main = "Scatterplot of Fat vs. Calories",
     ylab = "Fat (in grams per serving)",
     xlab = "Calories (in calories)",
     col = "green",
     pch = 20,
     cex = 1.5)

round(
  cov(cbind(calories,fat)), 1)

#c(1, 2, 3)   # [1] 1 2 3

round(cor(calories, fat), 4)


#QUESTION 5
#C)Use the plot() function to produce a Scatterplot, based on the roles you defined in the above part. [2 marks]
# Note: Make sure you label your graph and axes appropriately.

moveData <- read_excel('/Users/olubunmi/Downloads/DANA-4800-001 - Data Analysis and Stat Infer - 10112026 - 1022 PM/DANA4800_HW2_Q5_Data.xlsx')
View(moveData)

size <- moveData$Size
labour <- moveData$Labour

summary(size)
summary(labour)

plot (size, 
      labour,
      main = "Scatterplot of Labour vs. Size",
      ylab = "Labour (in hours)",
      xlab = "Size (in cubic feet)",
      col = "brown",
      pch = 20,
      cex = 1.5)

round(cor(size, labour), 4)


kidsData <- read_excel('/Users/olubunmi/Downloads/DANA-4800-001 - Data Analysis and Stat Infer - 10112026 - 1022 PM/DANA4800_HW2_Q6_Data.xlsx')
View(kidsData)

gender <- kidsData$Gender
goals <- kidsData$Goals

myKidsData <- table(gender, goals)
myKidsData

round(proportions(myKidsData), 2)
round(proportions(myKidsData, margin = 1), 2)
round(proportions(myKidsData, margin = 2), 2)

colProp <- proportions(myKidsData, margin = 2)
barplot (colProp,
         main = "Side-by-side Bar Graph of Goals by Gender",
         xlab = "Goals",
         ylab = "proportion",
         ylim = c(0, 0.7),
         beside = TRUE,
         col = c("lightblue", "Pink"),
         legend.text = rownames(colProp),
         args.legend = list(x = "topright", title = "Gender"))
























