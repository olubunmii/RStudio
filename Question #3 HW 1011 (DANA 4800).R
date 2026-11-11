# Question 3

#A) Create a Frequency Table of the variable “Distracted” using the table() function. [1 mark]
# Note: When copy-and-pasting text output from R to Word document, for example, make sure
# you use “fixed-width fonts”, like Courier New. Otherwise, the output does not look right or
# aligned properly.
library(readxl)
q3Data <- read_excel('/Users/olubunmi/Downloads/DANA-4800-001 - Data Analysis and Stat Infer - 10112026 - 1022 PM/DANA4800_HW2_Q3_Data.xlsx')
View(q3Data)

Distracted <- q3Data$Distracted

counts <- table(Distracted)
counts

#B) Create a Probability Table of the variable “Distracted” using the proportions() function. [1 mark]

props <- round(proportions(counts),2)
props

#F)Produce a Pie Chart using the pie() function, with the “clockwise” arguments set as TRUE, and
# “Yes” goes before “No”. Please also submit the code to produce such graph. Please use fixed-
#   width fonts. [2+1 marks]
Distracted_f <- factor(Distracted, levels = c("Yes", "No"))
counts <- table(Distracted_f)

pie (counts, 
     main = "Distracted Drivers in Vancouver (n=80)",
     clockwise = TRUE,
     col = c("pink", "lightblue"),
     labels = paste0(names(counts), 
                     "\n", 
                     counts,
                     " (", 
                     round(100 * proportions(counts), 
                           1), "%)"))

#H) Produce a Bar Graph using the barplot() function, “Yes” goes to the left of “No”. Please also
# submit the code to produce such graph. Please use fixed-width fonts. [2+1 marks]
# Note: A graph directly copied from Excel without any annotation will get a zero.

barplot(counts, 
        main = "Distracted Drivers in Vancouver (n = 80)",
        xlab = "Distracted While Driving",
        ylab = "Number of Drivers",
        col = c("pink", "Purple"),
        ylim = c(0, 50))