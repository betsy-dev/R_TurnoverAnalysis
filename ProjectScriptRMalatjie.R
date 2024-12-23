#Exploratory Data Analysis Using R#

#import packages#
install.packages("readxl")
install.packages("ggplot2")
install.packages("knitr")
install.packages("rmarkdown")


#1  Import data turnover excel file#
library(readxl)
turnoverdata <-read_excel("Turnover.xlsx")

#inspection of no. of rows and columns#

#Code to inspect rows and columns
rowsandcolumns<- dim(turnoverdata)
print(rowsandcolumns)# to view the no. of rows and columns


cont_variables<-str(turnoverdata) #To check the structure of the data

#2  Histogram showing distribution of work experience#
library(ggplot2)
#histogram creation
hist(turnoverdata$experience,breaks = 15,                    
     col = "red",               # Color for the bars according the question
     border = "black",                # Border color
     main = "Distribution of Work Experience",  # Title
     xlab = "Experience (Years)",     # X-axis label
     ylab = "Frequency")              # Y-axis label

#3 Descriptive stats for extraversion and anxiety#
?mean

extrav_mean<-mean(turnoverdata$extraversion, na.rm= TRUE)
extrav_median<-median(turnoverdata$extraversion, na.rm= TRUE)

#calculate mean and median for anxiety
anxiety_mean<-mean(turnoverdata$anxiety, na.rm= TRUE)
anxiety_median<-median(turnoverdata$anxiety, na.rm= TRUE)

print(extrav_mean) 
print(extrav_median)
print(anxiety_mean)
print(anxiety_median)



#4 Bar chart showing gender within profession#
?bar
?scale_fill_manual
?aes
genderProfession_table <- table(turnoverdata$profession, turnoverdata$gender)

barplot(genderProfession_table[, "m"],# Select counts for males
        col = "green",                            # Color for male bars
        main = "Number of Males in Each Profession", 
        xlab = "Profession", 
        ylab = "Count", 
        names.arg = rownames(genderProfession_table),  # Profession names
        las = 2, # Rotate x-axis labels to be vertical
        cex.names = 0.7, # Font size for profession names
        border = "black") 

# Bar chart for females
barplot(genderProfession_table[,"f"], # Select counts for females
        col = "green", # Color for female bars
        main = "Number of Females in Each Profession", 
        xlab = "Profession", 
        ylab = "Count", 
        names.arg = rownames(genderProfession_table),  # Profession names
        las = 2.5,# Rotate x-axis labels to be vertical
        cex.names = 0.7, # Font size for profession names
        border = "black")  

#5 Cross tabulation#
# Finding max value to evaluate categorizing

library(knitr)
maxexperience <- max(turnoverdata$experience, na.rm = TRUE)

# Categorizing the experience into groups
turnoverdata$experiencegroup <- cut(turnoverdata$experience,
                                    breaks = c(0, 5, 10, 20, 30, 50, Inf),
                                    labels = c("0-5 years", "6-10 years", "11-20 years", 
                                               "21-30 years", "31-50 years", "50+ years"),
                                    right = FALSE)

# Create a contingency table for industry and experience groups
industryAndexperience_table <- table(turnoverdata$industry, turnoverdata$experiencegroup)

kable(industryAndexperience_table, format = "markdown", 
      caption = "Contingency Table of Industry and Experience Groups")

#Bar plot for visualiztion of table
barplot(industryAndexperience_table,
        main = "Experience Groups Distribution Across Industries",
        xlab = "Industry",
        ylab = "Count",
        col = c("lightblue", "lightgreen", "yellow", "orange", "purple", "pink"),  # Colors for different experience groups
        beside = FALSE,  # Stacked bars
        las = 2,  # Rotate x-axis labels
        border = NA)  # Remove black borders
legend("topright",  
       legend = colnames(industryAndexperience_table),  # Experience group labels
       fill = c("lightblue", "lightgreen", "yellow", "orange", "purple", "pink"),
       title = "Experience Groups",
       cex = 0.4)
      

#6 Distribution of Employees based on mode of Transport#
wayTable <- table(turnoverdata$way)

# Bar plot for transport modes
barplot(wayTable,
        col = "magenta",    # magenta
        main = "Distribution of Transport Modes",
        xlab = "Mode of Transport",
        ylab = "Count")


#7 Examine personality traits(extraversion and Self-Control) by gender
#Descriptive Statistics by gender
# Calculating the average extraversion by gender
avg_extraversion <- tapply(turnoverdata$extraversion, turnoverdata$gender, mean, na.rm = TRUE)

# bar plot for average extraversion by gender
barplot(avg_extraversion,
        main = "Bar plot: Extraversion by Gender",
        ylab = "Average Extraversion",
        col = c("blue", "green"),
        names.arg = c("Male", "Female"), # Change labels if needed
        beside = TRUE)

# Calculate the average self-control by gender
avg_selfcontrol <- tapply(turnoverdata$selfcontrol, turnoverdata$gender, mean, na.rm = TRUE)

# bar plot for average self-control by gender
barplot(avg_selfcontrol,
        main = "Bar plot: Self-Control by Gender",
        ylab = "Average Self-Control",
        col = c("cyan", "red"),
        names.arg = c("Male", "Female"), # Change labels if needed
        beside = TRUE)

#8 Calculation fpr the average experience by profession
avgExperiencebyProfession <- 
  aggregate(experience ~ profession, data = turnoverdata, FUN = mean)

print(avgExperiencebyProfession) #to show the tabular results

## Bar chart for average experience by profession
# code to calculate the average experience by profession
avgExperiencebyProfession <- tapply(turnoverdata$experience, turnoverdata$profession, mean, na.rm = TRUE)

# bar plot code for average experience by profession
barplot(avgExperiencebyProfession,
        main = "Average Work Experience by Profession",
        xlab = "Profession",
        ylab = "Average Experience",
        col = "lightblue",
        las = 2) 

#9 Proportion of the employees getting coaching by gender#

coachingGender <- table(turnoverdata$gender, turnoverdata$coach)

print(coachingGender)

# Proportion of employees receiving coaching by gender
coachingGenderProp <- prop.table(coachingGender, margin = 1)*100

# Print the cross-tabulation table
library(knitr)
kable(coachingGenderProp, format = "pipe", 
      caption = " Employees Coaching count by Gender")

#Bar Plot for question 9

barplot(coachingGenderProp,
        main = "Proportion of Employees Receiving Coaching by Gender",
        xlab = "Coaching Status",
        ylab = "Percentage",
        col = c("yellow", "brown"),
        legend = rownames(coachingGenderProp), # Add a legend for gender
        beside = TRUE, # Separate bars for each gender
        ylim = c(0, 100))

library(knitr)
library(rmarkdown)