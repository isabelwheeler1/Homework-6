library(tidyverse)
happy <- readRDS("data/HAPPY.rds")
str(happy)

#recode missing values to all be NA
#HAPPY <- replace(HAPPY, HAPPY %in% c("IAP", "DK", "NA"), NA)
#can't use %in% here
happy <- replace(happy, happy == "IAP", NA)
happy <- replace(happy, happy == "DK", NA)
happy <- replace(happy, happy == "NA", NA)

#Check the type of the variable and cast into the right type
#For age, change “89 OR OLDER” to 89 and assume the variable should be numeric
typeof(happy$AGE) #character
as.numeric(happy$AGE)

happy$AGE[happy$AGE == "89 OR OLDER"] <- 89
happy$AGE <- as.numeric(happy$AGE)
#check:
is.numeric(happy$AGE)

#Bring all levels of factors into a sensible order
happy1 = 
  happy %>% mutate(AGE = replace(AGE, AGE == "89 OR OLDER", 89)) %>%
  mutate(AGE = as.numeric(AGE), 
         HAPPY = as.factor(HAPPY), 
         SEX = as.factor(SEX), 
         MARITAL = as.factor(MARITAL), 
         DEGREE = as.factor(DEGREE)) %>%
  mutate(DEGREE = factor(DEGREE, levels = c("LT HIGH SCHOOL", 
                                            "HIGH SCHOOL" , 
                                            "JUNIOR COLLEGE", 
                                            "BACHELOR", 
                                            "GRADUATE")),
         MARITAL = factor(MARITAL, levels = c("NEVER MARRIED", 
                                              "MARRIED", 
                                              "DIVORCED",
                                              "SEPARATED", 
                                              "WIDOWED")),
         FINRELA = factor(FINRELA, levels = c("FAR BELOW AVERAGE",
                                              "BELOW AVERAGE",
                                              "AVERAGE",
                                              "ABOVE AVERAGE",
                                              "FAR ABOVE AVERAGE")),
         HEALTH = factor(HEALTH, levels = c("POOR",
                                            "FAIR",
                                            "GOOD",
                                            "EXCELLENT")),
         PARTYID = factor(PARTYID, levels = c("STRONG DEMOCRAT",
                                              "NOT STR DEMOCRAT",
                                              "IND,NEAR DEM",
                                              "INDEPENDENT",
                                              "IND,NEAR REP",
                                              "NOT STR REPUBLICAN",
                                              "STRONG REPUBLICAN",
                                              "OTHER PARTY")),
         POLVIEWS = factor(POLVIEWS, levels = c("EXTREMELY LIBERAL",
                                                "SLIGHTLY LIBERAL",
                                                "LIBERAL",
                                                "MODERATE",
                                                "SLIGHTLY CONSERVATIVE",
                                                "CONSERVATIVE",
                                                "EXTRMLY CONSERVATIVE")))


#make variables lowercase
happy1 <- happy %>% mutate(
  happy = factor(tolower(HAPPY)),
  year = factor(tolower(YEAR)),
  age = factor(tolower(AGE)),
  sex = factor(tolower(SEX)),
  marital = factor(tolower(MARITAL)),
  degree = factor(tolower(DEGREE)),
  finrela = factor(tolower(FINRELA)),
  health = factor(tolower(HEALTH)),
  wtssall = factor(tolower(WTSSALL)),
  partyid = factor(tolower(PARTYID)),
  polviews = factor(tolower(POLVIEWS)),
) %>% select(-HAPPY, -YEAR, -AGE, -SEX, -MARITAL, -DEGREE, -FINRELA, -HEALTH, 
             -WTSSALL, -PARTYID, -POLVIEWS)


#Investigate the relationship between happiness and two other variables in the data. 
#Find a visualization that captures the relationship and write a paragraph to describe
#Done by Isabel Wheeler

# compare marital status to happiness separated by sex
ggplot(aes(x = happy, fill = sex), data = happy1) +
  facet_wrap(~marital) +
  geom_bar(position = position_dodge())  +
  coord_flip() +
  labs(title = "Happiness on marital status and gender")
#The numbers for the count variable are a bit hard to read, but they aren't super
#important to this analysis. When looking at this bar plot, there is a much larger 
#number of married people, both male and female, that are in the very happy or pretty
#happy group. The largest group of people in the divorced group are in the pretty 
#happy group for males and females. For all of the groups the largest number of people
#is in the pretty happy group. It is interesting to look at the widowed group to see
#there are many more woman in this group than men and this can probably be attributed
#to the fact that woman live longer than men.




#just extra practice
ggplot(aes(x = happy, fill = finrela), data = happy1) +
  facet_wrap(~marital) +
  geom_bar() + 
  coord_flip() +
  labs(title = "Happiness on marital status and financial status")

 