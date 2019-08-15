setwd("/home/hitsnapper/courses/tdde01/exams/2017-04-18")
bank = read.csv2("bank.csv")

model = glm(formula=Visitors~Time, data=bank, family=poisson)

