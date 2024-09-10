#TAN YONG HONG 
#TP063960

#Import Data set, pre-processing
employeeAttrition = read.csv("C:\\Users\\yhcominthru\\Desktop\\Year 2 Sem 1\\PFDA\\ASsignment\\employee_attrition.csv", header = TRUE)

library(ggplot2)
library(crayon)
library(plotrix)
library(dplyr)
library(tidyr)
library(stringr)
#Data Exploration

#structure
str(employeeAttrition)                

#number of rows and columns
dim(employeeAttrition)

#Column Names
names(employeeAttrition)

#Summary of the data
summary(employeeAttrition)

names(employeeAttrition) = c("ID", "Record Date", "Birth Date", "Hired Date", "Termination Date", "Age", 
                             "Length of Service(Year)", "City", "Department", "Job Title", 
                             "Store Name", "Gender", "Gender(Full)", "Reason of Termination", 
                             "Type of Termination", "Year of Status", "Employee Status", "Business Unit")
employeeAttrition
employeeAttrition = data.frame(employeeAttrition)

View(employeeAttrition)
employeeAttrition[is.na(employeeAttrition$`Record Date`),]
unique(employeeAttrition$City)
unique(employeeAttrition$`Store Name`)
unique(employeeAttrition$`Reason of Termination`)
unique(employeeAttrition$`Type of Termination`)
unique(employeeAttrition$`Business Unit`)
unique(employeeAttrition$Department)
unique(employeeAttrition$`Job Title`)
unique(employeeAttrition$`Employee Status`)

terminationList = data.frame(filter(employeeAttrition, Reason.of.Termination == "Layoff"))
View(terminationList)

resignationList = data.frame(filter(employeeAttrition,  Reason.of.Termination == "Resignaton"))
View(resignationList)

retirementList = data.frame(filter(employeeAttrition,  Reason.of.Termination == "Retirement"))
View(retirementList)

naList = data.frame(filter(employeeAttrition,  Reason.of.Termination == "Not Applicable"& Employee.Status == "ACTIVE"))
View(naList)

dateCheck = data.frame(filter(employeeAttrition, Termination.Date < '2/1/1900', ))
View(dateCheck)

orderedByID_Year =  data.frame(employeeAttrition[order(employeeAttrition[,1], -employeeAttrition[,7]),  ])    
View(orderedByID_Year)  

removeDuplicates = orderedByID_Year[!duplicated(employeeAttrition$ID), ]   
View(removeDuplicates)

uniqueList = unique(rbind.data.frame(removeDuplicates, terminationList, resignationList, retirementList))
View(uniqueList)

#Question 1: Why do the employees want to resign?
#Analysis 1.1: The amount of people resigned by age

employeeResignationAge = c(resignationList$Age)
employeeAgeHist = hist(employeeResignationAge, main = "Employee Resignation Age",xlab = "Age", xlim = c(19,65), col = "darkmagenta")
text(employeeAgeHist$mids, employeeAgeHist$counts, labels = employeeAgeHist$counts, adj=c(0.5,-0.5))

resignationListMale = data.frame(filter(resignationList, resignationList$Gender == "M"))
resignationListFemale = data.frame(filter(resignationList, resignationList$Gender == "F"))
resignationListMaleAge = c(resignationListMale$Age)
resignationListMaleHist = hist(resignationListMaleAge, main = "Male Employees Resignation Age",xlab = "Age", xlim = c(19,65), col = "blue")
text(resignationListMaleHist$mids,resignationListMaleHist$counts, labels = resignationListMaleHist$counts, adj=c(0.5,-0.5))

#Analysis 1.2: The amount of people resigned based on their jobs
jobCountAndGender = data.frame(table(resignationList$Job.Title, resignationList$Gender))
names(jobCountAndGender) = c("Job", "Gender", "Frequency")
jobCountAndGender$Gender = factor(jobCountAndGender$Gender, levels = c("M", "F"))
jobCountAndGenderOrdered = data.frame(jobCountAndGender[order(jobCountAndGender$Job),])
View(jobCountAndGenderOrdered)

jobs = c(jobCountAndGenderOrdered$Job)
frequency = c(jobCountAndGenderOrdered$Frequency)
gender = c("F", "M")

barplot(frequency, names.arg = jobs, xlab = "Jobs", ylab = "Frequency", col = c("red", "blue"), 
        main = "Employees Resigned according to jobs", border = "black")
legend("topright", legend = gender, cex = 1.3, fill = c("red", "blue"))

#Analysis 1.3: The average resignation age according to jobs

resignedBakers = resignationList[which(resignationList$Job.Title == 'Baker'),]
resignedCashiers = resignationList[which(resignationList$Job.Title == 'Cashier'),]
resignedDairyPersons = resignationList[which(resignationList$Job.Title == 'Dairy Person'),]
resignedHRISAnalysts = resignationList[which(resignationList$Job.Title == 'HRIS Analyst'),]
resignedMeatCutters = resignationList[which(resignationList$Job.Title == 'Meat Cutter'),]
resignedFoodManagers = resignationList[which(resignationList$Job.Title == 'Processed Foods Manager'),]
resignedClerks = resignationList[which(resignationList$Job.Title == 'Produce Clerk'),]
resignedShelfStockers = resignationList[which(resignationList$Job.Title == 'Shelf Stocker'),]
resignedStoreManagers = resignationList[which(resignationList$Job.Title == 'Store Manager'),]

averageJobResignationAge = c(mean(resignedBakers$Age), mean(resignedCashiers$Age),mean(resignedDairyPersons$Age), mean(resignedHRISAnalysts$Age), 
                             mean(resignedMeatCutters$Age), mean(resignedFoodManagers$Age), mean(resignedClerks$Age), mean(resignedShelfStockers$Age), mean(resignedStoreManagers$Age))
ResignationAgeRounded = round(averageJobResignationAge, digits = 2)
resignedJobsList = c('Baker', 'Cashier',  'Dairy Person', 'HRIS Analyst', 'Meat Cutter', 'Processed Foods Manager','Produce Clerk', 'Shelf Stocker','Store Manager' )
averageAgeAndJob = data.frame(ResignationAgeRounded, resignedJobsList)
names(averageAgeAndJob) = c("Average Age", "Job")
View(averageAgeAndJob)
ggplot(averageAgeAndJob, aes(x=averageAgeAndJob$Job, y=averageAgeAndJob$`Average Age`)) + 
  labs(title="Average Resignation Age According to Jobs", x="Job Titles", y = "Average Age")+
  geom_bar(stat="identity", color="black", fill="blue", width=0.5) + 
  geom_text(aes(label=averageAgeAndJob$`Average Age`), vjust=1.6, color="white", size=3.5) + 
  theme_minimal()

#Analysis 1.4: The amount of people resigned for each store and unit
orderedByStoreDeparmentUnit = data.frame(resignationList[order(resignationList$Store.Name, resignationList$Business.Unit), ] )
View(orderedByStoreDeparmentUnit)

storeNameAndUnit = data.frame(table(resignationList$Store.Name, resignationList$Business.Unit))
names(storeNameAndUnit) = c("Store Name", "Unit", "Frequency")
storeNameAndUnitOrdered = data.frame(storeNameAndUnit[order(storeNameAndUnit[,1], storeNameAndUnit[,2]), ])  

ggplot(storeNameAndUnitOrdered, aes(fill=`Unit`, y=`Frequency`, x=`Store.Name`)) + 
  geom_bar(position="stack", stat="identity")  +
  labs(title="Job Resignation for Each Store", x="Stores", y = "Frequency") +
  guides(fill = guide_legend((title = "Unit")))


unique(employeeAttrition$`Store Name`) #y-axis bar chart
unique(employeeAttrition$Department) #bar plot
unique(employeeAttrition$`Business Unit`)

#Question 2: Why do the employees get laid off?
#Analysis 2.1: The number of employees that were laid off every year
terminationList = data.frame(filter(employeeAttrition, Reason.of.Termination == "Layoff"))
View(terminationList)
terminationByYear = data.frame(terminationList[order(terminationList$Year.of.Status), ])
View(terminationByYear)

ggplot(terminationByYear, aes(x=as.factor(`Year.of.Status`))) + 
  geom_bar(stat ="count") + labs(title = "Number of lay-offs by year", x = "Year", y = "Quantity" )

#Analysis 2.2: The number of employees that were laid off based on age
ageArranged = data.frame(table(terminationList$Age))
View(ageArranged)
ggplot(ageArranged, aes(y=Freq, x=Var1)) + geom_bar(stat = "identity", color = "black") +
  labs(title = "Number of employees being laid off based on age", x = "Age", y = "Quantity") +
  geom_text(aes(label=Freq), vjust=1.6, color="white", size=3.5) +
  theme_minimal()


#Analysis 2.3: The number of employees that were laid off based on job title ; store & unit (according to job title, according to store & unit)
jobArranged = data.frame(table(terminationList$Job.Title))
jobArrangedOrdered = data.frame(jobArranged[order(-jobArranged$Freq), ])
View(jobArrangedOrdered)
storeArranged = data.frame(table(terminationList$Store.Name))
View(storeArranged)
unitArranged = data.frame(table(terminationList$Business.Unit))
View(unitArranged)

store11 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 11) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store11Frame = as.data.frame(table(store11$Job.Title))
store11Frame$percent = round(100*store11Frame$Freq/sum(store11Frame$Freq), digits = 1)
store11Frame$label = paste(store11Frame$Var1," (", store11Frame$percent,"%)", sep = "")
pie(store11Frame$Freq, labels = store11Frame$label, main = "Jobs of employees fired in store 11", radius = 1,init.angle = 45, col = rainbow(nrow(store11Frame)))

store20 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 20) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store20Frame = as.data.frame(table(store20$Job.Title))
store20Frame$percent = round(100*store20Frame$Freq/sum(store20Frame$Freq), digits = 1)
store20Frame$label = paste(store20Frame$Var1," (", store20Frame$percent,"%)", sep = "")
pie(store20Frame$Freq, labels = store20Frame$label, main = "Jobs of employees fired in store 20", radius = 1,init.angle = 45, col = rainbow(nrow(store20Frame)))

store13 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 13) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store13Frame = as.data.frame(table(store13$Job.Title))
store13Frame$percent = round(100*store13Frame$Freq/sum(store13Frame$Freq), digits = 1)
store13Frame$label = paste(store13Frame$Var1," (", store13Frame$percent,"%)", sep = "")
pie(store13Frame$Freq, labels = store13Frame$label, main = "Jobs of employees fired in store 13", radius = 1,init.angle = 45, col = rainbow(nrow(store13Frame)))

store39 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 39) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store39Frame = as.data.frame(table(store39$Job.Title))
store39Frame$percent = round(100*store39Frame$Freq/sum(store39Frame$Freq), digits = 1)
store39Frame$label = paste(store39Frame$Var1," (", store39Frame$percent,"%)", sep = "")
pie(store39Frame$Freq, labels = store39Frame$label, main = "Jobs of employees fired in store 39", radius = 1,init.angle = 45, col = rainbow(nrow(store39Frame)))

store14 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 14) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store14Frame = as.data.frame(table(store14$Job.Title))
store14Frame$percent = round(100*store14Frame$Freq/sum(store14Frame$Freq), digits = 1)
store14Frame$label = paste(store14Frame$Var1," (", store14Frame$percent,"%)", sep = "")
pie(store14Frame$Freq, labels = store14Frame$label, main = "Jobs of employees fired in store 14", radius = 1,init.angle = 45, col = rainbow(nrow(store14Frame)))

store27 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 27) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store27Frame = as.data.frame(table(store27$Job.Title))
store27Frame$percent = round(100*store27Frame$Freq/sum(store27Frame$Freq), digits = 1)
store27Frame$label = paste(store27Frame$Var1," (", store27Frame$percent,"%)", sep = "")
pie(store27Frame$Freq, labels = store27Frame$label, main = "Jobs of employees fired in store 27", radius = 1,init.angle = 45, col = rainbow(nrow(store27Frame)))

store9 = sample_frac(terminationList, 1) %>% subset(`Store.Name` == 9) %>% select(`ID`, `Job.Title`, `Store.Name`) %>% arrange(`Job.Title`)
store9Frame = as.data.frame(table(store9$Job.Title))
store9Frame$percent = round(100*store9Frame$Freq/sum(store9Frame$Freq), digits = 1)
store9Frame$label = paste(store9Frame$Var1," (", store9Frame$percent,"%)", sep = "")
pie(store9Frame$Freq, labels = store14Frame$label,main = "Jobs of employees fired in store 9", radius = 1,init.angle = 45, col = rainbow(nrow(store9Frame)))

#job count of each store (11, 20, 13, 39, 14, 27, 9) -> digging into the percentage of the stores that fired most peeps can be done

#Question 3:Does the organization have enough human resources to fill in the vacant spots from termination?
#Analysis 3.1: The number of new employees and terminated employees per year
fullTerminationList = uniqueList %>% filter(Reason.of.Termination == "Layoff"|Reason.of.Termination == "Resignaton" |Reason.of.Termination == "Retirement") %>%
  select(`ID`, `Reason.of.Termination`, `Store.Name`,`Year.of.Status`) %>% arrange(`Year.of.Status`)
View(fullTerminationList)
terminationsByYear = table(fullTerminationList$Year.of.Status)  # line chart later

fullNewEmployeeList = employeeAttrition %>% filter(Length.of.Service.Year. == 0) %>% select(`ID`, `Year.of.Status`, `Job.Title`, `Store.Name`) %>% arrange(`Year.of.Status`)
newEmployeesByYear = table(fullNewEmployeeList$Year.of.Status)

plot(terminationsByYear,type = "o", col = "red", xlab = "Year", ylab = "Amount",
     main = "Number of terminated employees every year")

plot(newEmployeesByYear,type = "o", col = "red", xlab = "Year", ylab = "Amount",
     main = "Number of new employees every year")


#Analysis 3.2: The number of working employees for each year 
workingEmployeeList = uniqueList %>% filter(uniqueList$Reason.of.Termination == "Not Applicable") %>% select(`ID`, `Year.of.Status`, `Store.Name`,`Job.Title`) %>% arrange(`Year.of.Status`)
View(data.frame(workingEmployeeList))
workingEmployeeListByYear = table(workingEmployeeList$Year.of.Status)
View(workingEmployeeListByYear)
plot(fullEmployeeListByYear,type = "o", col = "red", xlab = "Year", ylab = "Amount",
     main = "Number of total working employees every year")


#Analysis 3.3: The number of new employees and terminated employees for the stores that terminated the most amount of people each year
#Store 11
store11Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 11, ])
store11TerminationPerYear = as.data.frame(table(store11Termination$Year.of.Status))
View(store11TerminationPerYear)

store11New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 11, ])
store11NewPerYear = as.data.frame(table(store11New$Year.of.Status))
View(store11NewPerYear)

store11Final = full_join(store11TerminationPerYear, store11NewPerYear, by = "Var1", all.x = TRUE)
store11Final[is.na(store11Final)] = 0
names(store11Final)[2:3] = c("Terminated", "New")
View(store11Final)

cols <- c('red','blue');
ylim <- c(0,max(store11Final[c('Terminated','New')])*1.8);
par(lwd=6);
store11Bar = barplot(t(store11Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 11 by year", beside=T,ylim=ylim,border=cols,col='white',
  names.arg=store11Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Store 20
store20Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 20, ])
store20TerminationPerYear = as.data.frame(table(store20Termination$Year.of.Status))
View(store20TerminationPerYear)

store20New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 20, ])
store20NewPerYear = as.data.frame(table(store20New$Year.of.Status))
View(store20NewPerYear)

store20Final = full_join(store20TerminationPerYear, store20NewPerYear, by = "Var1", all.x = TRUE)
store20Final[is.na(store20Final)] = 0
names(store20Final)[2:3] = c("Terminated", "New")
View(store20Final)

cols <- c('red','blue');
ylim <- c(0,max(store20Final[c('Terminated','New')])*1.8);
par(lwd=6);
store20Bar = barplot(t(store20Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 20 by year", beside=T,ylim=ylim,border=cols,col='white',
                     names.arg=store20Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Store 13
store13Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 13, ])
store13TerminationPerYear = as.data.frame(table(store13Termination$Year.of.Status))
View(store13TerminationPerYear)

store13New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 13, ])
store13NewPerYear = as.data.frame(table(store13New$Year.of.Status))
View(store13NewPerYear)

store13Final = full_join(store13TerminationPerYear, store13NewPerYear, by = "Var1", all.x = TRUE)
store13Final[is.na(store13Final)] = 0
names(store13Final)[2:3] = c("Terminated", "New")
View(store13Final)

cols <- c('red','blue');
ylim <- c(0,max(store13Final[c('Terminated','New')])*1.8);
par(lwd=6);
store13Bar = barplot(t(store13Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 13 by year", beside=T,ylim=ylim,border=cols,col='white',
                     names.arg=store13Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Store 39
store39Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 39, ])
store39TerminationPerYear = as.data.frame(table(store39Termination$Year.of.Status))
View(store39TerminationPerYear)

store39New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 39, ])
store39NewPerYear = as.data.frame(table(store39New$Year.of.Status))
View(store39NewPerYear)

store39Final = full_join(store39TerminationPerYear, store39NewPerYear, by = "Var1", all.x = TRUE)
store39Final[is.na(store39Final)] = 0
names(store39Final)[2:3] = c("Terminated", "New")
View(store39Final)


cols <- c('red','blue');
ylim <- c(0,max(store39Final[c('Terminated','New')])*1.8);
par(lwd=6);
store39Bar = barplot(t(store39Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 39 by year", beside=T,ylim=ylim,border=cols,col='white',
                     names.arg=store39Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Store 14
store14Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 14, ])
store14TerminationPerYear = as.data.frame(table(store14Termination$Year.of.Status))
View(store14TerminationPerYear)

store14New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 14, ])
store14NewPerYear = as.data.frame(table(store14New$Year.of.Status))
View(store14NewPerYear)

store14Final = full_join(store14TerminationPerYear, store14NewPerYear, by = "Var1", all.x = TRUE)
store14Final[is.na(store14Final)] = 0
names(store14Final)[2:3] = c("Terminated", "New")
View(store14Final)

cols <- c('red','blue');
ylim <- c(0,max(store14Final[c('Terminated','New')])*1.8);
par(lwd=6);
store14Bar = barplot(t(store14Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 14 by year", beside=T,ylim=ylim,border=cols,col='white',
                     names.arg=store14Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Store 27
store27Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 27, ])
store27TerminationPerYear = as.data.frame(table(store27Termination$Year.of.Status))
View(store27TerminationPerYear)

store27New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 27, ])
store27NewPerYear = as.data.frame(table(store27New$Year.of.Status))
View(store27NewPerYear)

store27Final = full_join(store27TerminationPerYear, store27NewPerYear, by = "Var1", all.x = TRUE)
store27Final[is.na(store27Final)] = 0
names(store27Final)[2:3] = c("Terminated", "New")
View(store27Final)


cols <- c('red','blue');
ylim <- c(0,max(store27Final[c('Terminated','New')])*1.8);
par(lwd=6);
store27Bar = barplot(t(store27Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 27 by year", beside=T,ylim=ylim,border=cols,col='white',
                     names.arg=store27Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Store 9
store9Termination = as.data.frame(fullTerminationList[fullTerminationList$Store.Name == 9, ])
store9TerminationPerYear = as.data.frame(table(store9Termination$Year.of.Status))
View(store9TerminationPerYear)

store9New = as.data.frame(fullNewEmployeeList[fullNewEmployeeList$Store.Name == 9, ])
store9NewPerYear = as.data.frame(table(store9New$Year.of.Status))
View(store9NewPerYear)

store9Final = full_join(store9TerminationPerYear, store9NewPerYear, by = "Var1", all.x = TRUE)
store9Final[is.na(store9Final)] = 0
names(store9Final)[2:3] = c("Terminated", "New")
View(store9Final)
store9Final = arrange(store9Final, "Var1")
View(store9Final)

cols <- c('red','blue');
ylim <- c(0,max(store9Final[c('Terminated','New')])*1.8);
par(lwd=6);
store9Bar = barplot(t(store9Final[c('Terminated','New')]),main = "New and Terminated Employees in Store 9 by year", beside=T,ylim=ylim,border=cols,col='white',
                     names.arg=store9Final$`Var1`,xlab='Year',ylab='Amount',legend = TRUE,args.legend=list(text.col=cols,col=cols,border=cols,bty='n'));
box();

#Question 4: Is there any store being affected by the terminations? If yes, how's the condition?
#Analysis 4.1: How many stores are active?
employeeAttrition %>%
  filter(employeeAttrition$Employee.Status == "ACTIVE") %>%
  ggplot(aes(x = as.factor(Store.Name), y = as.factor(Year.of.Status))) +
  geom_count(aes(color = ..n..))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(titles = "No. of active employees per store in 2006 ~ 2015", x = "Store Name", y = "Year")
  
#Analysis 4.2: How many stores are facing potential employee shortage in year 2014 and 2015?

activeEmployees = filter(employeeAttrition, employeeAttrition$Employee.Status == "ACTIVE")

for(x in 1:46){
  store = filter(activeEmployees, activeEmployees$Store.Name == x)
  sumActiveEmployees = 0
  for(y in 2006:2015){
    storeYear = filter(store, store$Year.of.Status == y)
    sumActiveEmployees = sumActiveEmployees + nrow(storeYear)
  }
  storeAverage = sumActiveEmployees / 10
  round(storeAverage, 2)
  for(z in 2014:2015){
    storeYear = filter(store, store$Year.of.Status == z)
    if(sum(storeYear$Employee.Status == 'ACTIVE') < storeAverage){
      difference = storeAverage - sum(storeYear$Employee.Status == 'ACTIVE')
      print(paste("Store ", x, " is facing employee shortage on year ", z, " with ", sum(storeYear$Employee.Status == 'ACTIVE'), " amount of employees, that's ", difference, " lesser than the average"  ))
      
    }
  }
}

#Question 5: Are any jobs affected by the terminations?
#Analysis 5.1: How many jobs still consist of employees?
employeeAttrition %>%
  filter(employeeAttrition$Employee.Status == "ACTIVE") %>%
  ggplot(aes(x = Job.Title, y = as.factor(Year.of.Status))) +
  geom_count(aes(color = ..n..))+
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  labs(titles = "No. of active employees per job title in 2006 ~ 2015", x = "Job Title", y = "Year")

#Analysis 5.2:  Why are certain director jobs currently vacant?

directors = employeeAttrition %>%
  filter(str_detect(Job.Title, "Director"))
directors = as.data.frame(directors)
View(directors)

directors %>%
  filter(Type.of.Termination == "Voluntary") %>%
  ggplot(aes(x = Job.Title, y = Reason.of.Termination)) +
  geom_tile(size = 1, fill = "plum") +
  coord_flip() +
  facet_wrap(~Year.of.Status) +
  labs(x = "Job Title", y = "Termination reason", title = "Director Status", fill = "Type of Termination") 

#Analysis 5.3: Why are certain analyst jobs currently vacant?
  
analysts = employeeAttrition %>% 
  filter(str_detect(Job.Title, "Analyst")) %>%
  filter(Employee.Status == "TERMINATED") 
analysts = as.data.frame(analysts)
View(analysts)

analysts %>%
  ggplot(aes(x = Job.Title, y = Reason.of.Termination)) +
  geom_tile(size = 1, fill = "plum") + 
  coord_flip() +
  facet_wrap(~Year.of.Status) +
  labs(x = "Job Title", y = "Termination reason", title = "Analyst Status", fill = "Type of Termination")
  
employeeAttrition %>%  
  filter(str_detect(Job.Title, "Analyst")) %>%
  ggplot(aes(x = Job.Title, y = Employee.Status)) +
  geom_tile(size = 1, fill = "plum") + 
  coord_flip() +
  facet_wrap(~Year.of.Status) +
  labs(x = "Job Title", y = "Employee Status", title = "Analyst Status")
  













