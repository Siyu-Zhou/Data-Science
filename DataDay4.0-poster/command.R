setwd("D:/2017Winter/5000/project/")
getwd()
install.packages(c("Rcpp", "readr"))
library(readr)
mydata <- read_csv("1.csv")
summary(mydata)
mydata1 <- read.csv("1.csv")
names(mydata1)
summary(mydata1)
mydata <- subset( mydata1, select = -c(url,desc))
summary(mydata)
mydata <- subset(mydata, issue_d=="Dec-15")
summary(mydata)
write.csv(mydata, file="mydata.csv")
mydata <- read.csv("mydata.csv")

afterdeletemydata <- subset( mydata, select = -c(id,member_id,issue_d,title,zip_code,addr_state,last_pymnt_d,next_pymnt_d,annual_inc_joint,dti_joint,
                                       mths_since_recent_bc_dlq))
write.csv(afterdeletemydata, file="afterdeletemydata.csv")
summary(afterdeletemydata)
#complete is a logic victor
complete <- complete.cases(afterdeletemydata)
nob <- sum(complete)
nob# use nob to see how many complete data do I have
#just652row
#mths_since_last_major_derog has 31251 misssing value so I delete this att
delete2mydata <- subset( afterdeletemydata, select = -c(mths_since_last_major_derog))
write.csv(delete2mydata, file="delete2mydata.csv")
complete <- complete.cases(delete2mydata)
nob <- sum(complete)
nob#1022
summary(delete2mydata)
#mths_since_last_record has 36197 misssing value so I delete this att
delete3mydata <- subset( delete2mydata, select = -c(mths_since_last_record))
write.csv(delete3mydata, file="delete3mydata.csv")
delete3mydata <- read.csv("delete3mydata.csv")
complete <- complete.cases(delete3mydata)
nob <- sum(complete)
nob#5869
summary(delete3mydata)
existsalldata <- na.omit(delete3mydata)
summary(existsalldata)
#delete specific rows, those rows missing same attris,the number of rows is 22971
df <- data.frame(delete3mydata)
df<- df[-which(is.na(df$open_il_6m)), ]
summary(df)
write.csv(df, file="deleterow.csv")
complete <- complete.cases(df)
existsalldata <- na.omit(df)
nob <- sum(complete)
nob#5689
summary(existsalldata)
delete4mydata <- subset(df, select = -c(mths_since_recent_revol_delinq))
write.csv(delete4mydata, file="delete4mydata.csv")
summary(delete4mydata)
complete <- complete.cases(delete4mydata)
existsalldata <- na.omit(delete4mydata)
nob <- sum(existsalldata)
nob#8530
summary(existsalldata)
write.csv(existsalldata, file="deleterow.csv")

delete5mydata <- subset(delete4mydata, select = -c(verification_status_joint))
write.csv(delete5mydata, file="delete5mydata.csv")
summary(delete5mydata)
complete <- complete.cases(delete5mydata)
existsalldata <- na.omit(delete5mydata)
nob <- sum(complete)
nob#8530
summary(existsalldata)
write.csv(existsalldata, file="comdel5.csv")
empunknow <- read.csv("comdel5.csv", header=T, na.strings=c("","Un"))
write.csv(empunknow, file="empunknow.csv")
summary(empunknow)
names(empunknow)
mydata6 <- subset(empunknow, select = -c(X.3,X.2,X.1,X))
names(mydata6)
summary(mydata6)
write.csv(mydata6, file="mydata6.csv")
#combine grace and all late kinds
mydata6 <- read.csv("mydata6.csv")
mydata6$loan_status <- as.character(mydata6$loan_status)
mydata6$loan_status[mydata6$loan_status == "In Grace Period"] <- "Late"
mydata6$loan_status[mydata6$loan_status == "Late (16-30 days)"] <- "Late"
mydata6$loan_status[mydata6$loan_status == "Late (31-120 days)"] <- "Late"
#summary(mydata6)
write.csv(mydata6, file="mydata7.csv")
mydata6$loan_status <- as.factor(mydata6$loan_status)
summary(mydata6)
names(mydata6)
write.csv(mydata6, file="mydata7.csv")


##test am I filled empty employ as NA
#just calculate the number of NA in job type, by calculate the number of complete cases
#don't execute to generate new dataset
complete <- complete.cases(empunknow)
existsalldata <- na.omit(empunknow)
nob <- sum(complete)
nob#8112 so I have fill empty employ as 

mydata7 <- read.csv("mydata7.csv")

#I want to see column data type
sapply(mydata7, class)
#I want to convert emp_title into numeric
#first extract emp_title
subdat <- subset(mydata7, select=c("emp_title"))
write.csv(subdat, file="subdat1.csv")
#use excel to change NA in emp_title into Unknow
subdat <- read.csv("subdat1.csv")
#summary(subdat)
is.factor(subdat$emp_title)
ranks <- rank(-table(subdat$emp_title), ties.method="first")
DF <- data.frame(category=subdat$emp_title, rank=ranks[as.character(subdat$emp_title)])
print(DF)
write.csv(DF, file="subdat2.csv")
summary(DF$emp_title)
##change DF$rank to mydata7$emp_title use excel
##use excel to replce n/a in emp_length as Unknow
mydata8 <- read.csv("mydata7.csv")
subdat3 <- subset(mydata8, select=c("home_ownership"))
##tranfer RENT into number
subdat3 <- factor(subdat3$home_ownership)
ranks <- rank(-table(subdat3), ties.method="first")
DF <- data.frame(category=subdat3, rank=ranks[as.character(subdat3)])
print(DF)
write.csv(DF, file="subdat3.csv")

##use excel change home_ownership into new value which generate by last step
#change emp_length value
mydata8 <- read.csv("mydata7.csv")
subdat4 <- subset(mydata8, select=c("emp_length"))
##tranfer RENT into number
subdat4 <- factor(subdat4$emp_length)
ranks <- rank(-table(subdat4), ties.method="first")
DF <- data.frame(category=subdat4, rank=ranks[as.character(subdat4)])
print(DF)
write.csv(DF, file="subdat4.csv")

##use excel change home_ownership into new value which generate by last step
#change verification_status value
mydata8 <- read.csv("mydata7.csv")
subdat5 <- subset(mydata8, select=c("verification_status"))
##tranfer verification_status into number
subdat5 <- factor(subdat5$verification_status)
ranks <- rank(-table(subdat5), ties.method="first")
DF <- data.frame(category=subdat5, rank=ranks[as.character(subdat5)])
print(DF)
write.csv(DF, file="subdat5.csv")

##use excel change verification_status into new value which generate by last step
#change verification_status value
mydata8 <- read.csv("mydata7.csv")
subdat6 <- subset(mydata8, select=c("pymnt_plan"))
##tranfer verification_status into number
subdat6 <- factor(subdat6$pymnt_plan)
ranks <- rank(-table(subdat6), ties.method="first")
DF <- data.frame(category=subdat6, rank=ranks[as.character(subdat6)])
print(DF)
write.csv(DF, file="subdat6.csv")

##use excel change pymnt_plan into new value which generate by last step
#change verification_status value
mydata8 <- read.csv("mydata7.csv")
subdat7 <- subset(mydata8, select=c("purpose"))
##tranfer purpose into number
subdat7 <- factor(subdat7$purpose)
ranks <- rank(-table(subdat7), ties.method="first")
DF <- data.frame(category=subdat7, rank=ranks[as.character(subdat7)])
print(DF)
write.csv(DF, file="subdat7.csv")

##use excel change purpose into new value which generate by last step
##use excel change term into number
#change grade value
mydata8 <- read.csv("mydata7.csv")
subdat8 <- subset(mydata8, select=c("grade"))
##tranfer grade into number
subdat8 <- factor(subdat8$grade)
ranks <- rank(-table(subdat8), ties.method="first")
DF <- data.frame(category=subdat8, rank=ranks[as.character(subdat8)])
print(DF)
write.csv(DF, file="subdat8.csv")


##use excel change grade into new value which generate by last step
#change sub_grade value
mydata8 <- read.csv("mydata7.csv")
subdat9 <- subset(mydata8, select=c("sub_grade"))
##tranfer sub_grade into number
subdat9 <- factor(subdat9$sub_grade)
ranks <- rank(-table(subdat9), ties.method="first")
DF <- data.frame(category=subdat9, rank=ranks[as.character(subdat9)])
print(DF)
write.csv(DF, file="subdat9.csv")

##earliest_cr_line
##just keep year
##earliest_cr_line.xlxs
##last_credit_pull_d, just keep month

##use excel change sub_grade into new value which generate by last step
#change application_type value
mydata8 <- read.csv("mydata7.csv")
subdat10 <- subset(mydata8, select=c("application_type"))
##tranfer application_type into number
subdat10 <- factor(subdat10$application_type)
ranks <- rank(-table(subdat10), ties.method="first")
DF <- data.frame(category=subdat10, rank=ranks[as.character(subdat10)])
print(DF)
write.csv(DF, file="subdat10.csv")


##use excel change application_type into new value which generate by last step
#change initial_list_status value
mydata8 <- read.csv("mydata7.csv")
subdat11 <- subset(mydata8, select=c("initial_list_status"))
##tranfer initial_list_status into number
subdat11 <- factor(subdat11$initial_list_status)
ranks <- rank(-table(subdat11), ties.method="first")
DF <- data.frame(category=subdat11, rank=ranks[as.character(subdat11)])
print(DF)
write.csv(DF, file="subdat11.csv")

#after transform, I wan to see all column attris
mydata9 <- read.csv("mydata7.csv")
sapply(mydata9, class)

##use number to represent loan_status
subdat12 <- subset(mydata9, select=c("loan_status"))
##tranfer loan_status into number
subdat12 <- factor(subdat12$loan_status)
ranks <- rank(-table(subdat12), ties.method="first")
DF <- data.frame(category=subdat12, rank=ranks[as.character(subdat12)])
print(DF)
write.csv(DF, file="subdat12.csv")
##1current
##2 fully paid
##3 late
##4 chagered off

##feature selection --step-wise
library(MASS)
selectdata <- read.csv("mydata7.csv")
fit <- lm(loan_status~loan_amnt+funded_amnt+funded_amnt_inv+term+int_rate+installment+grade+sub_grade+emp_title+emp_length+home_ownership+annual_inc+verification_status+pymnt_plan+purpose+dti+delinq_2yrs+earliest_cr_line+inq_last_6mths+mths_since_last_delinq+open_acc+pub_rec+revol_bal+revol_util+total_acc+initial_list_status+out_prncp_inv+total_pymnt+total_pymnt_inv+total_rec_prncp+total_rec_int+total_rec_late_fee+recoveries+collection_recovery_fee+last_pymnt_amnt+last_credit_pull_d+collections_12_mths_ex_med+policy_code+application_type+acc_now_delinq+tot_coll_amt+tot_cur_bal+open_acc_6m+open_il_6m+open_il_12m+open_il_24m+mths_since_rcnt_il+total_bal_il+il_util+open_rv_12m+open_rv_24m+max_bal_bc+all_util+total_rev_hi_lim+inq_fi+total_cu_tl+inq_last_12m+acc_open_past_24mths+avg_cur_bal+bc_open_to_buy+bc_util+chargeoff_within_12_mths+delinq_amnt+mo_sin_old_il_acct+mo_sin_old_rev_tl_op+mo_sin_rcnt_rev_tl_op+mo_sin_rcnt_tl+mort_acc+mths_since_recent_bc+mths_since_recent_inq+num_accts_ever_120_pd+num_actv_bc_tl+num_actv_rev_tl+num_bc_sats+num_bc_tl+num_il_tl+num_op_rev_tl+num_rev_accts+num_rev_tl_bal_gt_0+num_sats+num_tl_120dpd_2m+num_tl_30dpd+num_tl_90g_dpd_24m+num_tl_op_past_12m+pct_tl_nvr_dlq+percent_bc_gt_75+pub_rec_bankruptcies+tax_liens+tot_hi_cred_lim+total_bal_ex_mort+total_bc_limit+total_il_high_credit_limit, data=selectdata)
step <- stepAIC(fit, direction = "both")
summary(step)
step$anova
#step$anova
summary(step$anova)


setwd("D:/2017Winter/5000/project/wekausefile/")
mydata <- read.csv("mydata7-threeclass-featueselect.csv")
#I want to see column data type
sapply(mydata, class)



#I want to calculte avegare of work year # FIND NO USE
workyearcol <- data.frame(empunknow$emp_length)
write.csv(workyearcol, file="workyearcol.csv")
newdata <- mydata[ which(mydata$gender=='F' & mydata$age > 65), ]


existsalldata <- na.omit(delete3mydata)
mydata <- subset(existsalldata, loan_status=="Charged Off")
nrow(mydata)


mths_since_recent_revol_delinq
tansfer data in to nnumeric for  earliest_cr_line, 
too much missing value in mths_since_last_deling &mths_since_last_record &mths_since_last_major_derog
revol_bal, revol_util
out_prncp_inv divide out_prncp,||total_pymnt_inv divide total_pymnt
total_rec_prncp divide funded_amnt
last_pymnt_amnt divide installment

