#####################
# This file captures all the neat ideas that my fellow students had in their 
# assignments
#####################

# Student #2 used a data.table to calculate the mean for each metric

# newdt< -dt[,lapply(.SD,mean),by=list(Activity,Subject)]
# this command above allows you to apply the "aggregation" function on subset 
# of the table, that is not used in the by or keyby columns.


# Student 3 used melt and dcast for the means
# melted the dataframe with id=c("Subject","Activity')
# dcast(meltedtable,subject+activity~variable, mean)