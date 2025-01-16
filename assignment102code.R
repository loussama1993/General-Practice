

library(tidyverse)
library(RPostgreSQL)


# Load the driver to access the PostgreSQL server.
drv <- dbDriver('PostgreSQL')



# Establishing connection to the GP practice database on the server.
postgresql_port <- '5432'
database_name <- 'gp_practice_data'
con <- dbConnect(drv, dbname=database_name, host='localhost',
                 port=postgresql_port, user='postgres',
                 password=.rs.askForPassword('Password:'))


# Confirming that we have a correct connection by displaying the available tables.
cat('The following tables are available:\n')
print(dbListTables(con))
cat('\n')

#----------------------------------------------------------------------------------------
#Q1

numofgp<-dbGetQuery(con,'select hb, practiceid, avg(actcost * quantity) as totalspend
    from  gp_data_up_to_2015  
    group by hb,practiceid')

numofpat<-dbGetQuery(con,'select orgcode as practiceid, max(field4) as num_of_patient 
    from qof_achievement
    group by orgcode')
healtboard<-dbGetQuery(con,"select  practiceid as hb, locality
    from address
    where practiceid like '%7A%' ") %>% 
    mutate(hb=str_trim(hb))

numofindicator<-dbGetQuery(con,'select orgcode as practiceid ,count( distinct indicator) as num_of_indicator
    from qof_achievement
    group by orgcode ')


firsttable<- numofgp %>% 
    inner_join(numofpat) %>% 
    inner_join(numofindicator) %>% 
    group_by(hb) %>% 
    summarise(hb,num_of_gp=n(),avg_of_indictor =mean(num_of_indicator), total_patients = sum(num_of_patient),avg_spend=mean(totalspend)) %>%
    filter(row_number()==1) %>% 
    ungroup() %>%
    inner_join(healtboard) %>% 
    rename(Health_Board=locality) %>% 
    select(Health_Board,num_of_gp,total_patients,avg_of_indictor,avg_spend)


view(firsttable)
#---------------------------------------------------------------------------------
#Q2
#Allow the user to select one health board from the 7 health board we have


selecthb <- healtboard %>% select(hb)
selecthb <- as.vector(healtboard$hb)
userchoice <- select.list(selecthb, preselect = NULL, multiple = FALSE,
                          title = NULL, graphics = TRUE)
#Listing all the practices in the health board that the user selected

listpractices <- dbGetQuery(con, sprintf("select distinct address.practiceid
                  from address
                  inner join gp_data_up_to_2015 ON address.practiceid = gp_data_up_to_2015.practiceid
                  where trim(gp_data_up_to_2015.hb) = '%s'", userchoice))

#The most prevalent three conditions in the health board and their prevalence
prevalentconditions <- dbGetQuery(con, sprintf("select  indicator as condition, sum(numerator) as population_with_the_condition, avg(ratio) as prevalence
                  from qof_achievement
                  inner join gp_data_up_to_2015 ON qof_achievement.orgcode = gp_data_up_to_2015.practiceid
                  where gp_data_up_to_2015.hb = '%s'
                  group by hb,indicator
                  order by population_with_the_condition DESC
                  limit 3", userchoice)) 

view(prevalentconditions)
ggplot(prevalentconditions, aes(x = condition, y = population_with_the_condition)) + 
  geom_col() +
  labs(title = "Prevalent Conditions", x = "Condition", y = "Population with the Conditions") 
------------------------------------------------------------------------------------------------------------------------
#Q3
#Allow the user to select one GP from the health board they chose.
selectGP <- as.vector(listpractices$practiceid)
userchoice2 <- select.list(selectGP, preselect = NULL, multiple = FALSE,
                          title = NULL, graphics = TRUE)
#The 5 most prescribed types of drugs in that practice by the number 
query <- "select 
    gp_data_up_to_2015.bnfname, 
    SUM(gp_data_up_to_2015.actcost*gp_data_up_to_2015.quantity) AS spend,
    bnf.chapterdesc AS type, 
    bnf.sectiondesc AS category1, 
    bnf.subsectiondesc AS category2  
  FROM 
    gp_data_up_to_2015
  INNER JOIN 
    bnf ON SUBSTRING(gp_data_up_to_2015.bnfcode FROM 1 FOR 9) = bnf.bnfchemical
  WHERE 
    practiceid =  $1
  GROUP BY 
    gp_data_up_to_2015.bnfname, type, category1, category2
  ORDER BY 
    spend DESC
  limit 5"
    
five_most_prescribed_drugs <- dbGetQuery(con, query, params = list(userchoice2)) 



view(five_most_prescribed_drugs)


ggplot(five_most_prescribed_drugs, aes(x = bnfname, y = log(spend), fill = type)) + 
  geom_col() +
  labs(title = "Most prescribed drugs", x = "Drug name", y = "The spending")
-------------------------------------------------------------------------------------------------------------------------------
#Q4
#Visualisation of the distribution of the average GP practice spend per month between health broads
GPspendpermonth <- dbGetQuery(con, 'select hb, avg(actcost * quantity) as avgspend, period 
                              from gp_data_up_to_2015
                              group by period, hb') 

ggplot(GPspendpermonth, aes(x = avgspend)) +
geom_histogram() 

print(GPspendpermonth ) 

#The average GP spend per month is not normally distributed that's why I am using a non-parametric test 
alpha <- 0.05
test <- kruskal.test(avgspend ~ hb, data = GPspendpermonth)

# p-value < alpha, therefore we reject the null hypothesis and there is no significant difference between the average 
#GP practice spend per month between Health Boards
print(test)
----------------------------------------------------------------------------------------------------------------------------------
#part2
#Visualisation of prescription spending over time for each health board 
prescriptionspending <- dbGetQuery(con, 'select period, hb, avg(actcost * quantity) as averagemonthlyspend
                                         from gp_data_up_to_2015
                                         group by period, hb
                                         order by period
                                   ')
prescriptionspending$period <- as.character(prescriptionspending$period)

# Add a day component (e.g., the first day of each month)
prescriptionspending$period <- as.Date(paste0(prescriptionspending$period, "01"), format = "%Y%m%d")

ggplot(prescriptionspending, aes(x = period, y = averagemonthlyspend, color = hb)) +
  geom_point() +
  facet_wrap(~hb, scales = "free", ncol = 1) +
  labs(x = "Period", y = "Average Monthly Spend") +
  scale_x_date(labels = scales::date_format("%b %y"),
               breaks = unique(prescriptionspending$period)) +
  theme_minimal()

------------------------------------------------------------------------------------------------------------------------------------------
# The location of the GP practice selected
  
query <- "select * from address where practiceid =  $1"
selectedaddress <- dbGetQuery(con, query, params = list(userchoice2))
 
 

location <- paste(selectedaddress$street, selectedaddress$area, selectedaddress$posttown, selectedaddress$county, selectedaddress$postcode)
cat('\nThe practice ',userchoice2, ' is located in .\n', sep='')
print(location)

-----------------------------------------------------------------------------------------------------------------------------------------
#Occurance of stroke and Transient Ischemic Attack (TIA) in Wales
stroke_TIA <- dbGetQuery(con,
                           "select
                              max(numerator) as totalpatients,
                              max(field4 ) as totalpopulation,
                              round( max(numerator)/ max(field4 )::numeric, 6) as stroke_ratio
                           from
                              qof_achievement
                           where
                              indicator like 'STIA%' and orgcode like 'WAL'")
cat('\nThe stroke and transient ischemic attack ratio in Wales was:\n', sep='')
print(stroke_TIA)
ratio <- as.vector(stroke_TIA$stroke_ratio)
condition_frequency <- if (ratio < 0.1) {
  'not frequent'
} else if (0.5 > ratio) {
  'frequent'
} else {
  'very frequent'
}

cat('Condition Frequency:', condition_frequency, '\n')

#Occurance of diabetes in Wales 


diabetes <- dbGetQuery(con,
                           "select
                              max(numerator) as totalpatients,
                              max(field4) as totalpopulation,
                              round(max(numerator) / max(field4)::numeric, 6) as diabetes_ratio
                           from
                              qof_achievement
                           where
                              indicator like 'DM%' and orgcode like 'WAL'")
cat('\nThe diabetes ratio in Wales was:\n', sep='')
print(diabetes)
ratio <- as.vector(diabetes$diabetes_ratio)
condition_frequency <- if (ratio < 0.1) {
  'not frequent'
} else if (0.5 > ratio) {
  'frequent'
} else {
  'very frequent'
}

cat('Condition Frequency:', condition_frequency, '\n')


stroke_diabetes <- bind_rows(diabetes,stroke_TIA)

view(stroke_diabetes)

------------------------------------------------------------------------------------------------------------------------
#Most 100  prescribed medicines

top100bnf <- dbGetQuery(con, 'select bnfname, count(bnfname) from gp_data_up_to_2015
group by bnfname
order by count desc
limit 100')

cat('the most prescribed medecines are :\n')
view(top100bnf)
# "Ventolin_Evohaler 100mcg (200 D)" is the 11th most prescribed drug. It's mostly used for asthma
#let's see if it is more prescribed in winter 
ventolin_each_season<- dbGetQuery(con, "select bnfname, 
                                  count(bnfname), 
                                  month,
                                  case
                                   when month = 9 OR month = 10 OR month = 11 then 'Autumn'
                                   when month = 12 OR month = 1 OR month = 2 then 'Winter'
                                   when month = 3 OR month = 4 OR month = 5 then 'Spring'
                                   else 'Summer'
                                   end as season
                                   from (
                                        select bnfname, 
                                             period - (period/100)*100 as month
                                        from gp_data_up_to_2015
                                        where bnfname = 'Ventolin_Evohaler 100mcg (200 D)'
                                        ) as vperiod
                                   group by bnfname, month, season")
#no correlation between Ventolin prescription and the season. It's most likely because asthmatic patients are renewing
#their prescription monthly. Therefore the prescription number is mostly the same.
---------------------------------------------------------------------------------------------------------------------
# Coronary Heart Disease prevalence in each health board during August 2014
CHD_per_hb <- dbGetQuery(con, "select hb as health_board, sum(numerator) as number_of_patients,indicator as chd_indicators
  from qof_achievement
  inner join gp_data_up_to_2015
  on qof_achievement.orgcode = gp_data_up_to_2015.practiceid
  where indicator like 'CHD%' and period = 201408 
  group by hb, indicator")

ggplot(CHD_per_hb, aes(x = health_board, y= number_of_patients, fill = chd_indicators)) +
  geom_col() +
  labs(title = "Coronary Heart Disease prevalence in each health board during August 2014", x = "Health Board", y = "Number of patients with Coronary Heart Disease", fill = "Coronary Heart Disease indicators")



  