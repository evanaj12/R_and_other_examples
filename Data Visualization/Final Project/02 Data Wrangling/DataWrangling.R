dfSTATEHOUSINGTOTALS<-left_join(dfSTATETOTALS, dfSTATEHOUSING, by = "STATE")
dfSTATEHOUSINGTOTALS<- dfSTATEHOUSINGTOTALS %>% mutate(EMP_RATE=EMPLOYMENT/POPULATION) %>% mutate(WATER_PERC=WATERAREA/TOTALAREA) %>% mutate(LAND_PERC=LANDAREA/TOTALAREA) %>% mutate(AVG_EMP=EMPLOYMENT/NUM_ESTABLISHMENTS) %>% mutate(AVG_PAY=PAYROLL/EMPLOYMENT)
dfSTATECOMPGROWTH <- dfTOPCOMPANIES %>% group_by(STATE) %>% summarise (groAvg = mean(GROWTH), revAvg = mean(REVENUE))
dfTOTALS<- left_join(dfSTATEHOUSINGTOTALS, dfSTATECOMPGROWTH, by="STATE")
dfNormalized <- dfTOTALS %>% mutate(normalEMP_RATE=((EMP_RATE-mean(EMP_RATE))/(sd(EMP_RATE)))) %>% mutate(normalAVG_EMP=((AVG_EMP-mean(AVG_EMP))/(sd(AVG_EMP)))) %>% mutate(normalAVG_PAY=((AVG_PAY-mean(AVG_PAY))/(sd(AVG_PAY)))) %>% mutate(normalGro_Avg=((groAvg-mean(groAvg))/(sd(groAvg))))
dfNormalized <- dfNormalized %>% select(normalEMP_RATE, normalAVG_EMP, normalAVG_PAY, normalGro_Avg)

dfSTATEHOUSINGTOTALS1<-left_join(dfSTATETOTALS, dfSTATEHOUSING, by = "STATE")
dfSTATEHOUSINGTOTALS1<-transform(dfSTATEHOUSINGTOTALS1, NUM_ESTABLISHMENTS = as.numeric(as.character(NUM_ESTABLISHMENTS)))
dfSTATEHOUSINGTOTALS1<-transform(dfSTATEHOUSINGTOTALS1, NUM_FIRMS = as.numeric(as.character(NUM_FIRMS)))
dfSTATEHOUSINGTOTALS1<-transform(dfSTATEHOUSINGTOTALS1, PAYROLL = as.numeric(as.character(PAYROLL)))
dfSTATEHOUSINGTOTALS1<-transform(dfSTATEHOUSINGTOTALS1, EMPLOYMENT = as.numeric(as.character(EMPLOYMENT)))
dfSTATEHOUSINGTOTALS2 <- dfSTATEHOUSINGTOTALS1%>%select(STATE, NUM_FIRMS, NUM_ESTABLISHMENTS, EMPLOYMENT, POPULATION, POPULATIONDENSITY)%>%filter("NUM_FIRMS">100000)
dfSTATEHOUSINGTOTALS3<-dfSTATEHOUSINGTOTALS1%>%select(STATE,NUM_FIRMS, NUM_ESTABLISHMENTS,EMPLOYMENT, POPULATION, POPULATIONDENSITY)%>%mutate(EMPLOYMENT_RATE=EMPLOYMENT/POPULATION)

dfTopCompanies2<-dfTOPCOMPANIES%>%group_by(INDUSTRY, STATE)%>%summarise(AVG_REVENUE = mean(REVENUE),AVG_GROWTH = mean(GROWTH))
dfTopCompanies3<-dfTopCompanies2%>%filter(STATE=="TX")

#sql crosstab
dfRANKREVENUE <- data.frame(eval(parse(text=substring(getURL(URLencode('http://129.152.144.84:5001/rest/native/?query="SELECT COMPANYID, INDUSTRY, REVENUE, rank() OVER (PARTITION BY INDUSTRY order by REVENUE desc) as INDUSTRY_RANK FROM TOPCOMPANIES order by 2,3 desc"'), httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_nh5797', PASS='orcl_nh5797', MODE='native_mode', MODEL='model', returnFor = 'R', returnDimensions = 'False'), verbose = TRUE), 1, 2^31-1))))
dfRANKPOPDENSITY <- data.frame(eval(parse(text=substring(getURL(URLencode('http://129.152.144.84:5001/rest/native/?query="SELECT STATE, POPULATION, HOUSING,POPULATIONDENSITY, rank() OVER ( order by POPULATIONDENSITY desc) as POPDENSITY_RANK from STATEHOUSING"'), httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_nh5797', PASS='orcl_nh5797', MODE='native_mode', MODEL='model', returnFor = 'R', returnDimensions = 'False'), verbose = TRUE), 1, 2^31-1))))

  