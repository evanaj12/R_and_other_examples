txCaCompanyData <- companyData %>% select(RANK, STATE, COMPANY, INDUSTRY)%>% filter(STATE %in% c("TX", "CA"))
industryGrowthData <- companyData %>% group_by(INDUSTRY) %>% summarise (revMean = mean(REVENUE), groMean = mean(PERCENTAGEGROWTH))
stateGrowth <- companyData %>% group_by(STATE) %>% summarise (groMax = max(PERCENTAGEGROWTH), revMax = max(REVENUE))
cityGrowth <- companyData %>% group_by (CITY) %>% summarise (groMin = min(PERCENTAGEGROWTH), revMin = min(REVENUE))%>% arrange(desc(groMin))
