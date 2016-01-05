g1 <- prcp_InnerJoin %>% filter(DATE_YMD<20140000) %>% ggplot(aes(x=DATE_YMD, color = 'City')) + geom_point(aes(y = austPrcp, colour = "Austin"))+ geom_point(aes(y = nashPrcp, colour = "Nash")) + geom_point(aes(y = seatPrcp, colour = "Seat"))+ ylab("Precipitation") + xlab("Date") + labs(title='Rainfall Amount') + theme_economist()+scale_colour_wsj()

g2 <- nash_seatTEMP%>%filter(DATE_YMD<20140000)%>%ggplot(aes(x= DATE_YMD, y = nashMaxTemp, color = 'City'))+geom_point(aes(y=nashMinTemp, colour = "Nash Min Temp"))+geom_point(aes(y=nashMaxTemp, colour = "Nash Max Temp"))+geom_point(aes(y=seatMinTemp, colour = "Seattle Min Temp"))+geom_point(aes(y=seatMaxTemp, colour = "Seattle Max Temp")) + ylab("Temperature") + xlab("Date") + labs(title='Temperature Range') + theme_economist()+scale_colour_wsj()

g3 <- all_data %>% filter(DATE_YMD<20140000, TMAX!=-9999) %>% ggplot(aes(x=DATE_YMD, y=TMAX, color=CITY)) +geom_point() + ylab("Maximum Temperature") + xlab("Date") + labs(title='Max Temperature by City') + theme_economist()+scale_colour_wsj()

g4 <- all_data %>% filter(DATE_YMD<20140000, TMAX!=-9999) %>% ggplot(aes(x=CITY, y=TMAX, color=SEASON)) +geom_violin() + ylab("Maximum Temperature") + xlab("Date") + labs(title='Max Temperature Range') + theme_economist()+scale_colour_wsj()


