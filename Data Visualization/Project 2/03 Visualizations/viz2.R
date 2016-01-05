viz2 <- industryGrowthData %>% ggplot(aes(x=reorder(groMean, INDUSTRY), y=groMean, color=INDUSTRY, fill=INDUSTRY)) + geom_bar(stat="identity", width=0.5) + scale_x_discrete("INDUSTRY", labels="")

