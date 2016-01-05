#Visualizations

viz1 <- dfSTATEHOUSING %>% ggplot(aes(x=STATE)) + 
  geom_bar(aes(y=POPULATION, fill=HOUSING), stat="identity") + 
  geom_pointrange(aes(y=LANDAREA*100, ymax=LANDAREA*100, ymin=POPULATION, color=LANDAREA*100), size=1, shape=15) + 
  scale_x_discrete(name="State") + 
  scale_y_continuous(name="Population", labels = comma, breaks=seq(0, 55000000, by=5000000)) + 
  scale_fill_continuous(name="Housing Units", low="blue", high="orange", labels=comma, breaks=seq(2000000, 12000000, by=2000000)) + 
  scale_color_continuous(name="Land Area (sq. ft.) * 100", low="dark green", high="brown", labels=comma, breaks=seq(10000000, 50000000, by=10000000)) + 
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90))
#Shows Population by state as the bars, Housing by state as color fill, and landarea*100 as points with a line segment showing the difference between landarea*100 and population. This shows that landarea is not necessarily related to population or amount of housing.
Rsqr <- round(with(dfSTATEHOUSINGTOTALS, cor(EMP_RATE,AVG_PAY)), 4)
Rsqr <- paste("R squared: ",Rsqr)
viz2 <- dfSTATEHOUSINGTOTALS %>% ggplot(aes(x=EMP_RATE, y=AVG_PAY)) + 
  geom_point(aes(size=POPULATION, color=NUM_ESTABLISHMENTS)) + 
  geom_text(aes(label=STATE), vjust=-1) +
  geom_text(aes(0.45, 60, label=Rsqr)) +
  geom_smooth(method=lm, color="dark green", fill="green", size=1, linetype=5) +
  geom_line(group=1, linetype=2, size=0.25) +
  scale_y_continuous(name="Average Annual Pay (thousands)", labels = dollar, breaks=seq(35, 60, by=5)) +
  scale_x_continuous(name="Employment Rate of Population", labels = percent, breaks=seq(.3, .5, by=.025)) +
  scale_color_continuous(name="Number of Employing Establishments", low="blue", high="red", labels=comma) +
  scale_size_continuous(name="Population", range=c(3,10), labels=comma, breaks=seq(0, 55000000, by=5000000)) + 
  theme(text = element_text(size=18))
#Shows scatterplot with connecting line and linear regression of employment rate and average payroll per person. Shows a strong correlation between payment and employment rate. Also shows number of establishments and population.

y <- density(dfNormalized$normalEMP_RATE)$y
max <- match(max(y),y)
normalEMP_RATE_max <- density(dfNormalized$normalEMP_RATE)$x[max]
y <- density(dfNormalized$normalAVG_EMP)$y
max <- match(max(y),y)
normalAVG_EMP_max <- density(dfNormalized$normalAVG_EMP)$x[max]
y <- density(dfNormalized$normalAVG_PAY)$y
max <- match(max(y),y)
normalAVG_PAY_max <- density(dfNormalized$normalAVG_PAY)$x[max]
y <- density(dfNormalized$normalGro_Avg)$y
max <- match(max(y),y)
normalGro_Avg_max <- density(dfNormalized$normalGro_Avg)$x[max]
# creates the peak marks of the normalized distributions
breaklist <- c(-3, -2.5, -2, -1.5, -1, normalAVG_PAY_max, normalGro_Avg_max, normalEMP_RATE_max, 0, normalAVG_EMP_max, 0.5, 1, 1.5, 2, 2.5, 3)
for (element in breaklist){
  breaklist[element] <- round(breaklist[element], 2)
}

viz3 <- melt(dfNormalized) %>% ggplot(aes(fill=variable, alpha=0.01)) + 
  geom_density(aes(x=value)) + 
  stat_function(fun = dnorm, linetype=5) + 
  geom_vline(aes(xintercept=mean(value)),linetype=5, size=1) + 
  geom_vline(aes(xintercept=normalEMP_RATE_max), linetype=5, size=1, color="red") + 
  geom_vline(aes(xintercept=normalAVG_EMP_max), linetype=5, size=1, color="green") + 
  geom_vline(aes(xintercept=normalAVG_PAY_max), linetype=5, size=1, color="blue") + 
  geom_vline(aes(xintercept=normalGro_Avg_max), linetype=5, size=1, color="purple") + 
  scale_y_continuous(name="Frequency", labels = percent, breaks=seq(0, 0.5, by=0.05)) + 
  scale_x_continuous(name="Normalized Standard Deviation", labels = comma, breaks=breaklist) + scale_fill_discrete(name="Measure", labels=c("Employment Rate", "Average Employment", "Averge Annual Pay", "Average Growth")) + 
  theme(text = element_text(size=18), axis.text.x = element_text(angle=90))
  
#Shows normalized distributions of Average Employment, Average Payroll, and Average Growth rate. Also shows a normal distribution for comparison. This shows how "normal" these distributions are, or not.
