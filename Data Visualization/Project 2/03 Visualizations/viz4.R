indTX <- txCaCompanyData %>% filter(STATE=="TX") %>% count(INDUSTRY) 
indTX$STATE <- "TX"
indCA <- txCaCompanyData %>% filter(STATE=="CA") %>% count(INDUSTRY) 
indCA$STATE <- "CA"
inds <- merge(indTX, indCA, all=TRUE)
viz4 <- ggplot(inds, aes(x=INDUSTRY, y=n, fill=STATE)) + geom_bar(stat="identity", position=position_dodge()) + scale_x_discrete(labels=abbreviate) + theme(axis.text.x=element_text(angle=90, vjust=0.5, size=12))

