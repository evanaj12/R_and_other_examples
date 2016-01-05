# Categoricals
categoricals <- eval(parse(text=substring(getURL(URLencode('http://129.152.144.84:5001/rest/native/?query="select * from CLIMATE_DATA_AUSTIN"'), httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL', USER='C##cs329e_eaj628', PASS='orcl_eaj628', MODE='native_mode', MODEL='model', returnFor = 'R', returnDimensions = 'True'), verbose = TRUE), 1, 2^31-1)))

#group by plot
myplot <- function(df, x) {
  names(df) <- c("x", "n")
  ggplot(df, aes(x=x, y=n)) + geom_point(aes(size = n))+ scale_x_discrete(x)+ theme(axis.text.x = element_text(size=6, angle=90, vjust=0.5))+labs(title=paste("Categorical Plot - ", x)) + theme(plot.title = element_text(size=40, face="bold", vjust=1, family="Times"))
}

ClimateList<- list()
for (i in names(austin_data)) { 
  if (i %in% categoricals[[1]]) {
    r <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select \\\""i"\\\", count(*) n from CLIMATE_DATA_AUSTIN group by \\\""i"\\\" "'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL',USER='C##cs329e_eaj628',PASS='orcl_eaj628',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON', i=i),verbose = TRUE)))
    p <- myplot(r,i)
    print(p) 
    ClimateList[[i]] <- p
  }
}

# Create png

if (length(ClimateList) > 0) {
  png("./00 Doc/Categoricals.png", width = 50, height = 70, units = "in", res = 72)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 6)))   
  print(ClimateList[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3, height = 0.5))
  print(ClimateList[[2]], vp = viewport(layout.pos.row = 2, layout.pos.col = 4:6, height = 0.5))
  dev.off()
}



##############


#histogram plot
myplot2 <- function(df, x) {
  names(df) <- c("x")
  ggplot(df, aes(x=x)) + geom_histogram(na.rm = TRUE) + scale_x_continuous(x) + theme (axis.text.x = element_text(size=6, angle=90, vjust=0.5)) + labs(title=paste("Non-Categorical Plot - ", x)) + theme(plot.title = element_text(size=40, face="bold", vjust=1, family="Times"))
}

ClimateList2<- list()
for (i in names(austin_data)) { 
  if (i %in% categoricals[[2]]) {
    r <- data.frame(fromJSON(getURL(URLencode('129.152.144.84:5001/rest/native/?query="select \\\""i"\\\" from CLIMATE_DATA_AUSTIN where \\\""i"\\\" is not null"'),httpheader=c(DB='jdbc:oracle:thin:@129.152.144.84:1521:ORCL',USER='C##cs329e_eaj628',PASS='orcl_eaj628',MODE='native_mode',MODEL='model',returnDimensions = 'False',returnFor = 'JSON', i=i),verbose = TRUE)))
    p <- myplot2(r,i)
    print(p) 
    ClimateList2[[i]] <- p
  }
}

# Create png

if (length(ClimateList2) > 0) {
  png("./00 Doc/Categoricals2.png", width = 50, height = 70, units = "in", res = 72)
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(3, 6)))   
  print(ClimateList2[[1]], vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))
  print(ClimateList2[[2]], vp = viewport(layout.pos.row = 1, layout.pos.col = 4:6))
  print(ClimateList2[[3]], vp = viewport(layout.pos.row = 2, layout.pos.col = 1:6, height = 0.5))
  print(ClimateList2[[4]], vp = viewport(layout.pos.row = 3, layout.pos.col = 1:6, height = 0.5))
  dev.off()
}
