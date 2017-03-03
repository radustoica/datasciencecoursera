# Questions
# 
# Your data analysis must address the following questions:
#     
#     Across the United States, which types of events (as indicated in the EVTYPE variable) are most 
#         harmful with respect to population health?
#     Across the United States, which types of events have the greatest economic consequences?
# Consider writing your report as if it were to be read by a government or municipal manager who 
#         might be responsible for preparing for severe weather events and will need to prioritize 
#         resources for different types of events. However, there is no need to make any specific 
#         recommendations in your report.


if (!file.exists("StormData.csv.bz2")) {

	url = "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2";
	download.file(url, "StormData.csv.bz2");
	rm("url");
}

dataset = read.csv("StormData.csv.bz2", na.string = ""
	, colClasses = c(rep("NULL", 7), "factor", rep("NULL", 14), rep("numeric", 3)
	, "character", "numeric", "character", rep("NULL", 9)));
	
bool = grepl("M|K|B", dataset$PROPDMGEXP, ignore.case = T);
dataset$PROPDMGEXP[!bool] = NA;
dataset$PROPDMGEXP = factor(toupper(dataset$PROPDMGEXP));

bool = grepl("M|K|B", dataset$CROPDMGEXP, ignore.case = T);
dataset$CROPDMGEXP[!bool] = NA;
dataset$CROPDMGEXP = factor(toupper(dataset$CROPDMGEXP));

rm("bool");

fatalities = tapply(dataset$FATALITIES, dataset$EVTYPE, sum, na.rm = T);
injuries = tapply(dataset$INJURIES, dataset$EVTYPE, sum, na.rm = T);

f1 = as.data.frame(cbind(names(sort(fatalities, decreasing = T)[1:10])
            , unname(sort(fatalities, decreasing = T)[1:10])));
i1 = as.data.frame(cbind(names(sort(injuries, decreasing = T)[1:10])
            , unname(sort(injuries, decreasing = T)[1:10])));

names(i1) = c("EVTYPE", "INJURIES");
names(f1) = c("EVTYPE", "FATALITIES");

i1$INJURIES = as.integer(as.character(i1$INJURIES));
f1$FATALITIES = as.integer(as.character(f1$FATALITIES));

require(ggplot2);
require(gridExtra);

g1 = ggplot(i1, aes(x = EVTYPE, y = INJURIES)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    theme_dark() + 
    ggtitle("Top injuries across US caused by harmful natural event by type") +
    geom_text(aes(label = INJURIES, vjust = -0.25)) ;

g2 = ggplot(f1, aes(x = EVTYPE, y = FATALITIES)) +
    geom_bar(stat = "identity", fill = "lightblue", color = "black") +
    theme_dark() + 
    ggtitle("Top fatalities across US caused by harmful natural event by type") +
    geom_text(aes(label = FATALITIES, vjust = -0.25));
	
# x = merge(f1, i1);
# x$total = x$FATALITIES + x$INJURIES;

# g3 = ggplot(data = x, aes(x = EVTYPE, y = interaction(FATALITIES, INJURIES, sep = " + "))) + 
	# geom_bar(stat = "sum") + theme_dark() +
	# geom_text(aes(label = total, vjust = -0.25));

grid.arrange(g1, g2, nrow = 2);



