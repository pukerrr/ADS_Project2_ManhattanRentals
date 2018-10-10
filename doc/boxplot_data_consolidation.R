load('../output/housing_all.RData')

df.temp <- housing_all[c("zipcode",colnames(housing_all)[grep("count",colnames(housing_all))])]
df.temp <- df.temp[!duplicated(df.temp),]
colnames(df.temp) <- gsub(" count","",colnames(df.temp))
colnames(df.temp) <- c("zipcode","restaurant violations", "American", "Chinese", "Italian", 
                       "Japanese", "pizzerias", "bars", "clubs", "subway stations", "buses", "hospitals", "markets", 
                       "theatres", "galleries", "crime", "parks", "car crashes", "complaints")
df.temp <- df.temp[-ncol(df.temp)]

df.boxplot <- data.frame(
  Zipcode = rep(df.temp$zipcode,ncol(df.temp[-1])),
  gather(df.temp[-1],"Category","Count"),
  Group = NA
)

df.boxplot[c(which(df.boxplot$Category=="American"),
             which(df.boxplot$Category=="Chinese"),
             which(df.boxplot$Category=="Italian"),
             which(df.boxplot$Category=="Japanese"),
             which(df.boxplot$Category=="pizzerias"),
             which(df.boxplot$Category=="markets")),"Group"] <- "Food"
df.boxplot[c(which(df.boxplot$Category=="hospitals"),
             which(df.boxplot$Category=="crime"),
             which(df.boxplot$Category=="car crashes"),
             which(df.boxplot$Category=="restaurant violations")),"Group"] <- "Safety"
df.boxplot[c(which(df.boxplot$Category=="bars"),
             which(df.boxplot$Category=="clubs")), "Group"] <- "Night Life"
df.boxplot[c(which(df.boxplot$Category=="subway stations"),
             which(df.boxplot$Category=="buses")), "Group"] <- "Transportation"
df.boxplot[c(which(df.boxplot$Category=="theatres"),
             which(df.boxplot$Category=="galleries"),
             which(df.boxplot$Category=="parks")), "Group"] <- "Recreation"

write.csv(df.boxplot,"../output/boxplot_data.csv")
