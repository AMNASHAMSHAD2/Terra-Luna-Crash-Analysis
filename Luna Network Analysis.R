library(igraph)
library(tidyverse)
library(sna)
library(ergm)
#install.packages('e1071', dependencies=TRUE)
library(e1071)#for skewness 
library(lubridate)#month function

#finding correlation between the 5 cryptocoins

setwd('C:/Users/ENVY/Documents/Semester 2/Social Network Analysis/Labs/Project/Converted data')

# Get a list of files in the directory
files <- list.files()

# Create an empty list to store the data frames
data_list <- list()

# Loop through the files and read them into data frames
for (file in files) {
  # Read the file into a data frame
  df <- read.table(file, header = TRUE,sep = ",")
  df2 = df[5]
  
  # Extract the file name from the file path
  colname <- basename(file)
  
  # Remove file extension from the column name
  colname <- sub("\\..*", "", colname)
  
  # Set the column name in the data frame
  colnames(df2)[1] <- colname
  
  # Add the data frame to the list
  data_list[[file]] <- df2
  #view(data_list[[file]])
  
}
merged_df <- do.call(cbind, data_list)

CorMatrix=cor(merged_df)
view(CorMatrix)

##############################################################################################################



#APRIL STATS FOR 5 COINS


setwd('C:/Users/ENVY/Documents/Semester 2/Social Network Analysis/Labs/Project/Converted data')

# Get a list of files in the directory
files2 <- list.files()

# Create an empty list to store the data frames
data_list2 <- list()

# Loop through the files and read them into data frames
for (file in files2) {
  # Read the file into a data frame
  df2 <- read.table(file, header = TRUE,sep = ",")
  #df2 = df[,c("contract_address","close")]
  
  # Extract the file name from the file path
  colname <- basename(file)
  
  # Remove file extension from the column name
  colname <- sub("\\..*", "", colname)
  
  # Set the column name in the data frame
  df2$CryptoCoin <- colname
  
  # Add the data frame to the list
  data_list2[[file]] <- df2
  #view(data_list[[file]])
  
}
df_All_coins <- do.call(rbind, data_list2)


head(df_All_coins)


# Extract all rows where the month of month_column is equal to desired_month
April_df <- df_All_coins[month(df_All_coins$timestamp) == 04,]
head(April_df)



# Add a new column coin name based on the contract_address
#April_df$Crypto_coin <-ifelse(April_df$contract_address =="0xdac17f958d2ee523a2206206994597c13d831ec7", "USDT",
                              #ifelse(April_df$contract_address =="0xd2877702675e6ceb975b4a1dff9fb7baf4c91ea9", "WLUNA",
                                     #ifelse(April_df$contract_address =="0xa47c8bf37f92abed4a126bda807a7b7498661acd", "USTC",
                                           # ifelse(April_df$contract_address =="0xa0b86991c6218b36c1d19d4a2e9eb0ce3606eb48", "USDC",
                                                   #ifelse(April_df$contract_address =="0x8e870d67f660d95d5be530380d0ec0bd388289e1", "PAX","DAI")))))
  
                               
April_df_Summary<-April_df%>%
  group_by(CryptoCoin)%>%
  summarise(MaxPrice=max(close,na.rm=T),MinimumPrice=min(close,na.rm=T),MeanPrice=mean(close,na.rm=T),StandardDeviation=sd(close,na.rm=T),Skewness=skewness(close,na.rm=T))
view(April_df_Summary)



##########################################################################################################
#LUNA NETWORK IN APRIL


setwd('D:/ERC20/Luna Edge List')
LunaStats=data.frame(Month=character(),Density=character(),Reciprocity=character(),
                     Transitivity=character(),Centralization=character(),stringsAsFactors = FALSE)


df_Luna<- read_csv(file="Luna_April_EdgeList.csv") #read first row as header
LunaApril <- graph_from_data_frame(df_Luna,directed=TRUE)

head(LunaApril)


new_row <- data.frame(Month = "April", Density=graph.density(LunaApril), 
                      Reciprocity=reciprocity(LunaApril),
                      Transitivity=transitivity(LunaApril),
                      Centralization=centralization.degree(LunaApril)$centralization)

# add the new row to the data frame
LunaStats <- rbind(LunaStats, new_row)


#MayData
df_Luna<- read_csv(file="Luna_May_EdgeList.csv") #read first row as header
LunaMay <- graph_from_data_frame(df_Luna,directed=TRUE)

head(LunaMay)


new_row <- data.frame(Month = "May", Density=graph.density(LunaMay), 
                      Reciprocity=reciprocity(LunaMay),
                      Transitivity=transitivity(LunaMay),
                      Centralization=centralization.degree(LunaMay)$centralization)

# add the new row to the data frame
LunaStats <- rbind(LunaStats, new_row)


#june
df_Luna<- read_csv(file="Luna_june_EdgeList.csv") #read first row as header
LunaJune <- graph_from_data_frame(df_Luna,directed=TRUE)

head(LunaJune)


new_row <- data.frame(Month = "June", Density=graph.density(LunaJune), 
                      Reciprocity=reciprocity(LunaJune),
                      Transitivity=transitivity(LunaJune),
                      Centralization=centralization.degree(LunaJune)$centralization)

# add the new row to the data frame
LunaStats <- rbind(LunaStats, new_row)

#July

df_Luna<- read_csv(file="Luna_july_EdgeList.csv") #read first row as header
LunaJuly <- graph_from_data_frame(df_Luna,directed=TRUE)

head(LunaJuly)


new_row <- data.frame(Month = "July", Density=graph.density(LunaJuly), 
                      Reciprocity=reciprocity(LunaJuly),
                      Transitivity=transitivity(LunaJuly),
                      Centralization=centralization.degree(LunaJuly)$centralization)

# add the new row to the data frame
LunaStats <- rbind(LunaStats, new_row)


#AUGUST

df_Luna<- read_csv(file="Luna_August_EdgeList.csv") #read first row as header
LunaAugust <- graph_from_data_frame(df_Luna,directed=TRUE)

head(LunaAugust)


new_row <- data.frame(Month = "August", Density=graph.density(LunaAugust), 
                      Reciprocity=reciprocity(LunaAugust),
                      Transitivity=transitivity(LunaAugust),
                      Centralization=centralization.degree(LunaAugust)$centralization)

# add the new row to the data frame
LunaStats <- rbind(LunaStats, new_row)




########luna stats for may

setwd('D:/ERC20/Luna Edge List')
LunaStats=data.frame(Month=character(),Density=character(),Reciprocity=character(),
                     Transitivity=character(),Centralization=character(),stringsAsFactors = FALSE)


May_df2<- read.csv("Luna_May_EdgeList.csv") #read first row as header
May_df1<-May_df2[50637:231070,]
head(May_df1)
Maydf_g1<- graph_from_data_frame(May_df1,directed=TRUE)

head(Maydf_g1)


new_row <- data.frame(Month = "May(09-12)", Density=graph.density(Maydf_g1), 
                      Reciprocity=reciprocity(Maydf_g1),
                      Transitivity=transitivity(Maydf_g1),
                      Centralization=centralization.degree(Maydf_g1)$centralization)

LunaStats <- rbind(LunaStats, new_row)

LunaStats$Month[8]="May(13-31)"

LunaStats1<-LunaStats[-c(1:5),]

May_df1 <- df_Luna[day(df_Luna$timestamp) <= 08,]
head(May_df1)
Maydf_g1<- graph_from_data_frame(May_df1,directed=TRUE)

head(LunaApril)


new_row <- data.frame(Month = "May(01-08)", Density=graph.density(LunaApril), 
                      Reciprocity=reciprocity(LunaApril),
                      Transitivity=transitivity(LunaApril),
                      Centralization=centralization.degree(LunaApril)$centralization)
LunaStats <- rbind(LunaStats, new_row)




#########ERGM ANALYSIS
# Load required packages
library(ergm)
library(ergm.ego)
install.packages("ergm.ego", dependencies=TRUE)

library(statnet)

# Load social network data into R
LunaMay <- read.csv("Luna_May_EdgeList.csv")
LunaMay<-LunaMay[1:100,]

# Convert data to a network object
net <- network(LunaMay,directed=TRUE,loops = TRUE,multiple = TRUE )

outdeg <- degree(net)

# Add the nodal attribute to the network object
set.vertex.attribute(net, "outdegree", outdeg)
# Define ERGM model for crash analysis
model <- ergm(net ~nodematch("outdegree", "high",diff=TRUE,keep=1))
#+ mutual + nodematch("outdegree", "high",diff=TRUE,keep=1) + triangle
# Fit model to data
fit <- ergm(model)

# Examine summary statistics for model
summary(model)
update.packages("dplyr")
install.packages("egor", type = "source")

install.packages("dplyr")
# Visualize model results
plot(model)
