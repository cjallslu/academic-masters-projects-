# LIBRARIES AND DATA CLEANING ####
rm(list = ls())
library(tidyverse)
library(lubridate)
library(rpart)
library(caret)
library(ROCR)
library(dplyr)
library(rpart.plot)
library(ggplot2)
library(factoextra)
library(dbscan)
library(kernlab)
library(factoextra)
library(dbscan)
library(cluster)
library(gridExtra)



# Load the Data
bsData <- read_csv('DM_Sheet.csv')
#bsData<- read_csv('/Users/vanessarodriguez/Desktop/IDS 572/hw 3/DM_Sheet.csv')


# better to change the colNames which contain punctuation, space 
names(bsData) <- gsub("[[:punct:]]|\\s", "_", names(bsData)) # replaces punctuation and white spaces with ‘_’


# Remove rows with 0 for Affluence Index, these households do not have buying habits or income that will be useful for advertising purposes
bsData <-bsData[!(bsData$`Affluence_Index`==0),]

# Adjust NA values
#colMeans(is.na(lcdf))[colMeans(is.na(lcdf))>0]
bsData <- bsData %>% replace_na(list(FEH = 0, MT = 0, EDU = 0, CHILD = 5, CS = 2))

#The data with '%' in values are read in as 'chr' type - change these to numeric
bsData[20:46]<-lapply(bsData[20:46], function(x) as.numeric(sub("%", "e-2", x)))

# DATA EXPLORATION ####

# Create Brand Loyalty Variables - maxBr represents the max percentage a household bought from one of the 8 major brands versus everything else
bsd <- bsData
bsd <- bsd %>% rowwise() %>% mutate(maxBr=max(Br__Cd__57__144, Br__Cd__55, Br__Cd__272, Br__Cd__286, Br__Cd__24, Br__Cd__481, Br__Cd__352, Br__Cd__5))
bsd$Brand_Jumping = as.numeric(format(round((bsd$No__of_Brands / bsd$Brand_Runs), 2), nsmall = 2))
bsd$ProDifBrands = as.numeric(format(round((bsd$No__of_Brands / bsd$No__of__Trans), 2), nsmall = 2))

exp <- bsd

# Consolidate Demographics for Native Tongue, Education, Household Size
exp<- exp %>% mutate(MT=if_else(MT %in% c(4, 5, 10, 17), MT, 20)) # use 20 as the classifier of 'Other'
exp<- exp %>% mutate(EDU=if_else(EDU %in% c(1,2,3,4,5), EDU, 6)) # For Education first consolidate College or Greater
exp<- exp %>% mutate(EDU=if_else(EDU %in% c(3,4,5,6), EDU, 2)) # Then combine Illiterate / no formal schooling into one
exp<- exp %>% mutate(HS=if_else(HS %in% c(1,2,3,4,5,6), HS, 7)) # For Household size combine the sizes greater than 6
exp<- exp %>% mutate(HS=if_else(HS %in% c(3,4,5,6,7), HS, 2)) # For Household size combine the sizes less than 3

# Examine the demographics
summary(as.factor(exp$SEC)) # Socio Economic Class (1=high, 4 = low)
summary(as.factor(exp$FEH)) # Food Eating habits 1-Vegetarian, 2- Veg but Eggs, 3 - Non-Veg
summary(as.factor(exp$MT)) # Native Language (Mother Tongue) Refer to Excel
summary(as.factor(exp$SEX)) # Sex of Homemaker 1-Male, 2-Female
summary(as.factor(exp$AGE)) # Age of Homemaker 1-Up to 24, 2- 25 to 34, 3- 35 to 44, 4- 45+
summary(as.factor(exp$EDU)) # Education - Refer to Excel Sheet
summary(as.factor(exp$HS)) # Number of people in the Household
summary(as.factor(exp$CHILD)) # Ages of children in household - 1- Up to 6, 2- 7 to 14, 3 Both, 4 None, 5 Not Specified
summary(as.factor(exp$CS)) # Television - 1 Cable or Broadcast TV Available, 2 Unavailable 

# Convert demographics into character variables for graph manipulation
exp$SEC <- as.character(exp$SEC)
exp$FEH <- as.character(exp$FEH)
exp$MT <- as.character(exp$MT)
exp$SEX <- as.character(exp$SEX)
exp$AGE <- as.character(exp$AGE)
exp$EDU <- as.character(exp$EDU)
exp$HS <- as.character(exp$HS)
exp$CHILD <- as.character(exp$CHILD)
exp$CS <- as.character(exp$CS)

# Plots 
ggplot(exp, aes(SEC))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Economic Class")  +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Low", "Med-Low", "Med-High", "High"))

ggplot(exp, aes(FEH))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Eating Habits") +
  scale_x_discrete(breaks=c("1","2","3"),
                   labels=c("Veg", "Veg w/Egg", "Non-Veg"))

ggplot(exp, aes(MT))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Native Language (Mother Tongue)")  +
  scale_x_discrete(breaks=c("4","5","10","17","20"),
                   labels=c("Gujarati", "Hindi", "Marathi", "Urdu", "Other"))

ggplot(exp, aes(SEX))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Gender of Homemaker") +
  scale_x_discrete(breaks=c("1","2"),
                   labels=c("Male", "Female"))

ggplot(exp, aes(AGE))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Age") +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Up to 24", "25-34", "35-44","45+"))


ggplot(exp, aes(EDU))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Education")   +
  scale_x_discrete(breaks=c("2","3","4","5","6"),
                   labels=c("No Schooling", "Up to 4 Yrs", "5-9 Yrs", "10-12 Yrs", "College or Grtr"))

ggplot(exp, aes(HS))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Size of Household") +
  scale_x_discrete(breaks=c("2","3","4","5","6","7"),
                   labels=c("<3", "3", "4","5","6",">6"))

ggplot(exp, aes(CHILD))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("# of Children By Age Group") +
  scale_x_discrete(breaks=c("1","2","3","4"),
                   labels=c("Up to 6", "7-14", "Both","None"))

ggplot(exp, aes(CS))+
  geom_bar(fill = "#0073C2FF") + 
  ggtitle("Television Acess") +
  scale_x_discrete(breaks=c("1","2"),
                   labels=c("Cable or Broadband", "Unavailable"))

# K-Means Clustering ####
# Purchase Behavior Clustering - #brands, brand runs, total volume, #transactions, value, avg. price, share to other brands, (brand loyalty)]
PURCHASE_BEHAVIOR <- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 'Trans___Brand_Runs', 'Vol_Tran',
                       'Avg__Price', 'maxBr', 'Others_999', 'Brand_Jumping', 'ProDifBrands')

# Create a new dataset for kmeans clustering purposes
x<- bsd
# K-Means Cluster the purchase behavior variables 
kmClus_pb <- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
#Or create a scaled dataset for clustering, and use this 
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale()
kmClus_pb
# Visualization 
# The entire purchase behavior variable group
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

# For specific variables 
fviz_cluster(kmClus_pb, data=x %>% select(maxBr, Total_Volume))

# Cluster Description
#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables - to interpret the clusters
x$cluster = kmClus_pb$cluster 
x %>% group_by(cluster) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs', 'Brand_Jumping', 'ProDifBrands'), mean, ) %>% view()
# Determine optimal number of clusters - which method should be used? 
# Within-Sum-of-Squares
fviz_nbclust(xpb, kmeans, method = "wss")

# Silhoutte Method
fviz_nbclust(xpb, kmeans, method = "silhouette")

#k=2 

# K-Means Cluster the purchase behavior variables 
#kmClus_pb <- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb <- kmeans(xpb, centers=2, nstart=25) 
#Or create a scaled dataset for clustering, and use this 
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale()

kmClus_pb

# Visualization 
# The entire purchase behavior variable group
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))

# For specific variables 
fviz_cluster(kmClus_pb, data=x %>% select(maxBr, Total_Volume))

# Cluster Description
#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables - to interpret the clusters
x$cluster = kmClus_pb$cluster 
x %>% group_by(cluster) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs', 'Brand_Jumping', 'ProDifBrands'), mean, ) %>% view()
# Determine optimal number of clusters - which method should be used? 
# Within-Sum-of-Squares
fviz_nbclust(xpb, kmeans, method = "wss")

# Silhoutte Method
fviz_nbclust(xpb, kmeans, method = "silhouette")

# k4
# K-Means Cluster the purchase behavior variables 
#kmClus_pb <- x %>% select(PURCHASE_BEHAVIOR) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_pb <- kmeans(xpb, centers=4, nstart=25) 
#Or create a scaled dataset for clustering, and use this 
xpb<-x %>% select(PURCHASE_BEHAVIOR) %>% scale()
kmClus_pb
# Visualization 
# The entire purchase behavior variable group
fviz_cluster(kmClus_pb, data=x %>% select(PURCHASE_BEHAVIOR))
# For specific variables 
fviz_cluster(kmClus_pb, data=x %>% select(maxBr, Total_Volume))
# Cluster Description
#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables - to interpret the clusters
x$cluster = kmClus_pb$cluster 
x %>% group_by(cluster) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE',
                                           'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs', 'Brand_Jumping', 'ProDifBrands'), mean, ) %>% view()

# Determine optimal number of clusters - which method should be used? 
# Within-Sum-of-Squares
fviz_nbclust(xpb, kmeans, method = "wss")
# Silhoutte Method
fviz_nbclust(xpb, kmeans, method = "silhouette")



# Clusteting for Basis for puchase
# Basis-for-Purchase Clustering - purchase by promotions, price categories, selling propositions]
#k=2
Basis_For_Purchase <- c('PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8','PropCat_9','PropCat_10','PropCat_11', 'PropCat_12', 'PropCat_13', 'PropCat_14', 'PropCat_15', 
                        'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',
                        'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4')

x<-bsd
kmClus_bfp<- x %>% select(Basis_For_Purchase) %>% scale() %>% kmeans(centers=2, nstart=25)
kmClus_bfp
xbfp<-x %>% select(Basis_For_Purchase) %>% scale() #or create a scladx dataset for clusting

#visualize the cluster
fviz_cluster(kmClus_bfp, data=x %>% select(Basis_For_Purchase))


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
#x <- x %>% mutate(clusKM=kmClus_bfp$cluster)
x$clusKM = kmClus_bfp$cluster

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

# How many clusters is best
fviz_nbclust(xbfp, kmeans, method = "wss") # looks at the total within cluster sum of square as a function of the number of cluster

fviz_nbclust(xbfp, kmeans, method = "silhouette")



#k=3
x<-bsd
kmClus_bfp<- x %>% select(Basis_For_Purchase) %>% scale() %>% kmeans(centers=3, nstart=25)
kmClus_bfp
xbfp<-x %>% select(Basis_For_Purchase) %>% scale() #or create a scale dataset for clustering

#visualize the cluster
fviz_cluster(kmClus_bfp, data=x %>% select(Basis_For_Purchase))


#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
#x <- x %>% mutate(clusKM=kmClus_bfp$cluster)
x$clusKM = kmClus_bfp$cluster

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

# How many clusters is best
fviz_nbclust(xbfp, kmeans, method = "wss") # looks at the total within cluster sum of square as a function of the number of cluster

fviz_nbclust(xbfp, kmeans, method = "silhouette")



#k=4
x<-bsd
kmClus_bfp<- x %>% select(Basis_For_Purchase) %>% scale() %>% kmeans(centers=4, nstart=25)
kmClus_bfp
xbfp<-x %>% select(Basis_For_Purchase) %>% scale() #or create a scale dataset for clustering

#visualize the cluster
fviz_cluster(kmClus_bfp, data=x %>% select(Basis_For_Purchase))

#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
#x <- x %>% mutate(clusKM=kmClus_bfp$cluster)
x$clusKM = kmClus_bfp$cluster

x %>% group_by(clusKM) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()

# How many clusters is best
fviz_nbclust(xbfp, kmeans, method = "wss") # looks at the total within cluster sum of square as a function of the number of cluster

fviz_nbclust(xbfp, kmeans, method = "silhouette")



#Clustering on purchase behavior and basis for purchase variables
#k=2

PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE<- c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 
                                         #'Trans___Brand_Runs', 'Vol_Tran',
                                         
                                         'Avg__Price', 'maxBr', 'Others_999', 'Brand_Jumping', 'ProDifBrands','PropCat_5', 'PropCat_6', 'PropCat_7',
                                         'PropCat_8','PropCat_9','PropCat_10','PropCat_11', 'PropCat_12', 'PropCat_13', 'PropCat_14', 'PropCat_15', 
                                         'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',
                                         'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4')
x<- bsd

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=2, nstart=25)
kmClus_pbbp
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE) %>% scale() 
#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
#x <- x %>% mutate(clusKM=kmClus_bfp$cluster)
x$kmClus_pbbp= kmClus_pbbp$cluster
x %>% group_by(kmClus_pbbp) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()
# How many clusters is best
fviz_nbclust(xpbbp, kmeans, method = "wss") # looks at the total within cluster sum of square as a function of the number of cluster
fviz_nbclust(xpbbp, kmeans, method = "silhouette")

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE))


#k=3

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=3, nstart=25)
kmClus_pbbp
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE) %>% scale() 
#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
#x <- x %>% mutate(clusKM=kmClus_bfp$cluster)
x$kmClus_pbbp= kmClus_pbbp$cluster
x %>% group_by(kmClus_pbbp) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()
# How many clusters is best
fviz_nbclust(xpbbp, kmeans, method = "wss") # looks at the total within cluster sum of square as a function of the number of cluster

fviz_nbclust(xpbbp, kmeans, method = "silhouette")

#kmClus_pbbp

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE))

#k=4

kmClus_pbbp<- x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE) %>% scale() %>%kmeans(centers=4, nstart=25)
kmClus_pbbp
#nstart = if centers is a number, how many random sets should be chosen? Can also change the iterations
#Or create a scaled dataset for clustering, and use this
xpbbp<-x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE) %>% scale() 
#add the cluster variable to the data and check the cluster descriptions in terms of broader set of variables
#x <- x %>% mutate(clusKM=kmClus_bfp$cluster)
x$kmClus_pbbp= kmClus_pbbp$cluster
x %>% group_by(kmClus_pbbp) %>% summarise_at(c('SEC', 'HS', 'SEX', 'EDU', 'Affluence_Index','AGE', 'CHILD', 'maxBr', 'No__of_Brands', 'No__of__Trans', 'Brand_Runs', 'Total_Volume', 'Value', 'Trans___Brand_Runs'), mean) %>% View()
# How many clusters is best
fviz_nbclust(xpbbp, kmeans, method = "wss") # looks at the total within cluster sum of square as a function of the number of cluster
fviz_nbclust(xpbbp, kmeans, method = "silhouette")

#visualize the cluster - based on variables used for clustering
fviz_cluster(kmClus_pbbp, data=x %>% select(PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASE))


# pam partioning  around mediods ####
#purchase behavior
pam_pb<-pam(xpb,k=2,metric="euclidean")
pam_pb
pam_pb$clusinfo
fviz_cluster(pam_pb)

si<-silhouette(pam_pb)
summary(si)
plot(si,col=1:2,border=NA)
plot(x=xpb,k=2,metric="euclidean")
# ARE THE RESULTS FROM PAM AND KMEANS SIMILAR? DO THEY CLUSTER THE EXAMPLES SIMILAR?
table(kmClus_pb$cluster,pam_pb$clustering)


pam_pb<-pam(xpb,k=3,metric="euclidean")
pam_pb
pam_pb$clusinfo



si<-silhouette(pam_pb)
summary(si)
plot(si,col=1:2,border=NA)
plot(x=xpb,k=2,metric="euclidean")
table(kmClus_pb$cluster,pam_pb$clustering)
pam_pb<-pam(xpb,k=3,metric="euclidean")
pam_pb
pam_pb$clusinfo

si<-silhouette(pam_pb)
summary(si)
plot(si,col=1:3,border=NA)
plot(x=xpb,k=3,metric="euclidean")
table(kmClus_pb$cluster,pam_pb$clustering)

#db scan

# DBSCAN Clustering #### 

# Purchase Behavior Clustering - #brands, brand runs, total volume, #transactions, value, avg. price, share to other brands, (brand loyalty)]
#Perform a DBscan Clustering
pbDbscan <- dbscan(xpb[,1:12], eps = 0.75, minPts = 5)
# Visualization 
fviz_cluster(pbDbscan, data=xpb[,1:12], geom="point", ellipse = FALSE, main="dbscan eps=0.75, minPts=5")

# Find best eps - knee point at curve is the best
kNNdistplot(xpb[,1:2], k=4)

# Basis of Purchase Clustering


# Combined Clustering

# Kernal K-Means Clustering ####
# Purchase Behavior Clustering
kkc_pb<-kkmeans(xpb,centers=2)

fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Cluster Plot") 
kkc_pb

kkc_pb<-kkmeans(xpb,centers=3)

fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Cluster Plot") 
kkc_pb

kkc_pb<-kkmeans(xpb,centers=4)

fviz_cluster(list(data=xpb, cluster=kkc_pb@.Data), geom = "point", main="Cluster Plot") 
kkc_pb

# Basis of Purchase Clustering
kkc_bfp<-kkmeans(xbfp,centers=2)

fviz_cluster(list(data=xbfp, cluster=kkc_bfp@.Data), geom = "point", main="Cluster Plot") 
kkc_bfp

kkc_bfp<-kkmeans(xbfp,centers=3)

fviz_cluster(list(data=xbfp, cluster=kkc_bfp@.Data), geom = "point", main="Cluster Plot") 
kkc_bfp

kkc_bfp<-kkmeans(xbfp,centers=4)

fviz_cluster(list(data=xbfp, cluster=kkc_bfp@.Data), geom = "point", main="Cluster Plot") 
kkc_bfp

# Combined Clustering
kkc_pbbp<-kkmeans(xpbbp,centers=2)

fviz_cluster(list(data=xpbbp, cluster=kkc_pbbp@.Data), geom = "point", main="Cluster Plot") 
kkc_pbbp

kkc_pbbp<-kkmeans(xpbbp,centers=3)

fviz_cluster(list(data=xpbbp, cluster=kkc_pbbp@.Data), geom = "point", main="Cluster Plot") 
kkc_pbbp

kkc_pbbp<-kkmeans(xpbbp,centers=4)

fviz_cluster(list(data=xpbbp, cluster=kkc_pbbp@.Data), geom = "point", main="Cluster Plot") 
kkc_pbbp

PURCHASE_BEHAVIORdt <- subset(bsd, select = c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 'Trans___Brand_Runs', 'Vol_Tran',
                       'Avg__Price', 'maxBr', 'Others_999', 'Brand_Jumping', 'ProDifBrands'))

Basis_For_Purchasedt <- subset(bsd, select = c('PropCat_5', 'PropCat_6', 'PropCat_7', 'PropCat_8','PropCat_9','PropCat_10','PropCat_11', 'PropCat_12', 'PropCat_13', 'PropCat_14', 'PropCat_15', 
                        'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',
                        'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4'))

PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASEdt<- subset(bsd, select = c('No__of_Brands', 'Brand_Runs', 'Total_Volume', 'No__of__Trans', 'Value', 
                                         #'Trans___Brand_Runs', 'Vol_Tran',
                                         
                                         'Avg__Price', 'maxBr', 'Others_999', 'Brand_Jumping', 'ProDifBrands','PropCat_5', 'PropCat_6', 'PropCat_7',
                                         'PropCat_8','PropCat_9','PropCat_10','PropCat_11', 'PropCat_12', 'PropCat_13', 'PropCat_14', 'PropCat_15', 
                                         'Pur_Vol_No_Promo____', 'Pur_Vol_Promo_6__', 'Pur_Vol_Other_Promo__',
                                         'Pr_Cat_1', 'Pr_Cat_2', 'Pr_Cat_3', 'Pr_Cat_4'))
dfnew <- subset(bsd, select = c(SEX, EDU, SEC, CHILD, HS, AGE, Total_Volume))

dt1=rpart(Total_Volume ~ ., data= dfnew, control = rpart.control(minsplit = 30), na.action = na.omit)
dt1$variable.importance

# plotting the tree
rpart.plot::prp(dt1, type=2, extra=100)

dt2=rpart(Total_Volume ~ ., data= PURCHASE_BEHAVIORdt, control = rpart.control(minsplit = 30), na.action = na.omit)
dt2$variable.importance
rpart.plot::prp(dt2, type=2, extra=100)

dt3=rpart(Pur_Vol_No_Promo____ ~ ., data= Basis_For_Purchasedt, control = rpart.control(minsplit = 30), na.action = na.omit)
dt3$variable.importance
rpart.plot::prp(dt3, type=2, extra=100)

dt35=rpart(Pur_Vol_Promo_6__ ~ ., data= Basis_For_Purchasedt, control = rpart.control(minsplit = 30), na.action = na.omit)
dt35$variable.importance
rpart.plot::prp(dt3, type=2, extra=100)

dt4=rpart(Total_Volume ~ ., data= PURCHASE_BEHAVIOR_BASIS_FOR_PURCHASEdt, control = rpart.control(minsplit = 30), na.action = na.omit)
dt4$variable.importance
rpart.plot::prp(dt4, type=2, extra=100)

# CLEAN UP ####
rm(list = ls()) 

# Clear packages
p_unload(all)  # Remove all add-ons
detach("package:datasets", unload = TRUE)  # For base

# Clear console
cat("\014")  # ctrl+L

