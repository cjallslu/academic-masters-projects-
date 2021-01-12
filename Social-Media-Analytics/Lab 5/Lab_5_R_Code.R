#CJ All

getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- 'C:/Users/cjall/OneDrive/Desktop/Grad School/IDS 564/Lab Assignments/Lab 5'
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  #Read in the hs0 data over the internet using the read.table() function.

## Load package
library(igraph)
infile<-"Macrae_table5.5_mjbook.csv"
infile2<-"HamRadioOperators_Killworth.csv"
infile3<-"Coauthorship_GoyalEtAl.csv"

macrae_frame=read.csv(infile, header = TRUE, sep = ",")
macrae_frame$nop <-macrae_frame$Number.of.prisoners
HamRadio_frame=read.csv(infile2, header = TRUE, sep = ",")
HamRadio_frame$noO <-HamRadio_frame$Number.of.Operators
Coauthorship_frame=read.csv(infile3, header = TRUE, sep = ",")
Coauthorship_frame$noa <-Coauthorship_frame$Number.of.authors

View(macrae_frame)
View(HamRadio_frame)
View(Coauthorship_frame)

# This is the empirical cumulative distribution function; but it's not useful for this dataset
# It calculates F(d) for unaggregated data. But the current dataset is already aggregated; so you 
# should calculate F(d) using the cumulative sum instead
F_d<-ecdf(macrae_frame$nop)
F_d<-cumsum(macrae_frame$nop)

# Some useful functions, Suggested help look-ups to learn more:
help(cumsum)
help(lm)
help(coefficients) # Run after lm, to get the value of your Beta slope estimate. Then convert it to the alpha estimate.
help(log)

F_d_forplot=graph.data.frame(macrae_frame, directed = TRUE, vertices= NULL)
plot(F_d_forplot)
E(F_d_forplot)$weight <-1
E(F_d_forplot)$weight 
F_d_forplot_simpl<-simplify(F_d_forplot, edge.attr.comb="sum")
is.simple(F_d_forplot_simpl)
plot(F_d_forplot_simpl)

#2
# Degree centrality
#overall degree is 2tm; m is 1/2 of the avg degree 
degree_macrae <- degree(F_d_forplot_simpl)
degree_macrae
avg_deg <- 2 * mean(degree_macrae)
avg_deg #2.769231

#3
m <- 2.7164/2
alpha_0<-0.11

lm_mcrae <- lm(Degree ~ nop, data = macrae_frame)
lm_mcrae
((2/1-alpha_0)*log(m+((2*alpha_0*m)/(1-alpha_0)))) - ((2/1-alpha_0)*log(2.7164+((2*alpha_0*m)/(1-alpha_0))))
beta_1 <- log(2.7164 + 2*(alpha_0)*m)/(1 - alpha_0)
beta_1
#-1.112808; -1.096 was the closest # to this

#4
alpha_0<-0.10
((2/1-alpha_0)*log(m+((2*alpha_0*m)/(1-alpha_0)))) - ((2/1-alpha_0)*log(2.7164+((2*alpha_0*m)/(1-alpha_0))))
-2/(1+1.13589)
#-0.9363778; -0.896 was the closest # to this

#5
# Execute  the entire for loop code block together 
for(i in 1:9) {
  #print(i) 
  alpha_0<-i/10
  print("Alpha 0: ")
  print (alpha_0)
  alpha_1<- ((2/1-alpha_0)*log(m+((2*alpha_0*m)/(1-alpha_0)))) - ((2/1-alpha_0)*log(2.7164+((2*alpha_0*m)/(1-alpha_0))))
  print("Alpha 1: ")
  print(alpha_1)
  # For convenience, you can estimate a series of alpha_1 values within this for loop
}


# This it is also useful to calculate alpha_1 values for the following
alpha_0<-0.99
alpha_0<-0.999
alpha_0<-0.9999

library(rlist)
#6
F_d_forplot=graph.data.frame(macrae_frame, directed = TRUE, vertices= NULL)
plot(F_d_forplot)

alpha0.coa<-seq(0.2,0.9,0.1) 
alpha0.coa[9:10]<-c(0.99,0.999)
alpha0.list<-seq(0.1,0.19,0.01)
alpha0.list<-list.append(alpha0.list,alpha0.coa)
#coauthor$noa<-coauthor$Number.of.authors
#ecdf(coauthor$noa)
csum.ca<-cumsum(Coauthorship_frame$noa)
F_d.ca<-csum.ca/sum(Coauthorship_frame$noa)
col.sum<-colSums(Coauthorship_frame)
avg.degree_ca<-sum(Coauthorship_frame$Degree*Coauthorship_frame$noa)/col.sum[3]
m.ca<-0.5*avg.degree_ca
#compute x and y respectively
y.ca<-log(1-F_d.ca)
xlist<-list()
#compute x list for different alpha0
for (i in 1:length(alpha0.list)){
  x_1.ca<-(2*alpha0.list[i]*m.ca)/(1-alpha0.list[i])
  x_1.2_ca<-Coauthorship_frame$Degree + x_1.ca
  x.ca<-log(x_1.2_ca)
  xlist<-list.append(xlist,x.ca)
  
}
#eliminate inf/NA in x or y
y.ca[62]<-NA
#regress y on different x
alpha1.ca<-list()
for (i in 1:length(xlist)){
  model.ca<-lm(na.omit(y.ca)~xlist[[i]][1:61])
  beta.ca<-model.ca$coefficients[2]
  alpha_1<- 1+(2/beta.ca)
  alpha1.ca<-list.append(alpha1.ca,alpha_1)
  
}

#alpha1 list
unlist(alpha1.ca)
#visualize alpha1 and alpha0
plot(unlist(alpha1.ca)~alpha0.list,xlab='value of alpha0',ylab='value of alpha1')

#both points are the same at 0.4


#8
alpha0.radio<-seq(0.1,0.9,0.1)
avg.degree_ra<-sum(HamRadio_frame$Degree*HamRadio_frame$Number.of.Operators)/sum(HamRadio_frame$Number.of.Operators)
m.ra<-0.5*avg.degree_ra
F_d.ra<-cumsum(HamRadio_frame$Number.of.Operators)/sum(HamRadio_frame$Number.of.Operators)
y.ra<-log(1-F_d.ra)
#exclude inf
y.ra[28]<-NA

#compute x list for different alpha0
xlist_ra<-list()
for (i in 1:length(alpha0.radio)){
  x_1.ra<-(2*alpha0.radio[i]*m.ra)/(1-alpha0.radio[i])
  x_1.2_ra<-HamRadio_frame$Degree + x_1.ra
  x.ra<-log(x_1.2_ra)
  xlist_ra<-list.append(xlist_ra,x.ra)
  
}


#regress y on x for different alpha0
alpha1.ra<-list()
for (i in 1:length(xlist_ra)){
  model.ra<-lm(na.omit(y.ra)~xlist_ra[[i]][1:27])
  beta.ra<-model.ra$coefficients[2]
  alpha_1<- 1+(2/beta.ra)
  alpha1.ra<-list.append(alpha1.ra,alpha_1)
  
}


unlist(alpha1.ra)

