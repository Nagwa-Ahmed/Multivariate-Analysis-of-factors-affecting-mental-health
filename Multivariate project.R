"""install.packages('arsenal')
install.packages('vtable')
install.packages('formattable')
install.packages('sjPlot')
install.packages('htmlTable')
install.packages("devtools")
install.packages('RVAideMemoire')"""

library('arsenal')
library('gtsummary')


setwd('D:/CV Nagwa/Level 4 term 1/Multivariate analysis/Multivariate project')
library('readxl')
mydata1=read_excel('DB-missing droped.xlsx')
mydata=data.frame(mydata1)

#####Constructing summary statistics table

library('vtable')
htmlTable(sumtable(mydata[,-c(1,10,12,14,39,40,41)],title='Descriptive statistics'),
          header = c('Variable','N','Mean','Std.Dev','Min','Pctl. 25','Pctl. 75	'
                     ,'Max'))
sumtable(mydata[,c(2,3,4,5,6,7,8,9,11,13,15,16,17,18,19,20,
                   21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)]
         ,title='Descriptive statistics')
sumtable(mydata[,c(21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)],title='Descriptive statistics part_2')
library('formattable')

########Creating dummies for Gender and Student
mydata$Female=ifelse(mydata$Gender=='Female',c(1),c(0)) 
mydata$Student_binary=ifelse(mydata$Student=='Student',c(1),c(0))

######Performing factor analysis
mydata2=mydata[,c(15,16,17,18,19,20,21,22,23,24,25,26,27,28
                  ,29,30,31,32,33,34,35,36,37,38)]
library(psych)

pp=principal(mydata2)$values
princomp(mydata2)
plot(principal(mydata2)$values,type='b',xlab = "eignevalue's order"
     , ylab = 'eignevalue', pch = 16,lwd=2)
principal(mydata2)$values
plot(principal(mydata2)$values, type = "b",xlab ="eigenvalue'sorder",ylab="eigenvalue",main="scree plot")
princomp(mydata2)
summary(princomp(mydata2))


data_factors=fa(mydata2,nfactor=5, rotate = 'none',
                fm='pa',max.iter = 1)
data_factors
summary_factors=fa_table(data_factors,small=0.59,diffuse =0,sort = FALSE)
summary_factors$ind_table
summary_factors$f_table

correlation=cor(faScore)
upper=correlation
upper[upper.tri(correlation)]<-""
upper<-as.data.frame(correlation)
upper






data_factors2=principal(mydata2,nfactors=5 ,rotate = 'none')
data_factors2

data_factors_rotated2=fa(mydata2,nfactor=4, rotate = 'varimax',
                        fm='pa',max.iter = 1)
data_factors_rotated2
summary_factors2=fa_table(data_factors_rotated2,small=0.59,diffuse =0,sort = FALSE)

summary_factors2$ind_table
summary_factors2$f_table

co=factor.scores(mydata2,data_factors_rotated2)
tc=table(co$r.scores)
htmlTable(tc,header=c('1'))
data_factors_rotated3=fa(mydata2,nfactor=5, rotate = 'oblimin',
                        fm='pa',max.iter = 1)

summary_factors3=fa_table(data_factors_rotated3,small=0.59,diffuse =0,sort = FALSE)

summary_factors3$ind_table
summary_factors3$f_table


#########********Applying cluster analysis*******


set.seed(123)
k2=kmeans(mydata[,c(2,3,4,5)],centers = 2, nstart = 25)
mydata$cluster2=k2$cluster
k3cen=k2$centers
gm1=colMeans(group1_1)
gm2=colMeans(group2_1)

tk=table(gm1)
tk2=round(prop.table(tk,margin = 1),3)
htmlTable(tk2,header = c('1'))


set.seed(35)
k3=kmeans(mydata[,c(2,3,4,5)],centers = 3, nstart = 25)
mydata$cluster3=k3$cluster

#####Validating by discriminant analysis

##Clusters=2
#first : test normality

library('MVN')
mvn(data = mydata[,c(2,3,4,5)],mvnTest = 'mardia')


group1_1=mydata[mydata$cluster2==1,c(2,3,4,5)]
group2_1=mydata[mydata$cluster2==2,c(2,3,4,5)]

mvn(data = group1_1[,c(1,2,3,4)],mvnTest = 'mardia')
mvn(data = group2_1[,c(1,2,3,4)],mvnTest = 'mardia')


#test equality of covariance matrices
library('MVTests')
M2=BoxM(mydata[,c(2,3,4,5)] ,mydata[,42])
summary(M2)

##Testing equality of mean vector
library('Hotelling')
HT2=hotelling.test(group1_1[,c(1,2,3,4)],group2_1[,c(1,2,3,4)],var.equal = FALSE)
HT2




#discriminant
library('MASS')
library('htmlTable')
lda2=lda(cluster2~Openness+Restraint+Transcendence+Interpersonal ,mydata)
predict2=predict(lda2)
t2=table(mydata$cluster2,predict2$class)
t22=round(prop.table(t2,margin = 1)*100,1)
htmlTable(t22,header = c( 'Group 1','Group 2'),rowlabel = 'True group',
          cgroup='Classified group')

library('sjPlot')

#getting non standardized coefficients
library('dplyr')

n1=nrow(group1_1)
n2=nrow(group2_1)

group1_1means=apply(group1_1,2, mean)
group2_1means=apply(group2_1,2, mean)

w1 = (n1 - 1) * var(group1_1)
w2 = (n2 - 1) * var(group2_1)

sp1 = 1 / (n1 + n2 - 2) * (w1 + w2)

a = solve(sp1) %*% (group1_1means - group2_1means)
a
constant=-(a[1]*mean(mydata$Openness)+a[2]*mean(mydata$Restraint)+a[3]*mean(
  mydata$Transcendence+mydata$Interpersonal))
NonStandardized=c(a,constant)
NonStandardized


##clusters=3

#Testing normality

group1_3=mydata[mydata$cluster3==1,c(2,3,4,5)]
group2_3=mydata[mydata$cluster3==2,c(2,3,4,5)]
group3_3=mydata[mydata$cluster3==3,c(2,3,4,5)]


mvn(data = group1_3[,c(1,2,3,4)],mvnTest = 'mardia')
mvn(data = group2_3[,c(1,2,3,4)],mvnTest = 'mardia')
mvn(data = group3_3[,c(1,2,3,4)],mvnTest = 'mardia')

#Testing equality of covariance matrices
M3=BoxM(mydata[,c(2,3,4,5)],mydata[,43])
summary(M3)

#Testing equality of 3 means
library('car')
mlm=lm(cbind(Openness,Restraint,Transcendence,Interpersonal)~ cluster3,
       data=mydata)
Anova(mlm)

lda3=lda(cluster3~Openness+Restraint+Transcendence+Interpersonal ,mydata,
         prior=c(1/3,1/3,1/3))
predictl3=predict(lda3)
t3=table(mydata$cluster3,predictl3$class)
addmargins(t3)

library('htmlTable')


t33=round(prop.table(t3,margin = 1),3)
htmlTable(t33,header = c( 'Group 1','Group 2','Group 3'),rowlabel = 'True group',
          cgroup='Classified group')


########Q-Q plots
library('RVAideMemoire')
mqqnorm(group1_1, main = "Multi-normal Q-Q Plot for group1 in cluster=2")
"there are some deviations from the Q-Q plot but they are acceptable (within
acceptable range), variables in Group1 approximately follows multivariate normal"
mqqnorm(group2_1, main = "Multi-normal Q-Q Plot for group2 in cluster=2")
"there are deviations from the Q-Q plot so we have some doubts about multivariate
noramlity assumption in Group2"
mqqnorm(group1_3, main = "Multi-normal Q-Q Plot for group1 in cluster=3")
mqqnorm(group2_1, main = "Multi-normal Q-Q Plot for group2 in cluster=3")
mqqnorm(group3_3, main = "Multi-normal Q-Q Plot for group3 in cluster=3")





#######*****Multivariate regression******

mlm2=lm(cbind(DASS_21,GHQ_12,SEC)~Age+Female+Work+Student_binary+
          Sons+Openness+Restraint+Transcendence+Interpersonal, data=mydata)
summary(mlm2)
library('devtools')
library('sjPlot')
table(mlm2$coefficients)
tab_model(mlm2$coefficients)
lm1=lm(DASS_21~Age+Female+Work+Student_binary+
         Sons+Openness+Restraint+Transcendence+Interpersonal, data=mydata)
tab_model(lm1)

plot(rstandard(lm1)~fitted(lm1), xlab = 'fitted values for DASS_21', ylab = 'standardized residuals')
abline(h=0,lty=2)

hist(rstandard(lm1) , xlab= 'standardized residulas')
x1=rstandard(lm1)
shapiro.test(x1)

lm2=lm(GHQ_12~Age+Female+Work+Student_binary+
         Sons+Openness+Restraint+Transcendence+Interpersonal, data=mydata)
tab_model(lm2)
plot(rstandard(lm2)~fitted(lm2), xlab = 'fitted values for GHQ_12', ylab = 'standardized residuals')
abline(h=0,lty=2)

x2=rstandard(lm2)
shapiro.test(x2)

lm3=lm(SEC~Age+Female+Work+Student_binary+
         Sons+Openness+Restraint+Transcendence+Interpersonal, data=mydata)
tab_model(lm3,show.fstat=TRUE)
plot(rstandard(lm3)~fitted(lm3), xlab = 'fitted values for SEC', ylab = 'standardized residuals')
abline(h=0,lty=2)

x3=rstandard(lm3)
shapiro.test(x3)

tab_model(lm1,lm2,lm3,show.fstat=TRUE,p.threshold=c(0.05),collapse.ci = TRUE)

anova(mlm2)
table(coef(mlm2))
library('knitr')
options(knitr.kable.NA = '')
t=kable(anova(mlm2), digits = 3, format = "html", caption = "ANOVA table")


htmlTable(t,header = c( 'ANOVA table'))
          
