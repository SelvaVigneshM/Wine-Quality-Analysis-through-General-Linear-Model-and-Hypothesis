Data<-read.csv("C:/Users/Selva Vignesh M/Desktop/WINE/W.csv")
print(Data)
Qual<-c(rep("DENSITY",15),rep("ALCOHOL",15),rep("CITRIC.ACID",15),rep("pH",15), rep("TOTAL.SULPHUR.DIOXIDE ",15))
oth<-c(rep("FIXED.ACIDITY",15),rep("VOLATILE.ACIDITY",15),rep ("FREE.SULPHUR.DIOXIDE",15),rep("RESIDUAL.SUGAR",15),rep("CHLORIDES",15))
weight<-c(Data$DENSITY,Data$ALCOHOL,Data$CITRIC.ACID,Data$pH, Data$TOTAL.SULPHUR.DIOXIDE)
weight2<-c(Data$FIXED.ACIDITY,Data$VOLATILE.ACIDITY,Data$FREE.SULPHUR.DIOXIDE, Data$RESIDUAL.SUGAR,Data$CHLORIDES)
df<-data.frame(Qual,weight)
df1<-data.frame(Qual,weight,oth,weight2)
plot(weight ~ Qual,data = df,xlab="Primary Chemicals",ylab = "PC.weights",main="Box plot")
#ONE WAY ANOVA
Data.one<-oneway.test(weight ~ Qual,data=df)
print(Data.one)
print("INTERPRETATION OF ONE WAY ANOVA --- The p value(0.9864) is greater than 5% significance value , so there is no significant variation between weight and quality variables which affects the quality of wine")
#TWO WAY ANOVA
Data.two<-aov(weight ~ Qual+weight2, data=df1)
summary(Data.two)
print("INTERPRETATION OF TWO WAY ANOVA --- Here we have considered three variables(weight,weight2,Qual), where the pvalue(0.9820) is greater than 5% significance value So the Null hypothesis is rejectted.The variation in between the 3 variables are not satisfied thus resluting in a bitter wine")
#LINEAR REGRESSION
tr.lm = lm(QUALITY ~., data = Data)
summary(tr.lm)
#KRUSKAL WALLIS TEST
Data.k<-kruskal.test(weight ~ Qual,data=df)
print(Data.k)
print("KRUSKAL WALLIS TEST --- Without assuming the data to have normal distribution, test at 5% significance level if the Quality and weight variables have identical distributions. As p value is 0.9 the distribution between the variables are identical")
#MAXN-WHITNEY WILCOXON TEST
manwill<-wilcox.test(weight,weight2,data=df)
print(manwill)
print("MANWHITNEY WILCOXON TEST --- Without assuming the data to have normal distribution, test at 5% significance level if the Quality and weight variables have identical distributions. As p value is less than 0.5 the distribution between the variables are nonidentical")
#SPEARMAN'S RANK CORRELATION COEFFICIENT
s.corr <- cor.test(x=Data$QUALITY, y=Data$pH, method = 'spearman')
corr
print("SPEARMAN'S CORRELATION --- The value of p is 0.8464 which is greater than 5% significance level which infers that there is a strong relationship between the Quality and pH of the wine")
#WILCOXON SIGNED TEST, CONSIDERING THE VALUE OF NULL HYPOTHESIS IS 4
wilcox.test(x=weight, mu = 4, conf.int = TRUE)
wilcox.test(x=weight, mu = 4, conf.int = TRUE, alternative="less")
wilcox.test(x=weight, mu = 4, conf.int = TRUE, alternative="greater")
print("WILCOXON SIGNED RANK TEST --- This test infers that there is a significant difference in the median weight within the conditions of null hypothesis")
#PLOTS
print("The plots shows us the variations between the variables of wine data in a graphical way")
abline(h=0,col="blue")
plot(tr.lm)
#QUALITY PREDICTION
model_glm <- glm(category ~ . - QUALITY, data = Data, family=binomial)
model_gl<-step(model_glm)
Data$QUALITY2 <- as.factor(Data$QUALITY)
Data$category[Data$QUALITY <= 5] <- 0
Data$category[Data$QUALITY >= 5] <- 1
Data$category <- as.factor(Data$category)
Q_pred <- ifelse(predict(model_gl, type = "response") > 0.5,"Good Wine", "Bad Wine")
Q_tab <- table(predicted = Q_pred, actual = Data$category)
Q_tab
#BAR GRAPHS
hist(Data$QUALITY)
plot(Data$category)
plot(Q_tab)

