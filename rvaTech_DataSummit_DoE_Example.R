options(warn=-1)

library(readr)

## Get data
urlfile="https://raw.githubusercontent.com/davidrmoxley/DoE/master/b2b_sales.csv"
df <-read_csv(url(urlfile))

## Set Data Types
# Ensure Blocking Variables are factor data types
df <- transform(df
          , SalesPerson = as.factor(SalesPerson)# Ensure Blocking Variables are factor data types
          , FreeShipping = as.factor(FreeShipping))

## Check Data
head(df)
hist(df$PriceIncrease
     , main="Histogram of Price Increase", xlab="Price Increase")
aggregate(. ~ SalesPerson, data = df, FUN = length)
aggregate(. ~ FreeShipping, data = df, FUN = length)
length(df[is.na(df)==TRUE])
# Analysis: No indications of missing data/data entry issues

## Analyze
# Step 1: Code Variables
# Categorical Variables as Dummy Variables
df$FreeShipping_Cd[df$FreeShipping=="No"] <- 0
df$FreeShipping_Cd[df$FreeShipping=="Yes"] <- 1
df$FreeShipping_Cd <- as.integer(df$FreeShipping_Cd)

# Preferred Alternative Method: Level Coding
# Allows for comparison of Factors of different scales
df$FreeShipping_Cd[df$FreeShipping=="No"] <- -1 #Represent lower level of factor as -1
df$FreeShipping_Cd[df$FreeShipping=="Yes"] <- 1
df$FreeShipping_Cd <- as.integer(df$FreeShipping_Cd)
df$PriceIncrease_Cd[df$PriceIncrease==.05] <- -1 #Represent lower level of factor as -1
df$PriceIncrease_Cd[df$PriceIncrease==.1] <- 1
df$PriceIncrease_Cd <- as.integer(df$PriceIncrease_Cd)


#Step 2: Plot
with(df
     , plot(PriceIncrease,ChangeInSales
            ,main="Change in Sales by Price Increase",xlab="Price Increase",ylab="Change In Sales"))
     abline(lm(ChangeInSales~PriceIncrease,data=df),lwd=3,col="blue")
# Analysis: negative is consistent with what we'd expect to see
with(df, plot(FreeShipping,ChangeInSales
     ,main="Change in Sales by Shipping Charge",xlab="Free Shipping",ylab="Change In Sales"))
#### Analysis: generally positive response to free shipping is intuitive


#Step 3: Fit Full Model
m_full <- aov(ChangeInSales ~ factor(PriceIncrease_Cd) + factor(FreeShipping_Cd) 
         + factor(PriceIncrease_Cd)*factor(FreeShipping_Cd) + factor(SalesPerson), data = df)
summary(m_full)

# fitted model
m_fitted <-aov(ChangeInSales ~ PriceIncrease_Cd + FreeShipping_Cd
             + PriceIncrease_Cd*FreeShipping_Cd + factor(SalesPerson), data = df)
summary(m_fitted)


#Step 4: Check Residuals
# Method 1:
plot(m_fitted)

# Method 2:
residuals <- resid(m_fitted)
predicted <- predict(m_fitted)
# Homogeneity of 
plot(residuals, predicted, xlab="Predicted", ylab="Residuals",main="Variance of Residuals")
abline(0,0, lwd=2)
# Analysis: No discernable pattern to the data, assumption likely holds

# Normality of Error term
qqnorm(residuals, pch = 1, frame = FALSE)
qqline(residuals, col = "steelblue", lwd = 2)
print("Analysis: points roughly form a straight line suggesting residuals are normally distributed")


#Step 6: Main Effects
effect_price <- as.data.frame(aggregate(df$ChangeInSales ~ df$PriceIncrease, FUN = mean))
names(effect_price) <- c("PriceIncrease","ChangeInSales")
effect_price

effect_shipping <- as.data.frame(aggregate(df$ChangeInSales ~ df$FreeShipping, FUN = mean))
names(effect_shipping) <- c("FreeShipping","ChangeInSales")
effect_shipping


#Step 7: Interactions
#Analysis: 
interaction.plot(response=df$ChangeInSales, x.factor=df$PriceIncrease, trace.factor=df$FreeShipping,
                 , fun = mean
                 , type="b"
                 , col=c("red","blue"), pch=c(19, 17) # trace factor formating
                 , fixed=TRUE, leg.bty = "o"
                 , main = "Interaction of Price Increase and Free Shipping"
                 , xlab = "Price Increase"
                 , ylab = "Price Change"
                 , trace.label="Free Shipping"
          )


options(warn=0)

