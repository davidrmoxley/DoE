library(readr)

# Get data
urlfile="https://raw.githubusercontent.com/davidrmoxley/DoE/master/b2b_sales.csv"
df <-read_csv(url(urlfile))


#Step 1: Code Variables
df$FreeShipping[df$FreeShipping=="No"] <- 0
df$FreeShipping[df$FreeShipping=="Yes"] <- 1
df$FreeShipping <- as.integer(df$FreeShipping)


#Step 2: Plot
plot(df$PriceIncrease,df$ChangeInSales,
     main="Change in Sales by Price Increase")
plot(df$FreeShipping,df$ChangeInSales,
     main="Change in Sales by ShippingCharge")
plot(df$FreeShipping*df$PriceIncrease,df$ChangeInSales,
     main="Change in Sales by ShippingCharge")


#Step 3: Fit Full Model
m_full <- aov(ChangeInSales ~ factor(PriceIncrease) + factor(FreeShipping) 
         + factor(PriceIncrease)*factor(FreeShipping) + factor(SalesPerson), data = df)
summary(m_full)


# fitted model
m_fitted <-aov(ChangeInSales ~ PriceIncrease + FreeShipping
             + PriceIncrease*FreeShipping + factor(SalesPerson), data = df)
summary(m_fitted)



#Step 4: Check Residuals
residuals <- resid(m_fitted)

# Homogeneity
plot(residuals, m_fitted$ChangeInSales, xlab="Predicted", ylab="Residuals",main="Residuals Plot")
abline(0,0, lwd=2)

# Normality
qqnorm(residuals, pch = 1, frame = FALSE)
qqline(residuals, col = "steelblue", lwd = 2)


#Step 5: Transform
library(MASS)
# Box Cox
b <- boxcox(df$ChangeInSales ~ df$PriceIncrease)
lambda <- b$x
log_likelihood <- b$y
box_cox_table <- cbind(lambda, log_likelihood)
sorted_bc <- box_cox_table[order(-log_likelihood),]
head(sorted_bc, n = 10)

# Check Lambda
df$BC <- df$Response^.5


#Step 6: Main Effects
aggregate(df$ChangeInSales ~ df$PriceIncrease, FUN = mean)
aggregate(df$ChangeInSales ~ df$FreeShipping, FUN = mean)


#Step 7: Interactions
interaction.plot(x.factor = df$PriceIncrease,
                 trace.factor = df$FreeShipping,
                 response = df$ChangeInSales,
                 fun = mean,
                 type="b",
                 col=c("black","red","green"),  ### Colors for levels of trace var.
                 pch=c(19, 17, 15),             ### Symbols for levels of trace var.
                 fixed=TRUE,                    ### Order by factor order in data
                 leg.bty = "o",
                 main = "Interaction of Price Increase and Free Shipping",
                 xlab = "Price Increase",
                 ylab = "Price Change")


