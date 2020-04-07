library(glmnet)
library('MASS')

# Reading the data
data=read.csv("Cars_Data.csv", header=T)
y=data[,17]
x <- as.matrix(data[,2:16])

# Creating the corelation matrix 
x_cor <- cor(x)

# Performing eigen decomposition
out = eigen(x_cor)
va = out$values		# eigenvalues
ve = out$vectors	# eigenvectors. Each column is an eigenvector and has a unit length. 	

# Scree plot for the principal compotants
plot(va, type = "o", col = "blue")

# Calculating and flipping weights of first two principal componants
w1 = ve[,1]	* -1
w2 = ve[,2]	* -1

# Check each eignvector has unit length
chk = t(w1) %*% w1

# First two principal componants 
z1 = x %*% w1
z2 = x %*% w2

# Linear Regression 
z <- cbind(z1, z2)
linreg_pcr=lm(y~z)							
summary(linreg_pcr)

# Ideal Vector 									
b1_pcr =as.vector(coef(linreg_pcr)[2])
b2_pcr =as.vector(coef(linreg_pcr)[3])

slope.iso.preference_pcr = - b1_pcr/b2_pcr				
slope.ideal.vector_pcr = b2_pcr/b1_pcr						

angle.iso.preference_pcr = atan(slope.iso.preference_pcr)*180/pi	
angle.ideal.vector_pcr = atan(slope.ideal.vector_pcr)*180/pi

# Creating dataframe
labels <- data$Brands
label_no <- c(1,2,3,4,5,6,7,8,9,10)
z_data = data.frame(as.matrix(cbind(z1,z2)))
z_data <- cbind(labels, label_no, z_data)

# Building positioning map
plot(z2~z1, xlab = 'Premium or Luxury', 
     ylab = 'Spacious but basic', main = 'Positioning Map', data = z_data)
with(text(z2~z1, labels = z_data$labels, pos = 1, font=2))
abline(2, slope.iso.preference_pcr)





