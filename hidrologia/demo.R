x <- scan('ejemplo.txt')
means <- rep(0, 12)
sds <- rep(0,  12)

#
# calculo de la media y la desviacion
# 
for(mes in 1:12) {
	index = seq(mes, length(x), 12)
	means[mes] = mean(x[index])
	sds[mes] = sd(x[index])
}

#
# normalizacion
#
z = rep(0, length(x))
for(mes in 1:12) {
	index = seq(mes, length(x), 12)
	z[index] = (x[index] - means[mes]) / sds[mes]
}

plot(z, type = 'o')


y = c(z, rep(0, 50))

#
# generacion de la serie sintetica
#
for(t in 240:289) {
	
	y[t] = sqrt(0.6496) * rnorm(1) - 0.001752 + 0.5641 * y[t-1]
	
}
plot(y, type = 'l')


w = rep(0, length(y))
for(mes in 1:12) {
	index = seq(mes, length(y), 12)
	w[index] = y[index] * sds[mes] + means[mes]
}
plot(w, type = 'l', lwd = 1)
lines(240:289, w[240:289], col = 'red')

