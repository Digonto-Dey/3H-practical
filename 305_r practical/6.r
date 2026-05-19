sum = 0
for(n in 1:10) {
  sum = sum + exp(-30)*(30^n)/factorial(n)
}
sum