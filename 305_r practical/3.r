P1 = matrix(c(0, 1, 0, 0, 0, 1, 1, 0, 0), nrow = 3, byrow = TRUE) # P1 -> P^1
#Matrix-power helper
mat_pow = function(P, pow) {
  if(pow == 1) {
    return(P)
  } else {
    return(P %*% mat_pow(P, pow - 1))
  }
}
#Compute higher powers
P2 = mat_pow(P1, 2)
P3 = mat_pow(P1, 3)
P4 = mat_pow(P1, 4)
P5 = mat_pow(P1, 5)
P6 = mat_pow(P1, 6)
#Initialize storage vectors
f1 = numeric(nrow(P1))
f2 = numeric(nrow(P1))
f3 = numeric(nrow(P1))
f4 = numeric(nrow(P1))
f5 = numeric(nrow(P1))
f6 = numeric(nrow(P1))
#Compute first-return probabilities
for(i in 1:nrow(P1)) {
  f1[i] = P1[i, i]
  f2[i] = P2[i, i] - f1[i] * P1[i, i]
  f3[i] = P3[i, i] - f1[i] * P2[i, i] - f2[i] * P1[i, i]
  f4[i] = P4[i, i] - f1[i] * P3[i, i] - f2[i] * P2[i, i] - f3[i] * P1[i, i]
  f5[i] = P5[i, i] - f1[i] * P4[i, i] - f2[i] * P3[i, i] - f3[i] * P2[i, i] - f4[i] * P1[i, i]
  f6[i] = P6[i, i] - f1[i] * P5[i, i] - f2[i] * P4[i, i] - f3[i] * P3[i, i] - f4[i] * P2[i, i] - f5[i] * P1[i, i]
}
#Summation and mean recurrence time
Fii = f1 + f2 + f3 + f4 + f5 + f6
muii = 1*f1 + 2*f2 + 3*f3 + 4*f4 + 5*f5 + 6*f6
#Classification of states
result = ifelse(Fii == 1, "Persistent", "Trensient")
result2 = ifelse(Fii == 1, ifelse(muii < Inf, "Non-null", "Null") , "-" )

data.frame(state = 1:nrow(P1), Fii, Result = result, muii, Result2 = result2)



#periodicity
periodicity = function(P, pow = 2, max_pow = 100) {
  if(identical(P, mat_pow(P, pow))){
    return(pow - 1)
  } else if(pow >= max_pow) {
    return(NA)
  } else {
    # print(pow)
    # print("এখনও পাইনি, তাই পাওয়ারের মান ১ বাড়িয়ে দিলাম")
    periodicity(P, pow = pow + 1)
  }
}
periodicity(P1)