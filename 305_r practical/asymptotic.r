P1 = matrix(c(1/3, 2/3, 0, 0, 0, 0,
              2/3, 1/3, 0, 0, 0, 0,
              0, 0, 1/4, 3/4, 0, 0,
              0, 0, 1/5, 4/5, 0, 0,
              1/4, 0, 1/4, 0, 1/4, 1/4,
              1/6, 1/6, 1/6, 1/6, 1/6, 1/6 ), nrow = 6, byrow=TRUE)


mat_pow = function(P, pow) {
  if(pow == 1) {
    return(P)
  } else {
    return(P %*% mat_pow(P, pow - 1))
  }
}

P2 = mat_pow(P1, 2)
P3 = mat_pow(P1, 3)
P4 = mat_pow(P1, 4)
P5 = mat_pow(P1, 5)
P6 = mat_pow(P1, 6)
P7 = mat_pow(P1, 7)
P8 = mat_pow(P1, 8)
P9 = mat_pow(P1, 9)

f1 = numeric(nrow(P1))
f2 = numeric(nrow(P1))
f3 = numeric(nrow(P1))
f4 = numeric(nrow(P1))
f5 = numeric(nrow(P1))
f6 = numeric(nrow(P1))
f7 = numeric(nrow(P1))
f8 = numeric(nrow(P1))
f9 = numeric(nrow(P1))


for(i in 1:nrow(P1)) {
  f1[i] = P1[i, i]
  f2[i] = P2[i, i] - f1[i] * P1[i, i]
  f3[i] = P3[i, i] - f1[i] * P2[i, i] - f2[i] * P1[i, i]
  f4[i] = P4[i, i] - f1[i] * P3[i, i] - f2[i] * P2[i, i] - f3[i] * P1[i, i]
  f5[i] = P5[i, i] - f1[i] * P4[i, i] - f2[i] * P3[i, i] - f3[i] * P2[i, i] - f4[i] * P1[i, i]
  f6[i] = P6[i, i] - f1[i] * P5[i, i] - f2[i] * P4[i, i] - f3[i] * P3[i, i] - f4[i] * P2[i, i] - f5[i] * P1[i, i]
  f7[i] = P7[i, i] - f1[i] * P6[i, i] - f2[i] * P5[i, i] - f3[i] * P4[i, i] - f4[i] * P3[i, i] - f5[i] * P2[i, i] - f6[i] * P1[i, i]
  f8[i] = P8[i, i] - f1[i] * P7[i, i] - f2[i] * P6[i, i] - f3[i] * P5[i, i] - f4[i] * P4[i, i] - f5[i] * P3[i, i] - f6[i] * P2[i, i] - f7[i] * P1[i, i]
  f9[i] = P9[i, i] - f1[i] * P8[i, i] - f2[i] * P7[i, i] - f3[i] * P6[i, i] - f4[i] * P5[i, i] - f5[i] * P4[i, i] - f6[i] * P3[i, i] - f7[i] * P2[i, i] - f8[i] * P1[i, i]
}


Fii = f1 + f2 + f3 + f4 + f5 + f6 + f7 + f8 + f9
muii = 1*f1 + 2*f2 + 3*f3 + 4*f4 + 5*f5 + 6*f6 + 7*f7 + 8*f8 + 9*f9

result = ifelse(Fii >= 0.87, "Persistent", "Trensient")
result2 = ifelse(Fii >= 0.87, ifelse(muii < Inf, "Non-null", "Null") , "-" )
# 10

data.frame(state = 1:nrow(P1), Fii, Result = result, muii, Result2 = result2)


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
##determine p2 p3 and determine that first two row has no way affect

##Asymptotic behaviour: 2 ta matrix ek jaygay ese same houar try kore
mat_pow(P1, 100)
mat_pow(P1, 101)