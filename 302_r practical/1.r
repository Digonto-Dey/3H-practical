ni = c(4,10,24,12,6,3)
n = sum(ni)
n

chisq = function(a){
  sapply(a, function(single_a){
    lower = c(0,1,2,3,4,5)
    upper  = c(1,2,3,4,5, Inf)
    p = exp(-lower/single_a) - exp(-upper/single_a)
    
    
    sum((ni - n*p)^2/(n*p))
  })
}

est <- optimize(chisq, c(0,10))
est

cat("Estimated theta:",  est$minimum, "\n")
cat("Estimated chi square:", est$objective, "\n")

curve(chisq, from = 0, to = 10, col = "blue", ylim = c(20,70), xlab = expression(theta),
      ylab = "chisquare(theta)", lwd = 2)
abline(h = est$objective, v = est$minimum, lwd = 2, col = c(2,3), lty = c(2,3))





# 4. Justify Estimate through a Graph (ii)
curve(
  chisq, 
  from = 0.5, 
  to = 5, 
  col = "blue", 
  ylim = c(0, max(est$objective * 1.5, 60)), # Adjust y-limit for better visualization
  xlab = expression(theta),
  ylab = expression(chi^2~(theta)), 
  lwd = 2,
  main = "Minimum Chi-Square Estimation"
)

# Mark the minimum on the graph
# Horizontal line for the minimum chi-square value
abline(h = est$objective, lwd = 2, col = "red", lty = 2) 
# Vertical line for the estimated theta (the location of the minimum)
abline(v = est$minimum, lwd = 2, col = "darkgreen", lty = 3) 

# Add a point at the minimum
points(est$minimum, est$objective, col = "red", pch = 19, cex = 1.5)

# Add text labels for clarity
text(est$minimum, max(est$objective * 1.2, 55), 
     paste0(expression(hat(theta)), " = ", round(est$minimum, 4)), 
     pos = 4, col = "darkgreen")

text(1, est$objective, 
     paste0("Min ", expression(chi^2), " = ", round(est$objective, 4)), 
     pos = 4, col = "red")




