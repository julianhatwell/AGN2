# here are all the standard parameters. Can be tweaked for more further investigations
set.seed(10) # reprodicibility
lambda <- 0.2 # lambda for random samples
sim_size <- 1000 # simulation size
smpl_size <- 40 # sample size, each simulation

# theoretical values derived from the properties of the distribution
mu <- 1/lambda
sigma <- 1/lambda

# plotmath representation of lambda and 1/lambda
lam <- expression(lambda)
oolam <- expression(over(1, lambda))


mn_smpl <- numeric(sim_size)
var_smpl <- numeric(sim_size)
for (i in 1:sim_size) {
  smpl <- rexp(smpl_size, lambda)
  mn_smpl[i] <- mean(smpl)
  var_smpl[i] <- var(smpl)
}
  
  
  histogram(~mn_smpl, type = "density", breaks = 25, main = "Histogram: Frequency of mean values from the simulation samples", sub = "The reference line: theoretical mean 1/lambda = 1/0.2 = 5", refline = 1/lambda
            , panel=function(x,..., refline){
              panel.histogram(x,..., col = "steel blue", border="light grey")
              panel.abline(v=refline, lwd = 3, col = "orange")
              panel.text(lab = expression(paste("Theoretical mean = ", over(1, lambda))), x = refline + 0.65, y=10.5)
              panel.curve(dnorm(x, 5, 0.75),..., lwd = 2, col = "pink")
            }
  )
  
  
  histogram(~sqrt(var_smpl), breaks = 25, main = "Histogram: Frequency of st.dev values from the simulation samples", sub = "The reference line: theoretical st.dev 1/lambda = 1/0.2 = 5", refline = 1/lambda
            , panel=function(x,..., refline){
              panel.histogram(x,..., col = "steel blue", border="light grey")
              panel.abline(v=refline, lwd = 3, col = "orange")
              panel.text(lab = expression(paste("Theoretical st.dev = ", over(1, lambda))), x = refline + 0.65, y=10.5)
            }
  )
  
  
  mn_nrm <- (mn_smpl - mean(mn_smpl))/sqrt(var_smpl)
  histogram(~mn_nrm, breaks = 25, type = "density", main = "Histogram: Frequency of mean values from the simulation samples", sub = "The reference line: theoretical mean 1/lambda = 1/0.2 = 5", refline = 1/lambda
            , panel=function(x,..., refline){
              panel.histogram(x,..., col = "steel blue", border="light grey")
              panel.abline(v=refline, lwd = 3, col = "orange")
              panel.text(lab = expression(paste("Theoretical mean = ", over(1, lambda))), x = refline + 0.65, y=10.5)
              panel.curve(dnorm(x, 0, 0.15),..., lwd = 2, col = "pink")
            }
  )