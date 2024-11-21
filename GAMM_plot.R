
 pa1 <- bam(log_HellaSwag~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa2 <- bam(log_ARC~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)

pa3 <- bam(log_MMLU~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa4 <- bam(log_Winogrande~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa5 <- bam(log_GSM8K~ s(log_Param) + s(Architecture, bs="re"), data=df2, discrete=T)

pa6 <- bam(log_TruthfulQA~ s(log_Param) + s(Architecture, bs="re"), data=df2, discrete=T)



##

    
 pa1a <- bam(log_HellaSwag~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)
pa2a <- bam(log_ARC~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)

pa3a <- bam(log_MMLU~ s(log_Param, by=Type)+Type+ s(Architecture, bs="re"), data=df1, discrete=T)
pa4a <- bam(log_Winogrande~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)
pa5a <- bam(log_GSM8K~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df2, discrete=T)

pa6a <- bam(log_TruthfulQA~ s(log_Param, by=Type)+Type+ s(Architecture, bs="re"), data=df2, discrete=T)






###
 pa1b <- bam(log_HellaSwag_sc~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa2b <- bam(log_ARC_sc~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)

pa3b <- bam(log_MMLU_sc~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa4b <- bam(log_Winogrande_sc~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa5b <- bam(log_GSM8K_sc~ s(log_Param) + s(Architecture, bs="re"), data=df2, discrete=T)

pa6b <- bam(log_TruthfulQA_sc~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)



###
 pa1e <- bam(log_HellaSwag~ s(X.Param.B.) + s(Architecture, bs="re"), data=df1, discrete=T)
pa2e <- bam(log_ARC~ s(X.Param.B.) + s(Architecture, bs="re"), data=df1, discrete=T)

pa3e <- bam(log_MMLU~ s(X.Param.B.) + s(Architecture, bs="re"), data=df1, discrete=T)
pa4e <- bam(log_Winogrande~ s(X.Param.B.) + s(Architecture, bs="re"), data=df1, discrete=T)
pa5e <- bam(log_GSM8K~ s(X.Param.B.) + s(Architecture, bs="re"), data=df2, discrete=T)

pa6e <- bam(log_TruthfulQA~ s(X.Param.B.) + s(Architecture, bs="re"), data=df2, discrete=T)



###
pdf("para_eff_all.V2.pdf", wi=9.2, he=7.3)
#par=mfrow(3,2))
par(mfrow=c(2,3), cex=0.8)
#par(mar = c(5.1, 4.1, 4.1, 2.1))



setEPS()
postscript("para_eff_all_V2a.eps", wi=9.2, he=7.3)
par(mfrow=c(2,3), cex=0.8)

##total
plot(pa2, select=1, rug=T, xlab="log_Param", ylab="log_ARC (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.7, 0.7))
abline(h=0, col="indianred")


plot(pa1, select=1, rug=T, xlab="log_Param", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-1, 0.8))
abline(h=0, col="indianred")


plot(pa3, select=1, rug=T, xlab="log_Param", ylab="log_MMLU ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.5, 0.6))
abline(h=0, col="indianred")


plot(pa6, select=1, rug=T, xlab="log_Param",  ylab="log_TruthfulQA (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.15, 0.3))
abline(h=0, col="indianred")


plot(pa4, select=1, rug=T, xlab="log_Param", ylab="log_Winogrande ",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.3, 0.3))
abline(h=0, col="indianred")
 

plot(pa5, select=1, rug=T, xlab="log_Param", ylab="log_GSM8K",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-3.6, 3.9))
abline(h=0, col="indianred")

dev.off()

#abline(v=quantile(x,seq(0,1, by=0.2) ), col="grey")

###plot with vertical ablines
 x <- df$X.Param.B 
> x_quartiles <- quantile(x, seq(0, 1, by = 0.2))
> x_quartiles
    0%    20%    40%    60%    80%   100% 
  0.00   1.10   5.84   6.65  12.34 176.25 
###
###from the 2 and 3 quartiles, increase is seen (1.1~)
x_quartiles <- quantile(x, seq(0, 1, by = 1/6))
x_quartiles
   0%   16.66667%   33.33333%         50%   66.66667%   83.33333% 
  0.0000000   0.7883333   3.3200000   6.6100000   6.7400000  12.8500000 
       100% 
176.2500000

y <- df1$X.Param.B.
> y_quartiles <- quantile(x, seq(0, 1, by = 0.2))
4 
> y_quartiles <- quantile(y, seq(0, 1, by = 0.2))
> y_quartiles
   y_quartiles
    0%    20%    40%    60%    80%   100% 
  0.01   1.31   6.53   6.65  12.85 180.00

###40%-60%, all cases see an increase (6.47-6.65)
##five of six see an incase (1.31-6.47)
##however, from 6.47-12.85 show complex situations, during the range, a slight increase can be seen, and then a long fall.
###

# Assuming df is your data frame and "log_Param" is the column of interest
df1=df1[!is.infinite(df1$log_Param),]
 range(df1$log_Param)
#[1] -4.605170  5.171903
 x <- df1$log_Param
 x_quartiles <- quantile(x, seq(0, 1, by = 0.2))
 x_quartiles
        0%        20%        40%        60%        80%       100% 
#-4.6051702  0.2700271  1.8671475  1.8946169  2.5533438  5.171903

 
###from the 2 and 3 quartiles, increase is seen ()
# Now, within each plot command, add the abline for quartiles
setEPS()
postscript("para_eff_all.V3e.eps", wi=9.2, he=7.3)
par(mfrow=c(2,3), cex=0.8)

# Plot 1
plot(pa2, select=1, rug=T, xlab="log_Param", ylab="log_ARC (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.7, 0.7))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")  # Add quartile lines

# Repeat for each subsequent plot, ensuring you add the abline(v=x_quartiles, col="grey") line after each plot command

# Plot 2
plot(pa1, select=1, rug=T, xlab="log_Param", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-1, 0.8))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")


plot(pa3, select=1, rug=T, xlab="log_Param", ylab="log_MMLU ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.5, 0.6))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")


plot(pa6, select=1, rug=T, xlab="log_Param",  ylab="log_TruthfulQA (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.15, 0.3))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")


plot(pa4, select=1, rug=T, xlab="log_Param", ylab="log_Winogrande ",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.3, 0.3))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")
 

plot(pa5, select=1, rug=T, xlab="log_Param", ylab="log_GSM8K",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-3.6, 3.9))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")

dev.off()


###withe different color for special quartiels

# Calculate quartiles of log_Param
x <- df1$log_Param
x_quartiles <- quantile(x, seq(0, 1, by = 0.2))

# Define a function to determine the shade color based on quartiles
quartile_colors <- rep("grey", length(x_quartiles))
quartile_colors[which(names(x_quartiles) %in% c("20%","40%","60%"))] <- "darkolivegreen1"

# Set up the plot
setEPS()
postscript("para_eff_all.V3e.eps", wi=9.2, he=7.3)
par(mfrow=c(2,3), cex=0.8)

# Function to plot and add quartile lines
plot_and_add_quartiles <- function(pa, select, xlab, ylab, shade.col, ylim) {
  plot(pa, select=select, rug=T, xlab=xlab, ylab=ylab, lwd=1.2, shade=T, shade.col=shade.col, font.lab=2, ylim=ylim)
  abline(h=0, col="indianred")
  for (i in seq_along(x_quartiles)) {
    abline(v=x_quartiles[i], col=quartile_colors[i])
  }
}

# Plot 1
plot_and_add_quartiles(pa2, 1, "log_Param", "log_ARC (partial effects)", "steelblue2", c(-0.7, 0.7))

# Repeat for each subsequent plot
plot_and_add_quartiles(pa1, 1, "log_Param", "log_HellaSwag", "steelblue2", c(-1, 0.8))
# ... Continue for other plots ...
plot_and_add_quartiles(pa3, 1, "log_Param", "log_MMLU", "steelblue2", c(-0.5, 0.6))
plot_and_add_quartiles(pa4, 1, "log_Param", "log_Winogrande (partial effects)", "steelblue2", c(-0.3, 0.3))
plot_and_add_quartiles(pa5, 1, "log_Param", "log_GSM8K", "steelblue2", c(-3.6, 3.9))
# Close the device
plot_and_add_quartiles(pa6, 1, "log_Param", "log_TruthfulQA", "steelblue2", c(-0.15, 0.3))
# Close the device
dev.off()


# ... continue for all plots ...

#dev.off()
###with raw parameters

###plot with scaled data



# Assuming df is your data frame and "log_Param" is the column of interest
x <- df1$log_Param  # or the appropriate column or data for your x-axis

# Calculate the quartiles of x
x_quartiles <- quantile(x, seq(0, 1, by = 0.2))
x_quartiles <- x_quartiles[!is.infinite(x_quartiles)]

print(x_quartiles)
#       20%        40%        60%        80%       100% 
#0.09531018 1.76473080 1.89461685 2.51159624 5.17190344 
###from the 2 and 3 quartiles, increase is seen ()
# Now, within each plot command, add the abline for quartiles
setEPS()
postscript("para_eff_all_scaled.V1.eps", wi=9.2, he=7.3)
par(mfrow=c(2,3), cex=0.8)

# Plot 1
plot(pa2b, select=1, rug=T, xlab="log_Param", ylab="log_ARC (scaled) (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.7, 0.7))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="green")  # Add quartile lines

# Repeat for each subsequent plot, ensuring you add the abline(v=x_quartiles, col="grey") line after each plot command

# Plot 2
plot(pa1b, select=1, rug=T, xlab="log_Param", ylab="log_HellaSwag (scaled)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-1, 0.8))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="green")


plot(pa3b, select=1, rug=T, xlab="log_Param", ylab="log_MMLU (scaled)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.5, 0.6))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="green")


plot(pa6b, select=1, rug=T, xlab="log_Param",  ylab="log_TruthfulQA (scaled) (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.15, 0.3))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="green")


plot(pa4b, select=1, rug=T, xlab="log_Param", ylab="log_Winogrande (scaled) ",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.3, 0.3))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="green")
 

plot(pa5b, select=1, rug=T, xlab="log_Param", ylab="log_GSM8K (scaled)",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-3.6, 3.9))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="green")

dev.off()


###

setEPS()
postscript("para_eff_types_V1a.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

plot(pa2a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_ARC ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa2a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_ARC",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa2a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_ARC ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa2a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_ARC ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa2a, select=1, rug=T, xlab="log_Param  (unknown)", ylab="log_ARC",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")

plot(pa1a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa1a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa1a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa1a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa1a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")

plot(pa3a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa3a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa3a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa3a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa3a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")


plot(pa6a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_TruthfulQA ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa6a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_TruthfulQA ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa6a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_TruthfulQA ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa6a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="TruthfulQA ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa6a, select=1, rug=T, xlab="log_Param (unknown)", ylab="TruthfulQA ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")



plot(pa4a, select=2, rug=T, xlab="log_Param  (fine-tuned)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa4a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa4a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa4a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="logWinogrande ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa4a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")

plot(pa5a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa5a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa5a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa5a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")
plot(pa5a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2)
abline(h=0, col="indianred")

dev.off()

###

# Assuming you have an array or list of p-values corresponding to each plot
# Replace this with actual p-values extraction method
p_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0002, 0, 0, 0, 0,0, 0, 0, 0, 0, 0.0004, 0)  # Example p-values

setEPS()
postscript("para_eff_types_V1b.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

# A list of all your plots, assuming each plot command is followed by an abline command
plot_commands <- list(
  # pa2a plots
  list(plot=plot(pa2a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_ARC ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa2a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_ARC",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa2a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_ARC ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa2a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_ARC ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa2a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_ARC",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  
  # pa1a plots
  list(plot=plot(pa1a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa1a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa1a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa1a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  list(plot=plot(pa1a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
  
  # ... continue for pa3a, pa4a, pa5a, and pa6a with similar structure
  # Example for pa3a
  list(plot=plot(pa3a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline=abline(h=0, col="indianred")),
 list(plot=plot(pa3a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa3a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_HellaSwag ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2),abline(h=0, col="indianred")),
list(plot=plot(pa3a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa3a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_HellaSwag",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2),abline(h=0, col="indianred")),

list(plot=plot(pa4a, select=2, rug=T, xlab="log_Param  (fine-tuned)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa4a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa4a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa4a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="logWinogrande ",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")), list(plot=plot(pa4a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_Winogrande",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),

list(plot=plot(pa5a, select=2, rug=T, xlab="log_Param (fine-tuned)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa5a, select=3, rug=T, xlab="log_Param (instruction-tuned)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa5a, select=4, rug=T, xlab="log_Param (pretrained)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2), abline(h=0, col="indianred")),
list(plot=plot(pa5a, select=5, rug=T, xlab="log_Param (RL-tuned)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2),abline(h=0, col="indianred")),
list(plot=plot(pa5a, select=1, rug=T, xlab="log_Param (unknown)", ylab="log_GSM8K",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2),abline(h=0, col="indianred")
  # ... continue for all select values and all pa* datasets
  # ... continue for pa4a, pa5a, and pa6a
)


# Loop through each plot command and add p-value text
for (i in 1:length(plot_commands)) {
  # Execute plot and abline commands
  eval(plot_commands[[i]]$plot)
  eval(plot_commands[[i]]$abline)
  
  # Calculate middle top position for the p-value text
  x_mid <- mean(par("usr")[1:2])
  y_top <- par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.05  # Slightly below the top
  
  # Add p-value text
  text(x_mid, y_top, labels = paste("p-value:", round(p_values[i], 4)), cex = 1.2)
}

dev.off()

###


plot_with_annotation <- function(model, select, p_value, xlab, ylab) {
  # Plot the model with the given selection
  plot(model, select=select, rug=T, xlab=xlab, ylab=ylab, lwd=1.2, shade=T, shade.col = "steelblue2", font.lab=2)
  abline(h=0, col="indianred")
  
  # Calculate middle top position for the p-value text
  x_mid <- mean(par("usr")[1:2])
  y_top <- par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.05  # Slightly below the top
  
  # Add p-value text in green color with consistent formatting
 # formatted_p_value <- sprintf("%.4f", p_value)  # Format to 4 decimal places
  text(x_mid, y_top, labels = paste("p-value:", p_value), cex = 1.2, col = "red")
}



setEPS()
postscript("para_eff_types_V1e.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

# Assuming you have an array or list of p-values corresponding to each plot
p_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0002, 0,  0, 0, 0,0, 0, 0, 0, 0, 0.0004, 0, 0, 0, 0, 0.07, 0.0002)  # Example p-values

# Call the function for each plot
plot_with_annotation(pa2a, 2, p_values[1], "log_Param (fine-tuned)", "log_ARC ")
plot_with_annotation(pa2a, 3, p_values[2], "log_Param (instruction-tuned)", "log_ARC")
plot_with_annotation(pa2a, 4, p_values[3], "log_Param (pretrained)", "log_ARC")
plot_with_annotation(pa2a, 5, p_values[4], "log_Param (RL-tuned)", "log_ARC")
plot_with_annotation(pa2a, 1, p_values[5], "log_Param (unknown)", "log_ARC")
# ... continue for all plots
plot_with_annotation(pa1a, 2, p_values[6], "log_Param (fine-tuned)", "log_HellaSwag")
plot_with_annotation(pa1a, 3, p_values[7], "log_Param (instruction-tuned)", "log_HellaSwag")
plot_with_annotation(pa1a, 4, p_values[8], "log_Param (pretrained)", "log_HellaSwag")
plot_with_annotation(pa1a, 5, p_values[9], "log_Param (RL-tuned)", "log_HellaSwag")
plot_with_annotation(pa1a, 1, p_values[10], "log_Param (unknown)", "log_HellaSwag")
##
plot_with_annotation(pa3a, 2, p_values[11], "log_Param (fine-tuned)", "log_MMLU")
plot_with_annotation(pa3a, 3, p_values[12], "log_Param (instruction-tuned)", "log_MMLU")
plot_with_annotation(pa3a, 4, p_values[13], "log_Param (pretrained)", "log_MMLU")
plot_with_annotation(pa3a, 5, p_values[14], "log_Param (RL-tuned)", "log_MMLU")
plot_with_annotation(pa3a, 1, p_values[15], "log_Param (unknown)", "log_MMLU")

##
plot_with_annotation(pa4a, 2, p_values[16], "log_Param (fine-tuned)", "log_Winogrande")
plot_with_annotation(pa4a, 3, p_values[17], "log_Param (instruction-tuned)", "log_Winogrande")
plot_with_annotation(pa4a, 4, p_values[18], "log_Param (pretrained)", "log_Winogrande")
plot_with_annotation(pa4a, 5, p_values[19], "log_Param (RL-tuned)", "log_Winogrande")
plot_with_annotation(pa4a, 1, p_values[20], "log_Param (unknown)", "log_Winogrande")
##
plot_with_annotation(pa5a, 2, p_values[21], "log_Param (fine-tuned)", "log_GSM8K")
plot_with_annotation(pa5a, 3, p_values[22], "log_Param (instruction-tuned)", "log_GSM8K")
plot_with_annotation(pa5a, 4, p_values[23], "log_Param (pretrained)", "log_GSM8K")
plot_with_annotation(pa5a, 5, p_values[24], "log_Param (RL-tuned)", "log_GSM8K")
plot_with_annotation(pa5a, 1, p_values[25], "log_Param (unknown)", "log_GSM8K")


##
plot_with_annotation(pa6a, 2, p_values[26], "log_Param (fine-tuned)", "log_TruthfulQA")
plot_with_annotation(pa6a, 3, p_values[27], "log_Param (instruction-tuned)", "log_TruthfulQA")
plot_with_annotation(pa6a, 4, p_values[28], "log_Param (pretrained)", "log_TruthfulQA")
plot_with_annotation(pa6a, 5, p_values[29], "log_Param (RL-tuned)", "log_TruthfulQA")
plot_with_annotation(pa6a, 1, p_values[30], "log_Param (unknown)", "log_TruthfulQA")

dev.off()





###


# Assuming you have pre-calculated p-values for each plot in a list or array
# Each element of the list is a vector of p-values for each selection of a particular dataset
# For example, p_values_pa2a might be something like c(0.05, 0.01, 0.2, 0.15, 0.05)
# You would need similar vectors for each of pa1a, pa3a, pa6a, pa4a, and pa5a

# Start EPS output
setEPS()
postscript("para_eff_types_V2a.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

# Function to plot a series for a given dataset and add p-values
plot_series <- function(data=df1, p_values, ylab) {
  for(i in 1:5) {
    plot(data, select=i, rug=T, xlab=paste("log_Param (", colnames(data)[i], ")"), ylab=ylab, lwd=1.2, shade=T, shade.col = "steelblue2", font.lab=2)
    abline(h=0, col="indianred")
    # Add p-value text to the plot, adjust position as needed
    text(x = max(data[,i]), y = 0, labels = paste("p-value:", p_values[i]), pos = 4)
  }
}

# Plot each series with p-values
plot_series(pa2a, p_values_pa2a, "log_ARC")
plot_series(pa1a, p_values_pa1a, "log_HellaSwag")
# ... continue for pa3a, pa6a, pa4a, and pa5a with their respective p-value vectors and y-labels

# Close the plotting device
dev.off()


#######

extract_p_value <- function(gamm_object, select_num) {
  # Extracting the summary
  s <- summary(gamm_object)
  
  # Extracting p-value for the specific selection
  p_value <- s$s.table[select_num, 4]
  
  return(p_value)
}


get_ylab <- function(dataset_name) {
  # Define a mapping from dataset names to y-labels
  ylab_map <- list(
    pa1a = "log_HellaSwag",
    pa2a = "log_ARC",
    pa3a = "log_MMLU",
    pa4a = "log_Winogrande",
    pa5a = "log_GSM8K",
    pa6a = "log_TruthfulQA"
  )
  
  # Return the corresponding y-label
  return(ylab_map[[dataset_name]])
}


# Assuming you have already fitted your GAMM models: pa1a, pa2a, pa3a, pa4a, pa5a, pa6a
# And each model corresponds to a dataset with a similar structure



# Set up EPS output
setEPS()
postscript("para_eff_types.V2.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

# Define x-labels based on 'select' value
xlab_map <- list(
  "1" = "log_Param (unknown)",
  "2" = "log_Param (fine-tuned)",
  "3" = "log_Param (instruction-tuned)",
  "4" = "log_Param (pretrained)",
  "5" = "log_Param (RL-tuned)"
)

# List of all model names as strings
model_names <- c("pa1a", "pa2a", "pa3a", "pa4a", "pa5a", "pa6a")

# Loop through each model name
for (model_name in model_names) {
  # Retrieve the model based on the model name
  model <- get(model_name)
  # Assuming the data variable is named the same as the model
  data <- get(model_name)  # This line changed to directly use model_name
  ylab_name <- get_ylab(model_name)
  
  # Extract p-values from the model
  p_values <- extract_p_values(model)
  
  # Loop through each selection and plot
  for (j in 1:5) {
    xlab_name <- xlab_map[[as.character(j)]]  # Get x-label from the map
    p_value <- extract_p_value(model, j)  # Get corresponding p-value
    
    plot(data, select=j, rug=T, xlab=xlab_name, 
         ylab=ylab_name, lwd=1.2, shade=T, shade.col = "steelblue2", font.lab=2)
    abline(h=0, col="indianred")
    
    # Annotate plot with p-value
    text(x = max(data[,j]), y = 0, labels = paste("p-value:", round(p_value, 4)), pos = 4)
  }
}

# Close the plotting device
dev.off()


###scaled group plots


 pa1c <- bam(log_HellaSwag_sc~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)
pa2c <- bam(log_ARC_sc~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)

pa3c <- bam(log_MMLU_sc~ s(log_Param, by=Type)+Type+ s(Architecture, bs="re"), data=df1, discrete=T)
pa4c <- bam(log_Winogrande_sc~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)
pa5c <- bam(log_GSM8K_sc~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df2, discrete=T)

pa6c <- bam(log_TruthfulQA_sc~ s(log_Param, by=Type)+Type+ s(Architecture, bs="re"), data=df2, discrete=T)


plot_with_annotation <- function(model, select, p_value, xlab, ylab) {
  # Plot the model with the given selection
  plot(model, select=select, rug=T, xlab=xlab, ylab=ylab, lwd=1.2, shade=T, shade.col = "steelblue2", font.lab=2)
  abline(h=0, col="indianred")
  
  # Calculate middle top position for the p-value text
  x_mid <- mean(par("usr")[1:2])
  y_top <- par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.05  # Slightly below the top
  
  # Add p-value text in green color with consistent formatting
 # formatted_p_value <- sprintf("%.4f", p_value)  # Format to 4 decimal places
  text(x_mid, y_top, labels = paste("p-value:", p_value), cex = 1.2, col = "red")
}



setEPS()
postscript("para_scaled_eff_types.V1.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

# Assuming you have an array or list of p-values corresponding to each plot
p_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001, 0, 0, 0, 0, 0.051, 0.0003, 0, 0, 0,0, 0, 0, 0, 0, 0.0004, 0)  # Example p-values

# Call the function for each plot
plot_with_annotation(pa2c, 2, p_values[1], "log_Param (fine-tuned)", "log_scaled_ARC ")
plot_with_annotation(pa2c, 3, p_values[2], "log_Param (instruction-tuned)", "log_scaled_ARC")
plot_with_annotation(pa2c, 4, p_values[3], "log_Param (pretrained)", "log_scaled_ARC")
plot_with_annotation(pa2c, 5, p_values[4], "log_Param (RL-tuned)", "log_scaled_ARC")
plot_with_annotation(pa2c, 1, p_values[5], "log_Param (unknown)", "log_scaled_ARC")
# ... continue for all plots
plot_with_annotation(pa1c, 2, p_values[6], "log_Param (fine-tuned)", "log_scaled_HellaSwag")
plot_with_annotation(pa1c, 3, p_values[7], "log_Param (instruction-tuned)", "log_scaled_HellaSwag")
plot_with_annotation(pa1c, 4, p_values[8], "log_Param (pretrained)", "log_scaled_HellaSwag")
plot_with_annotation(pa1c, 5, p_values[9], "log_Param (RL-tuned)", "log_scaled_HellaSwag")
plot_with_annotation(pa1c, 1, p_values[10], "log_Param (unknown)", "log_scaled_HellaSwag")
##
plot_with_annotation(pa3c, 2, p_values[11], "log_Param (fine-tuned)", "log_scaled_MMLU")
plot_with_annotation(pa3c, 3, p_values[12], "log_Param (instruction-tuned)", "log_scaled_MMLU")
plot_with_annotation(pa3c, 4, p_values[13], "log_Param (pretrained)", "log_scaled_MMLU")
plot_with_annotation(pa3c, 5, p_values[14], "log_Param (RL-tuned)", "log_scaled_MMLU")
plot_with_annotation(pa3c, 1, p_values[15], "log_Param (unknown)", "log_scaled_MMLU")

##
plot_with_annotation(pa6c, 2, p_values[16], "log_Param (fine-tuned)", "log_scaled_TruthfulQA")
plot_with_annotation(pa6c, 3, p_values[17], "log_Param (instruction-tuned)", "log_scaled_TruthfulQA")
plot_with_annotation(pa6c, 4, p_values[18], "log_Param (pretrained)", "log_scaled_TruthfulQA")
plot_with_annotation(pa6c, 5, p_values[19], "log_Param (RL-tuned)", "log_scaled_TruthfulQA")
plot_with_annotation(pa6c, 1, p_values[20], "log_Param (unknown)", "log_scaled_TruthfulQA")
##
plot_with_annotation(pa4c, 2, p_values[21], "log_Param (fine-tuned)", "log_scaled_Winogrande")
plot_with_annotation(pa4c, 3, p_values[22], "log_Param (instruction-tuned)", "log_scaled_Winogrande")
plot_with_annotation(pa4c, 4, p_values[23], "log_Param (pretrained)", "log_scaled_Winogrande")
plot_with_annotation(pa4c, 5, p_values[24], "log_Param (RL-tuned)", "log_scaled_Winogrande")
plot_with_annotation(pa4c, 1, p_values[25], "log_Param (unknown)", "log_scaled_Winogrande")
##
plot_with_annotation(pa5c, 2, p_values[26], "log_Param (fine-tuned)", "log_scaled_GSM8K")
plot_with_annotation(pa5c, 3, p_values[27], "log_Param (instruction-tuned)", "log_scaled_GSM8K")
plot_with_annotation(pa5c, 4, p_values[28], "log_Param (pretrained)", "log_scaled_GSM8K")
plot_with_annotation(pa5c, 5, p_values[29], "log_Param (RL-tuned)", "log_scaled_GSM8K")
plot_with_annotation(pa5c, 1, p_values[30], "log_Param (unknown)", "log_scaled_GSM8K")



dev.off()

############################################################################################
#############################################################################################3
new scaled data in terms of more architecture patterns
#################################################################################################3

 df1$log_HellaSwag_sc1=log(df1$HellaSwagScaled1)
 df1$log_ARC_sc1=log(df1$ARCScaled1)
df1$log_MMLU_sc1=log(df1$MMLUScaled1)
df1$log_TruthfulQA_sc1=log(df1$TruthfulQAScaled1)
df1$log_Winogrande_sc1=log(df1$WinograndeScaled1)
df1$log_GSM8K_sc1=log(df1$GSM8KScaled1)

 df2=df1[!is.infinite(df1$log_GSM8K_sc1),]
> dim(df2)
[1] 908  50
 df2=df2[!is.infinite(df2$log_TruthfulQA_sc1),]
> dim(df2)
[1] 905  50


 pa1b1 <- bam(log_HellaSwag_sc1~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa2b1 <- bam(log_ARC_sc1~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)

pa3b1 <- bam(log_MMLU_sc1~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa4b1 <- bam(log_Winogrande_sc1~ s(log_Param) + s(Architecture, bs="re"), data=df1, discrete=T)
pa5b1 <- bam(log_GSM8K_sc1~ s(log_Param) + s(Architecture, bs="re"), data=df2, discrete=T)

pa6b1 <- bam(log_TruthfulQA_sc1~ s(log_Param) + s(Architecture, bs="re"), data=df2, discrete=T)




###plot with scaled data (without types)


y <- df1$X.Param.B.
> y_quartiles <- quantile(x, seq(0, 1, by = 0.2))
4 
> y_quartiles <- quantile(y, seq(0, 1, by = 0.2))
> y_quartiles
    0%    20%    40%    60%    80%   100% 
  0.01   1.31   6.47   6.65  12.85 176.25

###40%-60% all cases see an increase (6.47-6.65)
##five of six see an incase (1.31-6.47)
##however, from 6.47-12.85 show complex situations, during the range, a slight increase can be seen, and then a long fall.
###

# Assuming df is your data frame and "log_Param" is the column of interest
x <- df1$log_Param  # or the appropriate column or data for your x-axis

# Calculate the quartiles of x
x_quartiles <- quantile(x, seq(0, 1, by = 0.2))
x_quartiles <- x_quartiles[!is.infinite(x_quartiles)]

x_quartiles
        0%        20%        40%        60%        80%       100% 
-4.6051702  0.2700271  1.8671475  1.8946169  2.5533438  5.1719034 



# Now, within each plot command, add the abline for quartiles
setEPS()
postscript("para_eff_all_scaled.V2c.eps", wi=9.2, he=7.3)
par(mfrow=c(2,3), cex=0.8)

# Plot 1
plot(pa2b1, select=1, rug=T, xlab="log_Param", ylab="ARC (log_scaled) (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.7, 0.7))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")  # Add quartile lines

# Repeat for each subsequent plot, ensuring you add the abline(v=x_quartiles, col="grey") line after each plot command

# Plot 2
plot(pa1b1, select=1, rug=T, xlab="log_Param", ylab="HellaSwag (log_scaled)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-1, 0.8))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")


plot(pa3b1, select=1, rug=T, xlab="log_Param", ylab="MMLU (log_scaled)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.5, 0.6))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")


plot(pa6b1, select=1, rug=T, xlab="log_Param",  ylab="TruthfulQA (log_scaled) (partial effects)",  lwd=1.2,  shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.15, 0.3))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")


plot(pa4b1, select=1, rug=T, xlab="log_Param", ylab="Winogrande (log_scaled)",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-0.3, 0.3))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")
 

plot(pa5b1, select=1, rug=T, xlab="log_Param", ylab="GSM8K (log_scaled)",  lwd=1.2, shade=T, shade.col = "steelblue2",  font.lab=2, ylim=c(-3.6, 3.9))
abline(h=0, col="indianred")
abline(v=x_quartiles, col="grey")

dev.off()


###


###scaled group plots


 pa1c1 <- bam(log_HellaSwag_sc1~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)
pa2c1 <- bam(log_ARC_sc1~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)

pa3c1 <- bam(log_MMLU_sc1~ s(log_Param, by=Type)+Type+ s(Architecture, bs="re"), data=df1, discrete=T)
pa4c1 <- bam(log_Winogrande_sc1~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df1, discrete=T)
pa5c1 <- bam(log_GSM8K_sc1~ s(log_Param, by=Type)+Type + s(Architecture, bs="re"), data=df2, discrete=T)

pa6c1 <- bam(log_TruthfulQA_sc1~ s(log_Param, by=Type)+Type+ s(Architecture, bs="re"), data=df2, discrete=T)


plot_with_annotation <- function(model, select, p_value, xlab, ylab) {
  # Plot the model with the given selection
  plot(model, select=select, rug=T, xlab=xlab, ylab=ylab, lwd=1.2, shade=T, shade.col = "steelblue2", font.lab=2)
  abline(h=0, col="indianred")
  
  # Calculate middle top position for the p-value text
  x_mid <- mean(par("usr")[1:2])
  y_top <- par("usr")[4] - (par("usr")[4] - par("usr")[3]) * 0.05  # Slightly below the top
  
  # Add p-value text in green color with consistent formatting
 # formatted_p_value <- sprintf("%.4f", p_value)  # Format to 4 decimal places
  text(x_mid, y_top, labels = paste("p-value:", p_value), cex = 1.2, col = "red")
}



setEPS()
postscript("para_scaled_eff_types.V2b.eps", wi=15, he=21.5)
par(mfrow=c(6,5), cex=0.8)

# Assuming you have an array or list of p-values corresponding to each plot
p_values <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.0001, 0, 0, 0, 0, 0.05, 0.0003, 0, 0, 0,0, 0, 0, 0, 0, 0.0004, 0)  # Example p-values

# Call the function for each plot
plot_with_annotation(pa2c1, 2, p_values[1], "log_Param (fine-tuned)", "ARC (log_scaled) (partial effects)")
plot_with_annotation(pa2c1, 3, p_values[2], "log_Param (instruction-tuned)", "ARC (log_scaled)")
plot_with_annotation(pa2c1, 4, p_values[3], "log_Param (pretrained)", "ARC (log_scaled)")
plot_with_annotation(pa2c1, 5, p_values[4], "log_Param (RL-tuned)", "ARC (log_scaled)")
plot_with_annotation(pa2c1, 1, p_values[5], "log_Param (unknown)", "ARC (log_scaled)")
# ... continue for all plots
plot_with_annotation(pa1c1, 2, p_values[6], "log_Param (fine-tuned)", "HellaSwag (log_scaled) (partial effects)")
plot_with_annotation(pa1c1, 3, p_values[7], "log_Param (instruction-tuned)", "HellaSwag (log_scaled)")
plot_with_annotation(pa1c1, 4, p_values[8], "log_Param (pretrained)", "HellaSwag (log_scaled)")
plot_with_annotation(pa1c1, 5, p_values[9], "log_Param (RL-tuned)", "HellaSwag (log_scaled)")
plot_with_annotation(pa1c1, 1, p_values[10], "log_Param (unknown)", "HellaSwag (log_scaled)")
##
plot_with_annotation(pa3c1, 2, p_values[11], "log_Param (fine-tuned)", "MMLU (log_scaled)(partial effects)")
plot_with_annotation(pa3c1, 3, p_values[12], "log_Param (instruction-tuned)", "MMLU (log_scaled)")
plot_with_annotation(pa3c1, 4, p_values[13], "log_Param (pretrained)", "MMLU (log_scaled)")
plot_with_annotation(pa3c1, 5, p_values[14], "log_Param (RL-tuned)", "MMLU (log_scaled)")
plot_with_annotation(pa3c1, 1, p_values[15], "log_Param (unknown)", "MMLU (log_scaled)")


##
plot_with_annotation(pa4c1, 2, p_values[21], "log_Param (fine-tuned)", "Winogrande (log_scaled)(partial effects)")
plot_with_annotation(pa4c1, 3, p_values[22], "log_Param (instruction-tuned)", "Winogrande (log_scaled)")
plot_with_annotation(pa4c1, 4, p_values[23], "log_Param (pretrained)", "Winogrande (log_scaled)")
plot_with_annotation(pa4c1, 5, p_values[24], "log_Param (RL-tuned)", "Winogrande (log_scaled)")
plot_with_annotation(pa4c1, 1, p_values[25], "log_Param (unknown)", "Winogrande (log_scaled)")
##
plot_with_annotation(pa5c1, 2, p_values[26], "log_Param (fine-tuned)", "GSM8K (log_scaled) (partial effects)")
plot_with_annotation(pa5c1, 3, p_values[27], "log_Param (instruction-tuned)", "GSM8K (log_scaled)")
plot_with_annotation(pa5c1, 4, p_values[28], "log_Param (pretrained)", "GSM8K (log_scaled)")
plot_with_annotation(pa5c1, 5, p_values[29], "log_Param (RL-tuned)", "GSM8K (log_scaled)")
plot_with_annotation(pa5c1, 1, p_values[30], "log_Param (unknown)", "GSM8K (log_scaled)")
##
plot_with_annotation(pa6c1, 2, p_values[16], "log_Param (fine-tuned)", "TruthfulQA (log_scaled)(partial effects)")
plot_with_annotation(pa6c1, 3, p_values[17], "log_Param (instruction-tuned)", "TruthfulQA (log_scaled)")
plot_with_annotation(pa6c1, 4, p_values[18], "log_Param (pretrained)", "TruthfulQA (log_scaled)")
plot_with_annotation(pa6c1, 5, p_values[19], "log_Param (RL-tuned)", "TruthfulQA (log_scaled)")
plot_with_annotation(pa6c1, 1, p_values[20], "log_Param (unknown)", "TruthfulQA (log_scaled)")



dev.off()

