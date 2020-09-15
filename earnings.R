earnings <- read.csv("https://raw.githubusercontent.com/avehtari/ROS-Examples/master/Earnings/data/earnings.csv")

sink(file = "reg01.txt", append = FALSE)
fitted.model <- lm(earnings$earn ~ earnings$height)
print(Sys.time(), quote = FALSE)
print(summary(fitted.model))
closeAllConnections()

# graph the data and the regression line
png("reg01.png") # open a png file
plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
abline(fitted.model, col = "red") # plot the regression line
dev.off() # close the file

# Function to write a .txt file of the summary output for a regression model.
writeOutput <- function(model, filename){
    sink(file = filename, append = FALSE)
    print(Sys.time(), quote = FALSE)
    print(summary(model))
    closeAllConnections()
    return(model)
}

# regression including the male-ness
male.model <- function(){
    fitted.model <- lm(earnings$earn ~ earnings$height + earnings$male)
    writeOutput(fitted.model, 'male.txt')
    png('male.png')
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
    abline(fitted.model, col = "red") # plot the regression line
    dev.off() # close the file
}


# regression including the interaction term between height and male-ness
interaction.model <- function(){
    fitted.model <- lm(earnings$earn ~ earnings$height*earnings$male)
    writeOutput(fitted.model, 'interaction.txt')
    png('interaction.png')
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
    abline(fitted.model, col = "red") # plot the regression line
    dev.off() # close the file
    }

# 
reg04.model <- function(){
    earnings <- subset(earnings, earn>0)
    fitted.model <- lm(log(earnings$earn) ~ earnings$height + earnings$male)
    writeOutput(fitted.model, 'reg04.txt')
    png('rego4.png')
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
    abline(fitted.model, col = "red") # plot the regression line
    dev.off() # close the file
    }

# 
reg05.model <- function(){
    earnings <- subset(earnings, earn>0)
    fitted.model <- lm(log(earnings$earn) ~ earnings$height*earnings$male)
    writeOutput(fitted.model, 'reg05.txt')
    png('reg05.png')
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
    abline(fitted.model, col = "red") # plot the regression line
    dev.off() # close the file
    }

# 
reg06.model <- function(){
    earnings <- subset(earnings, earn>0)
    fitted.model <- lm(log(earnings$earn) ~ log(earnings$height) + earnings$male)
    writeOutput(fitted.model, 'reg06.txt')
    png('reg06.png')
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
    abline(fitted.model, col = "red") # plot the regression line
    dev.off() # close the file
    }

# 
reg07.model <- function(){
    earnings <- subset(earnings, earn>0)
    fitted.model <- lm(log(earnings$earn) ~ log(earnings$height)*earnings$male)
    writeOutput(fitted.model, 'reg07.txt')
    png('reg07.png')
    plot(earnings$height, earnings$earn, xlim = c(40, 90), xlab = "height", ylab = "earnings")
    abline(fitted.model, col = "red") # plot the regression line
    dev.off() # close the file
    }


male.model()
interaction.model()
reg04.model()
reg05.model()
reg06.model()
reg07.model()