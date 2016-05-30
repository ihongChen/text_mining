##  oop 

x<-c(1,2,3)
y<-c(1,3,8)
lmout<-lm(y~x)
class(lmout)
lmout

## counting employee
j <- list(name="Joe", salary=55000, union=T)
class(j) <- "employee"
attributes(j)
names(j)

print.employee <- function(wrkr) {
  cat(wrkr$name,"\n")
  cat("salary",wrkr$salary,"\n")
  cat("union member",wrkr$union,"\n")
}
# cat('hello','this',"\n")
print.employee(j)
j


### using inheritance

k <- list(name="Kate", salary= 68000, union=F, hrsthismonth= 2)
class(k) <- c("hrlyemployee","employee")



############################################
## set class
############################################
### Definition of an object BMI
setClass("BMI", representation(weight="numeric", size="numeric"))

setMethod("show", "BMI", 
            function(object){cat("BMI=",object@weight/(object@size^2)," \n ")})

### Creation of an object for me, and posting of my BMI
myBMI <- new("BMI",weight=85,size=1.84)


temp_f = function(object){
  cat('bmi=',object@weight/(object@size^2),'\n')
}


### Creation of an object for her, and posting of her BMI
herBMI <- new("BMI",weight=62,size=1.60)

### Object programming, control
setValidity("BMI",
               function(object){if(object@size<0 || object@weight<0){return("negative Size/Weight")}else{return(TRUE)}}
               )

new("BMI",weight=85,size=1.84)
