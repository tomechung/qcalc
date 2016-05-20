##qcalc function##
qcalc = function(dat,list)
{
  x = subset(dat,select=list)
  n = nrow(x)
  shared = numeric(n*(n-1))
  perratee = numeric(n)
  count = 1
  for(i in 1:n)
  {
    v1 = x[i,]               #ith row becomes a vector
    v1 = v1[!is.na(v1)]      #strips vector of NAs
    perratee[i] = length(v1) #number of raters per ratee i
    for(j in 1:n)
    {
      sum = 0
      v2 = x[j,]             #jth row becomes a vector
      v2 = v2[!is.na(v2)]    #strips vector of NAs
      if(i != j)
      {
        for(a in v1)
        {
          for(b in v2)
          {
            if(a == b)
            {
              sum = sum + 1
            }
          }
        }
        shared[count] = sum/(length(v1)*length(v2))
        count = count + 1
      }
    }
  }
  return((1/mean(perratee)) - sum(shared)/(n*(n-1)))
}
  
##Putka et al. (2008) Example Data##
#fully crossed
#Example1 <- data.frame(RateeID=c(1,2,3),
#                       Rater1=c(1,1,2),
#                       Rater2=c(2,2,2))
#semi crossed
#Example2 <- data.frame(RateeID=c(1,2,3),
#                       Rater1=c(1,2,3),
#                       Rater2=c(2,3,4))
#raters nested within ratees
#Example3 <- data.frame(RateeID=c(1,2,3),
#                       Rater1=c(1,3,5),
#                       Rater2=c(2,4,6))
#mock data
#Example4 <- data.frame(RateeID=c(1,2,3,4,5,6,7,8,9,10),
#                       Rater1=c(8,5,3,1,13,6,12,13,6,11),
#                       Rater2=c(7,9,4,6,2,10,2,8,5,8))
#
#qcalc(Example4,c("Rater1","Rater2"))

