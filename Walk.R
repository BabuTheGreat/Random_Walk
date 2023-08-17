setwd("/Users/Belal/Desktop/STAT 280 project")

#Task 1
  #Creating trajectory function
  AdaWalk <-function()
  {
  #Initialize list
  walk<-list()
  
  #Set origin
  walk[[1]]<-c(0,0)
  
  #Simulate 100 steps
  for (t in 1:100)
  {
    # 1 = move forward, 2 = move right, 3 = move backwards, 4 = move left
    x<- sample(1:4, 1, replace = TRUE, prob= c(0.25, 0.25, 0.25, 0.25))
    
    #If Ada moves forward, increment the "y" value
    if(x==1)
    {
      walk[[t+1]]<- c(walk[[t]][1], walk[[t]][2]+1)
      
    }
    #If Ada moves right, increment the "x" value
    if(x==2)
    {
      walk[[t+1]]<- c(walk[[t]][1]+1, walk[[t]][2])
     
    }
    #If Ada moves backwards, decrement the "y" value
    if(x==3)
    {
      walk[[t+1]]<- c(walk [[t]][1], walk[[t]][2]-1)
    
    }
    #If Ada moves left, decrement the "x" value
    if(x==4)
    {
      walk[[t+1]]<- c(walk[[t]][1]-1, walk[[t]][2])
    
    }
    #If Ada returns back to origin, break out of loop and end walk 
    if(walk[[t+1]][1]==0 && walk[[t+1]][2]==0)
    {
      break
    }
  }
  #Initialize vectors for horizontal and vertical movements
  x<-c()
  y<-c()
  
  #Fill data respectively
  for(i in 2:(length(walk)))
  {
    x<-c(x, walk[[i]][1])
    y<-c(y, walk[[i]][2])
  }
  
  #Create the matrix
  trajectory<-rbind(x,y)
  
  #Return trajectory
  return(trajectory)
  }
  
#Task 2
  
  #Create plot function to keep plots consistent and decrease lines of code
  plotWalk<-function(trajectory)
  {
      #Same amount of steps in order to plot 
      steps<-length(trajectory)/2
      
      #Plot two lines, one for vertical and the other for horizontal movements. 
      plot(1:steps, trajectory[1,], type = "l", lty=1 ,xlab = "Steps", ylab = "Trajectory", ylim = c(min(trajectory), max(trajectory)))
      lines(1:steps, trajectory[2,], type="l",lty=2, col= "red")
      
      #Legend to understand different lines
      legend(x = "topleft",inset=c(-0.12,-0.2),cex = 0.75, lty = c(1,2), text.font = 2, 
             col= c("black","red"),text.col = "black", 
             legend=c("Horizontal Movement", "Vertical Movement"))
      title("Ada's Trajectory", cex.main = 2,   font.main= 4, col.main= "Brown")
      
      #Dynamic subtitle to give more context
      if(length(trajectory)/2 == 100)
        {
          mtext(paste("Ada has not went back to the origin"), side = 3, line = 0, col = "azure4")
        }else
      {
        mtext(paste("Ada went back to the origin after ", length(trajectory)/2, " steps", sep = ""), side = 3, line = 0, col = "azure4")
      }
  }
  
  #Plotting 4 examples
  par(mfrow=c(1,1), xpd=TRUE)
  
  #Get trajectory
  trajectory<-AdaWalk()
  
  #Plot
  plotWalk(trajectory)
  
  #Get trajectory
  trajectory<-AdaWalk()
  
  #Plot
  plotWalk(trajectory)
  
  #Get trajectory
  trajectory<-AdaWalk()
  
  #Plot
  plotWalk(trajectory)
  
  #Get trajectory
  trajectory<-AdaWalk()
  
  #Plot
  plotWalk(trajectory)
  
#Task 3
  
  #Set variable to count how many times Ada walks back to the origin in a span of 10000 walks
  count<-0
  
  #Begin Monte Carlo simulation
  for(i in 1:10000)
  {
    #Get walking trajectory
    trajectory<-AdaWalk()
    
    #Save number of steps
    steps<-length(trajectory)/2
    
    #Iterate through trajectory 
    for(j in steps)
    {
      
      #If Ada walked backed to the origin, increment count variable
      if(trajectory[,j][1]==0 && trajectory[,j][2]==0)
      {
        count<-count+1
      }
    }
  }
  #Get probability by dividing count by number of experiments
  probabilty_origin<-count/10000
  
  cat("The estimated probability that Ada comes back to the origin is", probabilty_origin)
  
#Task 4
  
  #Create vector to save number of steps needed to reach origin
  steps_needed<- c()
  
  #Begin Monte Carlo simulation
  for(i in 1:10000)
  {
    
    #Get walking trajectory
    trajectory<-AdaWalk()
    
    #Save number of steps
    steps<-length(trajectory)/2
    
    #Iterate through trajectory 
    for(j in steps)
    {
      #If Ada walked back to the origin, added the number of steps to the vector
      if(trajectory[,j][1]==0 && trajectory[,j][2]==0)
      {
        steps_needed<-c(steps_needed, j)
      }
    }
  }
  
  #Get average by finding the sum of vector and dividing it by the number of experiments
  average_of_needed_steps<-sum(steps_needed)/10000
  
  cat("The estimated average number of steps needed by Ada to return to the origin is", average_of_needed_steps)
  
  
  
  
  
  
  