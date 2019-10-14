bulls_cows_game<-function(){
  generate_computer_vector<-function(){
    computer_choice<-sample(1:10,4)
    return(computer_choice)
  }#generate the computer vector
  
  get_guess<-function(guess){
    print(paste("You still have",10-guess,"guesses."))
    user_choice<-as.integer(unlist(strsplit(readline("Please enter four numbers > "), " ")))
    return(user_choice)
  }
  
  number_bulls_and_cows<-function(computer_vector,user_guess){
    cows<-0
    number_bulls<-function(computer_vector,user_guess){
      bulls<-0
      for(i in 1:4){
        if(computer_vector[i]==user_guess[i]){
          bulls<-bulls+1
        }
      }
      return(bulls)
    }#calculate the bulls
    bulls<-number_bulls(computer_vector,user_guess)
    number_cows<-function(computer_vector,user_guess,bulls){
      cows<-length(intersect(computer_vector,user_guess))-bulls
      return(cows)
    }#calculate the cows
    cows<-number_cows(computer_vector,user_guess,bulls)
    return(c(bulls,cows))
  }
  
  do_response<-function(bulls,cows){
    if(bulls==4){
      print(paste("You have won, you took",  guess, "guesses to get the right answer."))
      end_game<-1 #note that the game is over and user win
    }
    else{
      print(paste(bulls,"bulls and",cows,"cows. "))
      end_game<-0
    }
    return(end_game)
  }#print the computer’s response to the guess
  
  guess=0
  end_game<-0
  computer_vector<-generate_computer_vector()#store the computer vector
  while(guess<10){
    incorrect_count<-0 #count the incorrect input
    correct_guess<-0 #judge whether the input is correct
    while(incorrect_count<3&correct_guess==0){
      user_guess<-get_guess(guess)
      if(length(user_guess)==4&length(unique(user_guess))==4){
        correct_guess<-1#correct input format
      } 
      else{
        incorrect_count<-incorrect_count+1
        print("Incorrect inputs, inputs should be 4 different digits from 1 to 10")
      }
    }
    if(incorrect_count==3){
      print("Three incorrect inputs, game over.")
      break
    }
    guess<-guess+1
    bulls_cows<-number_bulls_and_cows(computer_vector,user_guess)
    end_game<-do_response(bulls_cows[1],bulls_cows[2])
    if(guess==10&end_game==0){
      print(paste("Sorry you did not get the right answer, the correct answer is",computer_vector)) 
      #end game when user guesses 10 times incorrectly
    }
    if(end_game==1){break}
  }
}