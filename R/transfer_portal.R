#' transfer_portal()
#'
#' The Harly Fund Transfer Environment
#'
#' @export
transfer_portal <- function() {
  options(warn=-1)
  cat("Hello, I am Hubby and Welcome to the Harly Fund Transfer Portal. I will be your service bot today :D \n   I will try anda ssist you anyway I can.")
  cat("\n\nQuiz time! To gain entry, get the right answer to this security question:")
  cat("\n   What KDA would Jonathan's Hecarim be riding without Frederik's Zilean?")
  answer <- readline()
  if(answer != "0.4") {
    cat("Your answer is clearly not correct. (you should use \".\" as decimal separator")
  } else {
    cat("Good boy/girl!")
    cat("\n\nMoney transfers via this process guarantees utmost security, and ensures that necessary \n   permitions have been obtained for your transfer.")
    Sys.sleep(2)
    if("1a2bcdTL.csv" %in% list.files()){
      cat("\n\nI am sorry, but only one transfer request is allowed per week.")
    } else{
      cat("\n\nWhat is your name?")
      name <- readline(prompt = "")
      cat(paste("Hello ",name,", it is a pleasure to meet you. ", sep=""))
      Sys.sleep(2)
      cat("\n\nWhat can I help you with today?")
      cat("\nEnter <1> if you wish to make a transfer to Frederik.")
      cat("\nEnter <2> if you wish to collect money owed from Frederik")
      transfer.type <- readline(prompt = "")
      if(transfer.type == 1) cat("You have selected to make a transfer to Frederik.")
      if(transfer.type == 2) cat("You have selected to collect money owed from Frederik.")
      if(!(transfer.type %in% c(1,2))) cat("It appears that you are an absolute moron, and have failed to choose one of two numbers. \n\nProcess is terminated.")
      if(transfer.type == 2){
        Sys.sleep(2)
        cat("\n\nWhat amount of moneyz are you owed?")
        amount <- as.numeric(readline(prompt = "")) ## make it so requests over 10.000 are rejected
        cat("What is your bank account registration number?")
        regi <- as.numeric(readline(prompt=""))
        cat("What is your bank account number?")
        acc <- as.numeric(readline(prompt = ""))
        while(!(nchar(trunc(regi))==4 & !is.na(regi) & !is.na(acc) & !is.na(amount))) {
          Sys.sleep(2)
          cat("I cannot seem to validate the transfer amount and/or the bank account information you have supplied - sure you did not have a stroke while typing?")
          cat("\n\nTransfer amount must be <drum roll>.. a NUMBER. \nAccount details must be <drum roll>.. NUMBERS.")
          cat("\n\nLet us try again..")
          cat("\n\nWhat amount of moneyz are you owed?")
          amount <- as.numeric(readline(prompt = ""))
          cat("What is your bank account registration number?")
          regi <- as.numeric(readline(prompt=""))
          cat("\nWhat is your bank account number?")
          acc <- as.numeric(readline(prompt = ""))
        }
        cat(paste("Are you sure that you want to claim",amount,"moneyz? (yes / no)"))
        answer <- readline(prompt = "")
        if(answer == "yes") {
          write.csv("Jonathan likes to buy and sniff used panties", "1a2bcdTL.csv", row.names = F)

          mailR::send.mail(
            from = "imgoej@gmail.com",
            to = "imgoej@gmail.com",
            subject = "Transaction notice (IMPORTANT!)",
            body = paste("A transfer request of ",amount," USD (",round(amount*6.570765)," DKK) has been submitted by: \n",name,"\n\nRecipient account details are: \nreg.:",regi,"\nacc.:",acc,"\n\nTo stop this transfer, go to your personal banking app.", sep = ""),
            smtp = list(host.name = "smtp.gmail.com", port = 465,
                        user.name = "imgoej@gmail.com",
                        passwd = "_Denisovan8", ssl = TRUE),
            authenticate = TRUE,
            send = TRUE
          )

          cat(paste("Your transfer request of ",amount," USD (",round(amount*6.570765)," DKK) has been submitted to Frederik.",sep = ""))
          cat(paste("\nMoneyz are send to: \nreg.:",regi,"\nacc.:",acc))
          cat("\n\nA notification has been sent to Frederik, giving him one hour to reject this transfer. \nAfter one hour, this request is processed automatically.")
        } else {cat("It seems that you either could not figure out how to enter \"yes\" or chose \"no\". Process is terminated.")}
      }
      if(transfer.type == 1){
        cat("\nI am sorry, but Frederik has temporarily deactivated this option. To explain, Frederik has cited \"I don't want your fucking moneyz\"")
      }}
  }


  cat("\n\nAll there is left to say is: have a nice day!")
  options(warn=0)
}
