
# Player data

# Spammer
## pi: probability of sending a spam message
## beta_r: average benefit from each spam message that is read
## beta_d: average cost of posting a spam message that is not read
spammer <- c(pi, beta_r, beta_d)

# Spam filter
## epsilon: probability of misclassifying spam as legitimate
## eta: probability of misclassifying legitimate as spam
spam_filter <- c(epsilon, eta)

# Player
## sigma: probability of reading a message (s or l)
## alpha_s: average cost of reading a spam message
## alpha_l: average benefit/cost of reading a legitimate message
##          note: if the user deletes a legitimate message, the cost is -alpha_l, not 0
user <- c(sigma_s, sigma_l, alpha_s, alpha_l)

# Message
## val: actual value of message (spam or legitimate)
## filter_val: what the filter classifies the message as,
##             this is what the player sees
message <- c(val, filter_val)

xi <- alpha_l/alpha_s # how much worse it is for user to miss a legitimate message 
                      # compared to reading a spam message

gamma <- beta_r/beta_d # ratio of spammer's average benefit to cost of a message that is not read