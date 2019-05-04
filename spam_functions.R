reset_u_list <- function(n, sigma_s, sigma_l, alpha_s, alpha_l) {
  u_list <- list()
  for (u in 1:n) {
    u_list[[u]] <- vector("list", 4)
    names(u_list[[u]]) <- c("id", "sigma_s", "sigma_l", "points")
    u_list[[u]]$id <- u
    u_list[[u]]$sigma_s <- sigma_s # replace with distribution
    u_list[[u]]$sigma_l <- sigma_l # replace with distribution
    u_list[[u]]$alpha_s <- alpha_s # replace with distribution
    u_list[[u]]$alpha_l <- alpha_l # replace with distribution
    u_list[[u]]$points <- 0
  }
  return(u_list)
}

generate_messages <- function(n, pi_s, epsilon, eta) {
  messages <- data.frame(spam_probs = runif(n),
                         filter_probs = runif(n))
  messages <- messages %>% 
    mutate(val = ifelse(spam_probs <= pi_s, "spam", "legitimate")) %>% 
    mutate(filter_val = ifelse(val == "spam" & filter_probs < epsilon, "legitimate",
                               ifelse(val == "legitimate" & filter_probs < eta, "spam", val)),
           read_status = NA)
  return(messages)
}
calculate_points <- function(spammer, u_list, messages) {
  for (u in 1:n) {
    rn <- runif(1)
    messages[u, "read_status"] <- ifelse(messages[u, "filter_val"] == "spam" & rn < u_list[[u]]$sigma_s, "read",
                                         ifelse(messages[u, "filter_val"] == "legitimate" & rn < u_list[[u]]$sigma_l, "read", "unread"))
    u_list[[u]]$points <- ifelse(messages[u, "val"] == "spam" & messages[u, "read_status"] == "read", -u_list[[u]]$alpha_s,
                                 ifelse(messages[u, "val"] == "legitimate" & messages[u, "read_status"] == "read", u_list[[u]]$alpha_l,
                                        ifelse(messages[u, "val"] == "legitimate" & messages[u, "read_status"] == "unread", -u_list[[u]]$alpha_l, 0)))
  }
  
  u_df <- do.call(rbind.data.frame, u_list)
  spammer$points <- spammer$beta_r*sum(messages$val == "spam" & messages$read_status == "read") - spammer$beta_d*sum(messages$val == "spam" & messages$read_status == "unread")
  points_users <- sum(u_df$points)
  
  return(list(spammer = spammer,
              u_list = u_list,
              messages = messages,
              u_df = u_df,
              points_users = points_users))
}
