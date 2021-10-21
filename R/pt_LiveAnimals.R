#' Function to estimate the probability of importing a case via live animals
#' 
#' @param pars List of parameters for the model
#' @export

pt_LiveAnimals <- function(pars) {
  with(pars, {
    # probability of test not detected
    pit <- pin(p0, Se, Sp)
    # Origin (P1)
    P1 <- c(ifelse(Vacc == 1, (1 - pv*pe), 1),
            ifelse('Origin' %in% T_place & Testing == 1, pit, 1),
            ifelse('Origin' %in% Q_place & Quarantine == 1, 1 - pq, 1)) %>% 
      prod()
    # Border (P2)
    P2 <- c(ifelse('Border' %in% T_place & Testing == 1, pit, 1),
            ifelse('Border' %in% Q_place & Quarantine == 1, 1 - pq, 1)) %>% 
      prod()
    # Transport (P3)
    # P3 <- 1
    P3 <- (1 - pmt)
    # Destination (P4)
    P4 <- c(ifelse('Destination' %in% T_place & Testing == 1, pit, 1),
            ifelse('Destination' %in% Q_place & Quarantine == 1, 1 - pq, 1)) %>% 
      prod()
    
    pt <- p0*P1*P2*P3*P4
    
    return(c(pt = pt, P1 = P1, P2 = P2, P3 = P3, P4 = P4))
  })
}