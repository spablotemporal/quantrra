# plot score ----------
ra_plotScore <- function(x, main = "", brks = NULL, range = NULL){
  m <- x %>% quantile()
  
  fig <- plot_ly(
    domain = list(x = c(0, 10), y = c(0, 10)),
    value = m[3],
    title = list(text = main),
    type = "indicator",
    mode = "gauge+number",
    # This following part is to add a threshold and the difference to it
    # mode = "gauge+number+delta",
    # delta = list(reference = 380),
    gauge = list(
      axis =list(range = list(range[1], range[2])),
      bar = list(color = "#205040"),
      steps = list(
        list(range = c(brks[1], brks[2]), color = "#D0FFD0"),
        list(range = c(brks[2], brks[3]), color = "#FFFFD0"),
        list(range = c(brks[3], brks[4]), color = "#FFD0D0"), 
        list(range = c(m[2], m[4]), thicness=0.5, color = "#00000000", line = list(color="#505020", width=4))
      )
    )
  ) 
  fig <- fig %>%
    layout(margin = list(l=20,r=30))
  
  fig 
}