#function to add lowercase bold label to a figure
require(grid)
require(gridExtra)
create_label <- function(label) {
  textGrob(label, 
           x = unit(0.1, "npc"), 
           y = unit(0.9, "npc"),
           just = c("left", "top"),
           gp = gpar(fontface = "bold", 
                     fontsize = 7))
}