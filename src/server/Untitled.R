library(circlize)

# Your data as a data frame
funding_data <- data.frame(
  source = c("Grant_A", "Grant_A", "Grant_A", "Grant_B", "Grant_B", 
             "Donation_1", "Donation_1", "Investment_X", "Investment_Y"),
  expense = c("Salaries", "Research", "Equipment", "Salaries", "Operations",
              "Research", "Travel", "Equipment", "Operations"),
  amount = c(5000, 3000, 2000, 4000, 1500, 2500, 1000, 3500, 2000)
)

# Add unallocated amounts for each source
unallocated <- data.frame(
  source = c("Grant_A", "Grant_B", "Donation_1", "Investment_X", "Investment_Y"),
  unallocated = c(5000, 2000, 1500, 1000, 3000)  # Empty portions
)

# Get all unique sectors
all_sources <- unique(funding_data$source)
all_expenses <- unique(funding_data$expense)
all_sectors <- c(all_sources, all_expenses)

# Create empty matrix
n <- length(all_sectors)
mat <- matrix(0, nrow = n, ncol = n)
rownames(mat) <- colnames(mat) <- all_sectors

# Fill in the allocations (source → expense)
for(i in 1:nrow(funding_data)) {
  mat[funding_data$source[i], funding_data$expense[i]] <- funding_data$amount[i]
}

# Fill in unallocated amounts (self-links)
for(i in 1:nrow(unallocated)) {
  mat[unallocated$source[i], unallocated$source[i]] <- unallocated$unallocated[i]
}

print(mat)

# Hide self-links
link.visible <- mat > 0
diag(link.visible) <- FALSE

# Create gaps to separate funding sources from expenses
n_sources <- length(all_sources)
gap_vector <- rep(3, n)
names(gap_vector) <- all_sectors
gap_vector[all_sources[n_sources]] <- 15  # Larger gap after last funding source
gap_vector[all_expenses[length(all_expenses)]] <- 15  # Larger gap after last expense

circos.par(gap.after = gap_vector)

# Assign colors
source_colors <- rainbow(length(all_sources))
expense_colors <- heat.colors(length(all_expenses))
grid.col <- c(setNames(source_colors, all_sources), 
              setNames(expense_colors, all_expenses))

chordDiagram(mat,
             grid.col = grid.col,
             annotationTrack = "grid",
             preAllocateTracks = list(track.height = 0.3),
             self.link = 1,
             link.visible = link.visible)

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1],
              CELL_META$sector.index,
              facing = "clockwise", niceFacing = TRUE, 
              adj = c(0, 0.5), cex = 0.6)  # Smaller text for many sectors
}, bg.border = NA)

circos.clear()