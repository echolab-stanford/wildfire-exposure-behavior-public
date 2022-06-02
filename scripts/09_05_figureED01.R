#-------------------------------------------------------------------------------
# Assemble Figure ED 1
# Written by: Jessica Li
#-------------------------------------------------------------------------------
# Load panels
panel_a = image_read_pdf(file.path(path_github, "figures/raw/figureED01a.pdf"))
panel_b = image_read_pdf(file.path(path_github, "figures/raw/figureED01b.pdf"))

# Trim and scale
panel_a = image_trim(panel_a)
panel_a = image_scale(panel_a, "x1500")
panel_a = image_border(panel_a, "none", "x50")
panel_a = image_annotate(panel_a, "a", font = "Arial", size = 12, weight = 700)

panel_b = image_trim(panel_b)
panel_b = image_scale(panel_b, "x1500")
panel_b = image_border(panel_b, "none", "x50")
panel_b = image_annotate(panel_b, "b", font = "Arial", size = 12, weight = 700)

# Combine
fig = image_append(c(panel_a, panel_b))

# Save
image_write(fig, path = file.path(path_github, "figures/raw/figureED01.pdf"), format = "pdf")
