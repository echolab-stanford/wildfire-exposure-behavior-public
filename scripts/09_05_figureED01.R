panel_a = image_read_pdf(file.path(path_github, "figures/figureED01a.pdf"))
panel_b = image_read_pdf(file.path(path_github, "figures/figureED01b.pdf"))

panel_a = image_trim(panel_a)
panel_a = image_scale(panel_a, "x1500")
panel_a = image_border(panel_a, "none", "x50")
panel_a = image_annotate(panel_a, "a", font = "Arial", size = 12, weight = 700)

panel_b = image_trim(panel_b)
panel_b = image_scale(panel_b, "x1500")
panel_b = image_border(panel_b, "none", "x50")
panel_b = image_annotate(panel_b, "b", font = "Arial", size = 12, weight = 700)

fig = image_append(c(panel_a, panel_b))

image_write(fig, path = file.path(path_github, "figures/figureED01.pdf"), format = "pdf")
