#------------------------------------------------------------------------------*
# Render all reports
#*-----------------------------------------------------------------------------*

# Load used packages
library(package = rmarkdown)

# Render reports
render(input = "content/main.Rmd", output_file = "index.html", output_dir = "docs")
render(input = "content/respi.Rmd", output_file = "respi.html", output_dir = "docs")
render(input = "content/diarrea.Rmd", output_file = "diarrea.html", output_dir = "docs")
render(input = "content/febril.Rmd", output_file = "febril.html", output_dir = "docs")

# End of script
