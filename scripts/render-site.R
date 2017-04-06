#------------------------------------------------------------------------------*
# Render all reports
#*-----------------------------------------------------------------------------*

# Load used packages
library(package = rmarkdown)

# Render reports
render(input = "content/main.Rmd", output_file = "../docs/index.html")
render(input = "content/respi.Rmd", output_file = "../docs/respi.html")
render(input = "content/diarrea.Rmd", output_file = "../docs/diarrea.html")
render(input = "content/febril.Rmd", output_file = "../docs/febril.html")

# End of script
