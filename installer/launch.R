# Starts the FishDiveR app for the standalone Windows app.
#
# Run by the FishDiveR shortcut with the R bundled alongside it:
#   R\bin\x64\Rscript.exe --vanilla app\launch.R
# The app opens in the default web browser and stops a few seconds after its
# last browser tab is closed, or when this window is closed.

# Use only the packages bundled with the app, not any R installed on this
# computer, so other versions can't conflict
.libPaths(.Library)

# Draw the 3D cluster plot in the browser, without opening an R graphics window
options(rgl.useNULL = TRUE)

cat(
  "FishDiveR ", as.character(utils::packageVersion("FishDiveR")), "\n\n",
  "The app is opening in your web browser.\n",
  "Closing its browser tab stops FishDiveR, as does closing this window.\n\n",
  sep = ""
)

FishDiveR::run_app(stop_on_close = TRUE)
