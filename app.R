# Launch the ShinyAPP (Do not remove this content)
# To deploy, run: rsconnect::deployApp()
# Or use the blue button on the top of this file.

pkgload::load_all(export_all = FALSE, helpers = FALSE,
                  attach_testthat = FALSE)
options("golem.app.prod" = TRUE)

# Uncomment below line for QA release
Sys.setenv("R_CONFIG_ACTIVE" = "default")
ShareMarketAnalysis::run_app()
