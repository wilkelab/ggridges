library(vdiffr)
data(GvHD)
dataDir <- system.file("extdata",package="flowWorkspaceData")
gs_dir <- list.files(dataDir, pattern = "gs_bcell_auto",full = TRUE)
# set_default_backend("tile")
if(get_default_backend()=="tile")
{
  tmp <- tempfile()
  convert_backend(gs_dir, tmp)
  gs_dir <- tmp
}
gs <- load_gs(gs_dir)
