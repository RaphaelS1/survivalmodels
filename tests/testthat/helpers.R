library(survival)

skip_on_os("windows")
set.seed(1)
np <- reticulate::import("numpy")
np$random$seed(1L)
torch <- reticulate::import("torch")
torch$manual_seed(1L)
