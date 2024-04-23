## --- for testing specific issue: ---
# ug_group_urls_test <- ug_group_urls[1:100]
# # ug_group_urls_test <- "https://www.tist.org/i2//detgroup.php?groupid=16396"
# attr(ug_group_urls_test, "country") <- "uganda"
# ug_all_test <- get_all_group_tabs(ug_group_urls_test)
# ug_all_test[[2]]$geometry
# ply_test <- clean_up_spat(ug_all_test[[2]])
# pnt_test <- clean_up_spat(ug_all_test[[1]])

# mapview::mapview(ply_test) +
#   mapview::mapview(pnt_test)
