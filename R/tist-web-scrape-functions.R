#' Get project URLs for a given country
#' @param country The country to get project URLs for (e.g. "uganda")
#' @param tist_base_url The base URL for the TIST website
#' @return A tibble with project IDs and URLs
project_urls <- function(country,
                         tist_base_url = "https://www.tist.org/i2/") {
  country <- tolower(country)
  country_url <- paste0(tist_base_url, "/", country, ".php")

  uganda_page <- insis_read_html(country_url)

  prjs <- uganda_page |>
    rvest::html_elements(
      xpath = "//a[contains(@href, 'proarea.php?varareaid=')]"
    )

  df <- tibble::tibble(
    project_id = rvest::html_text2(prjs),
    project_url = paste(tist_base_url,
      rvest::html_attr(prjs, "href"),
      sep = "/"
    )
  )

  attr(df, "country") <- country
  return(df)
}

#' Get cluster URLs for a given country
#' @param projects_df A tibble with project IDs and URLs from the
#' `project_urls()` function.
#' @param tist_base_url The base URL for the TIST website
#' @return A tibble with project IDs, cluster IDs, and URLs
cluster_urls <- function(
    projects_df,
    tist_base_url = "https://www.tist.org/i2/") {
  insist_clusters <- function(x) {
    insis_read_html(x) |>
      rvest::html_elements("table") |>
      rvest::html_elements(
        xpath = "//a[contains(@href, 'progclustern.php?varclusterid=')]"
      )
  }

  df <- purrr::pmap(projects_df,
    function(project_id, project_url) {
      x_clusters <- insist_clusters(project_url)
      tibble::tibble(
        project_id = project_id,
        cluster_id = rvest::html_text2(x_clusters),
        cluster_url = paste(
          tist_base_url,
          rvest::html_attr(x_clusters, "href"),
          sep = "/"
        )
      )
    },
    .progress = TRUE
  ) |>
    dplyr::bind_rows()

  attr(df, "country") <- attr(projects_df, "country")
  return(df)
}


group_urls <- function(
    cluster_df,
    tist_base_url = "https://www.tist.org/i2/") {
  insist_groups <- function(cluster_url) {
    group_urls <- cluster_url |>
      insis_read_html() |>
      rvest::html_elements("table") |>
      rvest::html_elements(
        xpath = "//a[contains(@href, 'detgroup.php?groupid=')]"
      )

    gurl <- paste(
      tist_base_url,
      rvest::html_attr(group_urls, "href"),
      sep = "/"
    )
    # return urls with a number (ignores no data groups
    # like: https://www.tist.org/i2//progclustern.php?varclusterid=1100)
    gurl[grepl("\\d$", gurl)]
  }


  gr_vec <- purrr::map(
    cluster_df$cluster_url,
    insist_groups,
    .progress = TRUE
  ) |>
    unlist()

  attr(gr_vec, "country") <- attr(cluster_df, "country")
  return(gr_vec)
}

#' insistently request html from a url
#' @param x a url.
#' @return html
insis_read_html <- purrr::insistently(function(x) {
  rvest::read_html(x)
})


paste_base_url <- function(x, base_url = "https://www.tist.org/i2/", sep = "/") {
  paste(base_url, x, sep = sep)
}
