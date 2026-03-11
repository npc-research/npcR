pull_site_info = function(url) {
  #' Function for getting info on if a link is still valid or not
  #'
  #' @param url A class of "string" that is used to search through your default browser
  #'
  #' @return list with info about https request
  #' @export
  #'
  #' @examples
  #'
  #' pull_site_info("https://npcresearch.com/")
  #' pull_site_info("https://httpbin.org/status/200")
  #' pull_site_info("https://httpbin.org/redirect/1")
  #' pull_site_info("https://httpbin.org/status/302")
  #' pull_site_info("https://httpbin.org/status/401")
  #' pull_site_info("https://httpbin.org/status/403")
  #' pull_site_info("https://httpbin.org/status/404")
  #' pull_site_info("https://httpbin.org/status/429")
  #' pull_site_info("https://github.com")
  #'
  #' tribble(
  #'  ~name, ~link,
  #'  "npc", "https://npcresearch.com/",
  #'  "200", "https://httpbin.org/status/200",
  #'  "401", "https://httpbin.org/status/401",
  #'  "403", "https://httpbin.org/status/403",
  #'  "404", "https://httpbin.org/status/404"
  #'  ) %>%
  #'  mutate(info = map(link, pull_site_info)) %>%
  #'  tidyr::unnest_wider(info)

  resp <- tryCatch(
    request(url) |>
      req_method("GET") |>
      req_error(is_error = function(resp) FALSE) |>
      req_options(followlocation = FALSE) |>
      req_perform(),
    error = function(e) return(NULL)
  )

  if (is.null(resp)) {
    return(list(
      status = NA,
      exists = FALSE,
      possible_login = NA,
      possible_paywall = NA
    ))
  }

  status <- resp_status(resp)

  # Some responses have empty bodies
  html <- tryCatch(resp_body_string(resp), error = function(e) "")

  response_types =
    tribble(
      ~ Status, ~ Meaning,
      200, "Page exists",
      301, "Redirect",
      302, "Redirect",
      401, "Login required",
      403, "Forbidden",
      404, "Page does not exist",
      429, "Rate"
    )

  status_meaning = response_types %>% filter(Status == status) %>% pull(Meaning)

  list(
    status = status,
    status_meaning = status_meaning,
    exists = status == 200,
    possible_login = grepl("login|sign in", html, ignore.case = TRUE),
    possible_paywall = grepl("subscribe|subscription|paywall", html, ignore.case = TRUE)
  )
}
