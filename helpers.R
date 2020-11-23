get_data <-
  function(path = "kennzahlen-neuwagenflotte-metadata.xml") {
    new_cars_meta <- xml2::read_xml(path) %>%
      xml2::xml_child(1) %>%
      xml2::as_list()

    csv_file_url <-
      new_cars_meta$distribution$Distribution$accessURL %>%
      attr("resource")

    readr::read_csv(csv_file_url) %>%
      janitor::clean_names()
  }

descriptive_categories <- list(
  t1 = "new_car_count", # Number of new passenger cars
  t2 = "percentage_awd", # Share of all-wheel-drive vehicles
  t3 = "avg_fuel_consumption", # Average fuel consumption
  t4 = "avg_co2_emissions", # Average CO2 emissions
  t5 = "avg_weight", # Average empty weight
  t6 = "percentage_electric", # Share of electric vehicles
  t7 = "avg_price" # Average vehicle price, according to the FSO
)

get_canton_codes <-
  function(path = "https://en.wikipedia.org/wiki/Data_codes_for_Switzerland") {
    xml2::read_html(path) %>%
      rvest::html_table() %>%
      purrr::pluck(3) %>%
      janitor::clean_names() %>%
      select(two_letter_abbr, numeric)
  }

tidy_data <- function(data) {
  data %>%
    dplyr::left_join(get_canton_codes(), by = c("kanton" = "numeric")) %>%
    mutate(kanton = two_letter_abbr) %>%
    select(- two_letter_abbr) %>%
    tidyr::pivot_longer(-kanton,
                        names_to = "year_and_category",
                        values_to = "value") %>%
    dplyr::mutate(
      year = stringr::str_sub(year_and_category, start = 2, end = 5),
      category = stringr::str_sub(year_and_category, start = 6, end = 7)
    ) %>%
    dplyr::select(-year_and_category) %>%
    dplyr::mutate(
      year = as.integer(year),
      category = unlist(descriptive_categories[category])
    ) %>%
    tidyr::pivot_wider(names_from = category,
                       values_from = value)
}

plot_comparison <- function(data,
                            selected_year,
                            selected_cantons,
                            selected_metric) {
  data %>%
    dplyr::filter(year == as.integer(selected_year),
                  kanton %in% selected_cantons) %>%
    ggplot2::ggplot(ggplot2::aes(x = kanton,
                                 y = .data[[selected_metric]],
                                 fill = kanton)) +
    ggplot2::geom_col() +
    ggplot2::labs(
      title = paste("Newly-registerd cars:", snakecase::to_sentence_case(selected_metric)),
      subtitle = paste("For selected cantons in", selected_year)
    ) +
    ggplot2::xlab(snakecase::to_sentence_case("kanton")) +
    ggplot2::ylab(snakecase::to_sentence_case(selected_metric)) +
    hrbrthemes::theme_ipsum() +
    ggplot2::theme(legend.position = "none")
}
