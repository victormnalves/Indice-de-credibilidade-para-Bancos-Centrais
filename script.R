library(rbcb) # dados para o Banco Central do Brasil
library(tidyverse) # manipulação e visualização de dados
library(lubridate) # manipulação de datas
library(zoo)
library(rvest)
library(xml2)
library(paletteer)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # defininindo diretório como pasta onde o arquivo está
`%not_in%` <- purrr::negate(`%in%`) # crianção de operador para negação de pertencimento

brazil_target <- readxl::read_xlsx('Dados/brazil_target.xlsx') %>% 
  select(-c(Norma, Data, 
            `Tamanho do intervalo +/- (p.p.)`, `Inflação efetiva (Variação do IPCA, %)`)) %>% 
  mutate(
    Ano = case_when(Ano == '2003*' ~ '2003',
                    Ano == '2004*' ~ '2004',
                    TRUE ~ Ano)) %>% 
  separate_rows(`Meta (%)`, `Intervalo de tolerância (%)`, convert = TRUE, sep = '\n') %>% 
  separate( 
    col = `Intervalo de tolerância (%)`, 
    into = c('lower', 'upper'), 
    sep = '-' 
  ) %>% 
  rename(meta = `Meta (%)`,
         year = Ano) %>% 
  mutate(meta = str_replace(meta, ",", "."),
         lower = str_replace(lower, ",", "."),
         upper = str_replace(upper, ",", ".")) %>% 
  mutate(meta = iconv(meta, 'utf-8', 'ascii', sub=''),
         lower = as.numeric(lower),
         upper = as.numeric(upper),
         year = as.numeric(year))

brazil_forecast <- get_annual_market_expectations('IPCA') %>% 
  filter(base == 0) %>% 
  mutate(month = month(ymd(date)), 
         year = year(ymd(date)),
         current = case_when(year == reference_date ~ 1, 
                             TRUE ~ 0)) %>%
  filter(current == 1) %>% 
  group_by(month, year, reference_date) %>% 
  summarise(median = median(median)) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep='-'), '%Y-%m-%d')) 

data_brazil <- right_join(brazil_target, brazil_forecast, by = 'year') %>% 
  select(-c(year, month, reference_date)) %>% 
  rename(date = date,
         expectative = median,
         current = meta) %>% 
  mutate(country = 'Brasil', 
         regime = 'interval',
         date = lubridate::as_date(date, format = '%Y-%m-%d'))

canada_target <- read.csv('Dados/canada_target.csv', 
                          skip = 19) %>% 
  rename(lower = STATIC_ATABLE_CPILL,
         upper = STATIC_ATABLE_CPIHL,
         current = STATIC_ATABLE_V41690973) %>% 
  mutate(date = as.yearqtr(date,
                           format = "%Y-%m-%d"))

canada_forecast <- read.csv('Dados/canada_forecast.csv', 
                            skip = 27, na.strings = "") %>% 
  select(date, INDINF_EXPECTTWOTHREE_Q) %>% 
  mutate(date = str_replace(date, "Q", " Q"),
         date = as.yearqtr(date),
         year = year(date),
         reference_date = year + 2) %>% 
  rename(expectative = INDINF_EXPECTTWOTHREE_Q)

data_canada <- right_join(canada_target, canada_forecast, by = 'date') %>% 
  separate(date, into = c('year', 'quarter'), sep = ' ') %>% 
  mutate(
    month = case_when(quarter == 'Q1' ~ '01/01',
                      quarter == 'Q2' ~ '01/04',
                      quarter == 'Q3' ~ '01/07',
                      quarter == 'Q4' ~ '01/12',
                      TRUE ~ quarter),
    country = 'Canada',
    regime = 'interval') %>% 
  unite(date, month, year, sep = "/") %>% 
  select(-c(quarter, reference_date)) %>% 
  mutate(date = lubridate::as_date(date, format = '%d/%m/%Y')) %>% 
  drop_na()

argentina_forecast <- readxl::read_xlsx('Dados/argentina_forecast.xlsx', 
                                        sheet = 2,
                                        skip = 1) %>% 
  select(-c(Promedio:`Cantidad de participantes`)) %>% 
  filter(Variable == 'Precios minoristas (IPC nivel general; INDEC)',
         Referencia %in% c('var. % i.a.; dic-16', 'var. % i.a.; dic-17', 
                           'var. % i.a.; dic-18', 'var. % i.a.; dic-19',
                           'var. % i.a.; dic-20', 'var. % i.a.; dic-21',
                           'var. % i.a.; dic-22', 'var. % i.a.; dic-23',
                           'var. % i.a.; dic-24'),
         Período %in% c('2016', '2017', '2018', '2019',
                        '2020', '2021', '2022', '2023', '2024')) %>% 
  select(-c(Variable, Referencia)) %>% 
  rename(date = `Fecha de pronóstico`,
         reference_date = Período,
         expectative = Mediana) %>% 
  mutate(year = year(ymd(date)),
         current = case_when(year == reference_date ~ 1, 
                             TRUE ~ 0)) %>%
  filter(current == 1) %>% 
  select(-c(current, reference_date))

argentina_target <- tibble(year = c(2019, 2020, 2021, 2022, 2023, 2024), 
                           current = c(17, 13, 9, 5, 5, 5),
                           upper = c(NA, NA, NA, NA, NA, NA),
                           lower = c(NA, NA, NA, NA, NA, NA))

data_argentina <- right_join(argentina_target, argentina_forecast, by = 'year') %>% 
  select(-year) %>% 
  mutate(country = 'Argentina', 
         regime = 'target')

chile_forecast <- readxl::read_xlsx('Dados/chile_forecast.xlsx',
                                    skip = 3) %>% 
  filter(...1 %not_in% c('En el mes', 'El próximo mes', 'En 23 meses (var. 12 meses)',
                         'En 35 meses (var. 12 meses)', 'En 11 meses (var. 12 meses)'))

chile_forecast <- chile_forecast[-c(20:109, 1),]

chile_forecast <- chile_forecast %>% 
  pivot_longer(cols = -...1,
               names_to = 'month_year',
               values_to = 'expectative') %>% 
  rename(reference_date = ...1) %>% 
  filter(expectative != '.') %>% 
  separate(month_year, into = c('month', 'year'), sep = -4) %>% 
  separate(reference_date, into = c('reference_month', 'reference_year'), sep = ' ') %>% 
  mutate(
    current = case_when(reference_year == year ~ 1,
                        TRUE ~ 0),
    month = case_when(month == 'Enero' ~ '01-01',
                      month == 'Febrero' ~ '02-01',
                      month == 'Marzo' ~ '03-01',
                      month == 'Abril' ~ '04-01',
                      month == 'Mayo' ~ '05-01',
                      month == 'Junio' ~ '06-01',
                      month == 'Julio' ~ '07-01',
                      month == 'Agosto' ~ '08-01',
                      month == 'Septiembre' ~ '09-01',
                      month == 'Octubre' ~ '10-01',
                      month == 'Noviembre' ~ '11-01',
                      month == 'Diciembre' ~ '12-01',
                      TRUE ~ month)) %>%
  filter(current == 1) %>% 
  mutate(date = lubridate::as_date(paste(year, month, sep='-'), format = '%Y-%m-%d'),
         expectative = as.numeric(expectative)) %>%
  select(-c(current, reference_year, reference_month, month))

chile_target <- tibble(year = c('2007', '2008', '2009', '2010', '2011','2012', '2013', '2014', 
                                '2015', '2016', '2017', '2018', '2019', '2020', '2021', 
                                '2022', '2023', '2024'), 
                       current = c(3, 3, 3, 3, 3, 3, 3,
                                   3, 3, 3, 3, 3, 3, 3,
                                   3, 3, 3, 3),
                       upper = c(4, 4, 4, 4, 4, 4, 4,
                                    4, 4, 4, 4, 4, 4, 4,
                                    4, 4, 4, 4),
                       lower = c(2, 2, 2, 2, 2, 2, 2,
                                    2, 2, 2, 2, 2, 2, 2,
                                    2, 2, 2, 2))

data_chile <- right_join(chile_target, chile_forecast, by = 'year') %>% 
  select(-c(year)) %>% 
  mutate(country = 'Chile', 
         regime = 'interval')

peru_forecast <- readxl::read_xlsx('Dados/peru_forecast.xlsx', 
                                   skip = 3) %>% 
  filter(Fecha != 'de encuesta') %>% 
  mutate(
    reference_year =  case_when(
      str_detect(Fecha, 'E') == T ~ str_sub(Fecha, -4, - 1)),
    Analistas =  na_if(Analistas, "n.d."),
    Sistema =  na_if(Sistema, "n.d."),
    Empresas =  na_if(Empresas, "n.d.")) %>% 
  fill(reference_year, .direction = "downup") %>% 
  select(-c(Analistas, Empresas)) %>% 
  rename(date = Fecha, 
         expectative = Sistema) %>% 
  select(c(date, reference_year, expectative)) %>% 
  drop_na() %>% 
  mutate(expectative = round(as.numeric(expectative)*100, 4)) %>% 
  separate(date, into = c('month', 'year'), sep = -4) %>% 
  mutate(
    current = case_when(reference_year == year ~ 1,
                        TRUE ~ 0),
    month = case_when(str_detect(month, 'de enero de') == T  ~ '01-30',
                      str_detect(month, 'de febrero de') == T ~ '02-28',
                      str_detect(month, 'de marzo de') == T  ~ '03-30',
                      str_detect(month, 'de abril de') == T ~ '04-30',
                      str_detect(month, 'de mayo de') == T ~ '05-30',
                      str_detect(month, 'de junio de') == T ~ '06-30',
                      str_detect(month, 'de julio de') == T ~ '07-30',
                      str_detect(month, 'de agosto de') == T ~ '08-30',
                      str_detect(month, 'de setiembre de') == T ~ '09-30',
                      str_detect(month, 'de octubre de') == T ~ '10-30',
                      str_detect(month, 'de noviembre de') == T ~ '11-30',
                      str_detect(month, 'de noviembre') == T ~ '11-30',
                      str_detect(month, 'de diciembre de') == T ~ '12-30',
                      str_detect(month, 'de dicembre de') == T ~ '12-30',
                      TRUE ~ month)) %>%
  filter(current == 1)  %>% 
  mutate(date = lubridate::as_date(paste(year, month, sep='-'), format = '%Y-%m-%d')) %>%
  select(-c(current, reference_year, month))

peru_target <- tibble(year = c('2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', 
                               '2010', '2011', '2012', '2013', '2014', '2015', '2016', '2017', 
                               '2018', '2019', '2020', '2021', '2022', '2023', '2024'), 
                      current = c(2.5, 2.5, 2.5, 2.5, 2.5, 2, 2, 2,
                                  2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 
                                  2, 2, 2, 2),
                      upper = c(3.5, 3.5, 3.5, 3.5, 3.5, 3, 3, 3,
                                   3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 
                                   3, 3, 3, 3),
                      lower = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 1, 1,
                                   1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
                                   1, 1, 1, 1))
data_peru <- right_join(peru_target, peru_forecast, by = 'year') %>% 
  select(-c(year)) %>% 
  mutate(country = 'Peru', 
         regime = 'interval')

uruguay_forecast <- readxl::read_xls('Dados/uruguay_forecast.xls',
                                     skip = 9) %>% 
  select(`Fecha encuesta`, 
         c(`4. Pronóstico de inflación anual para el año calendario corriente`:...21)) %>%
  mutate(`Fecha encuesta` = case_when(`Fecha encuesta` == 'Set-05' ~ '38596', # reposição de valor string por número respectivo à contagem de data
                                      TRUE ~ `Fecha encuesta`),
         date = as.numeric(`Fecha encuesta`), 
         date = as.Date(date, origin = "1899-12-30"), # quando se converte de data para número, o Excel tem como dia origem essa data
         ...18 = as.numeric(...18)) %>% 
  select(c(date, 
           ...18)) %>% 
  rename(expectative = ...18) %>% 
  slice(4:n())

uruguay_target <- tibble(
  date = seq(ymd('2004-11-01'), ymd('2022-12-01'), by = 'months')
) %>% 
  mutate(
    upper = case_when(
      date >= '2022-09-01' ~ 6,
      date >= '2014-07-01' & date <= '2022-08-01' ~ 7,
      date >= '2011-07-01' & date <= '2014-07-01' ~ 6,
      date >= '2008-01-01' & date <= '2014-06-01' ~ 7,
      date >= '2007-01-01' & date <= '2007-12-01' ~ 6.5 # finalizar 2004 - 2006
    ),
    lower = case_when(
      date >= '2022-09-01' ~ 3,
      date >= '2014-07-01' & date <= '2022-08-01' ~ 3,
      date >= '2011-07-01' & date <= '2014-07-01' ~ 4,
      date >= '2008-01-01' & date <= '2014-06-01' ~ 3,
      date >= '2007-01-01' & date <= '2007-12-01' ~ 4.5 # finalizar 2004 - 2006
    ),
    current = NA
  )


data_uruguay <- right_join(uruguay_target, uruguay_forecast, by = 'date') %>% 
  mutate(country = 'Uruguai', 
         regime = 'interval')

mexico_forecast <- rbind(
  read.csv(unzip('Dados/mexico_forecast.zip', 'mexico_forecast1.csv'), 
           fileEncoding="latin1"),
  read.csv(unzip('Dados/mexico_forecast.zip', 'mexico_forecast2.csv'), 
           fileEncoding="latin1")) %>% 
  filter(NombreRelativoLargo == 'Inflación general al cierre del año en curso (año t)') %>% 
  mutate(month = month(ymd(FechaEncuesta)), 
         year = year(ymd(FechaEncuesta))) %>%
  group_by(month, year) %>% 
  summarise(expectative = median(Dato)) %>% 
  mutate(date = as.Date(paste(year, month, 1, sep='-'), '%Y-%m-%d')) %>% 
  select(-c(month, year))

mexico_target <- tibble(
  date = seq(ymd('2003-01-01'), ymd('2022-12-01'), by = 'months')
) %>% 
  mutate(
    upper = 4,
    lower = 2,
    current = 3
  )

data_mexico <- right_join(mexico_target, mexico_forecast, by = 'date') %>% 
  select(-month) %>% 
  mutate(country = 'Mexico', 
         regime = 'interval') 

colombia_forecast <- readxl::read_xls('Dados/colombia_forecast.xls') %>% 
  select(c(...1, `EXPECTATIVAS DE INFLACIÓN PARA DICIEMBRE PRESENTE AÑO`, 
           ...7, ...8)) %>% 
  janitor::row_to_names(1) %>% 
  select(FECHA, MEDIANA) %>% 
  rename(date = FECHA,
         expectative = MEDIANA) %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         yearmonth = format_ISO8601(date, precision = "ym"),
         expectative = 100*as.numeric(expectative))

colombia_target <- readxl::read_xlsx('Dados/colombia_target.xlsx',
                                     skip = 7) %>% 
  drop_na() %>% 
  rename(date = `Año(aaaa)-Mes(mm)`,
         upper = `Límite superior`,
         lower = `Límite inferior`,
         current = `Meta de inflación`) %>% 
  select(-`Inflación total 1`) %>% 
  mutate(date = as.Date(paste(as.character(round(as.numeric(date))), 
                              '01', 
                              sep = ''), 
                        "%Y%m%d"),
         yearmonth = format_ISO8601(date, precision = "ym"))

data_colombia <- right_join(colombia_target, colombia_forecast, by = 'yearmonth') %>% 
  select(-c(date.x, date.y)) %>% 
  rename(date = yearmonth) %>% 
  mutate(country = 'Colombia', 
         regime = 'interval',
         date = as.Date(paste(date, '01', sep = '-'), "%Y-%m-%d")) 

paraguay_forecast <- readxl::read_xlsx('Dados/paraguay_forecast.xlsx',
                                       skip = 13) %>% 
  pivot_longer(cols = !starts_with('...'),
               names_to = 'date',
               values_to = 'expectative') %>% 
  mutate(
    reference_date = case_when(
      ...1 %in% c('Expectativa del mes', 'Expectativa del próximo mes',
                  'Expectativa año t', 'Expectativa año t+1',
                  'Próximos 12 meses', 
                  'Horizonte de Política Monetaria (próximos 24 meses)') ~ ...1),
    variable = case_when(
      ...1 %not_in% c('Expectativa del mes', 'Expectativa del próximo mes',
                      'Expectativa año t', 'Expectativa año t+1',
                      'Próximos 12 meses', 
                      'Horizonte de Política Monetaria (próximos 24 meses)') ~ ...1)
  ) %>% 
  fill(variable, .direction = "downup") %>% 
  filter(variable == 'Bloque de Inflación' & reference_date == 'Expectativa año t') %>% 
  select(-c(...1, variable, reference_date)) %>% 
  mutate(date = as.Date(as.numeric(date), origin = "1899-12-30"),
         expectative = 100*expectative)

paraguay_target <- tibble(
  date = seq(ymd('2011-05-01'), ymd('2022-12-01'), by = 'months')
) %>% 
  mutate(
    current = case_when(
      date >= '2017-03-01' ~ 4,
      date >= '2014-12-01' & date <= '2017-12-01' ~ 4.5,
      date >= '2014-01-01' & date <= '2014-11-01' ~ 5,
      date >= '2011-05-01' & date <= '2011-12-01' ~ 5
    ),
    upper = case_when(
      date >= '2017-03-01' ~ current + 2,
      date >= '2014-12-01' & date <= '2017-12-01' ~ current + 2,
      date >= '2014-01-01' & date <= '2014-11-01' ~ current + 2,
      date >= '2011-05-01' & date <= '2011-12-01' ~ current + 2.5),
    lower = case_when(
      date >= '2017-03-01' ~ current - 2,
      date >= '2014-12-01' & date <= '2017-12-01' ~ current - 2,
      date >= '2014-01-01' & date <= '2014-11-01' ~ current - 2,
      date >= '2011-05-01' & date <= '2011-12-01' ~ current - 2.5)
  )

data_paraguay <- right_join(paraguay_target, paraguay_forecast, by = 'date') %>% 
  mutate(country = 'Paraguai', 
         regime = 'interval')

database <- rbind(data_brazil, data_canada, data_argentina, 
                  data_chile, data_peru, data_uruguay, data_mexico, 
                  data_colombia, data_paraguay) %>% 
  mutate(
    current = as.numeric(current),
    credibility = case_when(
      regime == 'interval' & expectative < lower ~ 1/(exp(expectative - lower) - (expectative - lower)),
      regime == 'interval' & expectative >= lower & expectative <= upper ~ 1,
      regime == 'interval' & expectative > upper ~ 1/(exp(expectative - upper) - (expectative - upper)),
      regime == 'target' ~ 1/(exp(expectative - current) - (expectative - current)) 
    )
  )

database %>% 
  ggplot() + 
  geom_line(aes(date, credibility)) +
  scale_x_date(limits = as.Date(c("2001-12-12", "2022-12-30"))) +
  labs(title = 'Índice de credibilidade do Banco Central',
       x = 'Data',
       y = 'Índice de credibilidade',
       caption = 'Baseado em Levieuge, Lucotte, Ringuedé (2015)',
       color = 'País') +
  theme_bw() +
  theme(
    plot.title=element_text(size=16, face="bold", hjust = 0.5),
    legend.text=element_text(size=10),
    axis.text=element_text(size=10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10)) +
  scale_color_grey() +
  facet_wrap(vars(country), ncol = 2)

database %>%
  filter(country != 'Argentina') %>% 
  ggplot() + 
  geom_line(aes(date, expectative), colour = 'black') +
  geom_line(aes(date, upper), colour = 'gray') +
  geom_line(aes(date, lower), colour = 'gray') + 
  scale_x_date(limits = as.Date(c("2001-12-12", "2022-12-30"))) +
  labs(title = 'Evolução da expectativa de inflação em relação à meta',
       x = 'Data',
       y = 'Inflação') +
  theme_bw() +
  theme(
    plot.title=element_text(size=16, face="bold", hjust = 0.5),
    legend.text=element_text(size=10),
    axis.text=element_text(size=10),
    axis.title = element_text(size = 12),
    legend.title = element_text(size = 10)) +
  scale_color_grey() +
  facet_wrap(vars(country), ncol = 2)