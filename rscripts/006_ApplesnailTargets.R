
library(lubridate)

mwd.hydrology.summ <- mwd.hydrology |>
  mutate(target.depths = if_else(DEPTH >=20 & DEPTH <= 50, true = 1,false = 0),
         target.dates = if_else(MONTH >=3 & MONTH <= 6, true = 1, false = 0),
         target.days = if_else(target.depths == 1 & target.dates == 1,
                               true = 1, false = 0),
         dry = if_else(DEPTH > 0, true = 0, false = 1)) |> 
  group_by(REGION,SITE,YEAR) |> 
    mutate(tot.target.days = sum(target.days),
           dry = sum(dry),
           dry_down = if_else(dry > 0, true = "yes", false = "no")) |> 
  select(-target.depths,-target.dates,-target.days)

rm(mwd.hydrology.nest)


