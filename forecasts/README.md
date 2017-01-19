Coup forecasts

**`coup-forecasts-2017.csv`**

The file contains forecasts for the risk of a coup attempt for 161 countries in 2017. The outcome is based on the Powell & Thyne (P&T) list of coup attempts, available [here](http://www.jonathanmpowell.com/coup-detat-dataset.html) or [here](http://www.uky.edu/~clthyn2/coup_data/home.htm). 

- `gwcode`: Gleditsch & Ward [numeric country code](http://privatewww.essex.ac.uk/~ksg/statelist.html). 
- `country`: country name
- `year`: the year for the forecasts, i.e. 2017.
- `coup_attempt`: probability of any coup attempt whether successful or not. This corresponds to either 1 or 2 in the P&T coup data. This estimate is derived from the successful/failed coup attempt estimates in the other columns using the rule for probability disjunction: $P(\textrm{attempt}) = P(\textrm{coup}) + P(\textrm{failed attempt}) - P(\textrm{coup}) \times P(\textrm{failed attempt})$
- `successful_coup`: probability of a successful coup, i.e. a 2 in the P&T coup data. This estimate is based on averaged predictions from two separate models, a lasso regression and a random forest. 
- `failed_attempt`: probability of a failed coup attempt, i.e. a 1 in the P&T coup data. This estimate is based on averaged predictions from two separate models, a lasso regression and a random forest. 