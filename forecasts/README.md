Coup forecasts

## `coup-forecasts-2017.csv`

`gwcode` and `year`  are identifying Gleditsch & Ward country code and year for which the forecast is.

`outcome` is based on the Powell and Thyne coup data.

    - `pt_any_attempt`: one or more coup attempt(s) (P&T `coup` is 1 or 2)
    - `pt_coup`: one or more successful coup (P&T `coup` is 2)
    - `pt_coup_attempt` one or more failed coup attempts (P&T `coup` is 1)

The remaining columns are predictions from different models.

    - `disjunction`: for `pt_any_attempt` only, calculated using the ensemble predictions for succesful coup and failed coup attempt with *P(disjunction) = P(coup) + P(Failed attempt) - P(coup) x P(failed attempt)
    - `ens_sa`: ensemble formed by taking the simple average of the remaining models.
    - `mdl_lasso`: lasso logistic regression
    - `mdl_rf`: random forest

