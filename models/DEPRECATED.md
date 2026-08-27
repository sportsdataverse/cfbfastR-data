# Deprecated model artifacts in this directory

## `xgb_ep_model.model` · `xgb_wp_naive_model.model` · `xgb_wp_spread_model.model`

**Status: deprecated, unusable, retained only for archaeology. Do not consume.**

These three files are stored in XGBoost's **legacy binary format**, which was
deprecated in XGBoost 1.6 and **removed in 3.1**. Verified 2026-08-27 against R
`xgboost` 3.2.1.1 — every one of them fails to load:

```text
Failed to load model: .../xgb_wp_spread_model.model
The binary format has been deprecated in 1.6 and removed in 3.1, use UBJ or JSON
instead.
```

There is no supported XGBoost release that can both read these files and be
installed today, so they cannot be revived by re-saving; they would have to be
retrained.

**Superseded by** the `cfb_model_artifacts` bundle, which ships modern `.ubj`
artifacts read by *both* `cfbfastR` and `sportsdataverse-py`:

<https://github.com/sportsdataverse/sportsdataverse-data/releases/tag/cfb_model_artifacts>

That bundle carries `ep_model`, `wp_naive`, `wp_spread`, `fg_model`,
`cfb_cp_model`, `qbr_model`, `fd_model`, `two_pt_model`, `xpass_model`, the
punt/field-position lookup tables, model cards, and a `MANIFEST.json` with
per-asset `sha256`, the feature contract of every booster, and the EP next-score
class order. Publishing there updates both libraries in one change.

Context and migration plan: [cfbfastR#138](https://github.com/sportsdataverse/cfbfastR/issues/138).

## `ep_model.Rdata` · `fg_model.Rdata` · `wp_model.Rdata`

**Status: live today, scheduled for replacement.**

These are what `cfbfastR::.onLoad()` currently downloads (`nnet::multinom` EP,
`mgcv::bam` FG and WP). They still work, with one known defect: the EP model
aborts on mid-era CFBD data — `predict.nnet(): missing values in 'x'` for
seasons ~2006–2013 whenever `epa_wpa = TRUE`
([cfbfastR#5](https://github.com/sportsdataverse/cfbfastR/issues/5)).

They are scheduled to be replaced by the `cfb_model_artifacts` bundle above
(cfbfastR#138 P1/P2). **Do not delete them until that migration has landed and
been released** — every currently-published cfbfastR EPA/WPA number was produced
by these files, so they are the reproduction path for existing datasets.

⚠️ The replacement is **not** a drop-in: the `.ubj` EP model uses one-hot
`down_1..down_4` plus raw `distance` (no `log_ydstogo` / `Goal_To_Go` /
`Under_two` / interactions), and — critically — its 7 next-score classes are in
a **different order** than `ep_model$lev`, with no fixed point between the two
orderings. Consult `MANIFEST.json`'s `ep_class_contract` before scoring anything
with it.
