# Social-card generators

Two scripts, two jobs. Both write 1280x640, the size GitHub and the major
social scrapers render.

| script | what it does |
|---|---|
| `touchup_cards.py` | Polishes the **existing** cards. Keeps the artwork; re-sets the wordmark in Chivo Black where the background behind it is recoverable, and adds the package URL in a gold-topped footer bar. Produces the canonical `<card>.png` names. |
| `make_cards.py` | Generates the **alternative** vector set (`*_alt.png` + `.svg` source): a field in the package's own brand color, a sport motif, wordmark, tagline, gold divider. |

Run:

```sh
python themes/generators/touchup_cards.py       # -> out/<card>.png
python themes/generators/make_cards.py          # -> svg/<card>_alt.svg
Rscript -e 'for (s in list.files("svg", "[.]svg$", full.names=TRUE))
  rsvg::rsvg_png(s, file.path("png", sub("svg$","png",basename(s))), 1280, 640)'
```

`make_cards.py` emits SVG only; **R's `rsvg` is the renderer** -- there is no
cairosvg in the sdv-py venv and no magick/inkscape on PATH.

Both read the source cards from sibling checkouts under `sdv-dev/`, so run
them from a full workspace. Fonts used: Chivo (variable, weight axis driven to
900) and Inter Medium.

## Gotchas worth keeping

- **`hoopR`'s sky.** The band under the old wordmark is rebuilt by
  interpolating between two rows *measured* to be pure sky. Picking the lower
  row by a row-mean smoothness test failed twice: the rim spans only ~200 of
  1280 columns, so it barely moves the mean, and it smeared up into the sky as
  vertical streaks. The band is bounded to the old wordmark's rows instead.
- **No gradient scrims.** Over bright artwork a scrim's top edge shows as a
  band. The footer bar replaced it.
- **`odds-data` does not start with `oddsapiR`.** The two-tone data-repo
  wordmark splits on the first hyphen; slicing the package name off the front
  rendered it as "ta".

## Sizing is driven by the preview, not the file

X/`summary_large_image` and most unfurls render these around **600px wide**, so
every element is sized to survive a ~47% downscale. `qa2.py` asserts it:
URL cap-height must clear **13px at 600w**, the footer bar must keep >=14px
padding, and no text may sit within 6% of a side edge. All 31 cards pass.

Two things this QA pass changed:

- **The URL moved into a solid bar on both sets.** Loose 26px text measured
  under 12px at preview width and the motif ran through it.
- **The `_alt` cards lost their gold divider + diamond.** It landed ~140px
  above the bar's own gold border, so two gold elements competed for the same
  job and the pair ate the vertical breathing room.

`src/` holds **pristine** copies of the original cards, pulled from each repo's
`origin/main`. `touchup_cards.py` reads from there, never from a sibling
working tree -- pointing it at the live repos made it read back its own output
on the second run, which the hoopR sky assertion caught.
