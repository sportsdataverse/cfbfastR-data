"""Touch up the EXISTING SportsDataverse social cards.

Not a redesign -- the original artwork (the dusk hoop, the wehoop silhouette,
the powerplay bolt, the cfbplotR logo wall) is kept and polished:

  * the baked-in wordmark is re-typeset in Chivo Black where the background
    behind it is recoverable (flat fill, or a per-column interpolation of the
    sky gradient), with real tracking instead of default spacing;
  * every card gains the package URL, which none of them carried;
  * a short gold rule ties the set together (same #FFB612 as the _alt set);
  * a soft scrim is laid under bottom text that sits on photography.

Cards whose wordmark IS the artwork (cfb4th's distressed logo, cfbplotR's
serif over the logo wall, powerplay's band logo) keep it -- they get the
URL, rule and scrim only.
"""

import pathlib
from PIL import Image, ImageDraw, ImageFont
import numpy as np

# Sources are PRISTINE copies pulled from each repo's origin/main into src/
# (see README). Reading them from the working tree made the script read back
# its own output on a second run -- it is destructive and non-idempotent that
# way, and the hoopR sky assertion caught it.
R = pathlib.Path(__file__).parent / "src"
OUT = pathlib.Path(__file__).parent / "out"; OUT.mkdir(parents=True, exist_ok=True)

CHIVO = "C:/Users/saiem/AppData/Local/Microsoft/Windows/Fonts/Chivo-VariableFont_wght.ttf"
INTER_M = "C:/Windows/Fonts/Inter-Medium.ttf"
INTER_S = "C:/Windows/Fonts/Inter-SemiBold.ttf"
GOLD = (255, 182, 18)


def chivo(size, weight=900):
    f = ImageFont.truetype(CHIVO, size)
    f.set_variation_by_axes([weight])
    return f


def track(draw, xy, text, font, fill, spacing=0, anchor_mid=False, stroke=0,
          stroke_fill=None):
    """Draw text with letter tracking; PIL has no tracking of its own."""
    widths = [draw.textlength(c, font=font) for c in text]
    total = sum(widths) + spacing * (len(text) - 1)
    x, y = xy
    if anchor_mid:
        x -= total / 2
    for c, w in zip(text, widths):
        draw.text((x, y), c, font=font, fill=fill, stroke_width=stroke,
                  stroke_fill=stroke_fill)
        x += w + spacing
    return total


def repaint_flat(im, y0, y1, sample_y):
    """Cover a band with the flat colour sampled from `sample_y`."""
    a = np.asarray(im).astype(np.uint8).copy()
    a[y0:y1, :, :] = a[sample_y, :, :][None, :, :]
    return Image.fromarray(a)


def clean_row(im, lo, hi, thresh=0.95):
    """Last clean row before art appears, scanning down from `lo`.

    Sky is smooth along x; the rim, net and backboard are not. Taking the LAST
    row under the threshold picked up a row containing the rim (its thin net
    strings barely move the mean), and interpolating from it smeared those
    strings upward through the sky as vertical streaks. Stopping at the FIRST
    violation guarantees the row is sky and nothing else.
    """
    a = np.asarray(im).astype(np.float64)
    last = lo
    for y in range(lo, hi):
        if np.abs(np.diff(a[y], axis=0)).mean() > thresh:
            break
        last = y
    return last


def repaint_gradient(im, y0, y1, above, below):
    """Rebuild a band by interpolating per column between two clean rows.

    The hoopR sky is a smooth two-axis gradient, so a flat fill would band.
    Interpolating the actual rows above and below the wordmark reproduces it.
    """
    a = np.asarray(im).astype(np.float64).copy()
    top, bot = a[above].copy(), a[below].copy()
    n = y1 - y0
    for i in range(n):
        t = (y0 + i - above) / float(below - above)
        a[y0 + i] = top * (1 - t) + bot * t
    return Image.fromarray(a.clip(0, 255).astype(np.uint8))


def scrim(im, height=150, strength=150, colour=(0, 0, 0)):
    """Soft bottom-up gradient so footer text reads over photography.

    Skipped when the footer band is already dark -- on the near-black cards a
    black scrim only drew a visible rectangle edge across the artwork.
    """
    w, h = im.size
    band = np.asarray(im.convert("RGB")).astype(int)[h - height:, :, :]
    if colour == (0, 0, 0) and band.mean() < 42:
        return im.convert("RGB")
    ov = Image.new("RGBA", (w, h), (0, 0, 0, 0))
    d = ImageDraw.Draw(ov)
    for i in range(height):
        alpha = int(strength * (i / height) ** 2.2)
        d.line([(0, h - 1 - i), (w, h - 1 - i)], fill=(*colour, alpha))
    return Image.alpha_composite(im.convert("RGBA"), ov).convert("RGB")


def inset(im, pct=0.055):
    """Shrink the artwork and edge-replicate the margin.

    cfb4th's and cfbplotR's wordmarks run right off both edges, and at a real
    unfurl width (~600px) that reads as a rendering error rather than a bleed.
    The art cannot be re-laid-out, so it is scaled down and the new margin is
    filled by replicating the outermost row/column -- seamless on the flat and
    gradient fields these cards use.
    """
    w, h = im.size
    iw, ih = int(w * (1 - 2 * pct)), int(h * (1 - 2 * pct))
    small = im.resize((iw, ih), Image.LANCZOS)
    a = np.asarray(small)
    ox, oy = (w - iw) // 2, (h - ih) // 2
    out = np.zeros((h, w, 3), dtype=np.uint8)
    out[oy:oy + ih, ox:ox + iw] = a
    out[:oy, ox:ox + iw] = a[0]                      # top
    out[oy + ih:, ox:ox + iw] = a[-1]                # bottom
    out[:, :ox] = out[:, ox:ox + 1]                  # left
    out[:, ox + iw:] = out[:, ox + iw - 1:ox + iw]   # right
    return Image.fromarray(out)


def footer(im, url, light=True, bar=(14, 22, 34)):
    """A solid footer bar carrying the URL every one of these cards lacked.

    A gradient scrim was tried first and rejected: over bright artwork its top
    edge showed as a band, and the URL still landed on the hoop pole / the
    ball / the player's hands. A deliberate bar with a gold top border reads as
    design rather than as a wash, and is legible on every card regardless of
    what sits behind it. The gold border is the thread shared with the _alt set.
    """
    w, h = im.size
    bh = max(62, int(h * 0.132))
    d = ImageDraw.Draw(im)
    d.rectangle([0, h - bh, w, h], fill=bar)
    d.rectangle([0, h - bh, w, h - bh + max(3, h // 200)], fill=GOLD)
    f = ImageFont.truetype(INTER_S, max(26, int(w / 42)))
    asc, desc = f.getmetrics()
    y = h - bh + (bh - (asc + desc)) / 2 + max(3, h // 200) / 2
    track(d, (w // 2, y), url, f, (240, 244, 248),
          spacing=max(1.4, w / 700), anchor_mid=True)
    return im


def wordmark(im, text, accent_text=None, *, y, size, colour=(255, 255, 255),
             accent=(190, 30, 45), weight=900, spacing=-1.0, shadow=None):
    d = ImageDraw.Draw(im)
    f = chivo(size, weight)
    parts = [(text, colour)] + ([(accent_text, accent)] if accent_text else [])
    total = 0
    for t, _ in parts:
        total += sum(d.textlength(c, font=f) for c in t) + spacing * (len(t) - 1)
    x = im.size[0] / 2 - total / 2
    for t, col in parts:
        for c in t:
            if shadow:
                d.text((x + shadow[0], y + shadow[1]), c, font=f, fill=shadow[2])
            d.text((x, y), c, font=f, fill=col)
            x += d.textlength(c, font=f) + spacing
    return im


# ---------------------------------------------------------------------------
# per-card recipes
# ---------------------------------------------------------------------------
def hoopR(src, out, data=False):
    """Dusk hoop kept; HOOPR re-set in Chivo Black over a rebuilt sky."""
    im = Image.open(R / src).convert("RGB")
    # Repaint ONLY the band the old wordmark occupied (rows 33-135), between
    # two rows measured as pure sky. Reaching further down kept catching the
    # rim: it spans ~200 of 1280 columns, so it barely moves a row mean and
    # then smeared into the sky. Rows 24 and 156 are clean on all three cards.
    assert clean_row(im, 150, 170, thresh=0.95) >= 156, "row 156 is not sky"
    im = repaint_gradient(im, 26, 154, above=24, below=156)
    if data:
        wordmark(im, "hoopR", "-data", y=52, size=150, accent=(214, 58, 48),
                 spacing=-2.0, shadow=(3, 4, (10, 26, 44)))
    else:
        wordmark(im, "hoopR", y=52, size=150, spacing=-2.0,
                 shadow=(3, 4, (10, 26, 44)))
    return footer(im, "hoopR.sportsdataverse.org", bar=(13, 30, 50)), out


def wehoop(src, out, data=False, py=False):
    """Cream field + silhouette kept; wordmark re-set, footer added."""
    im = Image.open(R / src).convert("RGB")
    w, h = im.size
    band = (18, 150) if not py else (60, 174)
    im = repaint_flat(im, band[0], band[1], sample_y=6)
    size = int(w / 9.7)
    y = band[0] + int((band[1] - band[0]) * 0.06)
    navy = (32, 46, 74)
    if data:
        wordmark(im, "wehoop", "-data", y=y, size=size, colour=navy,
                 accent=(168, 38, 58), spacing=-1.5)
    elif py:
        wordmark(im, "wehoop", " py", y=y, size=size, colour=navy,
                 accent=(53, 114, 165), spacing=-1.5)
    else:
        wordmark(im, "wehoop", y=y, size=size, colour=navy, spacing=-1.5)
    return footer(im, "wehoop.sportsdataverse.org", bar=(32, 46, 74)), out


def cfbfastR(src, out, data=False):
    """Photo + quote kept; flat backdrop deepened, wordmark re-set."""
    im = Image.open(R / src).convert("RGB")
    a = np.asarray(im).astype(int)
    base = tuple(int(v) for v in a[5, 5])
    im = repaint_flat(im, 10, 170, sample_y=5)
    # the flat grey backdrop reads as unfinished; give it a vertical fall-off
    arr = np.asarray(im).astype(np.float64)
    h = arr.shape[0]
    for y in range(h):
        arr[y] *= 1.0 - 0.16 * (y / h) ** 1.6
    im = Image.fromarray(arr.clip(0, 255).astype(np.uint8))
    if data:
        wordmark(im, "cfbfastR", "-data", y=46, size=118,
                 accent=(196, 40, 44), spacing=-2.0,
                 shadow=(3, 4, tuple(max(0, c - 45) for c in base)))
    else:
        wordmark(im, "cfbfastR", y=46, size=118, spacing=-2.0,
                 shadow=(3, 4, tuple(max(0, c - 45) for c in base)))
    return footer(im, "cfbfastR.sportsdataverse.org", bar=(46, 8, 8)), out


def art_only(src, out, url, light=True, bar=(14, 22, 34), inset_pct=0.0):
    """Wordmark IS the artwork -- add the footer and rule, touch nothing else."""
    im = Image.open(R / src).convert("RGB")
    if im.size != (1280, 640):
        im = im.resize((1280, 640), Image.LANCZOS)
    if inset_pct:
        im = inset(im, inset_pct)
    return footer(im, url, bar=bar), out


JOBS = [
    (hoopR, "hoopR_gh.png", "hoopR_gh.png", {}),
    (hoopR, "hoopR_social_card_data_repo.png",
     "hoopR_social_card_data_repo.png", {"data": True}),
    (hoopR, "hoopR_data_repo_social_card.png",
     "hoopR_data_repo_social_card.png", {"data": True}),
    (wehoop, "wehoop_gh.png", "wehoop_gh.png", {}),
    (wehoop, "wehoop_social_card_data_repo.png",
     "wehoop_social_card_data_repo.png", {"data": True}),
    (wehoop, "wehoop-py-gh.png",
     "wehoop-py-gh.png", {"py": True}),
    (cfbfastR, "social_card_cfbfastR_data_repo.png",
     "social_card_cfbfastR_data_repo.png", {"data": True}),
    (cfbfastR, "social_card_cfbfastR_final_quote.png",
     "social_card_cfbfastR_final_quote.png", {}),
]
# (src, out, url, bar colour, inset) -- inset only where the art bleeds off
ART = [
    ("social_card_cfb4th.png",
     "social_card_cfb4th.png", "cfb4th.sportsdataverse.org", (20, 12, 12), 0.085),
    ("social_card_cfbplotR.png",
     "social_card_cfbplotR.png", "cfbplotR.sportsdataverse.org", (10, 22, 38), 0.075),
    ("powerplay_gh.png",
     "powerplay_gh.png", "fastRhockey.sportsdataverse.org", (10, 14, 26), 0.03),
    ("powerplay_data_repo_gh.png",
     "powerplay_data_repo_gh.png", "fastRhockey.sportsdataverse.org", (10, 14, 26), 0.055),
]

if __name__ == "__main__":
    for fn, src, name, kw in JOBS:
        im, _ = fn(src, name, **kw)
        im.save(OUT / name)
        print("touched", name, im.size)
    for src, name, url, bar, ins in ART:
        im, _ = art_only(src, name, url, bar=bar, inset_pct=ins)
        im.save(OUT / name)
        print("footer ", name, im.size)
