"""Build social cards for the packages that never had one.

House style, as practised by the existing SDV cards (wehoop's is the clearest
example): take the package's own hex, extend its flat field colour across the
full card, and carry the hex's own artwork and wordmark onto it. Nothing is
invented -- the type and illustration are the package's own.

sportyR and sportypy already ship exactly this as `*-logo-full.png` (a flat
green panel with the pictogram wordmark), so those two are lifted directly.

Output is 1280x640 with the same gold-topped footer bar as the rest of the
family, so the URL is legible at real unfurl width (~600px).
"""
import pathlib
import numpy as np
from PIL import Image, ImageDraw, ImageFont
from scipy import ndimage

R = pathlib.Path("C:/Users/saiem/Documents/GitHub-Data/sdv-dev")
OUT = pathlib.Path(__file__).parent / "out"; OUT.mkdir(parents=True, exist_ok=True)
INTER_S = "C:/Windows/Fonts/Inter-SemiBold.ttf"
GOLD = (255, 182, 18)
W, H, BAR = 1280, 640, 84

# src, url, footer bar colour, how much of the field width the art should fill
CARDS = {
    "baseballr":  ("baseball-dev/baseballr/data-raw/baseballr-logo1036.png",
                   "baseballr.sportsdataverse.org", (58, 12, 24), 0.60),
    "softballR":  ("softballR-dev/softballR/logo.png",
                   "github.com/sportsdataverse/softballR", (196, 124, 32), 0.62),
    "oddsapiR":   ("oddsapiR-dev/oddsapiR/man/figures/logo.png",
                   "oddsapiR.sportsdataverse.org", (74, 16, 16), 0.68),
    "sportyR":    ("sportyR/logos/sportyr-logo-full.png",
                   "sportyR.sportsdataverse.org", (14, 74, 26), 0.86),
    "sportypy":   ("sportypy/logos/sportypy-logo-full.png",
                   "sportypy.sportsdataverse.org", (14, 74, 26), 0.86),
    "cfbseedR":   ("cfbseedR/man/figures/logo-2x.png",
                   "cfbseedR.sportsdataverse.org", (10, 20, 34), 0.66),
}


def lift(rel):
    """Return (artwork RGB crop, field colour) from a hex or panel logo.

    The hex/panel BORDER has to be excluded before measuring the artwork,
    otherwise the content bbox is just the outline and spans the whole file.
    The opaque (or on-field) region is eroded first, then the artwork is
    whatever differs from the field colour inside what remains.
    """
    im = Image.open(R / rel).convert("RGBA")
    w, h = im.size
    flat = Image.new("RGBA", im.size, (255, 255, 255, 255))
    flat.alpha_composite(im)
    rgb = np.asarray(flat.convert("RGB")).astype(int)
    alpha = np.asarray(im)[:, :, 3]

    from collections import Counter
    c = Counter(map(tuple, rgb[int(h*.35):int(h*.65), int(w*.35):int(w*.65)]
                    .reshape(-1, 3)))
    field = np.array(c.most_common(1)[0][0])

    opaque = alpha > 200
    if opaque.mean() > 0.95:                     # flat panel (sportyR/sportypy)
        region = np.abs(rgb - field).sum(2) < 40
        region = ndimage.binary_fill_holes(region)
    else:                                        # hex with transparent corners
        region = ndimage.binary_fill_holes(opaque)

    # Everything OUTSIDE the hex becomes field colour, and anything already
    # near-field is snapped exactly to it. Without this the crop carried the
    # hex's own corners onto the card -- as white triangles under oddsapiR's
    # dice, and as a faint hexagon ghost behind baseballr's stitching.
    clean = rgb.copy()
    clean[~region] = field
    clean[np.abs(rgb - field).sum(2) < 26] = field

    pad = int(min(w, h) * 0.085)                 # drop the border ring
    inner = ndimage.binary_erosion(region, np.ones((pad, pad)))

    art = (np.abs(clean - field).sum(2) > 60) & inner
    art = ndimage.binary_dilation(art, np.ones((5, 5)))

    # Drop specks before measuring. A single stray pixel high in sportyR's
    # panel stretched the bbox to 1343x759 for a pictogram that is really a
    # wide strip, so the artwork came out at 57% of the card instead of
    # filling it.
    lab, n = ndimage.label(art)
    if n > 1:
        sizes = ndimage.sum(art, lab, range(1, n + 1))
        keep = np.isin(lab, 1 + np.flatnonzero(sizes >= max(sizes) * 0.004))
        art = keep
    ys, xs = np.where(art)
    box = (xs.min(), ys.min(), xs.max() + 1, ys.max() + 1)
    out = Image.fromarray(clean.astype(np.uint8)).crop(box)
    return out, tuple(int(v) for v in field)


def card(pkg, rel, url, bar_rgb, fill):
    art, field = lift(rel)
    field_h = H - BAR
    im = Image.new("RGB", (W, H), field)

    # scale the artwork into the field with generous margins on all sides
    max_w, max_h = int(W * fill), int(field_h * 0.74)
    s = min(max_w / art.width, max_h / art.height)
    art = art.resize((max(1, int(art.width * s)), max(1, int(art.height * s))),
                     Image.LANCZOS)
    im.paste(art, ((W - art.width) // 2, (field_h - art.height) // 2))

    d = ImageDraw.Draw(im)
    d.rectangle([0, field_h, W, H], fill=bar_rgb)
    d.rectangle([0, field_h, W, field_h + 4], fill=GOLD)
    f = ImageFont.truetype(INTER_S, 32)
    widths = [d.textlength(ch, font=f) for ch in url]
    x = W / 2 - (sum(widths) + 2 * (len(url) - 1)) / 2
    for ch, cw in zip(url, widths):
        d.text((x, field_h + 22), ch, font=f, fill=(240, 244, 248))
        x += cw + 2
    return im


# Data-repo variants. The wordmark is baked into the logo art, so it cannot be
# two-toned the way the vector cards do it; the repo is named in the bar
# instead, which distinguishes the card without touching the artwork.
DATA_REPOS = {
    "baseballr": "github.com/sportsdataverse/baseballr-data",
    "softballR": "github.com/sportsdataverse/softballR-data",
    "oddsapiR": "github.com/sportsdataverse/odds-data",
}

if __name__ == "__main__":
    for pkg, (rel, url, bar, fill) in CARDS.items():
        im = card(pkg, rel, url, bar, fill)
        im.save(OUT / f"{pkg}_gh.png")
        print(f"built {pkg}_gh.png  {im.size}")
        if pkg in DATA_REPOS:
            im = card(pkg, rel, DATA_REPOS[pkg], bar, fill)
            im.save(OUT / f"{pkg}_social_card_data_repo.png")
            print(f"built {pkg}_social_card_data_repo.png  {im.size}")
