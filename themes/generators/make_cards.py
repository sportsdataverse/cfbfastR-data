"""Generate the SportsDataverse social-card family as 1280x640 SVGs.

One template, varied per package by palette + sport motif + copy. The template
is the one merged as cfbfastR-data/themes/social_card_cfbseedR.png (PR #17):
gradient field, motif watermark, wordmark, tagline, gold divider carrying a
diamond, URL. The gold divider is the family signature -- it is the same on
every card and is what ties the set together across sports.

Palettes are derived from each package's own hex logo (dominant colors), not
invented: cfbfastR maroon #600000, cfbplotR near-black teal #001818, hoopR
blue #3078A8, fastRhockey holographic lavender/pink over navy #183048,
baseballr crimson #A81830, oddsapiR red #C00000, sportyR turf green #18A818.

Render with R: rsvg::rsvg_png(svg, png, width = 1280, height = 640).
"""

import pathlib

OUT = pathlib.Path(__file__).parent / "svg"
OUT.mkdir(exist_ok=True)

W, H = 1280, 640
GOLD = "#FFB612"  # family signature, identical on every card
FONT = "'DejaVu Sans','Segoe UI',Verdana,sans-serif"


# --------------------------------------------------------------------------
# motifs -- each returns the watermark <g>, drawn in `line` / `seed` colors
# --------------------------------------------------------------------------
def m_gridiron(line, seed):
    """Yard lines, hash marks and a goal line -- football."""
    p = []
    for i, x in enumerate(range(80, 1281, 100)):
        p.append(f'<path d="M{x},60 V580"/>')
    out = [
        f'<g stroke="{line}" stroke-width="6" fill="none" stroke-linecap="round">',
        *p,
        "</g>",
        f'<g stroke="{seed}" stroke-width="5" stroke-linecap="round">',
    ]
    # hash marks on two rows
    for y in (215, 425):
        for x in range(105, 1281, 25):
            out.append(f'<path d="M{x},{y} V{y + 18}"/>')
    out.append("</g>")
    # goal lines heavier
    out.append(f'<g stroke="{seed}" stroke-width="10" fill="none"><path d="M80,60 V580"/><path d="M1180,60 V580"/></g>')
    return "\n    ".join(out)


def m_fourthdown(line, seed):
    """A 4th-down marker chain: down box, chain, and the line to gain."""
    out = [f'<g stroke="{line}" stroke-width="6" fill="none">']
    for x in range(120, 1281, 110):
        out.append(f'<path d="M{x},70 V570"/>')
    out.append("</g>")
    # the line to gain, in the accent, plus the chain
    out.append(
        f'<g stroke="{seed}" stroke-width="12" fill="none" stroke-linecap="round">'
        f'<path d="M340,70 V570"/><path d="M340,320 H960"/><path d="M960,70 V570"/></g>'
    )
    out.append(f'<g fill="{seed}">')
    for x in range(360, 961, 40):
        out.append(f'<circle cx="{x}" cy="320" r="9"/>')
    out.append("</g>")
    # The down number, tucked into the corners so it frames the wordmark
    # instead of colliding with it -- a centered 300px "4" fought the mark.
    for x, anchor in ((70, "start"), (1210, "end")):
        out.append(
            f'<text x="{x}" y="575" text-anchor="{anchor}" font-family="{FONT}" '
            f'font-size="190" font-weight="700" fill="{seed}" opacity="0.45">4</text>'
        )
    return "\n    ".join(out)


def m_logogrid(line, seed):
    """A scatterplot of team-logo chips -- cfbplotR draws logos on ggplot2."""
    out = [
        f'<g stroke="{line}" stroke-width="6" fill="none"><path d="M120,560 H1160"/><path d="M120,80 V560"/></g>',
        f'<g stroke="{line}" stroke-width="3" fill="none" opacity="0.6">',
    ]
    for y in range(140, 561, 105):
        out.append(f'<path d="M120,{y} H1160"/>')
    for x in range(240, 1161, 120):
        out.append(f'<path d="M{x},80 V560"/>')
    out.append("</g>")
    # hexagonal logo chips scattered along a trend
    pts = [
        (240, 500),
        (360, 455),
        (480, 470),
        (600, 390),
        (720, 350),
        (840, 300),
        (960, 255),
        (1080, 175),
        (300, 380),
        (660, 505),
        (900, 420),
        (1020, 330),
    ]
    out.append(f'<g fill="{seed}" opacity="0.9">')
    for cx, cy in pts:
        r = 26
        pth = " ".join(
            f"{cx + r * dx:.0f},{cy + r * dy:.0f}"
            for dx, dy in ((0, -1), (0.866, -0.5), (0.866, 0.5), (0, 1), (-0.866, 0.5), (-0.866, -0.5))
        )
        out.append(f'<polygon points="{pth}"/>')
    out.append("</g>")
    return "\n    ".join(out)


def m_court(line, seed):
    """Half court: key, free-throw circle, three-point arc -- basketball."""
    return "\n    ".join(
        [
            f'<g stroke="{line}" stroke-width="8" fill="none" stroke-linecap="round">',
            '<rect x="70" y="60" width="1140" height="520" rx="8"/>',
            '<path d="M640,60 V580"/>',
            '<circle cx="640" cy="320" r="86"/>',
            # left key + arc
            '<rect x="70" y="200" width="230" height="240"/>',
            '<circle cx="300" cy="320" r="86"/>',
            '<path d="M70,120 A290,290 0 0 1 70,520"/>',
            # right key + arc
            '<rect x="980" y="200" width="230" height="240"/>',
            '<circle cx="980" cy="320" r="86"/>',
            '<path d="M1210,120 A290,290 0 0 0 1210,520"/>',
            "</g>",
            f'<g fill="{seed}"><circle cx="130" cy="320" r="16"/><circle cx="1150" cy="320" r="16"/></g>',
        ]
    )


def m_rink(line, seed):
    """Faceoff circles, blue lines, creases -- hockey."""
    out = [
        f'<g stroke="{line}" stroke-width="8" fill="none" stroke-linecap="round">',
        '<rect x="60" y="55" width="1160" height="530" rx="120"/>',
        '<path d="M400,55 V585"/><path d="M880,55 V585"/>',
        '<circle cx="640" cy="320" r="92"/>',
        "</g>",
        f'<g stroke="{seed}" stroke-width="7" fill="none">',
        '<path d="M640,55 V585"/>',
    ]
    for cx in (220, 1060):
        for cy in (175, 465):
            out.append(f'<circle cx="{cx}" cy="{cy}" r="62"/>')
            out.append(f'<circle cx="{cx}" cy="{cy}" r="8" fill="{seed}"/>')
    out.append('<path d="M60,250 A80,70 0 0 1 60,390"/>')
    out.append('<path d="M1220,250 A80,70 0 0 0 1220,390"/>')
    out.append("</g>")
    out.append(f'<circle cx="640" cy="320" r="12" fill="{seed}"/>')
    return "\n    ".join(out)


def m_diamond(line, seed):
    """Infield diamond, basepaths and the outfield arc -- bat and ball."""
    return "\n    ".join(
        [
            f'<g stroke="{line}" stroke-width="9" fill="none" stroke-linejoin="round">',
            # outfield arc from the two foul lines
            '<path d="M640,600 L240,200"/><path d="M640,600 L1040,200"/>',
            '<path d="M240,200 A566,566 0 0 1 1040,200"/>',
            "</g>",
            f'<g stroke="{seed}" stroke-width="10" fill="none" stroke-linejoin="round">',
            # the diamond itself
            '<polygon points="640,540 800,380 640,220 480,380"/>',
            "</g>",
            f'<g fill="{seed}">',
            '<rect x="618" y="518" width="44" height="44" rx="6"/>',
            '<rect x="778" y="358" width="44" height="44" rx="6"/>',
            '<rect x="618" y="198" width="44" height="44" rx="6"/>',
            '<rect x="458" y="358" width="44" height="44" rx="6"/>',
            '<circle cx="640" cy="380" r="26"/>',
            "</g>",
        ]
    )


def m_odds(line, seed):
    """Implied-probability bars on a baseline, price line through the tops."""
    base = 580  # bars stand ON this line rather than hanging from the top
    out = [f'<g fill="{line}">']
    vals = [0.86, 0.61, 0.74, 0.42, 0.55, 0.31, 0.68, 0.48, 0.79, 0.36]
    for i, v in enumerate(vals):
        x = 90 + i * 112
        bh = int(v * 460)
        out.append(f'<rect x="{x}" y="{base - bh}" width="72" height="{bh}" rx="8"/>')
    out.append("</g>")
    out.append(f'<path d="M70,{base} H1210" stroke="{line}" stroke-width="6" fill="none"/>')
    # the line through the tops -- a price moving
    pts = " ".join(f"{90 + i * 112 + 36},{base - int(v * 460)}" for i, v in enumerate(vals))
    out.append(
        f'<polyline points="{pts}" fill="none" stroke="{seed}" '
        f'stroke-width="9" stroke-linejoin="round" stroke-linecap="round"/>'
    )
    out.append(f'<g fill="{seed}">')
    for i, v in enumerate(vals):
        out.append(f'<circle cx="{90 + i * 112 + 36}" cy="{base - int(v * 460)}" r="13"/>')
    out.append("</g>")
    return "\n    ".join(out)


def m_surfaces(line, seed):
    """Overlapping regulation surfaces -- which is exactly what these draw."""
    return "\n    ".join(
        [
            # basketball court
            f'<g stroke="{line}" stroke-width="7" fill="none">',
            '<rect x="70" y="130" width="500" height="300" rx="6"/>',
            '<circle cx="320" cy="280" r="60"/>',
            '<path d="M320,130 V430"/>',
            '<rect x="70" y="215" width="110" height="130"/>',
            '<rect x="460" y="215" width="110" height="130"/>',
            "</g>",
            # hockey rink
            f'<g stroke="{seed}" stroke-width="7" fill="none">',
            '<rect x="640" y="90" width="560" height="290" rx="80"/>',
            '<path d="M920,90 V380"/><circle cx="920" cy="235" r="52"/>',
            '<circle cx="760" cy="235" r="34"/><circle cx="1080" cy="235" r="34"/>',
            "</g>",
            # baseball diamond, lower left
            f'<g stroke="{seed}" stroke-width="7" fill="none" stroke-linejoin="round">',
            '<polygon points="200,580 300,480 200,380 100,480"/>',
            '<path d="M200,580 L60,440"/><path d="M200,580 L340,440"/>',
            "</g>",
            # football gridiron, lower right
            f'<g stroke="{line}" stroke-width="7" fill="none">',
            '<rect x="440" y="430" width="760" height="160" rx="4"/>',
            *[f'<path d="M{x},430 V590"/>' for x in range(520, 1181, 80)],
            "</g>",
        ]
    )


def m_bracket(line, seed):
    """The 16-slot playoff skeleton: 12 filled seeds, 4 open. cfbseedR."""
    out = [
        f'<g stroke="{line}" stroke-width="9" fill="none" stroke-linecap="round" stroke-linejoin="round">',
        '<path d="M150,150 H250 V190 H150 M250,170 H350"/>',
        '<path d="M150,250 H250 V290 H150 M250,270 H350"/>',
        '<path d="M150,350 H250 V390 H150 M250,370 H350"/>',
        '<path d="M150,450 H250 V490 H150 M250,470 H350"/>',
        '<path d="M350,170 V270 M350,220 H470"/>',
        '<path d="M350,370 V470 M350,420 H470"/>',
        '<path d="M470,220 V420 M470,320 H600"/>',
        '<path d="M1130,150 H1030 V190 H1130 M1030,170 H930"/>',
        '<path d="M1130,250 H1030 V290 H1130 M1030,270 H930"/>',
        '<path d="M1130,350 H1030 V390 H1130 M1030,370 H930"/>',
        '<path d="M1130,450 H1030 V490 H1130 M1030,470 H930"/>',
        '<path d="M930,170 V270 M930,220 H810"/>',
        '<path d="M930,370 V470 M930,420 H810"/>',
        '<path d="M810,220 V420 M810,320 H680"/>',
        "</g>",
        f'<g fill="{seed}">',
        '<circle cx="150" cy="150" r="14"/>',
        '<circle cx="150" cy="250" r="14"/><circle cx="150" cy="290" r="14"/>',
        '<circle cx="150" cy="350" r="14"/><circle cx="150" cy="390" r="14"/>',
        '<circle cx="150" cy="490" r="14"/>',
        '<circle cx="1130" cy="150" r="14"/>',
        '<circle cx="1130" cy="250" r="14"/><circle cx="1130" cy="290" r="14"/>',
        '<circle cx="1130" cy="350" r="14"/><circle cx="1130" cy="390" r="14"/>',
        '<circle cx="1130" cy="490" r="14"/>',
        "</g>",
        f'<g fill="none" stroke="{seed}" stroke-width="5" stroke-opacity="0.55">',
        '<circle cx="150" cy="190" r="12"/><circle cx="150" cy="450" r="12"/>',
        '<circle cx="1130" cy="190" r="12"/><circle cx="1130" cy="450" r="12"/>',
        "</g>",
    ]
    return "\n    ".join(out)


MOTIFS = {
    "gridiron": m_gridiron,
    "fourthdown": m_fourthdown,
    "logogrid": m_logogrid,
    "court": m_court,
    "rink": m_rink,
    "diamond": m_diamond,
    "odds": m_odds,
    "surfaces": m_surfaces,
    "bracket": m_bracket,
}


# --------------------------------------------------------------------------
# per-package configuration
# --------------------------------------------------------------------------
# top/bot   -- the field gradient (from the package's own dominant logo color)
# line/seed -- motif stroke and highlight
# tag       -- package-card tagline;  dtag -- data-repo-card tagline
# url       -- the footer line
PKGS = {
    "cfbfastR": dict(
        top="#4A0C0C",
        bot="#1C0303",
        line="#8A4444",
        seed="#E8C2A0",
        motif="gridiron",
        tagline="COLLEGE FOOTBALL PLAY-BY-PLAY, FAST",
        dtagline="THE COLLEGE FOOTBALL DATA RELEASES",
        url="cfbfastR.sportsdataverse.org",
        data_repo="cfbfastR-data",
    ),
    "cfb4th": dict(
        top="#2A1010",
        bot="#0D0505",
        line="#6B3A3A",
        seed="#F0A800",
        motif="fourthdown",
        tagline="FOURTH DOWN DECISIONS, MODELED",
        dtagline="FOURTH DOWN MODEL RELEASES",
        url="cfb4th.sportsdataverse.org",
        data_repo=None,
    ),
    "cfbplotR": dict(
        top="#0B2430",
        bot="#04121A",
        line="#2F5F72",
        seed="#7FD4C1",
        motif="logogrid",
        tagline="TEAM LOGOS AND COLORS FOR GGPLOT2",
        dtagline="LOGO AND COLOR ASSET RELEASES",
        url="cfbplotR.sportsdataverse.org",
        data_repo=None,
    ),
    "cfbseedR": dict(
        top="#12243A",
        bot="#0B1622",
        line="#41566B",
        seed="#A9D6E5",
        motif="bracket",
        tagline="SIMULATE & EVALUATE COLLEGE FOOTBALL SEASONS",
        dtagline="SEASON SIMULATION RELEASES",
        url="cfbseedR.sportsdataverse.org",
        data_repo=None,
    ),
    "hoopR": dict(
        top="#0E3A57",
        bot="#061A28",
        line="#2E6E96",
        seed="#E4762F",
        motif="court",
        tagline="MEN'S BASKETBALL DATA, NBA THROUGH NCAA",
        dtagline="THE MEN'S BASKETBALL DATA RELEASES",
        url="hoopR.sportsdataverse.org",
        data_repo="hoopR-data",
    ),
    "wehoop": dict(
        top="#2A1638",
        bot="#120A1A",
        line="#5C3A70",
        seed="#F26DA4",
        motif="court",
        tagline="WOMEN'S BASKETBALL DATA, WNBA THROUGH NCAA",
        dtagline="THE WOMEN'S BASKETBALL DATA RELEASES",
        url="wehoop.sportsdataverse.org",
        data_repo="wehoop-data",
    ),
    "fastRhockey": dict(
        top="#1B2E4A",
        bot="#0A1424",
        line="#4A6791",
        seed="#C9B6F0",
        motif="rink",
        tagline="PWHL AND NHL HOCKEY DATA",
        dtagline="THE HOCKEY DATA RELEASES",
        url="fastRhockey.sportsdataverse.org",
        data_repo="fastRhockey-data",
    ),
    "baseballr": dict(
        top="#3A0B18",
        bot="#160408",
        line="#7A3245",
        seed="#E8CBA0",
        motif="diamond",
        tagline="BASEBALL DATA FROM MLB TO COLLEGE",
        dtagline="THE BASEBALL DATA RELEASES",
        url="baseballr.sportsdataverse.org",
        data_repo="baseballr-data",
    ),
    "softballR": dict(
        top="#123024",
        bot="#06140E",
        line="#316E52",
        seed="#F2D24B",
        motif="diamond",
        tagline="COLLEGE AND PRO SOFTBALL DATA",
        dtagline="THE SOFTBALL DATA RELEASES",
        url="github.com/sportsdataverse/softballR",
        data_repo="softballR-data",
    ),
    "oddsapiR": dict(
        top="#33090C",
        bot="#140304",
        line="#7A2A30",
        seed="#6FD08C",
        motif="odds",
        tagline="SPORTS BETTING ODDS FROM THE ODDS API",
        dtagline="THE HISTORICAL ODDS RELEASES",
        url="oddsapiR.sportsdataverse.org",
        data_repo="odds-data",
    ),
    "sportyR": dict(
        top="#0C2A16",
        bot="#04120A",
        line="#2C6B3E",
        seed="#7BD87B",
        motif="surfaces",
        tagline="DRAW REGULATION PLAYING SURFACES IN R",
        dtagline="PLAYING SURFACE ASSET RELEASES",
        url="sportyR.sportsdataverse.org",
        data_repo=None,
    ),
    "sportypy": dict(
        top="#0C2A16",
        bot="#04120A",
        line="#2C6B3E",
        seed="#6FB8E8",
        motif="surfaces",
        tagline="DRAW REGULATION PLAYING SURFACES IN PYTHON",
        dtagline="PLAYING SURFACE ASSET RELEASES",
        url="sportypy.sportsdataverse.org",
        data_repo=None,
    ),
}


# --------------------------------------------------------------------------
# template
# --------------------------------------------------------------------------
def fit(text, cap, per_char, floor):
    """Largest size at which `text` still fits `cap` px wide."""
    return max(floor, min(cap[0], int(cap[1] / (per_char * max(1, len(text))))))


def esc(s):
    """XML-escape card copy. Taglines carry '&' (SIMULATE & EVALUATE ...)."""
    return s.replace("&", "&amp;").replace("<", "&lt;").replace(">", "&gt;")


def card(pkg, cfg, suffix=None, sub=None):
    """One 1280x640 card.

    Layout is driven by how these are actually seen: X and most unfurls render
    a summary_large_image around 600px wide, so everything is sized to survive
    a ~47% downscale. The URL lives in a solid footer bar rather than as loose
    text -- at preview size the old 26px line came out under 12px and the
    motif ran straight through it. The bar also matches the touched-up cards,
    so both sets read as one family.
    """
    BAR = 86                       # footer bar height
    FIELD = H - BAR                # area the artwork and text get
    motif = MOTIFS[cfg["motif"]](cfg["line"], cfg["seed"])
    word = pkg + (suffix or "")

    # Wordmark: cap at 76% of the canvas so it never crowds the sides.
    size = fit(word, (146, int(W * 0.76)), 0.62, 60)
    if suffix:
        mark = (f'<tspan fill="#FFFFFF">{esc(pkg)}</tspan>'
                f'<tspan fill="{cfg["seed"]}">{esc(suffix)}</tspan>')
    else:
        mark = f'<tspan fill="#FFFFFF">{esc(pkg)}</tspan>'
    tag = sub or cfg["tagline"]
    tsize = fit(tag, (34, int(W * 0.82)), 0.74, 22)

    # Optically centre the wordmark + tagline block in the field above the bar.
    # The gold rule + diamond that used to sit between the tagline and the bar
    # was dropped: it landed ~140px above the bar's own gold border, so two
    # gold elements competed and the pair ate the breathing space. The bar's
    # border now carries the family thread on its own.
    wm_y = int(FIELD * 0.525)
    tag_y = wm_y + int(tsize * 2.05)

    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
  <!--
    {esc(word)} social card, {W}x{H}. Part of the SportsDataverse card family:
    gradient field in the package's own brand color, a sport motif watermark,
    the wordmark, a tagline, and the gold-topped footer bar carrying the URL.
    Generated by make_cards.py; edit the config there and re-render rather
    than hand-editing this file.
  -->
  <defs>
    <linearGradient id="field" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0%" stop-color="{cfg["top"]}"/>
      <stop offset="100%" stop-color="{cfg["bot"]}"/>
    </linearGradient>
    <radialGradient id="focus" cx="50%" cy="{int(wm_y / H * 100) - 6}%" r="62%">
      <stop offset="0%" stop-color="{cfg["bot"]}" stop-opacity="0.86"/>
      <stop offset="100%" stop-color="{cfg["bot"]}" stop-opacity="0"/>
    </radialGradient>
  </defs>

  <rect width="{W}" height="{H}" fill="url(#field)"/>

  <!-- motif sits back so the type leads; it fought the tagline at 0.45 -->
  <g opacity="0.32">
    {motif}
  </g>
  <!-- soft pool behind the type so the motif never runs through a letterform -->
  <rect width="{W}" height="{FIELD}" fill="url(#focus)"/>

  <text x="640" y="{wm_y}" text-anchor="middle" font-family="{FONT}"
        font-size="{size}" font-weight="700"
        stroke="{cfg["bot"]}" stroke-width="14" paint-order="stroke">{mark}</text>

  <text x="640" y="{tag_y}" text-anchor="middle" font-family="{FONT}"
        font-size="{tsize}" font-weight="600" fill="{cfg["seed"]}"
        letter-spacing="3">{esc(tag)}</text>

  <rect x="0" y="{FIELD}" width="{W}" height="{BAR}" fill="{cfg["bot"]}"/>
  <rect x="0" y="{FIELD}" width="{W}" height="4" fill="{GOLD}"/>
  <text x="640" y="{FIELD + 58}" text-anchor="middle" font-family="{FONT}"
        font-size="32" font-weight="600" fill="#EEF2F6"
        letter-spacing="2">{esc(cfg["url"])}</text>
</svg>
"""


if __name__ == "__main__":
    made = []
    for pkg, cfg in PKGS.items():
        p = OUT / f"{pkg}_gh.svg"
        p.write_text(card(pkg, cfg), encoding="utf-8")
        made.append(p.name)
        if cfg["data_repo"]:
            # The data-repo card wordmarks the DATA REPO's name two-toned:
            # stem white, remainder in the accent. Split on the first hyphen
            # rather than slicing the package name off the front -- oddsapiR's
            # data repo is `odds-data`, which that slice would render as "ta".
            stem, _, rest = cfg["data_repo"].partition("-")
            p = OUT / f"{pkg}_social_card_data_repo.svg"
            p.write_text(
                card(stem, cfg, suffix=f"-{rest}", sub=cfg["dtagline"]),
                encoding="utf-8",
            )
            made.append(p.name)
    print(f"wrote {len(made)} svgs to {OUT}")
    for m in made:
        print("  " + m)
