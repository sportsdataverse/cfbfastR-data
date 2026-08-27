"""Rebuild softballR's hex logo.

What was wrong with the shipped `logo.png` (518x600, the package's only art):

* **The hexagon is clipped.** Its left and right vertices run past x=0 and
  x=517, so the points are sliced flat.
* **The border is ~5px**, where the rest of the SDV hexes use 9px.
* **No vector source**, and the raster is the only copy, so nothing downstream
  can be rendered larger than 518x600.

The frame is rebuilt as SVG with correct geometry. The softball and the
wordmark are the designer's own artwork and are preserved -- no rounded
typeface is available here, so re-setting the wordmark would change its
character. They are lifted from the source and re-composited.

Geometry: pointy-top regular hexagon, centre (259,300), circumradius R.
Stroke is centred on the path, so it extends w/2 beyond it:
    R*sqrt(3)/2 + w/2 <= 259   and   R + w/2 <= 300
With w=9 that gives R <= 293.9; R=293.5 is used, which fits with a hair of
margin instead of running off the canvas.
"""
import pathlib
import numpy as np
from PIL import Image
from scipy import ndimage

SRC = pathlib.Path("C:/Users/saiem/Documents/GitHub-Data/sdv-dev"
                   "/softballR-dev/softballR/logo.png")
OUT = pathlib.Path(__file__).parent
ORANGE = "#ECAB55"
W, H, CX, CY = 518, 600, 259, 300
# Two FILLED hexagons, not one stroked path: a stroke sits half outside the
# path and its round joins push further still, so a stroked hex that fits on
# paper still ran off both side edges. Outer R is chosen so the widest points
# (the 30/150/210/330-degree vertices, at CX +/- R*cos30) stay on canvas.
R_OUT = 297.0                       # 259 +/- 257.2  ->  1.8 .. 516.2
BORDER = 9.0                        # perpendicular border width
R_IN = R_OUT - BORDER / (3 ** 0.5 / 2)   # inset by inradius, = 286.6


def _pts(r):
    return " ".join(
        f"{CX + r*np.cos(np.deg2rad(90 + 60*k)):.2f},"
        f"{CY - r*np.sin(np.deg2rad(90 + 60*k)):.2f}" for k in range(6))


def hex_svg(scale=1):
    return f'''<svg xmlns="http://www.w3.org/2000/svg" width="{W*scale}" height="{H*scale}"
     viewBox="0 0 {W} {H}">
  <!--
    softballR hex frame. Pointy-top regular hexagon drawn as two filled
    polygons: outer in the brand orange, inner in white, inset by the
    inradius so the visible border is exactly {BORDER}px, matching the rest of
    the SportsDataverse hexes. The previous logo's left and right vertices
    were clipped flat by the artboard; these stay on canvas.
  -->
  <polygon points="{_pts(R_OUT)}" fill="{ORANGE}"/>
  <polygon points="{_pts(R_IN)}" fill="#FFFFFF"/>
</svg>
'''


def content_rgba():
    """The softball + wordmark, with the old hex outline removed."""
    im = Image.open(SRC).convert("RGBA")
    a = np.asarray(im).copy()
    # Label at a LOW alpha threshold and dilate the frame before clearing it.
    # Labelling at >128 left the outline's anti-aliased fringe behind, and that
    # fringe still spanned the whole canvas, so the content bbox came back as
    # the full 518x600.
    mask = a[:, :, 3] > 20
    lab, n = ndimage.label(mask)
    sizes = ndimage.sum(mask, lab, range(1, n + 1))
    frame = 1 + int(np.argmax(sizes))
    kill = ndimage.binary_dilation(lab == frame, np.ones((5, 5)))
    a[kill] = 0
    return Image.fromarray(a)


def compose(scale):
    """Render the frame at `scale` and lay the preserved artwork on it."""
    import subprocess, tempfile, os
    svg = OUT / "hex.svg"
    png = OUT / f"_frame_{scale}x.png"
    r = subprocess.run(["Rscript", "-e",
        f'rsvg::rsvg_png("{svg.as_posix()}","{png.as_posix()}",'
        f'width={W*scale},height={H*scale})'], capture_output=True, text=True)
    if r.returncode: raise RuntimeError(r.stderr[-400:])
    frame = Image.open(png).convert("RGBA")

    art = Image.open(OUT / "content.png").convert("RGBA")
    # the artwork sat at (66,193)-(451,406) in the 518x600 original; keep that
    # placement so the rebuild is a drop-in replacement
    tw, th = int(art.width * scale), int(art.height * scale)
    art = art.resize((tw, th), Image.LANCZOS)
    frame.alpha_composite(art, (int(66 * scale), int(193 * scale)))
    out = OUT / (f"logo.png" if scale == 1 else f"logo-{scale}x.png")
    frame.save(out)
    os.remove(png)
    print(f"wrote {out.name} {frame.size}")
    return frame


if __name__ == "__main__":
    (OUT / "hex.svg").write_text(hex_svg(), encoding="utf-8")
    content = content_rgba()
    ys, xs = np.where(np.asarray(content)[:, :, 3] > 20)
    box = (xs.min(), ys.min(), xs.max() + 1, ys.max() + 1)
    content.crop(box).save(OUT / "content.png")
    print(f"content bbox {box} -> {box[2]-box[0]}x{box[3]-box[1]}")
    print("wrote hex.svg + content.png")
    compose(1)
    compose(2)


