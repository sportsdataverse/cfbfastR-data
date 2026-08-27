"""Final QA: URL legibility at unfurl size, and safe-area margins.

The bar is located by its GOLD top border, not by row variance -- a variance
walk up from the bottom stops at the first row of URL text and then measures
a fragment, which made every card look broken when none were.
"""
import pathlib, numpy as np
from PIL import Image
SP=pathlib.Path(__file__).parent.parent
SETS={"touched":SP/"touchup/out","alt":SP/"cards/png"}
PREVIEW=600; MIN_CAP=13
GOLD=np.array([255,182,18])

def metrics(a,w,h):
    near = (np.abs(a-GOLD).sum(2) < 90)
    frac = near.sum(1)/w
    cand = [y for y in range(int(h*0.60), h) if frac[y] > 0.90]
    if not cand: return None
    bar_top = min(cand)
    band = a[bar_top:h]
    bg = np.median(band[6:].reshape(-1,3), axis=0)
    ink = (np.abs(band-bg).sum(2) > 90)
    rows = [r for r in range(6, band.shape[0]) if 2 < ink[r].sum() < w*0.75]
    if not rows: return None
    cols = np.where(ink[min(rows):max(rows)+1].sum(0)>0)[0]
    return dict(bar_h=h-bar_top, cap=max(rows)-min(rows)+1,
                pad_top=min(rows), pad_bot=(h-bar_top)-max(rows)-1,
                side=min(int(cols.min()), int(w-1-cols.max())))

print(f"{'set':<8}{'card':<42}{'bar':<6}{'cap':<5}{'@600':<7}{'padT/B':<10}{'side':<7}verdict")
print("-"*112)
bad=[]
for s,d in SETS.items():
    for f in sorted(d.glob("*.png")):
        im=Image.open(f).convert("RGB"); w,h=im.size; a=np.asarray(im).astype(int)
        m=metrics(a,w,h)
        if not m: print(f"{s:<8}{f.name[:40]:<42}no gold bar detected"); bad.append(f.name); continue
        eff=m["cap"]*PREVIEW/w
        iss=[]
        if eff<MIN_CAP: iss.append(f"URL {eff:.1f}px<{MIN_CAP}")
        if min(m["pad_top"],m["pad_bot"])<14: iss.append(f"pad {min(m['pad_top'],m['pad_bot'])}px")
        if m["side"]<w*0.06: iss.append("URL near edge")
        v="; ".join(iss) or "OK"
        if iss: bad.append(f.name)
        print(f"{s:<8}{f.name[:40]:<42}{m['bar_h']:<6}{m['cap']:<5}{eff:<7.1f}"
              f"{str(m['pad_top'])+'/'+str(m['pad_bot']):<10}{m['side']:<7}{v}")
print(f"\n{len(bad)} of 31 with issues" + (": "+", ".join(bad) if bad else ""))
