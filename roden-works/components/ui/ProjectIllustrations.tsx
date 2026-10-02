'use client'

import { type ReactNode } from 'react'

/* ═══════════════════════════════════════════════════════
   ProjectIllustrations — bold line-art pictograms for the
   Roden Works portfolio site.

   Design rules (keep these when adding a scene):
   - One idea per illustration: a single recognizable subject.
   - Primary strokes at opacity ≥ 0.85 and ≥ 1.5px rendered;
     secondary marks at ≥ 0.45. Nothing fainter than 0.3.
   - Shape fills use opaque tints (palette color mixed into the
     dark surface) instead of low-opacity color, so overlaps
     stay clean and nothing washes out.
   - No numbers, stats or micro-labels inside illustrations.
   - Decorative only: every SVG is aria-hidden.
   ═══════════════════════════════════════════════════════ */

const C = {
  copper: '#B87333',
  copperLight: '#D08C4F',
  verdigris: '#3DA887',
  steel: '#6E8CA0',
  titanium: '#8A9BA8',
  white: '#FFFFFF',
  bg: '#151C1F',
}

/** Opaque tints: ~18% of each color over the #151C1F surface. */
const T = {
  copper: '#322C23',
  verdigris: '#1C3532',
  steel: '#253036',
}

/** Stroke props. `o` defaults to primary-level opacity. */
function ln(color: string, width: number, o = 0.9) {
  return {
    stroke: color,
    strokeWidth: width,
    strokeOpacity: o,
    strokeLinecap: 'round' as const,
    strokeLinejoin: 'round' as const,
    fill: 'none',
  }
}

/** Stroked shape with an opaque tint fill. */
function shape(color: string, width: number, tint: string, o = 0.9) {
  return { ...ln(color, width, o), fill: tint }
}

type Kind = 'scene' | 'card'

function Svg({ vb, kind = 'scene', children }: { vb: string; kind?: Kind; children: ReactNode }) {
  return (
    <svg
      viewBox={vb}
      className={kind === 'card' ? 'block w-full h-auto max-h-[220px]' : 'w-full h-full'}
      fill="none"
      xmlns="http://www.w3.org/2000/svg"
      aria-hidden="true"
      focusable="false"
    >
      {children}
    </svg>
  )
}

/* Featured scenes render in 320×180 (≈2× scale on the homepage);
   cards render in 320×160 (≈0.85–1.9× scale). Shared drawings are
   authored in the 320×160 card space and shifted down 10 units for
   featured use, with a lighter stroke weight `w` at the larger size. */

const CARD_VB = '0 0 320 160'
const SCENE_VB = '0 0 320 180'
const CARD_W = 3
const SCENE_W = 2.2

/* ═══════════════════════════════════════════════════════
   SHARED DRAWINGS (320×160 space)
   ═══════════════════════════════════════════════════════ */

/** ENFRA — central energy plant: boiler and chiller piping steam and chilled water to a hospital. */
function EnfraDrawing({ w }: { w: number }) {
  return (
    <g>
      {/* Ground */}
      <line x1="16" y1="146" x2="304" y2="146" {...ln(C.titanium, w * 0.6, 0.5)} />
      {/* Boiler + stack */}
      <rect x="40" y="18" width="16" height="30" rx="2" {...shape(C.copper, w, T.copper)} />
      <path d="M44 13 C40 9 48 6 44 1" {...ln(C.titanium, w * 0.6, 0.55)} />
      <rect x="24" y="48" width="64" height="98" rx="8" {...shape(C.copper, w, T.copper)} />
      <circle cx="56" cy="70" r="9" {...ln(C.titanium, w * 0.65)} />
      <line x1="56" y1="70" x2="61" y2="65" {...ln(C.white, w * 0.65)} />
      <circle cx="56" cy="110" r="17" {...ln(C.copper, w * 0.65)} />
      <path d="M56 123 C48 119 48 109 56 97 C64 109 64 119 56 123 Z" fill={C.copperLight} fillOpacity="0.95" />
      {/* Steam line */}
      <line x1="88" y1="66" x2="226" y2="66" {...ln(C.copper, w * 1.4)} />
      <polyline points="134,60 142,66 134,72" {...ln(C.copperLight, w * 0.8)} />
      <polyline points="184,60 192,66 184,72" {...ln(C.copperLight, w * 0.8)} />
      {/* Chiller */}
      <line x1="124" y1="136" x2="124" y2="146" {...ln(C.verdigris, w * 0.65, 0.7)} />
      <line x1="180" y1="136" x2="180" y2="146" {...ln(C.verdigris, w * 0.65, 0.7)} />
      <rect x="110" y="96" width="84" height="40" rx="20" {...shape(C.verdigris, w, T.verdigris)} />
      <g {...ln(C.white, w * 0.65, 0.9)}>
        <line x1="152" y1="105" x2="152" y2="127" />
        <line x1="142.5" y1="110.5" x2="161.5" y2="121.5" />
        <line x1="142.5" y1="121.5" x2="161.5" y2="110.5" />
      </g>
      {/* Chilled water line */}
      <line x1="194" y1="116" x2="226" y2="116" {...ln(C.verdigris, w * 1.4)} />
      <polyline points="206,110 214,116 206,122" {...ln(C.white, w * 0.7, 0.85)} />
      {/* Hospital */}
      <rect x="226" y="36" width="74" height="110" rx="3" {...shape(C.steel, w, T.steel)} />
      <line x1="263" y1="50" x2="263" y2="66" {...ln(C.white, w * 1.3, 0.95)} />
      <line x1="255" y1="58" x2="271" y2="58" {...ln(C.white, w * 1.3, 0.95)} />
      <g {...ln(C.steel, w * 0.6, 0.75)}>
        {[238, 258, 278].map((x) =>
          [80, 100].map((y) => <rect key={`${x}-${y}`} x={x} y={y} width="10" height="10" rx="1" />),
        )}
        <rect x="256" y="124" width="14" height="22" rx="1" />
      </g>
    </g>
  )
}

/** YCOD — organ-donor registration: a form with a checked box beside a heart. */
function YcodDrawing({ w }: { w: number }) {
  return (
    <g>
      {/* Registration form */}
      <rect x="62" y="22" width="96" height="120" rx="6" {...shape(C.titanium, w, T.steel)} />
      <line x1="76" y1="40" x2="126" y2="40" {...ln(C.titanium, w * 1.1)} />
      <rect x="76" y="58" width="14" height="14" rx="2" {...ln(C.titanium, w * 0.75)} />
      <polyline points="78,64 83,70 94,55" {...ln(C.copperLight, w * 1.1, 0.95)} />
      <line x1="100" y1="65" x2="144" y2="65" {...ln(C.titanium, w * 0.65, 0.6)} />
      <rect x="76" y="84" width="14" height="14" rx="2" {...ln(C.titanium, w * 0.75)} />
      <line x1="100" y1="91" x2="138" y2="91" {...ln(C.titanium, w * 0.65, 0.6)} />
      <line x1="76" y1="112" x2="144" y2="112" {...ln(C.titanium, w * 0.65, 0.6)} />
      <path d="M78 130 C84 120 88 134 94 124 S104 130 112 123" {...ln(C.titanium, w * 0.65, 0.7)} />
      {/* Heart with pulse */}
      <path
        d="M222 128 C222 128 180 104 180 76 C180 59 192 48 206 48 C214 48 219 53 222 58 C225 53 230 48 238 48 C252 48 264 59 264 76 C264 104 222 128 222 128 Z"
        {...shape(C.copper, w, T.copper)}
      />
      <polyline points="190,82 206,82 212,70 220,96 228,74 234,82 254,82" {...ln(C.white, w * 0.75, 0.9)} />
    </g>
  )
}

/** VA — a 3D-printed denture tool: a cradle on a stand, with a denture lifting in and out of it. */
const TEETH: [number, number, number][] = [
  // [center x, width, height]
  [104, 9, 10], [113.5, 9, 12], [123, 9.5, 14], [133, 10, 15], [144, 11, 17],
  [156, 11, 17], [167, 10, 15], [177, 9.5, 14], [186.5, 9, 12], [196, 9, 10],
]

function VaDrawing({ w }: { w: number }) {
  const gumY = (x: number) => {
    const t = (x - 96) / 108
    return 66 - 48 * t * (1 - t)
  }
  return (
    <g>
      {/* Denture: teeth under a gum arch */}
      {TEETH.map(([cx, tw, th]) => (
        <rect
          key={cx}
          x={cx - tw / 2 + 0.6}
          y={gumY(cx) - 3}
          width={tw - 1.2}
          height={th * 1.2}
          rx="3"
          fill="#E6EBEE"
          fillOpacity="0.92"
          stroke={C.bg}
          strokeWidth="1"
        />
      ))}
      <path d="M96 50 Q150 18 204 50 L204 66 Q150 42 96 66 Z" {...shape(C.copper, w, T.copper)} />
      {/* Place / remove arrows */}
      {[64, 236].map((x) => (
        <g key={x} {...ln(C.copperLight, w * 0.8)}>
          <line x1={x} y1="44" x2={x} y2="100" />
          <polyline points={`${x - 6},52 ${x},44 ${x + 6},52`} />
          <polyline points={`${x - 6},92 ${x},100 ${x + 6},92`} />
        </g>
      ))}
      {/* Printed cradle tool */}
      <line x1="150" y1="110" x2="150" y2="126" {...ln(C.verdigris, w * 1.3)} />
      <path d="M90 84 L97 102 Q150 118 203 102 L210 84" {...shape(C.verdigris, w, T.verdigris)} />
      <rect x="112" y="126" width="76" height="16" rx="3" {...shape(C.verdigris, w, T.verdigris)} />
      <line x1="120" y1="132" x2="180" y2="132" {...ln(C.verdigris, w * 0.5, 0.55)} />
      <line x1="120" y1="137" x2="180" y2="137" {...ln(C.verdigris, w * 0.5, 0.55)} />
    </g>
  )
}

/** Cinema camera over a film strip (320×180 space). */
function CameraDrawing({ w }: { w: number }) {
  return (
    <g>
      {/* Film strip */}
      <rect x="16" y="128" width="288" height="36" rx="2" {...shape(C.titanium, w * 0.8, T.steel)} />
      <g fill={C.titanium} fillOpacity="0.7">
        {Array.from({ length: 18 }).map((_, i) => (
          <g key={i}>
            <rect x={22 + i * 16} y="132" width="7" height="5" rx="1" />
            <rect x={22 + i * 16} y="155" width="7" height="5" rx="1" />
          </g>
        ))}
      </g>
      {Array.from({ length: 5 }).map((_, k) => (
        <rect key={k} x={26 + k * 56} y="140" width="46" height="12" rx="1" {...ln(C.titanium, w * 0.55, 0.6)} />
      ))}
      {/* Camera */}
      <path d="M118 46 L118 32 L180 32 L180 46" {...ln(C.copper, w)} />
      <rect x="76" y="54" width="24" height="36" rx="3" {...shape(C.titanium, w, T.steel)} />
      <rect x="100" y="46" width="100" height="64" rx="7" {...shape(C.copper, w, T.copper)} />
      <circle cx="118" cy="64" r="5" fill={C.copperLight} fillOpacity="0.95" />
      <rect x="200" y="58" width="12" height="40" rx="2" {...shape(C.steel, w, T.steel)} />
      <rect x="212" y="52" width="44" height="52" rx="4" {...shape(C.steel, w, T.steel)} />
      <line x1="226" y1="52" x2="226" y2="104" {...ln(C.steel, w * 0.65, 0.75)} />
      <line x1="240" y1="52" x2="240" y2="104" {...ln(C.steel, w * 0.65, 0.75)} />
      <path d="M256 52 L268 44 L268 112 L256 104" {...shape(C.steel, w, T.steel)} />
    </g>
  )
}

/* ═══════════════════════════════════════════════════════
   FEATURED SCENES — homepage, 320×180
   ═══════════════════════════════════════════════════════ */

function EnfraScene() {
  return (
    <Svg vb={SCENE_VB}>
      <g transform="translate(0 10)"><EnfraDrawing w={SCENE_W} /></g>
    </Svg>
  )
}

function YcodScene() {
  return (
    <Svg vb={SCENE_VB}>
      <g transform="translate(0 10)"><YcodDrawing w={SCENE_W} /></g>
    </Svg>
  )
}

function VaProstheticsScene() {
  return (
    <Svg vb={SCENE_VB}>
      <g transform="translate(0 10)"><VaDrawing w={SCENE_W} /></g>
    </Svg>
  )
}

function CinematographyScene() {
  return (
    <Svg vb={SCENE_VB}>
      <CameraDrawing w={SCENE_W} />
    </Svg>
  )
}

/* ═══════════════════════════════════════════════════════
   CARD ICONS — 320×160 banners above card text
   ═══════════════════════════════════════════════════════ */

const W = CARD_W

function EnfraIcon() {
  return <Svg vb={CARD_VB} kind="card"><EnfraDrawing w={W} /></Svg>
}

function YcodIcon() {
  return <Svg vb={CARD_VB} kind="card"><YcodDrawing w={W} /></Svg>
}

function VaProstheticsIcon() {
  return <Svg vb={CARD_VB} kind="card"><VaDrawing w={W} /></Svg>
}

/** Convergint — fire alarm pull station sounding. */
function ConvergintIcon() {
  return (
    <Svg vb={CARD_VB} kind="card">
      {/* Sound waves */}
      <path d="M222 58 Q234 80 222 102" {...ln(C.copperLight, W * 0.8)} />
      <path d="M236 46 Q256 80 236 114" {...ln(C.copperLight, W * 0.8, 0.55)} />
      <path d="M98 58 Q86 80 98 102" {...ln(C.copperLight, W * 0.8)} />
      <path d="M84 46 Q64 80 84 114" {...ln(C.copperLight, W * 0.8, 0.55)} />
      {/* Pull station */}
      <rect x="116" y="18" width="88" height="124" rx="8" {...shape(C.copperLight, W, T.copper)} />
      <text x="160" y="47" textAnchor="middle" fontSize="18" fontWeight="700" letterSpacing="2"
        fontFamily="ui-sans-serif, system-ui, sans-serif" fill={C.white} fillOpacity="0.92">FIRE</text>
      <rect x="134" y="60" width="52" height="48" rx="4" {...ln(C.copperLight, W * 0.75)} />
      <line x1="144" y1="76" x2="176" y2="76" {...ln(C.white, W * 1.3, 0.95)} />
      <line x1="160" y1="76" x2="160" y2="96" {...ln(C.white, W * 1.3, 0.95)} />
      <g fill={C.copperLight} fillOpacity="0.75">
        <circle cx="128" cy="128" r="2.5" />
        <circle cx="192" cy="128" r="2.5" />
      </g>
    </Svg>
  )
}

/** Odoo — ERP: a screen of connected business apps. */
function OdooIcon() {
  const tiles: { x: number; y: number; c: string; t: string; glyph: (cx: number, cy: number) => ReactNode }[] = [
    { x: 82, y: 26, c: C.copper, t: T.copper, glyph: (cx, cy) => (
      <path d={`M${cx - 9} ${cy - 4} L${cx} ${cy - 9} L${cx + 9} ${cy - 4} L${cx + 9} ${cy + 6} L${cx} ${cy + 11} L${cx - 9} ${cy + 6} Z M${cx - 9} ${cy - 4} L${cx} ${cy + 1} L${cx + 9} ${cy - 4} M${cx} ${cy + 1} L${cx} ${cy + 11}`} />
    ) },
    { x: 140, y: 26, c: C.verdigris, t: T.verdigris, glyph: (cx, cy) => (
      <g>
        <path d={`M${cx - 12} ${cy - 8} L${cx - 7} ${cy - 8} L${cx - 4} ${cy + 4} L${cx + 8} ${cy + 4} L${cx + 11} ${cy - 4} L${cx - 6} ${cy - 4}`} />
        <circle cx={cx - 3} cy={cy + 9} r="2" />
        <circle cx={cx + 7} cy={cy + 9} r="2" />
      </g>
    ) },
    { x: 198, y: 26, c: C.steel, t: T.steel, glyph: (cx, cy) => (
      <g>
        <circle cx={cx} cy={cy} r="6" />
        {Array.from({ length: 8 }).map((_, i) => {
          const a = (i * Math.PI) / 4
          return <line key={i} x1={cx + Math.cos(a) * 8} y1={cy + Math.sin(a) * 8} x2={cx + Math.cos(a) * 11} y2={cy + Math.sin(a) * 11} />
        })}
      </g>
    ) },
    { x: 82, y: 76, c: C.steel, t: T.steel, glyph: (cx, cy) => (
      <g>
        <line x1={cx - 7} y1={cy + 9} x2={cx - 7} y2={cy + 1} />
        <line x1={cx} y1={cy + 9} x2={cx} y2={cy - 4} />
        <line x1={cx + 7} y1={cy + 9} x2={cx + 7} y2={cy - 9} />
      </g>
    ) },
    { x: 140, y: 76, c: C.copper, t: T.copper, glyph: (cx, cy) => (
      <path d={`M${cx - 11} ${cy + 9} L${cx - 11} ${cy - 2} L${cx - 4} ${cy + 2} L${cx - 4} ${cy - 2} L${cx + 3} ${cy + 2} L${cx + 3} ${cy - 9} L${cx + 9} ${cy - 9} L${cx + 9} ${cy + 9} Z`} />
    ) },
    { x: 198, y: 76, c: C.verdigris, t: T.verdigris, glyph: (cx, cy) => (
      <g>
        <rect x={cx - 8} y={cy - 10} width="16" height="20" rx="1.5" />
        <line x1={cx - 4} y1={cy - 4} x2={cx + 4} y2={cy - 4} />
        <line x1={cx - 4} y1={cy + 1} x2={cx + 4} y2={cy + 1} />
        <line x1={cx - 4} y1={cy + 6} x2={cx + 1} y2={cy + 6} />
      </g>
    ) },
  ]
  return (
    <Svg vb={CARD_VB} kind="card">
      <path d="M148 126 L142 146 M172 126 L178 146" {...ln(C.steel, W)} />
      <line x1="124" y1="146" x2="196" y2="146" {...ln(C.steel, W)} />
      <rect x="66" y="12" width="188" height="114" rx="7" {...shape(C.steel, W, T.steel)} />
      {tiles.map(({ x, y, c, t, glyph }) => (
        <g key={`${x}-${y}`}>
          <rect x={x} y={y} width="40" height="40" rx="7" {...shape(c, W * 0.75, t)} />
          <g {...ln(C.white, W * 0.65, 0.9)}>{glyph(x + 20, y + 20)}</g>
        </g>
      ))}
    </Svg>
  )
}

/** HAPS — indoor air pollution: particles inside a home, linked to the heart. */
function HapsIcon() {
  const particles: [number, number, number, string][] = [
    [80, 96, 4, C.titanium], [97, 86, 2.5, C.copperLight], [112, 104, 5, C.titanium],
    [130, 90, 3, C.titanium], [148, 100, 4, C.copperLight], [162, 88, 2.5, C.titanium],
    [86, 120, 3, C.copperLight], [104, 132, 2.5, C.titanium], [124, 122, 4.5, C.titanium],
    [142, 134, 3, C.titanium], [158, 120, 4.5, C.copperLight], [120, 66, 3, C.titanium],
    [96, 108, 2, C.titanium], [136, 112, 2.5, C.copperLight], [72, 136, 2.5, C.titanium],
  ]
  return (
    <Svg vb={CARD_VB} kind="card">
      <path d="M60 146 L60 76 L120 34 L180 76 L180 146 Z" {...shape(C.titanium, W, T.steel)} />
      {particles.map(([x, y, r, c], i) => (
        <circle key={i} cx={x} cy={y} r={r} fill={c} fillOpacity={r > 3 ? 0.9 : 0.7} />
      ))}
      <polyline points="186,104 200,104 206,92 214,118 222,96 228,104 236,104" {...ln(C.copperLight, W * 0.8)} />
      <path
        d="M266 128 C266 128 238 112 238 92 C238 82 246 76 254 76 C260 76 264 80 266 84 C268 80 272 76 278 76 C286 76 294 82 294 92 C294 112 266 128 266 128 Z"
        {...shape(C.copper, W, T.copper)}
      />
    </Svg>
  )
}

/** SWIS — a saltwater wedge creeping upriver toward a drinking-water intake. */
function SwisIcon() {
  let wave = 'M16 56'
  for (let x = 16; x < 304; x += 32) wave += ` Q${x + 8} 50 ${x + 16} 56 T${x + 32} 56`
  const salt: [number, number][] = [[200, 138], [222, 130], [246, 126], [272, 120], [292, 128], [242, 138], [268, 134]]
  return (
    <Svg vb={CARD_VB} kind="card">
      {/* River */}
      <path d="M16 56 L304 56 L304 140 Q160 172 16 140 Z" fill={T.steel} />
      <path d="M16 140 Q160 172 304 140" {...ln(C.titanium, W)} />
      <path d={wave} {...ln(C.steel, W * 0.8)} />
      {/* Salt wedge */}
      <path d="M118 148 Q200 116 304 106 L304 140 Q230 156 118 148 Z" {...shape(C.verdigris, W, T.verdigris)} />
      {salt.map(([x, y]) => (
        <rect key={`${x}-${y}`} x={x - 2.5} y={y - 2.5} width="5" height="5" fill={C.white} fillOpacity="0.75" />
      ))}
      <line x1="270" y1="90" x2="190" y2="90" {...ln(C.verdigris, W)} />
      <polyline points="200,82 190,90 200,98" {...ln(C.verdigris, W)} />
      {/* Intake to tap */}
      <path d="M56 124 L56 26 L90 26 L90 36" {...ln(C.titanium, W * 1.2)} />
      <line x1="66" y1="16" x2="80" y2="16" {...ln(C.titanium, W)} />
      <line x1="73" y1="16" x2="73" y2="26" {...ln(C.titanium, W)} />
      <path d="M90 41 C86 46 86 50 90 50 C94 50 94 46 90 41 Z" fill={C.steel} fillOpacity="0.95" />
    </Svg>
  )
}

/** Wimley Lab — pore-forming peptide helices opening a channel through a lipid bilayer. */
function WimleyLabIcon() {
  const lipids = [20, 36, 52, 68, 84, 100, 116, 204, 220, 236, 252, 268, 284, 300]
  const helix = (x: number, y: number, h: number) => (
    <g>
      <rect x={x} y={y} width="16" height={h} rx="8" {...shape(C.copper, W, T.copper)} />
      {Array.from({ length: Math.floor((h - 8) / 10) }).map((_, i) => (
        <line key={i} x1={x + 3} y1={y + 12 + i * 10} x2={x + 13} y2={y + 6 + i * 10} {...ln(C.copperLight, W * 0.6)} />
      ))}
    </g>
  )
  return (
    <Svg vb={CARD_VB} kind="card">
      {/* Bilayer */}
      {lipids.map((x) => (
        <g key={x}>
          <g {...ln(C.verdigris, W * 0.55, 0.6)}>
            <line x1={x - 2} y1="62" x2={x - 2} y2="82" />
            <line x1={x + 2} y1="62" x2={x + 2} y2="82" />
            <line x1={x - 2} y1="106" x2={x - 2} y2="86" />
            <line x1={x + 2} y1="106" x2={x + 2} y2="86" />
          </g>
          <circle cx={x} cy="56" r="6" fill={C.verdigris} fillOpacity="0.9" />
          <circle cx={x} cy="112" r="6" fill={C.verdigris} fillOpacity="0.9" />
        </g>
      ))}
      {/* Peptides forming a pore */}
      {helix(130, 44, 80)}
      {helix(174, 44, 80)}
      <line x1="160" y1="30" x2="160" y2="136" {...ln(C.white, W * 0.8, 0.85)} />
      <polyline points="154,128 160,136 166,128" {...ln(C.white, W * 0.8, 0.85)} />
      {/* Free peptide arriving */}
      <g transform="rotate(-90 236 26)">{helix(228, -16, 56)}</g>
    </Svg>
  )
}

/** Our Climate — climate policy advocacy at the capitol. */
function OurClimateIcon() {
  return (
    <Svg vb={CARD_VB} kind="card">
      <line x1="160" y1="28" x2="160" y2="14" {...ln(C.titanium, W * 0.8)} />
      <rect x="155" y="28" width="10" height="12" rx="1" {...shape(C.titanium, W * 0.8, T.steel)} />
      <path d="M128 80 Q128 44 160 40 Q192 44 192 80 Z" {...shape(C.titanium, W, T.steel)} />
      <rect x="122" y="80" width="76" height="16" {...shape(C.titanium, W, T.steel)} />
      <rect x="84" y="96" width="152" height="36" {...shape(C.titanium, W, T.steel)} />
      <g {...ln(C.titanium, W * 0.65, 0.7)}>
        {[98, 114, 130, 146, 174, 190, 206, 222].map((x) => <line key={x} x1={x} y1="103" x2={x} y2="126" />)}
      </g>
      <rect x="72" y="132" width="176" height="10" {...shape(C.titanium, W, T.steel)} />
      {/* Leaf */}
      <path d="M254 108 C248 82 260 58 290 46 C294 74 282 98 254 108 Z" {...shape(C.verdigris, W, T.verdigris)} />
      <path d="M254 108 L281 58" {...ln(C.verdigris, W * 0.65)} />
    </Svg>
  )
}

/** TABI — rural broadband: a line run out to a farmhouse, with Wi-Fi. */
function TabiIcon() {
  return (
    <Svg vb={CARD_VB} kind="card">
      <path d="M16 142 Q90 130 160 140 T304 136" {...ln(C.titanium, W * 0.7, 0.6)} />
      {/* Pole + line */}
      <line x1="56" y1="140" x2="56" y2="34" {...ln(C.titanium, W)} />
      <line x1="42" y1="46" x2="70" y2="46" {...ln(C.titanium, W)} />
      <path d="M70 46 Q132 84 188 84" {...ln(C.copperLight, W * 0.85)} />
      {/* Silo */}
      <path d="M270 138 L270 78 Q281 62 292 78 L292 138" {...shape(C.titanium, W * 0.8, T.steel)} />
      {/* House */}
      <path d="M188 138 L188 88 L228 58 L268 88 L268 138 Z" {...shape(C.steel, W, T.steel)} />
      <g {...ln(C.steel, W * 0.65, 0.8)}>
        <rect x="220" y="112" width="16" height="26" rx="1" />
        <rect x="198" y="98" width="14" height="12" rx="1" />
        <rect x="244" y="98" width="14" height="12" rx="1" />
      </g>
      {/* Wi-Fi */}
      <circle cx="228" cy="45" r="3" fill={C.verdigris} fillOpacity="0.95" />
      <path d="M220 37 Q228 30 236 37" {...ln(C.verdigris, W * 0.85)} />
      <path d="M213 30 Q228 17 243 30" {...ln(C.verdigris, W * 0.85)} />
      <path d="M206 23 Q228 4 250 23" {...ln(C.verdigris, W * 0.85, 0.65)} />
    </Svg>
  )
}

/** New Orleans East — a raised, solar-powered house above floodwater. */
function NolaEastIcon() {
  let wave1 = 'M16 126'
  let wave2 = 'M24 140'
  for (let x = 16; x < 300; x += 24) wave1 += ` q6 -5 12 0 t12 0`
  for (let x = 24; x < 292; x += 24) wave2 += ` q6 -5 12 0 t12 0`
  return (
    <Svg vb={CARD_VB} kind="card">
      {/* Sun */}
      <circle cx="262" cy="38" r="10" {...ln(C.copperLight, W * 0.85)} />
      <g {...ln(C.copperLight, W * 0.65, 0.75)}>
        {Array.from({ length: 8 }).map((_, i) => {
          const a = (i * Math.PI) / 4
          return <line key={i} x1={262 + Math.cos(a) * 15} y1={38 + Math.sin(a) * 15} x2={262 + Math.cos(a) * 20} y2={38 + Math.sin(a) * 20} />
        })}
      </g>
      {/* Stilts */}
      <g {...ln(C.titanium, W * 0.85)}>
        {[126, 148, 172, 194].map((x) => <line key={x} x1={x} y1="104" x2={x} y2="150" />)}
      </g>
      <line x1="112" y1="104" x2="208" y2="104" {...ln(C.titanium, W)} />
      {/* House */}
      <path d="M120 104 L120 64 L160 34 L200 64 L200 104 Z" {...shape(C.copper, W, T.copper)} />
      <polygon points="166,37 197,60 192,66 161,43" fill={C.verdigris} fillOpacity="0.9" stroke={C.verdigris} strokeWidth={W * 0.5} strokeLinejoin="round" />
      <g {...ln(C.copper, W * 0.65)}>
        <rect x="132" y="72" width="16" height="14" rx="1" />
        <rect x="168" y="78" width="16" height="26" rx="1" />
      </g>
      {/* Water */}
      <path d={wave1} {...ln(C.steel, W * 0.85)} />
      <path d={wave2} {...ln(C.steel, W * 0.75, 0.55)} />
    </Svg>
  )
}

/** Midtown Metairie — a transit-oriented corridor: bus lane in front of mixed-use buildings. */
function MidtownMetairieIcon() {
  const buildings: [number, number, number][] = [[40, 54, 60], [104, 30, 64], [172, 62, 54], [230, 46, 50]]
  return (
    <Svg vb={CARD_VB} kind="card">
      {buildings.map(([x, y, bw]) => (
        <g key={x}>
          <rect x={x} y={y} width={bw} height={144 - y} {...shape(C.titanium, W * 0.85, T.steel)} />
          <g {...ln(C.titanium, W * 0.55, 0.55)}>
            {[y + 12, y + 30].map((wy) =>
              [0, 1, 2].map((k) => (
                <line key={`${wy}-${k}`} x1={x + 8 + k * ((bw - 16) / 3)} y1={wy} x2={x + 2 + (k + 1) * ((bw - 16) / 3)} y2={wy} />
              )),
            )}
          </g>
        </g>
      ))}
      {/* Tree */}
      <line x1="292" y1="144" x2="292" y2="118" {...ln(C.verdigris, W)} />
      <circle cx="292" cy="106" r="14" {...shape(C.verdigris, W, T.verdigris)} />
      {/* Street + bus */}
      <line x1="16" y1="144" x2="304" y2="144" {...ln(C.titanium, W * 0.8)} />
      <rect x="84" y="102" width="112" height="34" rx="6" {...shape(C.copper, W, T.copper)} />
      <g {...ln(C.white, W * 0.6, 0.8)}>
        {[94, 118, 142, 166].map((x) => <rect key={x} x={x} y="109" width="18" height="11" rx="1.5" />)}
      </g>
      <circle cx="108" cy="138" r="6" {...shape(C.copper, W, C.bg)} />
      <circle cx="172" cy="138" r="6" {...shape(C.copper, W, C.bg)} />
    </Svg>
  )
}

/** Partnership for Public Service — employee focus groups: a team in conversation. */
function PartnershipIcon() {
  return (
    <Svg vb={CARD_VB} kind="card">
      <circle cx="100" cy="84" r="13" {...shape(C.titanium, W * 0.85, T.steel)} />
      <path d="M74 142 Q74 106 100 106 Q126 106 126 142" {...shape(C.titanium, W * 0.85, T.steel)} />
      <circle cx="220" cy="84" r="13" {...shape(C.titanium, W * 0.85, T.steel)} />
      <path d="M194 142 Q194 106 220 106 Q246 106 246 142" {...shape(C.titanium, W * 0.85, T.steel)} />
      <circle cx="160" cy="70" r="16" {...shape(C.steel, W, T.steel)} />
      <path d="M128 142 Q128 96 160 96 Q192 96 192 142" {...shape(C.steel, W, T.steel)} />
      {/* Speech bubble */}
      <path d="M194 18 L236 18 Q246 18 246 28 L246 42 Q246 52 236 52 L210 52 L198 63 L200 52 L194 52 Q184 52 184 42 L184 28 Q184 18 194 18 Z"
        {...shape(C.copperLight, W, T.copper)} />
      <g fill={C.copperLight} fillOpacity="0.95">
        <circle cx="201" cy="35" r="3" />
        <circle cx="215" cy="35" r="3" />
        <circle cx="229" cy="35" r="3" />
      </g>
    </Svg>
  )
}

/* ═══════════════════════════════════════════════════════
   STUDIO SCENES — gallery thumbnails, various aspect ratios
   ═══════════════════════════════════════════════════════ */

const SW = 2.4

/** Tulane Freeman — interview setup: camera on tripod, subject in a chair, softbox. */
function TulaneFreeman() {
  return (
    <Svg vb={SCENE_VB}>
      <line x1="20" y1="162" x2="300" y2="162" {...ln(C.titanium, SW * 0.6, 0.5)} />
      {/* Tripod camera */}
      <g {...ln(C.titanium, SW)}>
        <line x1="86" y1="96" x2="58" y2="162" />
        <line x1="86" y1="96" x2="114" y2="162" />
        <line x1="86" y1="96" x2="86" y2="162" />
      </g>
      <rect x="58" y="60" width="54" height="36" rx="5" {...shape(C.copper, SW, T.copper)} />
      <rect x="112" y="66" width="20" height="24" rx="3" {...shape(C.copper, SW, T.copper)} />
      <circle cx="72" cy="70" r="3.5" fill={C.copperLight} fillOpacity="0.95" />
      <line x1="138" y1="80" x2="206" y2="92" {...ln(C.titanium, SW * 0.6, 0.5)} strokeDasharray="5 5" />
      {/* Subject in chair */}
      <g {...ln(C.steel, SW)}>
        <line x1="224" y1="118" x2="258" y2="118" />
        <line x1="228" y1="118" x2="224" y2="162" />
        <line x1="254" y1="118" x2="258" y2="162" />
        <line x1="256" y1="118" x2="262" y2="74" />
      </g>
      <circle cx="240" cy="62" r="10" {...shape(C.white, SW * 0.9, T.steel, 0.9)} />
      <g {...ln(C.white, SW * 1.2, 0.9)}>
        <line x1="242" y1="74" x2="244" y2="112" />
        <line x1="244" y1="112" x2="224" y2="114" />
        <line x1="224" y1="114" x2="222" y2="156" />
        <line x1="242" y1="84" x2="230" y2="102" />
      </g>
      {/* Softbox */}
      <line x1="294" y1="162" x2="294" y2="70" {...ln(C.titanium, SW * 0.85)} />
      <path d="M270 34 L304 22 L304 78 L270 66 Z" {...shape(C.titanium, SW, T.steel)} />
    </Svg>
  )
}

/** Fractured Futures — a kiln-fused glass panel of fractured color shards. */
function FracturedFutures() {
  const shards: [string, string][] = [
    ['20,20 70,20 84,74 20,64', C.copper],
    ['70,20 120,20 124,92 84,74', C.steel],
    ['120,20 160,20 160,60 124,92', C.verdigris],
    ['160,60 160,118 124,92', C.copperLight],
    ['124,92 160,118 160,160 110,160', C.titanium],
    ['84,74 124,92 110,160 76,118', C.verdigris],
    ['20,64 84,74 76,118 20,120', C.steel],
    ['20,120 76,118 110,160 20,160', C.copper],
  ]
  return (
    <Svg vb="0 0 180 180">
      {shards.map(([pts, c]) => (
        <polygon key={pts} points={pts} fill={c} fillOpacity="0.45" stroke={c} strokeOpacity="0.95" strokeWidth="2" strokeLinejoin="round" />
      ))}
      <g {...ln(C.white, 1.6, 0.6)}>
        <line x1="34" y1="34" x2="50" y2="30" />
        <line x1="132" y1="34" x2="148" y2="40" />
        <line x1="98" y1="100" x2="104" y2="124" />
      </g>
      <rect x="20" y="20" width="140" height="140" rx="3" {...ln(C.titanium, 2.4)} />
    </Svg>
  )
}

/** Bizar Audi · Schooltime — a model walking a lit runway. */
function VogueItaly() {
  const flash = (x: number, y: number) => (
    <g {...ln(C.copperLight, 2)}>
      <line x1={x - 9} y1={y} x2={x + 9} y2={y} />
      <line x1={x} y1={y - 9} x2={x} y2={y + 9} />
      <line x1={x - 5} y1={y - 5} x2={x + 5} y2={y + 5} />
      <line x1={x - 5} y1={y + 5} x2={x + 5} y2={y - 5} />
    </g>
  )
  return (
    <Svg vb="0 0 180 240">
      {/* Spotlight */}
      <rect x="80" y="6" width="20" height="10" rx="2" {...shape(C.titanium, 2, T.steel)} />
      <line x1="84" y1="16" x2="52" y2="196" {...ln(C.white, 1.8, 0.45)} />
      <line x1="96" y1="16" x2="128" y2="196" {...ln(C.white, 1.8, 0.45)} />
      {/* Runway */}
      <path d="M70 112 L110 112 L152 232 L28 232 Z" {...shape(C.titanium, 2.4, T.steel)} />
      <line x1="90" y1="128" x2="90" y2="226" {...ln(C.titanium, 1.6, 0.45)} strokeDasharray="6 6" />
      {/* Model */}
      <g {...ln(C.white, 2.6, 0.9)}>
        <line x1="83" y1="160" x2="78" y2="198" />
        <line x1="97" y1="160" x2="104" y2="196" />
      </g>
      <path d="M80 104 L100 104 L110 162 L70 162 Z" {...shape(C.copper, 2.4, T.copper)} />
      <g {...ln(C.white, 2.6, 0.9)}>
        <line x1="79" y1="107" x2="64" y2="146" />
        <line x1="101" y1="107" x2="116" y2="146" />
      </g>
      <circle cx="90" cy="92" r="8" {...shape(C.white, 2.2, T.steel)} />
      {flash(30, 96)}
      {flash(150, 128)}
    </Svg>
  )
}

/** Short films — a clapperboard. */
function DocumentaryWork() {
  const stripes = (y: number) =>
    Array.from({ length: 5 }).map((_, i) => {
      const x0 = 116 + i * 22
      return <polygon key={i} points={`${x0},${y} ${x0 + 10},${y} ${x0 + 4},${y + 14} ${x0 - 6},${y + 14}`} fill={C.white} fillOpacity="0.85" />
    })
  return (
    <Svg vb={SCENE_VB}>
      <g transform="translate(160 106) scale(1.15) translate(-160 -100)">
      <rect x="104" y="76" width="112" height="80" rx="4" {...shape(C.titanium, SW, T.steel)} />
      <g {...ln(C.titanium, SW * 0.6, 0.55)}>
        <line x1="116" y1="100" x2="204" y2="100" />
        <line x1="116" y1="124" x2="204" y2="124" />
        <line x1="160" y1="100" x2="160" y2="146" />
      </g>
      <rect x="104" y="62" width="112" height="14" rx="1" {...shape(C.titanium, SW, T.steel)} />
      {stripes(62)}
      <g transform="rotate(-18 104 62)">
        <rect x="104" y="48" width="112" height="14" rx="1" {...shape(C.copper, SW, T.copper)} />
        {stripes(48)}
      </g>
      <circle cx="104" cy="62" r="3" fill={C.copperLight} fillOpacity="0.95" />
      </g>
    </Svg>
  )
}

/** Medium format photography — a twin-lens reflex camera. */
function MediumFormat() {
  return (
    <Svg vb="0 0 192 240">
      <path d="M60 62 L66 30 L126 30 L132 62" {...shape(C.titanium, 2.4, T.steel)} />
      <rect x="136" y="120" width="10" height="26" rx="2" {...shape(C.titanium, 2, T.steel)} />
      <rect x="56" y="60" width="80" height="150" rx="6" {...shape(C.titanium, 2.4, T.steel)} />
      <line x1="66" y1="128" x2="126" y2="128" {...ln(C.titanium, 1.6, 0.5)} />
      <circle cx="96" cy="96" r="20" {...shape(C.copper, 2.4, T.copper)} />
      <circle cx="96" cy="96" r="11" {...ln(C.copperLight, 1.8)} />
      <circle cx="96" cy="166" r="25" {...shape(C.copper, 2.4, T.copper)} />
      <circle cx="96" cy="166" r="15" {...ln(C.copperLight, 1.8)} />
      <circle cx="96" cy="166" r="5" fill={C.copperLight} fillOpacity="0.9" />
      <path d="M86 156 Q90 151 96 151" {...ln(C.white, 1.8, 0.6)} />
    </Svg>
  )
}

/** Aurora Theatre — a curtained stage under a spotlight. */
function AuroraTheatre() {
  const curtain = 'M44 44 C80 38 112 34 124 32 C104 82 100 122 104 166 L44 166 Z'
  return (
    <Svg vb={SCENE_VB}>
      {/* Spotlight pool */}
      <line x1="152" y1="44" x2="128" y2="158" {...ln(C.white, SW * 0.6, 0.45)} />
      <line x1="168" y1="44" x2="192" y2="158" {...ln(C.white, SW * 0.6, 0.45)} />
      <ellipse cx="160" cy="160" rx="34" ry="5" fill={C.white} fillOpacity="0.35" stroke={C.white} strokeOpacity="0.7" strokeWidth={SW * 0.6} />
      {/* Curtains */}
      <path d={curtain} {...shape(C.copper, SW, T.copper)} />
      <path d={curtain} transform="translate(320 0) scale(-1 1)" {...shape(C.copper, SW, T.copper)} />
      <g {...ln(C.copper, SW * 0.6, 0.7)}>
        <path d="M62 44 L62 166" />
        <path d="M82 40 Q78 110 84 166" />
        <path d="M258 44 L258 166" />
        <path d="M238 40 Q242 110 236 166" />
      </g>
      {/* Proscenium + valance */}
      <path d="M40 40 Q160 10 280 40 L280 54 Q160 26 40 54 Z" {...shape(C.copperLight, SW, T.copper)} />
      <path d="M40 166 L40 40 Q160 10 280 40 L280 166" {...ln(C.titanium, SW)} />
      <line x1="24" y1="166" x2="296" y2="166" {...ln(C.titanium, SW)} />
    </Svg>
  )
}

/** Buffalo Central Terminal — the art deco clock tower and concourse wings. */
function BuffaloCentralTerminal() {
  const arches = (xs: number[]) =>
    xs.map((x) => <path key={x} d={`M${x} 158 L${x} 134 Q${x + 8} 124 ${x + 16} 134 L${x + 16} 158`} />)
  return (
    <Svg vb={SCENE_VB}>
      <rect x="40" y="112" width="96" height="54" {...shape(C.steel, SW, T.steel)} />
      <rect x="184" y="112" width="96" height="54" {...shape(C.steel, SW, T.steel)} />
      <g {...ln(C.steel, SW * 0.6, 0.75)}>
        {arches([56, 82, 108])}
        {arches([196, 224, 252])}
      </g>
      <line x1="160" y1="22" x2="160" y2="8" {...ln(C.titanium, SW * 0.8)} />
      <rect x="148" y="22" width="24" height="12" {...shape(C.titanium, SW, T.steel)} />
      <rect x="142" y="34" width="36" height="14" {...shape(C.titanium, SW, T.steel)} />
      <rect x="136" y="48" width="48" height="118" {...shape(C.titanium, SW, T.steel)} />
      <circle cx="160" cy="66" r="11" {...shape(C.copperLight, SW, T.copper)} />
      <g {...ln(C.white, SW * 0.7, 0.9)}>
        <line x1="160" y1="66" x2="160" y2="59" />
        <line x1="160" y1="66" x2="165" y2="68" />
      </g>
      <g {...ln(C.titanium, SW * 0.6, 0.7)}>
        <line x1="150" y1="88" x2="150" y2="158" />
        <line x1="160" y1="88" x2="160" y2="158" />
        <line x1="170" y1="88" x2="170" y2="158" />
      </g>
      <line x1="20" y1="166" x2="300" y2="166" {...ln(C.titanium, SW)} />
    </Svg>
  )
}

/* ═══════════════════════════════════════════════════════
   EXPORT MAP — Slug-based lookup for all illustrations
   ═══════════════════════════════════════════════════════ */

/** Featured scenes — 16:9 aspect ratio */
export const featuredScenes: Record<string, () => ReactNode> = {
  enfra: EnfraScene,
  ycod: YcodScene,
  'va-prosthetics': VaProstheticsScene,
  cinematography: CinematographyScene,
}

/** Card icons — 2:1 banner format for AnimatedCard */
export const cardIcons: Record<string, () => ReactNode> = {
  // Engineering
  enfra: EnfraIcon,
  convergint: ConvergintIcon,
  odoo: OdooIcon,
  'va-prosthetics': VaProstheticsIcon,
  haps: HapsIcon,
  swis: SwisIcon,
  'wimley-lab': WimleyLabIcon,
  // Advocacy
  ycod: YcodIcon,
  'our-climate': OurClimateIcon,
  tabi: TabiIcon,
  'nola-east': NolaEastIcon,
  'midtown-metairie': MidtownMetairieIcon,
  partnership: PartnershipIcon,
}

/** Studio gallery scenes — various aspect ratios */
export const studioScenes: Record<string, () => ReactNode> = {
  'claiborne-avenue': CinematographyScene,
  'tulane-freeman': TulaneFreeman,
  'fractured-futures': FracturedFutures,
  'vogue-italy': VogueItaly,
  'documentary-work': DocumentaryWork,
  'medium-format-photography': MediumFormat,
  'aurora-theatre': AuroraTheatre,
  'buffalo-central-terminal': BuffaloCentralTerminal,
}

/**
 * Render a project illustration by slug.
 * Falls back to null if no illustration exists.
 */
export function ProjectIllustration({ slug, variant = 'card' }: { slug: string; variant?: 'featured' | 'card' | 'studio' }) {
  const map = variant === 'featured' ? featuredScenes : variant === 'studio' ? studioScenes : cardIcons
  const Scene = map[slug]
  if (!Scene) return null
  return <Scene />
}
