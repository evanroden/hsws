'use client'

/* ═══════════════════════════════════════════════════════
   ProjectIllustrations — SVG pictographic graphics
   for the Roden Works portfolio site.
   
   Design system:
   - Stroke hierarchy: primary 1px, secondary 0.6px, grid 0.3px
   - Opacity: primary elements 25-40%, secondary 12-20%, bg 3-8%
   - Colors: copper (#B87333), forest (#2D5A45), titanium (#8A9BA8)
   - Aesthetic: technical blueprint / engineering schematic
   ═══════════════════════════════════════════════════════ */

const C = {
  copper: '#B87333',
  forest: '#2D5A45',
  titanium: '#8A9BA8',
}

/* ─── Shared: Blueprint Grid Background ────────────── */

function GridBg({
  w = 320,
  h = 180,
  gap = 16,
  color = C.forest,
  opacity = 0.04,
}: {
  w?: number
  h?: number
  gap?: number
  color?: string
  opacity?: number
}) {
  const hLines = Math.floor(h / gap) + 1
  const vLines = Math.floor(w / gap) + 1
  return (
    <g>
      {Array.from({ length: hLines }).map((_, i) => (
        <line key={`h${i}`} x1="0" y1={i * gap} x2={w} y2={i * gap}
          stroke={color} strokeOpacity={opacity} strokeWidth="0.3" />
      ))}
      {Array.from({ length: vLines }).map((_, i) => (
        <line key={`v${i}`} x1={i * gap} y1="0" x2={i * gap} y2={h}
          stroke={color} strokeOpacity={opacity} strokeWidth="0.3" />
      ))}
    </g>
  )
}

/* ═══════════════════════════════════════════════════════
   FEATURED SCENES — 16:9 illustrations for homepage
   viewBox: 0 0 320 180
   ═══════════════════════════════════════════════════════ */

function EnfraScene() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg />
      {/* Boiler building */}
      <rect x="25" y="42" width="68" height="98" rx="2"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="1" fill={C.copper} fillOpacity="0.05" />
      {/* Chimney */}
      <rect x="48" y="14" width="14" height="28" rx="1"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.8" fill={C.copper} fillOpacity="0.03" />
      {/* Steam wisps */}
      <path d="M53 14 C51 6 56 2 54 -4" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.7" />
      <path d="M58 12 C60 4 56 0 59 -6" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      <path d="M63 14 C61 8 65 4 62 -2" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" />
      {/* Fire tubes inside boiler */}
      {[56, 64, 72, 80].map((y) => (
        <line key={y} x1="32" y1={y} x2="86" y2={y}
          stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.4" />
      ))}
      {/* Flame icon */}
      <path d="M59 88 C55 80 59 72 59 72 C59 72 63 80 59 88Z"
        fill={C.copper} fillOpacity="0.2" />
      <path d="M59 92 C52 80 59 68 59 68 C59 68 66 80 59 92Z"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.6" fill="none" />
      {/* Pressure gauge */}
      <circle cx="78" cy="70" r="9" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.6"
        fill={C.titanium} fillOpacity="0.03" />
      <circle cx="78" cy="70" r="6.5" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.3" fill="none" />
      <line x1="78" y1="70" x2="83" y2="65" stroke={C.copper} strokeOpacity="0.4" strokeWidth="0.6" />
      <circle cx="78" cy="70" r="1" fill={C.copper} fillOpacity="0.3" />
      {/* Gauge ticks */}
      {[0, 30, 60, 90, 120, 150, 180].map((deg) => {
        const r = (deg - 90) * Math.PI / 180
        return (
          <line key={deg}
            x1={78 + Math.cos(r) * 7.5} y1={70 + Math.sin(r) * 7.5}
            x2={78 + Math.cos(r) * 9} y2={70 + Math.sin(r) * 9}
            stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.4" />
        )
      })}

      {/* === PIPE NETWORK === */}
      {/* Steam main header */}
      <line x1="93" y1="78" x2="228" y2="78"
        stroke={C.copper} strokeOpacity="0.22" strokeWidth="2.5" />
      <line x1="93" y1="78" x2="228" y2="78"
        stroke={C.copper} strokeOpacity="0.1" strokeWidth="5" strokeDasharray="0 16" strokeLinecap="round" />
      {/* Flow arrows */}
      <polygon points="135,75 143,78 135,81" fill={C.copper} fillOpacity="0.28" />
      <polygon points="175,75 183,78 175,81" fill={C.copper} fillOpacity="0.28" />
      {/* CHW supply line */}
      <line x1="93" y1="105" x2="228" y2="105"
        stroke={C.forest} strokeOpacity="0.22" strokeWidth="2" />
      {/* CHW return arrows */}
      <polygon points="200,102 192,105 200,108" fill={C.forest} fillOpacity="0.25" />
      <polygon points="155,102 147,105 155,108" fill={C.forest} fillOpacity="0.25" />
      {/* Condensate return */}
      <line x1="93" y1="92" x2="228" y2="92"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="1" strokeDasharray="4 3" />
      {/* Pump symbols */}
      <circle cx="120" cy="78" r="5" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <polygon points="117,78 123,75 123,81" fill={C.copper} fillOpacity="0.15" />
      <circle cx="120" cy="105" r="5" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.5" fill={C.forest} fillOpacity="0.03" />
      <polygon points="123,105 117,102 117,108" fill={C.forest} fillOpacity="0.15" />
      {/* Valve symbols */}
      <g transform="translate(160,78)">
        <line x1="-3" y1="-3" x2="3" y2="3" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" />
        <line x1="3" y1="-3" x2="-3" y2="3" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" />
      </g>
      <g transform="translate(160,105)">
        <line x1="-3" y1="-3" x2="3" y2="3" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" />
        <line x1="3" y1="-3" x2="-3" y2="3" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" />
      </g>

      {/* === COOLING TOWERS === */}
      <path d="M232 148 Q232 100 248 72 Q248 50 242 32 L258 32 Q252 50 252 72 Q268 100 268 148Z"
        stroke={C.forest} strokeOpacity="0.35" strokeWidth="1" fill={C.forest} fillOpacity="0.06" />
      <path d="M264 148 Q264 105 278 78 Q278 58 273 38 L287 38 Q282 58 282 78 Q296 105 296 148Z"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.8" fill={C.forest} fillOpacity="0.04" />
      {/* Water basin */}
      <rect x="228" y="148" width="72" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      {/* Mist dots */}
      {[244, 248, 252, 256, 276, 280, 284].map((x, i) => (
        <circle key={i} cx={x} cy={28 + (i % 3) * 3} r="1"
          fill={C.titanium} fillOpacity={0.08 + (i % 2) * 0.04} />
      ))}
      {/* Fan arcs at tower mouths */}
      <path d="M246 36 A6 6 0 0 1 254 36" stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.5" fill="none" />
      <path d="M277 42 A6 6 0 0 1 285 42" stroke={C.forest} strokeOpacity="0.12" strokeWidth="0.5" fill="none" />

      {/* === ELECTRICAL PANEL === */}
      <rect x="140" y="120" width="44" height="28" rx="2"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.04" />
      {/* Lightning bolt */}
      <path d="M162 125 L158 133 L164 133 L160 142"
        stroke={C.copper} strokeOpacity="0.45" strokeWidth="1" fill="none" strokeLinejoin="round" />
      {/* Indicator LEDs */}
      <circle cx="148" cy="127" r="1.5" fill="#2ECC71" fillOpacity="0.25" />
      <circle cx="148" cy="132" r="1.5" fill="#2ECC71" fillOpacity="0.2" />
      <circle cx="148" cy="137" r="1.5" fill={C.copper} fillOpacity="0.2" />
      {/* Meter */}
      <rect x="168" y="125" width="12" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <line x1="170" y1="131" x2="174" y2="127" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.4" />

      {/* === HOSPITAL SILHOUETTE (background) === */}
      <rect x="130" y="30" width="58" height="42" rx="1"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.015" />
      {/* Cross on top */}
      <line x1="159" y1="30" x2="159" y2="18" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" />
      <line x1="154" y1="18" x2="164" y2="18" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" />
      <rect x="155" y="36" width="8" height="16" rx="0.5" fill={C.titanium} fillOpacity="0.03" />
      <rect x="150" y="40" width="18" height="8" rx="0.5" fill={C.titanium} fillOpacity="0.03" />
      {/* Windows */}
      {[136, 144, 152, 168, 176].map((x) =>
        [38, 48, 58].map((y) => (
          <rect key={`${x}-${y}`} x={x} y={y} width="4" height="5" rx="0.5"
            fill={C.titanium} fillOpacity="0.03" />
        ))
      )}

      {/* === LABELS === */}
      <text x="98" y="74" fill={C.copper} fillOpacity="0.2" fontSize="4.5" fontFamily="monospace">STEAM</text>
      <text x="98" y="101" fill={C.forest} fillOpacity="0.2" fontSize="4.5" fontFamily="monospace">CHW</text>
      <text x="148" y="118" fill={C.titanium} fillOpacity="0.18" fontSize="4" fontFamily="monospace">ELEC</text>
      {/* Readout boxes */}
      <rect x="25" y="145" width="32" height="14" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <text x="41" y="154" fill={C.copper} fillOpacity="0.25" fontSize="4.5" fontFamily="monospace" textAnchor="middle">350°F</text>
      <rect x="62" y="145" width="32" height="14" rx="1"
        stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.5" fill={C.forest} fillOpacity="0.03" />
      <text x="78" y="154" fill={C.forest} fillOpacity="0.25" fontSize="4.5" fontFamily="monospace" textAnchor="middle">42°F</text>
      {/* Title label */}
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">CEP — SCHEMATIC</text>
      {/* Pipe riser connections */}
      <line x1="228" y1="78" x2="248" y2="78" stroke={C.copper} strokeOpacity="0.15" strokeWidth="1" />
      <line x1="248" y1="78" x2="248" y2="100" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.8" />
      <line x1="228" y1="105" x2="268" y2="105" stroke={C.forest} strokeOpacity="0.15" strokeWidth="1" />
      <line x1="268" y1="105" x2="268" y2="148" stroke={C.forest} strokeOpacity="0.1" strokeWidth="0.8" />
    </svg>
  )
}
