'use client'

import { type ReactNode } from 'react'

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
      <rect x="48" y="14" width="14" height="28" rx="1"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.8" fill={C.copper} fillOpacity="0.03" />
      {/* Steam wisps */}
      <path d="M53 14 C51 6 56 2 54 -4" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.7" />
      <path d="M58 12 C60 4 56 0 59 -6" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      {/* Fire tubes */}
      {[56, 64, 72, 80].map((y) => (
        <line key={y} x1="32" y1={y} x2="86" y2={y}
          stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.4" />
      ))}
      {/* Flame */}
      <path d="M59 92 C52 80 59 68 59 68 C59 68 66 80 59 92Z"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.6" fill={C.copper} fillOpacity="0.12" />
      {/* Pressure gauge */}
      <circle cx="78" cy="70" r="9" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.6"
        fill={C.titanium} fillOpacity="0.03" />
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
      {/* Pipe network */}
      <line x1="93" y1="78" x2="228" y2="78"
        stroke={C.copper} strokeOpacity="0.22" strokeWidth="2.5" />
      <polygon points="135,75 143,78 135,81" fill={C.copper} fillOpacity="0.28" />
      <polygon points="175,75 183,78 175,81" fill={C.copper} fillOpacity="0.28" />
      <line x1="93" y1="105" x2="228" y2="105"
        stroke={C.forest} strokeOpacity="0.22" strokeWidth="2" />
      <polygon points="200,102 192,105 200,108" fill={C.forest} fillOpacity="0.25" />
      <polygon points="155,102 147,105 155,108" fill={C.forest} fillOpacity="0.25" />
      <line x1="93" y1="92" x2="228" y2="92"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="1" strokeDasharray="4 3" />
      {/* Pump symbols */}
      <circle cx="120" cy="78" r="5" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <polygon points="117,78 123,75 123,81" fill={C.copper} fillOpacity="0.15" />
      <circle cx="120" cy="105" r="5" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.5" fill={C.forest} fillOpacity="0.03" />
      <polygon points="123,105 117,102 117,108" fill={C.forest} fillOpacity="0.15" />
      {/* Cooling towers */}
      <path d="M232 148 Q232 100 248 72 Q248 50 242 32 L258 32 Q252 50 252 72 Q268 100 268 148Z"
        stroke={C.forest} strokeOpacity="0.35" strokeWidth="1" fill={C.forest} fillOpacity="0.06" />
      <path d="M264 148 Q264 105 278 78 Q278 58 273 38 L287 38 Q282 58 282 78 Q296 105 296 148Z"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.8" fill={C.forest} fillOpacity="0.04" />
      <rect x="228" y="148" width="72" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      {/* Mist */}
      {[244, 248, 252, 256, 276, 280, 284].map((x, i) => (
        <circle key={i} cx={x} cy={28 + (i % 3) * 3} r="1"
          fill={C.titanium} fillOpacity={0.08 + (i % 2) * 0.04} />
      ))}
      {/* Electrical panel */}
      <rect x="140" y="120" width="44" height="28" rx="2"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.04" />
      <path d="M162 125 L158 133 L164 133 L160 142"
        stroke={C.copper} strokeOpacity="0.45" strokeWidth="1" fill="none" strokeLinejoin="round" />
      <circle cx="148" cy="127" r="1.5" fill="#2ECC71" fillOpacity="0.25" />
      <circle cx="148" cy="132" r="1.5" fill="#2ECC71" fillOpacity="0.2" />
      <circle cx="148" cy="137" r="1.5" fill={C.copper} fillOpacity="0.2" />
      {/* Hospital silhouette */}
      <rect x="130" y="30" width="58" height="42" rx="1"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.015" />
      <line x1="159" y1="30" x2="159" y2="18" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" />
      <line x1="154" y1="18" x2="164" y2="18" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" />
      {/* Labels */}
      <text x="98" y="74" fill={C.copper} fillOpacity="0.2" fontSize="4.5" fontFamily="monospace">STEAM</text>
      <text x="98" y="101" fill={C.forest} fillOpacity="0.2" fontSize="4.5" fontFamily="monospace">CHW</text>
      <text x="148" y="118" fill={C.titanium} fillOpacity="0.18" fontSize="4" fontFamily="monospace">ELEC</text>
      <rect x="25" y="145" width="32" height="14" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <text x="41" y="154" fill={C.copper} fillOpacity="0.25" fontSize="4.5" fontFamily="monospace" textAnchor="middle">350°F</text>
      <rect x="62" y="145" width="32" height="14" rx="1"
        stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.5" fill={C.forest} fillOpacity="0.03" />
      <text x="78" y="154" fill={C.forest} fillOpacity="0.25" fontSize="4.5" fontFamily="monospace" textAnchor="middle">42°F</text>
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">CEP — SCHEMATIC</text>
    </svg>
  )
}

function YcodScene() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.copper} opacity={0.03} />
      {/* Capitol dome silhouette */}
      <path d="M100 140 L100 90 Q100 50 130 40 L130 30 Q160 15 190 30 L190 40 Q220 50 220 90 L220 140Z"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <ellipse cx="160" cy="30" rx="18" ry="12"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" fill="none" />
      <line x1="160" y1="18" x2="160" y2="8" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      {/* Columns */}
      {[112, 128, 144, 176, 192, 208].map((x) => (
        <line key={x} x1={x} y1="90" x2={x} y2="140"
          stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.8" />
      ))}
      {/* Organ / heart symbol — center */}
      <path d="M148 75 C148 62 160 58 160 68 C160 58 172 62 172 75 C172 90 160 100 160 100 C160 100 148 90 148 75Z"
        stroke={C.copper} strokeOpacity="0.4" strokeWidth="1" fill={C.copper} fillOpacity="0.08" />
      {/* Pulse line through heart */}
      <polyline points="130,82 142,82 148,72 153,92 158,78 163,85 168,82 190,82"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.8" fill="none" />
      {/* Checkbox / ballot motif */}
      <rect x="40" y="50" width="32" height="24" rx="2"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.8" fill={C.copper} fillOpacity="0.04" />
      <polyline points="48,62 54,68 68,56"
        stroke={C.copper} strokeOpacity="0.4" strokeWidth="1.2" fill="none" strokeLinecap="round" strokeLinejoin="round" />
      {/* Ballot lines */}
      <line x1="40" y1="84" x2="72" y2="84" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" />
      <line x1="40" y1="92" x2="68" y2="92" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      <line x1="40" y1="100" x2="65" y2="100" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.5" />
      {/* NY state outline (simplified) */}
      <path d="M250 35 L270 30 L280 40 L285 55 L275 75 L260 80 L250 70 L245 55Z"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.6" fill={C.copper} fillOpacity="0.03" />
      <circle cx="268" cy="58" r="3" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill={C.copper} fillOpacity="0.06" />
      {/* Rising people silhouettes */}
      {[240, 255, 270, 285].map((x, i) => (
        <g key={i}>
          <circle cx={x} cy={100 + i * 2} r="3"
            stroke={C.forest} strokeOpacity={0.12 + i * 0.04} strokeWidth="0.5" fill="none" />
          <line x1={x} y1={103 + i * 2} x2={x} y2={118 + i * 2}
            stroke={C.forest} strokeOpacity={0.1 + i * 0.03} strokeWidth="0.5" />
          <line x1={x - 5} y1={110 + i * 2} x2={x + 5} y2={110 + i * 2}
            stroke={C.forest} strokeOpacity={0.08 + i * 0.02} strokeWidth="0.4" />
        </g>
      ))}
      {/* Data readout */}
      <rect x="20" y="130" width="60" height="20" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <text x="50" y="143" fill={C.copper} fillOpacity="0.25" fontSize="5" fontFamily="monospace" textAnchor="middle">OPT-OUT</text>
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">YCOD — LEGISLATIVE</text>
      {/* Scatter dots representing donor registrations */}
      {[{x:90,y:130},{x:105,y:135},{x:115,y:128},{x:125,y:132},{x:240,y:140},{x:255,y:145},{x:270,y:138},{x:285,y:142},{x:295,y:135}].map((p, i) => (
        <circle key={i} cx={p.x} cy={p.y} r="1.2"
          fill={C.copper} fillOpacity={0.06 + (i % 3) * 0.04} />
      ))}
    </svg>
  )
}

function VaProstheticsScene() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.03} />
      {/* 3D printer frame */}
      <rect x="30" y="28" width="100" height="105" rx="2"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="1" fill={C.titanium} fillOpacity="0.03" />
      {/* Print bed */}
      <rect x="40" y="100" width="80" height="6" rx="1"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.04" />
      {/* Z-rails */}
      <line x1="35" y1="32" x2="35" y2="130" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.6" />
      <line x1="125" y1="32" x2="125" y2="130" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.6" />
      {/* X-carriage */}
      <line x1="40" y1="65" x2="120" y2="65"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.8" />
      {/* Print head */}
      <rect x="72" y="60" width="16" height="10" rx="1"
        stroke={C.copper} strokeOpacity="0.35" strokeWidth="0.8" fill={C.copper} fillOpacity="0.08" />
      {/* Extrusion line */}
      <line x1="80" y1="70" x2="80" y2="85" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.4" strokeDasharray="2 2" />
      {/* Object being printed — prosthetic hand outline */}
      <path d="M65 100 L65 88 Q65 82 70 80 L70 72 Q72 70 74 72 L74 80 L78 74 Q80 72 82 74 L82 80 L86 76 Q88 74 90 76 L90 82 L94 80 Q96 78 96 82 L96 100Z"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.8" fill={C.copper} fillOpacity="0.06" />
      {/* Layer lines on printed object */}
      {[88, 91, 94, 97].map((y) => (
        <line key={y} x1="66" y1={y} x2="95" y2={y}
          stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" />
      ))}

      {/* Fusion 360 wireframe / CAD view */}
      <rect x="160" y="24" width="130" height="90" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.02" />
      {/* CAD toolbar mockup */}
      <rect x="162" y="26" width="126" height="8" rx="1"
        fill={C.titanium} fillOpacity="0.04" />
      {[170, 180, 190, 200, 210].map((x) => (
        <rect key={x} x={x} y="28" width="6" height="4" rx="0.5"
          fill={C.titanium} fillOpacity="0.06" />
      ))}
      {/* Wireframe hand in CAD */}
      <path d="M200 110 L200 85 Q200 78 208 76 L208 65 Q210 62 212 65 L212 76 L216 68 Q218 66 220 68 L220 76 L224 72 Q226 70 228 72 L228 78 L232 76 Q234 74 234 78 L234 110"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.7" fill="none" />
      {/* Dimension lines */}
      <line x1="198" y1="65" x2="198" y2="110" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.3" />
      <line x1="196" y1="65" x2="200" y2="65" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.3" />
      <line x1="196" y1="110" x2="200" y2="110" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.3" />
      <text x="194" y="90" fill={C.copper} fillOpacity="0.2" fontSize="3.5" fontFamily="monospace" textAnchor="end">148mm</text>
      {/* Grid dots in CAD view */}
      {Array.from({ length: 5 }).map((_, row) =>
        Array.from({ length: 7 }).map((_, col) => (
          <circle key={`${row}-${col}`} cx={170 + col * 16} cy={42 + row * 16} r="0.5"
            fill={C.titanium} fillOpacity="0.08" />
        ))
      )}
      {/* Cross-section view */}
      <rect x="160" y="122" width="60" height="40" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <ellipse cx="190" cy="142" rx="20" ry="12"
        stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.5" fill={C.forest} fillOpacity="0.03" />
      <ellipse cx="190" cy="142" rx="14" ry="8"
        stroke={C.forest} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      <text x="164" y="130" fill={C.titanium} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">SECTION A-A</text>
      {/* Material specs */}
      <rect x="230" y="122" width="60" height="40" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <text x="234" y="132" fill={C.titanium} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">PLA+ 1.75mm</text>
      <text x="234" y="140" fill={C.titanium} fillOpacity="0.12" fontSize="3" fontFamily="monospace">Infill: 25%</text>
      <text x="234" y="148" fill={C.titanium} fillOpacity="0.12" fontSize="3" fontFamily="monospace">Layer: 0.2mm</text>
      <text x="234" y="156" fill={C.copper} fillOpacity="0.15" fontSize="3" fontFamily="monospace">FlowIt v3.2</text>
      {/* Labels */}
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">VA PROSTHETICS — CAD/CAM</text>
      <text x="45" y="145" fill={C.titanium} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">LAYER 847</text>
    </svg>
  )
}

function CinematographyScene() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.025} />
      {/* Camera body — BlackMagic 6K style */}
      <rect x="40" y="50" width="90" height="60" rx="4"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="1" fill={C.copper} fillOpacity="0.05" />
      {/* Lens barrel */}
      <rect x="8" y="58" width="32" height="44" rx="2"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.04" />
      <circle cx="24" cy="80" r="18" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill="none" />
      <circle cx="24" cy="80" r="12" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill="none" />
      <circle cx="24" cy="80" r="6" stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.5"
        fill={C.copper} fillOpacity="0.06" />
      {/* Iris blades hint */}
      {[0, 60, 120, 180, 240, 300].map((deg) => {
        const r = deg * Math.PI / 180
        return (
          <line key={deg}
            x1={24 + Math.cos(r) * 7} y1={80 + Math.sin(r) * 7}
            x2={24 + Math.cos(r) * 11} y2={80 + Math.sin(r) * 11}
            stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
        )
      })}
      {/* LCD viewfinder */}
      <rect x="52" y="56" width="40" height="24" rx="1"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill={C.copper} fillOpacity="0.04" />
      {/* Viewfinder waveform */}
      <polyline points="56,72 60,68 64,74 68,66 72,70 76,64 80,72 84,68 88,74"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.4" fill="none" />
      {/* Record indicator */}
      <circle cx="56" cy="60" r="2" fill="#E74C3C" fillOpacity="0.3" />
      <text x="60" y="62" fill="#E74C3C" fillOpacity="0.25" fontSize="3" fontFamily="monospace">REC</text>
      {/* Camera controls */}
      <circle cx="100" cy="58" r="4" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <circle cx="114" cy="58" r="4" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <rect x="95" y="92" width="30" height="12" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      {/* Handle / rig */}
      <line x1="130" y1="65" x2="150" y2="55" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.6" />
      <line x1="130" y1="95" x2="150" y2="105" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.6" />
      <line x1="150" y1="55" x2="150" y2="105" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" />

      {/* Timeline / NLE interface */}
      <rect x="170" y="30" width="130" height="80" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.02" />
      {/* Track lanes */}
      {[45, 55, 65, 75, 85, 95].map((y, i) => (
        <g key={y}>
          <line x1="172" y1={y} x2="298" y2={y}
            stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" />
          {/* Clips on tracks */}
          <rect x={175 + (i * 7) % 20} y={y + 1} width={30 + (i * 13) % 40} height={8}
            rx="1" fill={i < 2 ? C.copper : i < 4 ? C.forest : C.titanium}
            fillOpacity={0.08 + (i % 3) * 0.04} />
        </g>
      ))}
      {/* Playhead */}
      <line x1="235" y1="35" x2="235" y2="108"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.5" />
      <polygon points="232,35 238,35 235,38" fill={C.copper} fillOpacity="0.3" />
      {/* Timecode */}
      <text x="176" y="38" fill={C.titanium} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">01:24:18:07</text>
      <text x="260" y="38" fill={C.titanium} fillOpacity="0.12" fontSize="3" fontFamily="monospace">24fps</text>

      {/* Film strip / reel accent */}
      <rect x="170" y="120" width="130" height="40" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.015" />
      {/* Sprocket holes */}
      {Array.from({ length: 16 }).map((_, i) => (
        <rect key={i} x={174 + i * 8} y="122" width="3" height="4" rx="0.5"
          fill={C.titanium} fillOpacity="0.06" />
      ))}
      {Array.from({ length: 16 }).map((_, i) => (
        <rect key={`b${i}`} x={174 + i * 8} y="154" width="3" height="4" rx="0.5"
          fill={C.titanium} fillOpacity="0.06" />
      ))}
      {/* Frame thumbnails */}
      {Array.from({ length: 7 }).map((_, i) => (
        <rect key={i} x={174 + i * 18} y="128" width="14" height="24" rx="0.5"
          stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3"
          fill={i === 3 ? C.copper : C.titanium} fillOpacity={i === 3 ? 0.06 : 0.02} />
      ))}

      {/* Labels */}
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">BMPCC 6K — POST</text>
      <text x="50" y="120" fill={C.titanium} fillOpacity="0.12" fontSize="3.5" fontFamily="monospace">BLACKMAGIC 6K</text>
    </svg>
  )
}

/* ═══════════════════════════════════════════════════════
   CARD ICONS — Small decorative SVGs for AnimatedCard
   viewBox: 0 0 320 80
   ═══════════════════════════════════════════════════════ */

function ConvergintIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* Fire alarm pull station */}
      <rect x="20" y="10" width="28" height="36" rx="2"
        stroke="#E74C3C" strokeOpacity="0.25" strokeWidth="0.8" fill="#E74C3C" fillOpacity="0.04" />
      <rect x="26" y="22" width="16" height="8" rx="1"
        stroke="#E74C3C" strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      <line x1="34" y1="30" x2="34" y2="44" stroke="#E74C3C" strokeOpacity="0.2" strokeWidth="0.6" />
      {/* Detection network lines */}
      <line x1="48" y1="28" x2="100" y2="28" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" />
      <line x1="100" y1="10" x2="100" y2="50" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      {/* Smoke detector */}
      <circle cx="100" cy="18" r="8" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.03" />
      <circle cx="100" cy="18" r="3" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.3" fill="none" />
      {/* Sprinkler head */}
      <path d="M95 50 L100 44 L105 50" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      <line x1="100" y1="40" x2="100" y2="44" stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.4" />
      {/* NFPA code reference */}
      <rect x="130" y="15" width="50" height="30" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.02" />
      <text x="155" y="28" fill={C.titanium} fillOpacity="0.15" fontSize="4" fontFamily="monospace" textAnchor="middle">NFPA 72</text>
      <text x="155" y="38" fill={C.copper} fillOpacity="0.12" fontSize="3" fontFamily="monospace" textAnchor="middle">COMPLIANT</text>
      {/* Panel with status lights */}
      <rect x="210" y="12" width="44" height="36" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.03" />
      {[20, 26, 32, 38].map((y) => (
        <g key={y}>
          <circle cx="220" cy={y} r="2" fill="#2ECC71" fillOpacity="0.2" />
          <line x1="226" y1={y} x2="248" y2={y} stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" />
        </g>
      ))}
      <text x="20" y="68" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">FIRE & LIFE SAFETY</text>
    </svg>
  )
}

function OdooIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* ERP module blocks */}
      {[
        { x: 20, y: 12, w: 50, h: 24, label: 'MFG', color: C.copper },
        { x: 80, y: 12, w: 50, h: 24, label: 'CRM', color: C.forest },
        { x: 140, y: 12, w: 50, h: 24, label: 'INV', color: C.titanium },
        { x: 200, y: 12, w: 50, h: 24, label: 'ACC', color: C.copper },
      ].map((m) => (
        <g key={m.label}>
          <rect x={m.x} y={m.y} width={m.w} height={m.h} rx="2"
            stroke={m.color} strokeOpacity="0.25" strokeWidth="0.6" fill={m.color} fillOpacity="0.04" />
          <text x={m.x + m.w / 2} y={m.y + 15} fill={m.color} fillOpacity="0.2" fontSize="4.5" fontFamily="monospace" textAnchor="middle">{m.label}</text>
        </g>
      ))}
      {/* Connection lines between modules */}
      <line x1="70" y1="24" x2="80" y2="24" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      <line x1="130" y1="24" x2="140" y2="24" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      <line x1="190" y1="24" x2="200" y2="24" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      {/* Revenue bar chart */}
      {[30, 50, 70, 90, 110].map((x, i) => (
        <rect key={x} x={x} y={58 - (i + 1) * 6} width="12" height={(i + 1) * 6 + 8} rx="1"
          fill={C.copper} fillOpacity={0.06 + i * 0.02} />
      ))}
      <text x="30" y="72" fill={C.copper} fillOpacity="0.15" fontSize="3" fontFamily="monospace">160% NRR</text>
      {/* Workflow arrows */}
      <path d="M160 50 L180 50 L180 60 L200 60" stroke={C.forest} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      <polygon points="200,58 204,60 200,62" fill={C.forest} fillOpacity="0.12" />
      <text x="210" y="68" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">ERP INTEGRATION</text>
    </svg>
  )
}

function HapsIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* House cross-section */}
      <path d="M30 45 L55 20 L80 45" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill="none" />
      <rect x="35" y="45" width="40" height="25" rx="1"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.03" />
      {/* PM2.5 particles floating inside */}
      {[{x:42,y:38},{x:55,y:35},{x:68,y:40},{x:48,y:50},{x:62,y:48},{x:50,y:58},{x:65,y:55}].map((p, i) => (
        <circle key={i} cx={p.x} cy={p.y} r={0.8 + (i % 3) * 0.4}
          fill={C.copper} fillOpacity={0.1 + (i % 3) * 0.06} />
      ))}
      {/* Air monitor device */}
      <rect x="100" y="30" width="20" height="28" rx="2"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.6" fill={C.forest} fillOpacity="0.04" />
      <circle cx="110" cy="40" r="4" stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <text x="110" y="54" fill={C.forest} fillOpacity="0.15" fontSize="3" fontFamily="monospace" textAnchor="middle">PM2.5</text>
      {/* Data chart — BP correlation */}
      <rect x="150" y="15" width="80" height="50" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.015" />
      <polyline points="155,55 165,50 175,52 185,42 195,38 205,30 215,25 225,22"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.7" fill="none" />
      <text x="155" y="12" fill={C.titanium} fillOpacity="0.12" fontSize="3" fontFamily="monospace">BC vs SBP</text>
      {/* NO2 label */}
      <rect x="250" y="25" width="40" height="18" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill={C.copper} fillOpacity="0.03" />
      <text x="270" y="37" fill={C.copper} fillOpacity="0.2" fontSize="4" fontFamily="monospace" textAnchor="middle">NO₂</text>
      <text x="20" y="76" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">INDOOR AIR QUALITY</text>
    </svg>
  )
}

function SwisIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.forest} opacity={0.025} />
      {/* River cross section */}
      <path d="M10 35 C50 30 100 25 160 25 C220 25 270 30 310 35"
        stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.6" fill="none" />
      <path d="M10 35 C50 40 100 50 160 55 C220 50 270 40 310 35"
        stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.5" fill={C.forest} fillOpacity="0.04" />
      {/* Saltwater wedge intruding */}
      <path d="M240 55 C260 50 280 42 310 35 L310 55Z"
        fill={C.copper} fillOpacity="0.08" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" />
      <text x="265" y="50" fill={C.copper} fillOpacity="0.18" fontSize="3.5" fontFamily="monospace">SALT</text>
      <text x="120" y="45" fill={C.forest} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">FRESH</text>
      {/* Water intake pipe */}
      <rect x="150" y="18" width="4" height="12" rx="0.5"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.06" />
      <rect x="142" y="10" width="20" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.03" />
      {/* Chloride concentration chart */}
      <rect x="30" y="58" width="80" height="18" rx="1"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.015" />
      <polyline points="35,72 45,70 55,68 65,66 75,64 85,58 95,55 105,55"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      <text x="35" y="56" fill={C.titanium} fillOpacity="0.1" fontSize="3" fontFamily="monospace">Cl⁻ mg/L</text>
      {/* Population stat */}
      <text x="200" y="70" fill={C.titanium} fillOpacity="0.12" fontSize="3.5" fontFamily="monospace">1.2M RESIDENTS</text>
    </svg>
  )
}

function WimleyLabIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.forest} opacity={0.025} />
      {/* Lipid bilayer membrane */}
      {Array.from({ length: 16 }).map((_, i) => (
        <g key={i}>
          {/* Upper leaflet */}
          <circle cx={20 + i * 18} cy="30" r="3"
            fill={C.forest} fillOpacity="0.1" stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.3" />
          <line x1={20 + i * 18} y1="33" x2={20 + i * 18} y2="40"
            stroke={C.forest} strokeOpacity="0.08" strokeWidth="0.3" />
          {/* Lower leaflet */}
          <circle cx={20 + i * 18} cy="50" r="3"
            fill={C.forest} fillOpacity="0.1" stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.3" />
          <line x1={20 + i * 18} y1="47" x2={20 + i * 18} y2="40"
            stroke={C.forest} strokeOpacity="0.08" strokeWidth="0.3" />
        </g>
      ))}
      {/* Peptide helix spanning membrane */}
      <path d="M140 18 C148 24 132 30 140 36 C148 42 132 48 140 54 C148 60 140 62 140 62"
        stroke={C.copper} strokeOpacity="0.35" strokeWidth="1" fill="none" />
      {/* Alpha helix backbone hint */}
      {[22, 30, 38, 46, 54].map((y) => (
        <circle key={y} cx={140 + ((y % 16) - 8)} cy={y} r="1.5"
          fill={C.copper} fillOpacity="0.12" />
      ))}
      {/* Molecular formula */}
      <text x="200" y="25" fill={C.titanium} fillOpacity="0.12" fontSize="3.5" fontFamily="monospace">PEPTIDE ASSEMBLY</text>
      <text x="200" y="45" fill={C.copper} fillOpacity="0.15" fontSize="3" fontFamily="monospace">pH-RESPONSIVE</text>
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">MEMBRANE BIOPHYSICS</text>
    </svg>
  )
}

function OurClimateIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.forest} opacity={0.025} />
      {/* Globe outline */}
      <circle cx="50" cy="40" r="25" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.6" fill={C.forest} fillOpacity="0.03" />
      <ellipse cx="50" cy="40" rx="12" ry="25" stroke={C.forest} strokeOpacity="0.1" strokeWidth="0.4" fill="none" />
      <line x1="25" y1="40" x2="75" y2="40" stroke={C.forest} strokeOpacity="0.08" strokeWidth="0.3" />
      <ellipse cx="50" cy="30" rx="22" ry="6" stroke={C.forest} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      <ellipse cx="50" cy="50" rx="22" ry="6" stroke={C.forest} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      {/* Temperature curve rising */}
      <polyline points="110,60 130,56 150,52 170,48 190,40 210,32 230,22"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.7" fill="none" />
      <text x="115" y="68" fill={C.titanium} fillOpacity="0.1" fontSize="3" fontFamily="monospace">POLICY ADVOCACY</text>
      {/* Leaf / sustainability */}
      <path d="M270 28 C255 20 250 35 260 42 C265 35 275 30 270 28Z"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.6" fill={C.forest} fillOpacity="0.06" />
      <text x="250" y="56" fill={C.forest} fillOpacity="0.12" fontSize="3.5" fontFamily="monospace">FELLOWSHIP</text>
    </svg>
  )
}

function TabiIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* Signal tower */}
      <rect x="50" y="20" width="6" height="50" rx="1"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.05" />
      {/* Signal arcs */}
      {[15, 22, 29].map((r, i) => (
        <path key={i} d={`M53 20 A${r} ${r} 0 0 1 ${53 + r * 0.7} ${20 + r * 0.7}`}
          stroke={C.copper} strokeOpacity={0.2 - i * 0.05} strokeWidth="0.5" fill="none" />
      ))}
      {/* Rural houses */}
      {[120, 160, 200].map((x, i) => (
        <g key={i}>
          <path d={`M${x} 50 L${x + 10} 40 L${x + 20} 50`}
            stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
          <rect x={x + 2} y="50" width="16" height="12" rx="0.5"
            stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.02" />
          {/* Connection line from tower */}
          <line x1="56" y1={30 + i * 5} x2={x + 10} y2={45}
            stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" strokeDasharray="3 3" />
        </g>
      ))}
      <text x="20" y="76" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">BROADBAND ACCESS</text>
    </svg>
  )
}

function NolaEastIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.forest} opacity={0.025} />
      {/* Solar panel array */}
      <g transform="translate(20,15)">
        {Array.from({ length: 3 }).map((_, row) =>
          Array.from({ length: 4 }).map((_, col) => (
            <rect key={`${row}-${col}`} x={col * 14} y={row * 10}
              width="12" height="8" rx="0.5"
              stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.4" fill={C.forest} fillOpacity="0.04" />
          ))
        )}
      </g>
      {/* Transit line */}
      <line x1="100" y1="40" x2="250" y2="40"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="1" />
      {[120, 160, 200, 240].map((x) => (
        <circle key={x} cx={x} cy="40" r="2.5"
          stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.4" fill={C.copper} fillOpacity="0.06" />
      ))}
      {/* Green housing */}
      <path d="M270 50 L280 38 L290 50" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      <rect x="273" y="50" width="14" height="14" rx="0.5"
        stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.4" fill={C.forest} fillOpacity="0.03" />
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">C40 REINVENTING CITIES</text>
    </svg>
  )
}

function MidtownMetairieIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* Street grid plan */}
      {[20, 55, 90, 125].map((x) => (
        <line key={x} x1={x} y1="10" x2={x} y2="65"
          stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      ))}
      {[20, 35, 50, 65].map((y) => (
        <line key={y} x1="20" y1={y} x2="125" y2={y}
          stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      ))}
      {/* Highlighted zone */}
      <rect x="55" y="35" width="35" height="15" rx="1"
        fill={C.copper} fillOpacity="0.08" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" />
      {/* Building footprints */}
      {[{x:24,y:23,w:10,h:8},{x:60,y:12,w:25,h:18},{x:95,y:40,w:20,h:12}].map((b, i) => (
        <rect key={i} x={b.x} y={b.y} width={b.w} height={b.h} rx="0.5"
          fill={C.titanium} fillOpacity="0.04" />
      ))}
      {/* Zoning legend */}
      <g transform="translate(170,15)">
        {[
          { label: 'RES', color: C.forest },
          { label: 'COM', color: C.copper },
          { label: 'MXD', color: C.titanium },
        ].map((z, i) => (
          <g key={i}>
            <rect x={0} y={i * 14} width="8" height="8" rx="1"
              fill={z.color} fillOpacity="0.1" />
            <text x="14" y={i * 14 + 7} fill={z.color} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">{z.label}</text>
          </g>
        ))}
      </g>
      <text x="20" y="76" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">URBAN PLANNING</text>
    </svg>
  )
}

function PartnershipIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* Federal building */}
      <rect x="30" y="25" width="60" height="35" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.03" />
      <path d="M25 25 L60 10 L95 25" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      {[40, 52, 64, 76].map((x) => (
        <line key={x} x1={x} y1="28" x2={x} y2="58"
          stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      ))}
      {/* Engagement score gauge */}
      <g transform="translate(160,40)">
        <path d="M-25 0 A25 25 0 0 1 25 0" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill="none" />
        <path d="M-25 0 A25 25 0 0 1 25 0" stroke={C.copper} strokeOpacity="0.25" strokeWidth="2"
          strokeDasharray="78.5" strokeDashoffset="39" fill="none" />
        <text x="0" y="-5" fill={C.copper} fillOpacity="0.3" fontSize="8" fontFamily="monospace" textAnchor="middle">74</text>
        <text x="0" y="4" fill={C.titanium} fillOpacity="0.12" fontSize="3" fontFamily="monospace" textAnchor="middle">ENGAGEMENT</text>
      </g>
      {/* Arrow from 37 to 74 */}
      <text x="230" y="30" fill={C.titanium} fillOpacity="0.12" fontSize="5" fontFamily="monospace">37</text>
      <line x1="245" y1="28" x2="265" y2="28" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" />
      <polygon points="265,26 270,28 265,30" fill={C.copper} fillOpacity="0.2" />
      <text x="275" y="30" fill={C.copper} fillOpacity="0.2" fontSize="5" fontFamily="monospace">74</text>
      <text x="230" y="42" fill={C.titanium} fillOpacity="0.1" fontSize="3" fontFamily="monospace">SAMHSA SCORE</text>
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">FEDERAL SERVICE</text>
    </svg>
  )
}

/* ═══════════════════════════════════════════════════════
   STUDIO GALLERY SCENES — Various aspect ratios
   ═══════════════════════════════════════════════════════ */

function TulaneFreeman() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.025} />
      {/* Campus building facade */}
      <rect x="60" y="40" width="200" height="100" rx="2"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      {/* Columns */}
      {[80, 110, 140, 170, 200, 230].map((x) => (
        <line key={x} x1={x} y1="45" x2={x} y2="138"
          stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.6" />
      ))}
      {/* Camera on tripod */}
      <rect x="30" y="90" width="18" height="12" rx="1"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.6" fill={C.copper} fillOpacity="0.05" />
      <circle cx="26" cy="96" r="6" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <line x1="39" y1="102" x2="32" y2="140" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" />
      <line x1="39" y1="102" x2="46" y2="140" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" />
      {/* Video frame overlay */}
      <rect x="90" y="60" width="140" height="70" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      {/* Rule of thirds */}
      <line x1="137" y1="60" x2="137" y2="130" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="183" y1="60" x2="183" y2="130" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="90" y1="83" x2="230" y2="83" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="90" y1="107" x2="230" y2="107" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">FREEMAN SCHOOL — DIGITAL</text>
    </svg>
  )
}

function FracturedFutures() {
  return (
    <svg viewBox="0 0 180 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={180} h={180} gap={18} color={C.copper} opacity={0.025} />
      {/* Glass sheet with fracture lines */}
      <rect x="30" y="30" width="120" height="120" rx="2"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.8" fill={C.copper} fillOpacity="0.03" />
      {/* Fracture pattern radiating from impact point */}
      {[
        'M90 90 L45 35', 'M90 90 L140 40', 'M90 90 L150 85',
        'M90 90 L145 140', 'M90 90 L80 148', 'M90 90 L35 130',
        'M90 90 L32 75', 'M90 90 L55 42', 'M90 90 L128 50',
        'M90 90 L148 110', 'M90 90 L110 148', 'M90 90 L40 100',
      ].map((d, i) => (
        <path key={i} d={d}
          stroke={C.copper} strokeOpacity={0.08 + (i % 4) * 0.04} strokeWidth="0.4" />
      ))}
      {/* Concentric rings from impact */}
      {[15, 30, 45].map((r) => (
        <circle key={r} cx="90" cy="90" r={r}
          stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      ))}
      {/* Impact point */}
      <circle cx="90" cy="90" r="4" fill={C.copper} fillOpacity="0.15" />
      {/* Glass shards with different opacities */}
      <path d="M55 42 L90 90 L45 35Z" fill={C.copper} fillOpacity="0.04" />
      <path d="M128 50 L90 90 L140 40Z" fill={C.copper} fillOpacity="0.06" />
      <path d="M148 110 L90 90 L150 85Z" fill={C.copper} fillOpacity="0.03" />
      <path d="M110 148 L90 90 L145 140Z" fill={C.copper} fillOpacity="0.05" />
      {/* Kiln temperature */}
      <text x="36" y="165" fill={C.copper} fillOpacity="0.15" fontSize="4" fontFamily="monospace">1475°F KILN</text>
    </svg>
  )
}

function VogueItaly() {
  return (
    <svg viewBox="0 0 180 240" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={180} h={240} gap={18} color={C.titanium} opacity={0.02} />
      {/* Runway lines */}
      <line x1="90" y1="0" x2="90" y2="240" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.4" />
      <line x1="50" y1="0" x2="50" y2="240" stroke={C.titanium} strokeOpacity="0.03" strokeWidth="0.3" />
      <line x1="130" y1="0" x2="130" y2="240" stroke={C.titanium} strokeOpacity="0.03" strokeWidth="0.3" />
      {/* Figure silhouette — fashion sketch style */}
      <circle cx="90" cy="35" r="10" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.6" fill="none" />
      <line x1="90" y1="45" x2="90" y2="120" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" />
      <line x1="90" y1="60" x2="70" y2="85" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      <line x1="90" y1="60" x2="110" y2="85" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      <line x1="90" y1="120" x2="75" y2="180" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      <line x1="90" y1="120" x2="105" y2="180" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      {/* Garment outline suggestion */}
      <path d="M75 55 Q70 80 72 100 Q75 115 90 120 Q105 115 108 100 Q110 80 105 55"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.02" />
      {/* Spotlight arcs */}
      <path d="M20 0 Q50 30 90 35" stroke={C.copper} strokeOpacity="0.04" strokeWidth="0.3" fill="none" />
      <path d="M160 0 Q130 30 90 35" stroke={C.copper} strokeOpacity="0.04" strokeWidth="0.3" fill="none" />
      {/* Magazine masthead hint */}
      <text x="90" y="215" fill={C.titanium} fillOpacity="0.1" fontSize="5" fontFamily="monospace" textAnchor="middle">VOGUE ITALIA</text>
      <text x="90" y="225" fill={C.copper} fillOpacity="0.08" fontSize="3.5" fontFamily="monospace" textAnchor="middle">BizarrAudi 2020</text>
    </svg>
  )
}

function DocumentaryWork() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.025} />
      {/* Film slate / clapperboard */}
      <rect x="30" y="30" width="80" height="60" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.03" />
      <rect x="30" y="30" width="80" height="14" rx="2"
        fill={C.titanium} fillOpacity="0.06" />
      {/* Clapper stripes */}
      {[35, 45, 55, 65, 75, 85, 95].map((x, i) => (
        i % 2 === 0 ? <rect key={x} x={x} y="30" width="5" height="14"
          fill={C.titanium} fillOpacity="0.12" /> : null
      ))}
      {/* Slate text */}
      <text x="40" y="58" fill={C.titanium} fillOpacity="0.15" fontSize="4" fontFamily="monospace">SCENE</text>
      <text x="40" y="68" fill={C.titanium} fillOpacity="0.12" fontSize="3.5" fontFamily="monospace">TAKE</text>
      <text x="40" y="78" fill={C.copper} fillOpacity="0.15" fontSize="3.5" fontFamily="monospace">PLATO&apos;S CAVE</text>
      {/* Film reel */}
      <circle cx="220" cy="70" r="35" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.6" fill="none" />
      <circle cx="220" cy="70" r="25" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" fill="none" />
      <circle cx="220" cy="70" r="10" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.03" />
      <circle cx="220" cy="70" r="3" fill={C.titanium} fillOpacity="0.08" />
      {/* Spokes */}
      {[0, 72, 144, 216, 288].map((deg) => {
        const r = deg * Math.PI / 180
        return (
          <line key={deg}
            x1={220 + Math.cos(r) * 10} y1={70 + Math.sin(r) * 10}
            x2={220 + Math.cos(r) * 25} y2={70 + Math.sin(r) * 25}
            stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.4" />
        )
      })}
      {/* Waveform */}
      <rect x="30" y="110" width="260" height="40" rx="1"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.015" />
      {Array.from({ length: 50 }).map((_, i) => {
        const h = 4 + Math.abs(Math.sin(i * 0.7)) * 16
        return (
          <line key={i} x1={36 + i * 5} y1={130 - h / 2} x2={36 + i * 5} y2={130 + h / 2}
            stroke={C.copper} strokeOpacity="0.12" strokeWidth="1" />
        )
      })}
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">DOCUMENTARY — NARRATIVE</text>
    </svg>
  )
}

function MediumFormat() {
  return (
    <svg viewBox="0 0 192 240" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={192} h={240} gap={16} color={C.titanium} opacity={0.02} />
      {/* Camera body — medium format */}
      <rect x="36" y="40" width="120" height="100" rx="4"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.03" />
      {/* Lens */}
      <circle cx="96" cy="90" r="30" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill="none" />
      <circle cx="96" cy="90" r="22" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      <circle cx="96" cy="90" r="12" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill={C.copper} fillOpacity="0.04" />
      {/* Focus ring markings */}
      {Array.from({ length: 12 }).map((_, i) => {
        const r = (i * 30) * Math.PI / 180
        return (
          <line key={i}
            x1={96 + Math.cos(r) * 24} y1={90 + Math.sin(r) * 24}
            x2={96 + Math.cos(r) * 28} y2={90 + Math.sin(r) * 28}
            stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" />
        )
      })}
      {/* Viewfinder */}
      <rect x="66" y="44" width="60" height="10" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.04" />
      {/* Film back */}
      <rect x="44" y="150" width="104" height="50" rx="2"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.02" />
      <text x="96" y="175" fill={C.titanium} fillOpacity="0.1" fontSize="4" fontFamily="monospace" textAnchor="middle">120 FILM</text>
      {/* Film sprocket holes */}
      {Array.from({ length: 6 }).map((_, i) => (
        <rect key={i} x={52 + i * 16} y="190" width="4" height="3" rx="0.5"
          fill={C.titanium} fillOpacity="0.06" />
      ))}
      {/* Aperture label */}
      <text x="96" y="225" fill={C.copper} fillOpacity="0.12" fontSize="3.5" fontFamily="monospace" textAnchor="middle">f/2.8 — 80mm</text>
    </svg>
  )
}

function AuroraTheatre() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.025} />
      {/* Stage / proscenium arch */}
      <path d="M60 150 L60 40 Q160 10 260 40 L260 150"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.8" fill="none" />
      <line x1="60" y1="150" x2="260" y2="150"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" />
      {/* Stage floor */}
      <rect x="70" y="115" width="180" height="35" rx="1"
        fill={C.titanium} fillOpacity="0.02" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" />
      {/* Stage lights */}
      {[100, 140, 180, 220].map((x, i) => (
        <g key={i}>
          <rect x={x - 4} y="42" width="8" height="6" rx="1"
            stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill={C.copper} fillOpacity="0.04" />
          {/* Light beams */}
          <path d={`M${x} 48 L${x - 15} 115 L${x + 15} 115Z`}
            fill={C.copper} fillOpacity="0.02" />
        </g>
      ))}
      {/* Curtain drape lines */}
      <path d="M62 40 C62 50 65 60 62 80 C65 100 62 120 62 150"
        stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.5" fill="none" />
      <path d="M258 40 C258 50 255 60 258 80 C255 100 258 120 258 150"
        stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.5" fill="none" />
      {/* Camera position marker */}
      <rect x="145" y="155" width="30" height="16" rx="2"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.03" />
      <circle cx="152" cy="163" r="4" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.3" fill="none" />
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">AURORA THEATRE — PRODUCTION</text>
    </svg>
  )
}

function BuffaloCentralTerminal() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.025} />
      {/* Central tower */}
      <rect x="130" y="18" width="60" height="130" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.03" />
      {/* Tower top ornament */}
      <rect x="140" y="10" width="40" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      <line x1="160" y1="2" x2="160" y2="10" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" />
      {/* Art deco window pattern */}
      {Array.from({ length: 6 }).map((_, row) =>
        [140, 155, 170].map((x) => (
          <rect key={`${row}-${x}`} x={x} y={24 + row * 18} width="10" height="14" rx="1"
            stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3"
            fill={C.titanium} fillOpacity="0.02" />
        ))
      )}
      {/* Side wings */}
      <rect x="50" y="70" width="80" height="78" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <rect x="190" y="70" width="80" height="78" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      {/* Art deco chevrons */}
      <path d="M145 148 L160 138 L175 148" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      <path d="M148 152 L160 144 L172 152" stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" fill="none" />
      {/* Train tracks hint */}
      <line x1="20" y1="160" x2="300" y2="160"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      <line x1="20" y1="165" x2="300" y2="165"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.5" />
      {[40, 60, 80, 100, 120, 140, 160, 180, 200, 220, 240, 260, 280].map((x) => (
        <line key={x} x1={x} y1="158" x2={x} y2="167"
          stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.3" />
      ))}
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">BUFFALO CENTRAL TERMINAL</text>
    </svg>
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

/** Card icons — wide banner format for AnimatedCard */
export const cardIcons: Record<string, () => ReactNode> = {
  // Engineering
  enfra: EnfraScene,
  convergint: ConvergintIcon,
  odoo: OdooIcon,
  'va-prosthetics': VaProstheticsScene,
  haps: HapsIcon,
  swis: SwisIcon,
  'wimley-lab': WimleyLabIcon,
  // Advocacy
  ycod: YcodScene,
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
