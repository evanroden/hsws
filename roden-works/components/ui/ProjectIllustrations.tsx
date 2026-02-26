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
      {/* Continent hints on globe */}
      <path d="M38 32 Q42 28 48 30 Q50 34 46 36" stroke={C.forest} strokeOpacity="0.08" strokeWidth="0.3" fill={C.forest} fillOpacity="0.03" />
      <path d="M54 42 Q58 38 62 40 Q60 46 56 48" stroke={C.forest} strokeOpacity="0.06" strokeWidth="0.3" fill={C.forest} fillOpacity="0.02" />
      {/* CO2 molecule */}
      <circle cx="95" cy="22" r="4" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <circle cx="85" cy="22" r="3" stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.3" fill="none" />
      <circle cx="105" cy="22" r="3" stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.3" fill="none" />
      <text x="95" y="24" fill={C.copper} fillOpacity="0.12" fontSize="3" fontFamily="monospace" textAnchor="middle">C</text>
      <text x="85" y="24" fill={C.copper} fillOpacity="0.1" fontSize="2.5" fontFamily="monospace" textAnchor="middle">O</text>
      <text x="105" y="24" fill={C.copper} fillOpacity="0.1" fontSize="2.5" fontFamily="monospace" textAnchor="middle">O</text>
      {/* Temperature anomaly chart */}
      <rect x="115" y="12" width="90" height="52" rx="1"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.015" />
      {/* Baseline */}
      <line x1="118" y1="44" x2="202" y2="44" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" strokeDasharray="2 2" />
      {/* Temperature bars — hockey stick */}
      {[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14].map((i) => {
        const h = 2 + (i < 8 ? i * 0.8 : (i - 4) * 2.5)
        return (
          <rect key={i} x={120 + i * 5.5} y={44 - h} width="4" height={h} rx="0.5"
            fill={h > 10 ? C.copper : C.forest} fillOpacity={0.06 + (i / 15) * 0.12} />
        )
      })}
      <text x="118" y="10" fill={C.titanium} fillOpacity="0.1" fontSize="2.5" fontFamily="monospace">TEMP ANOMALY °C</text>
      {/* Policy document */}
      <rect x="220" y="14" width="36" height="44" rx="1"
        stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.4" fill={C.forest} fillOpacity="0.03" />
      {/* Document lines */}
      {[22, 28, 34, 40, 46].map((y) => (
        <line key={y} x1="225" y1={y} x2={248 - (y % 6)} y2={y}
          stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" />
      ))}
      {/* Seal / stamp */}
      <circle cx="242" cy="48" r="5" stroke={C.forest} strokeOpacity="0.12" strokeWidth="0.3" fill={C.forest} fillOpacity="0.04" />
      {/* Leaf / sustainability */}
      <path d="M280 22 C265 14 258 30 270 38 C275 30 288 24 280 22Z"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.6" fill={C.forest} fillOpacity="0.06" />
      <line x1="270" y1="38" x2="274" y2="28" stroke={C.forest} strokeOpacity="0.12" strokeWidth="0.3" />
      {/* Wind turbine */}
      <line x1="290" y1="22" x2="290" y2="58" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" />
      <path d="M290 22 L286 10 L290 14Z" fill={C.forest} fillOpacity="0.08" />
      <path d="M290 22 L298 28 L292 24Z" fill={C.forest} fillOpacity="0.06" />
      <path d="M290 22 L284 30 L288 24Z" fill={C.forest} fillOpacity="0.07" />
      <circle cx="290" cy="22" r="1.5" fill={C.forest} fillOpacity="0.1" />
      <text x="115" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3" fontFamily="monospace">CLIMATE POLICY FELLOWSHIP</text>
    </svg>
  )
}

function TabiIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* Signal tower */}
      <rect x="50" y="15" width="6" height="55" rx="1"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.05" />
      {/* Tower cross-braces */}
      <line x1="50" y1="35" x2="56" y2="45" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" />
      <line x1="56" y1="35" x2="50" y2="45" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" />
      <line x1="50" y1="50" x2="56" y2="60" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" />
      <line x1="56" y1="50" x2="50" y2="60" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" />
      {/* Signal arcs — both sides */}
      {[12, 19, 26].map((r, i) => (
        <g key={i}>
          <path d={`M53 18 A${r} ${r} 0 0 1 ${53 + r * 0.7} ${18 + r * 0.7}`}
            stroke={C.copper} strokeOpacity={0.2 - i * 0.05} strokeWidth="0.5" fill="none" />
          <path d={`M53 18 A${r} ${r} 0 0 0 ${53 - r * 0.7} ${18 + r * 0.7}`}
            stroke={C.copper} strokeOpacity={0.15 - i * 0.04} strokeWidth="0.4" fill="none" />
        </g>
      ))}
      {/* Fiber optic line underground */}
      <line x1="56" y1="70" x2="300" y2="70"
        stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.8" />
      <line x1="56" y1="70" x2="300" y2="70"
        stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.8" strokeDasharray="6 4" />
      {/* Rural houses with details */}
      {[110, 155, 200, 245].map((x, i) => (
        <g key={i}>
          <path d={`M${x} 48 L${x + 10} 38 L${x + 20} 48`}
            stroke={C.titanium} strokeOpacity={0.12 + i * 0.02} strokeWidth="0.4" fill="none" />
          <rect x={x + 2} y="48" width="16" height="12" rx="0.5"
            stroke={C.titanium} strokeOpacity={0.1 + i * 0.01} strokeWidth="0.3" fill={C.titanium} fillOpacity="0.02" />
          {/* Window */}
          <rect x={x + 5} y="51" width="4" height="4" rx="0.3"
            fill={C.titanium} fillOpacity="0.04" />
          <rect x={x + 11} y="51" width="4" height="4" rx="0.3"
            fill={C.titanium} fillOpacity="0.04" />
          {/* WiFi symbol above house */}
          <path d={`M${x + 8} 34 A4 4 0 0 1 ${x + 16} 34`}
            stroke={C.copper} strokeOpacity={0.08 + i * 0.03} strokeWidth="0.3" fill="none" />
          <path d={`M${x + 10} 36 A2 2 0 0 1 ${x + 14} 36`}
            stroke={C.copper} strokeOpacity={0.06 + i * 0.03} strokeWidth="0.3" fill="none" />
          {/* Riser from fiber to house */}
          <line x1={x + 10} y1="60" x2={x + 10} y2="70"
            stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" />
        </g>
      ))}
      {/* Speed readout */}
      <rect x="280" y="15" width="32" height="16" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill={C.copper} fillOpacity="0.03" />
      <text x="296" y="26" fill={C.copper} fillOpacity="0.2" fontSize="4" fontFamily="monospace" textAnchor="middle">1Gbps</text>
      {/* Coverage stat */}
      <text x="280" y="42" fill={C.titanium} fillOpacity="0.1" fontSize="3" fontFamily="monospace">CAYUGA CO.</text>
      {/* Trees (rural landscape) */}
      {[90, 140, 185, 230, 270].map((x, i) => (
        <g key={i}>
          <line x1={x} y1={52 + (i % 2) * 3} x2={x} y2={62 + (i % 2) * 3}
            stroke={C.forest} strokeOpacity="0.06" strokeWidth="0.3" />
          <circle cx={x} cy={50 + (i % 2) * 3} r={2 + (i % 2)}
            fill={C.forest} fillOpacity="0.04" />
        </g>
      ))}
      <text x="20" y="76" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">RURAL BROADBAND ACCESS</text>
    </svg>
  )
}

function NolaEastIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.forest} opacity={0.025} />
      {/* Solar panel array */}
      <g transform="translate(20,10)">
        {Array.from({ length: 3 }).map((_, row) =>
          Array.from({ length: 4 }).map((_, col) => (
            <rect key={`${row}-${col}`} x={col * 14} y={row * 10}
              width="12" height="8" rx="0.5"
              stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.4" fill={C.forest} fillOpacity="0.04" />
          ))
        )}
        {/* Panel grid lines */}
        {Array.from({ length: 3 }).map((_, row) =>
          Array.from({ length: 4 }).map((_, col) => (
            <g key={`grid-${row}-${col}`}>
              <line x1={col * 14 + 6} y1={row * 10} x2={col * 14 + 6} y2={row * 10 + 8}
                stroke={C.forest} strokeOpacity="0.06" strokeWidth="0.2" />
              <line x1={col * 14} y1={row * 10 + 4} x2={col * 14 + 12} y2={row * 10 + 4}
                stroke={C.forest} strokeOpacity="0.06" strokeWidth="0.2" />
            </g>
          ))
        )}
        {/* Sun rays */}
        <circle cx="3" cy="-4" r="3" fill={C.copper} fillOpacity="0.06" />
        {[0, 45, 90, 135, 180, 225, 270, 315].map((deg) => {
          const r = deg * Math.PI / 180
          return (
            <line key={deg} x1={3 + Math.cos(r) * 4} y1={-4 + Math.sin(r) * 4}
              x2={3 + Math.cos(r) * 7} y2={-4 + Math.sin(r) * 7}
              stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
          )
        })}
      </g>
      {/* Transit line with stations */}
      <line x1="95" y1="40" x2="250" y2="40"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="1.2" />
      {[110, 145, 180, 215].map((x, i) => (
        <g key={x}>
          <circle cx={x} cy="40" r="3"
            stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.4" fill={C.copper} fillOpacity="0.06" />
          <circle cx={x} cy="40" r="1" fill={C.copper} fillOpacity="0.15" />
          {/* Station label */}
          <line x1={x} y1="43" x2={x} y2="48" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.2" />
        </g>
      ))}
      {/* Streetcar symbol */}
      <rect x="225" y="36" width="14" height="8" rx="2"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill={C.copper} fillOpacity="0.04" />
      <line x1="228" y1="36" x2="228" y2="33" stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.3" />
      {/* Green housing cluster */}
      {[260, 278, 296].map((x, i) => (
        <g key={i}>
          <path d={`M${x} 42 L${x + 7} 32 L${x + 14} 42`}
            stroke={C.forest} strokeOpacity={0.15 + i * 0.03} strokeWidth="0.4" fill="none" />
          <rect x={x + 1} y="42" width="12" height="14" rx="0.5"
            stroke={C.forest} strokeOpacity={0.12 + i * 0.02} strokeWidth="0.3" fill={C.forest} fillOpacity="0.03" />
          {/* Green roof indicator */}
          <line x1={x + 2} y1={33 + i} x2={x + 12} y2={33 + i}
            stroke={C.forest} strokeOpacity="0.1" strokeWidth="0.8" />
          {/* Window */}
          <rect x={x + 4} y="46" width="3" height="4" rx="0.3" fill={C.titanium} fillOpacity="0.03" />
        </g>
      ))}
      {/* Flood resilience — levee cross-section */}
      <path d="M95 62 C120 58 150 56 180 56 C210 56 240 58 270 62"
        stroke={C.forest} strokeOpacity="0.08" strokeWidth="0.4" fill="none" />
      <path d="M95 62 C120 66 150 68 180 68 C210 68 240 66 270 62"
        fill={C.forest} fillOpacity="0.02" />
      {/* C40 badge */}
      <rect x="20" y="48" width="28" height="14" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill={C.copper} fillOpacity="0.03" />
      <text x="34" y="58" fill={C.copper} fillOpacity="0.2" fontSize="4" fontFamily="monospace" textAnchor="middle">C40</text>
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">REINVENTING CITIES — AWARD</text>
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

/* Card-specific versions of featured projects (320×80 format) */

function EnfraIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} opacity={0.025} />
      {/* Boiler */}
      <rect x="20" y="10" width="40" height="50" rx="2"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.8" fill={C.copper} fillOpacity="0.04" />
      <rect x="34" y="2" width="8" height="10" rx="1"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      {/* Steam wisp */}
      <path d="M37 2 C36 -2 39 -4 38 -6" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.5" />
      {/* Fire tubes */}
      {[22, 30, 38, 46].map((y) => (
        <line key={y} x1="25" y1={y} x2="55" y2={y}
          stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" />
      ))}
      {/* Pressure gauge */}
      <circle cx="48" cy="30" r="6" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.03" />
      <line x1="48" y1="30" x2="52" y2="26" stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.5" />
      <circle cx="48" cy="30" r="1" fill={C.copper} fillOpacity="0.25" />
      {/* Supply pipe */}
      <line x1="60" y1="28" x2="160" y2="28"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="2" />
      <polygon points="100,25.5 106,28 100,30.5" fill={C.copper} fillOpacity="0.22" />
      {/* Return pipe */}
      <line x1="60" y1="44" x2="160" y2="44"
        stroke={C.forest} strokeOpacity="0.18" strokeWidth="1.5" />
      <polygon points="130,41.5 124,44 130,46.5" fill={C.forest} fillOpacity="0.18" />
      {/* Cooling tower */}
      <path d="M180 62 Q180 38 192 24 Q192 14 188 6 L200 6 Q196 14 196 24 Q208 38 208 62Z"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.8" fill={C.forest} fillOpacity="0.04" />
      {/* Electrical panel */}
      <rect x="230" y="14" width="32" height="24" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.03" />
      <path d="M246 18 L243 24 L248 24 L245 32"
        stroke={C.copper} strokeOpacity="0.35" strokeWidth="0.8" fill="none" />
      <circle cx="236" cy="20" r="1.5" fill="#2ECC71" fillOpacity="0.2" />
      <circle cx="236" cy="26" r="1.5" fill="#2ECC71" fillOpacity="0.18" />
      {/* Labels */}
      <text x="68" y="24" fill={C.copper} fillOpacity="0.18" fontSize="3.5" fontFamily="monospace">STEAM</text>
      <text x="68" y="41" fill={C.forest} fillOpacity="0.18" fontSize="3.5" fontFamily="monospace">CHW</text>
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">CEP — CENTRAL ENERGY</text>
    </svg>
  )
}

function YcodIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.copper} opacity={0.025} />
      {/* Heart / organ symbol */}
      <path d="M38 30 C38 20 50 17 50 25 C50 17 62 20 62 30 C62 42 50 50 50 50 C50 50 38 42 38 30Z"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.8" fill={C.copper} fillOpacity="0.06" />
      {/* Pulse through heart */}
      <polyline points="25,35 36,35 40,28 44,42 48,32 52,37 56,35 70,35"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.6" fill="none" />
      {/* Checkbox — opt-out */}
      <rect x="100" y="15" width="24" height="18" rx="2"
        stroke={C.copper} strokeOpacity="0.22" strokeWidth="0.6" fill={C.copper} fillOpacity="0.03" />
      <polyline points="106,24 110,28 120,19"
        stroke={C.copper} strokeOpacity="0.35" strokeWidth="1" fill="none" strokeLinecap="round" strokeLinejoin="round" />
      <line x1="100" y1="40" x2="124" y2="40" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      <line x1="100" y1="46" x2="120" y2="46" stroke={C.titanium} strokeOpacity="0.07" strokeWidth="0.4" />
      {/* Capitol dome */}
      <path d="M170 55 L170 38 Q190 20 210 38 L210 55"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.015" />
      <line x1="190" y1="20" x2="190" y2="12" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.4" />
      {/* People */}
      {[240, 258, 276, 294].map((x, i) => (
        <g key={i}>
          <circle cx={x} cy={28 + i * 2} r="3"
            stroke={C.forest} strokeOpacity={0.1 + i * 0.03} strokeWidth="0.4" fill="none" />
          <line x1={x} y1={31 + i * 2} x2={x} y2={42 + i * 2}
            stroke={C.forest} strokeOpacity={0.08 + i * 0.02} strokeWidth="0.4" />
        </g>
      ))}
      <rect x="230" y="52" width="50" height="14" rx="1"
        stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" fill={C.copper} fillOpacity="0.025" />
      <text x="255" y="62" fill={C.copper} fillOpacity="0.2" fontSize="4" fontFamily="monospace" textAnchor="middle">OPT-OUT</text>
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">YCOD — LEGISLATIVE REFORM</text>
    </svg>
  )
}

function VaProstheticsIcon() {
  return (
    <svg viewBox="0 0 320 80" className="w-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg w={320} h={80} gap={20} color={C.titanium} opacity={0.025} />
      {/* 3D printer frame */}
      <rect x="20" y="8" width="60" height="54" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.025" />
      {/* Z-rails */}
      <line x1="24" y1="12" x2="24" y2="60" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" />
      <line x1="76" y1="12" x2="76" y2="60" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" />
      {/* Print bed */}
      <rect x="28" y="50" width="44" height="4" rx="0.5"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.04" />
      {/* X-carriage */}
      <line x1="28" y1="30" x2="72" y2="30"
        stroke={C.copper} strokeOpacity="0.22" strokeWidth="0.6" />
      {/* Print head */}
      <rect x="42" y="26" width="12" height="8" rx="1"
        stroke={C.copper} strokeOpacity="0.28" strokeWidth="0.6" fill={C.copper} fillOpacity="0.06" />
      {/* Extrusion */}
      <line x1="48" y1="34" x2="48" y2="42" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.3" strokeDasharray="2 1" />
      {/* Prosthetic hand outline on bed */}
      <path d="M38 50 L38 44 Q42 40 44 42 L44 46 L47 42 Q49 41 50 43 L50 46 L53 44 Q55 43 55 46 L55 50Z"
        stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" fill={C.copper} fillOpacity="0.04" />
      {/* CAD wireframe */}
      <rect x="110" y="10" width="70" height="48" rx="1"
        stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <rect x="112" y="12" width="66" height="6" rx="0.5"
        fill={C.titanium} fillOpacity="0.03" />
      {/* Wireframe hand */}
      <path d="M130 55 L130 38 Q134 34 136 37 L136 42 L139 36 Q141 35 142 37 L142 42 L145 38 Q147 37 147 40 L147 55"
        stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      {/* Dimension line */}
      <line x1="127" y1="34" x2="127" y2="55" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.3" />
      <text x="124" y="46" fill={C.copper} fillOpacity="0.15" fontSize="3" fontFamily="monospace" textAnchor="end">148mm</text>
      {/* Material spec */}
      <rect x="210" y="14" width="56" height="32" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.015" />
      <text x="215" y="24" fill={C.titanium} fillOpacity="0.14" fontSize="3.5" fontFamily="monospace">PLA+ 1.75mm</text>
      <text x="215" y="32" fill={C.titanium} fillOpacity="0.1" fontSize="3" fontFamily="monospace">Infill: 25%</text>
      <text x="215" y="40" fill={C.copper} fillOpacity="0.12" fontSize="3" fontFamily="monospace">FlowIt v3.2</text>
      <text x="20" y="72" fill={C.titanium} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace">VA PROSTHETICS — CAD/CAM</text>
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
      <rect x="80" y="35" width="180" height="105" rx="2"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      {/* Pediment */}
      <path d="M80 35 L170 15 L260 35" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.4" fill="none" />
      {/* Columns */}
      {[95, 120, 145, 195, 220, 245].map((x) => (
        <g key={x}>
          <line x1={x} y1="38" x2={x} y2="138"
            stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.8" />
          {/* Column base */}
          <rect x={x - 3} y="135" width="6" height="3" rx="0.5"
            fill={C.titanium} fillOpacity="0.04" />
          {/* Column capital */}
          <rect x={x - 2} y="38" width="4" height="2" rx="0.5"
            fill={C.titanium} fillOpacity="0.03" />
        </g>
      ))}
      {/* Windows */}
      {[100, 130, 160, 200, 230].map((x) =>
        [50, 70, 90, 110].map((y) => (
          <rect key={`${x}-${y}`} x={x} y={y} width="8" height="12" rx="0.5"
            stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.3"
            fill={C.titanium} fillOpacity="0.015" />
        ))
      )}
      {/* Entrance */}
      <rect x="158" y="110" width="24" height="28" rx="1"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.02" />
      <path d="M158 110 Q170 104 182 110" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.3" fill="none" />

      {/* Camera on tripod — more detailed */}
      <rect x="20" y="82" width="22" height="14" rx="2"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.7" fill={C.copper} fillOpacity="0.05" />
      {/* Lens */}
      <circle cx="16" cy="89" r="7" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      <circle cx="16" cy="89" r="4" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.3" fill="none" />
      <circle cx="16" cy="89" r="2" fill={C.copper} fillOpacity="0.08" />
      {/* Viewfinder */}
      <rect x="26" y="80" width="10" height="6" rx="0.5"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.3" fill={C.copper} fillOpacity="0.03" />
      {/* Record light */}
      <circle cx="28" cy="82" r="1" fill="#E74C3C" fillOpacity="0.25" />
      {/* Tripod legs */}
      <line x1="31" y1="96" x2="22" y2="150" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" />
      <line x1="31" y1="96" x2="40" y2="150" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" />
      <line x1="31" y1="96" x2="31" y2="148" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      {/* Tripod head */}
      <rect x="28" y="94" width="6" height="4" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.04" />

      {/* Video frame overlay */}
      <rect x="90" y="50" width="160" height="90" rx="1"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" fill="none" />
      {/* Rule of thirds */}
      <line x1="143" y1="50" x2="143" y2="140" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="197" y1="50" x2="197" y2="140" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="90" y1="80" x2="250" y2="80" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="90" y1="110" x2="250" y2="110" stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" />
      {/* Safe area markers */}
      {[[90,50],[248,50],[90,138],[248,138]].map(([x,y], i) => (
        <g key={i}>
          <line x1={x} y1={y} x2={x + (i % 2 === 0 ? 6 : -6)} y2={y}
            stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.3" />
          <line x1={x} y1={y} x2={x} y2={y + (i < 2 ? 6 : -6)}
            stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.3" />
        </g>
      ))}

      {/* Interview subject silhouette */}
      <circle cx="170" cy="78" r="8" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.015" />
      <path d="M158 100 Q164 92 170 90 Q176 92 182 100"
        stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.01" />

      {/* Lighting panel */}
      <rect x="275" y="30" width="28" height="18" rx="1"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.02" />
      <line x1="289" y1="48" x2="289" y2="80" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" />
      {/* Light rays */}
      <path d="M280 48 L260 70" stroke={C.copper} strokeOpacity="0.03" strokeWidth="0.3" />
      <path d="M298 48 L270 70" stroke={C.copper} strokeOpacity="0.03" strokeWidth="0.3" />

      {/* Timecode overlay */}
      <text x="94" y="56" fill={C.copper} fillOpacity="0.12" fontSize="3" fontFamily="monospace">00:02:34:12</text>
      <text x="220" y="56" fill="#E74C3C" fillOpacity="0.15" fontSize="3" fontFamily="monospace">REC</text>
      {/* Audio levels */}
      <rect x="270" y="100" width="24" height="40" rx="1"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.015" />
      {Array.from({ length: 10 }).map((_, i) => (
        <rect key={i} x="273" y={104 + i * 3.4} width="8" height="2" rx="0.5"
          fill={i < 3 ? C.forest : i < 7 ? C.copper : '#E74C3C'}
          fillOpacity={i < 6 ? 0.12 : 0.06} />
      ))}
      <text x="285" y="146" fill={C.titanium} fillOpacity="0.08" fontSize="2.5" fontFamily="monospace" textAnchor="middle">dB</text>

      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">FREEMAN SCHOOL — DIGITAL MKT</text>
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
      {/* Runway platform — perspective lines converging */}
      <path d="M40 240 L70 0" stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.3" />
      <path d="M140 240 L110 0" stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.3" />
      <line x1="90" y1="0" x2="90" y2="240" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.4" />
      {/* Runway edge lights */}
      {[30, 60, 90, 120, 150, 180, 210].map((y) => (
        <g key={y}>
          <circle cx={55 + (y / 240) * 15} cy={y} r="1" fill={C.copper} fillOpacity="0.06" />
          <circle cx={125 - (y / 240) * 15} cy={y} r="1" fill={C.copper} fillOpacity="0.06" />
        </g>
      ))}
      {/* Spotlight rigs at top */}
      <rect x="15" y="4" width="12" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.02" />
      <rect x="153" y="4" width="12" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.02" />
      {/* Spotlight beams converging on model */}
      <path d="M21 12 Q50 25 90 35" stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      <path d="M159 12 Q130 25 90 35" stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      <path d="M21 12 L55 180" stroke={C.copper} strokeOpacity="0.02" strokeWidth="0.2" fill="none" />
      <path d="M159 12 L125 180" stroke={C.copper} strokeOpacity="0.02" strokeWidth="0.2" fill="none" />
      {/* Figure — fashion croquis style (elongated proportions) */}
      {/* Head */}
      <ellipse cx="90" cy="33" rx="7" ry="9" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.6" fill="none" />
      {/* Neck */}
      <line x1="90" y1="42" x2="90" y2="50" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" />
      {/* Shoulders */}
      <line x1="72" y1="52" x2="108" y2="52" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      {/* Torso */}
      <line x1="90" y1="50" x2="90" y2="115" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" />
      {/* Arms — posed */}
      <path d="M72 52 Q65 70 60 82 Q58 88 62 92" stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.4" fill="none" />
      <path d="M108 52 Q115 68 118 80 Q120 86 116 90" stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.4" fill="none" />
      {/* Hands */}
      <circle cx="62" cy="93" r="2" stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      <circle cx="116" cy="91" r="2" stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      {/* Legs — elongated stride */}
      <path d="M90 115 Q82 140 76 170 Q74 178 75 186" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      <path d="M90 115 Q98 145 104 170 Q106 178 105 186" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      {/* Shoes / heels */}
      <path d="M75 186 L70 190 L78 190" stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" fill="none" />
      <path d="M105 186 L100 190 L108 190" stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" fill="none" />
      {/* Garment — structured jacket/blazer */}
      <path d="M72 52 Q68 65 66 80 Q65 95 70 105 L80 115 Q85 118 90 120 Q95 118 100 115 L110 105 Q115 95 114 80 Q112 65 108 52"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      {/* Lapel lines */}
      <path d="M82 52 L86 70 L90 52" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      <path d="M98 52 L94 70 L90 52" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      {/* Button details */}
      <circle cx="90" cy="75" r="1" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.2" fill="none" />
      <circle cx="90" cy="85" r="1" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.2" fill="none" />
      <circle cx="90" cy="95" r="1" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.2" fill="none" />
      {/* Garment hem / skirt suggestion */}
      <path d="M80 115 Q75 130 73 145 Q72 155 76 170"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      <path d="M100 115 Q105 130 107 145 Q108 155 104 170"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" fill="none" />
      {/* Audience silhouettes — front row */}
      {[15, 28, 38].map((x) => (
        <g key={`l-${x}`}>
          <circle cx={x} cy={140 + x * 0.3} r="3" stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.25" fill="none" />
          <line x1={x} y1={143 + x * 0.3} x2={x} y2={155 + x * 0.3} stroke={C.titanium} strokeOpacity="0.03" strokeWidth="0.2" />
        </g>
      ))}
      {[142, 155, 165].map((x) => (
        <g key={`r-${x}`}>
          <circle cx={x} cy={140 + (180 - x) * 0.3} r="3" stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.25" fill="none" />
          <line x1={x} y1={143 + (180 - x) * 0.3} x2={x} y2={155 + (180 - x) * 0.3} stroke={C.titanium} strokeOpacity="0.03" strokeWidth="0.2" />
        </g>
      ))}
      {/* Camera flash bursts */}
      {[22, 160].map((x) => (
        <g key={`flash-${x}`}>
          {[0, 45, 90, 135].map((deg) => {
            const r = deg * Math.PI / 180
            return (
              <line key={deg}
                x1={x + Math.cos(r) * 2} y1={135 + Math.sin(r) * 2}
                x2={x + Math.cos(r) * 5} y2={135 + Math.sin(r) * 5}
                stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.2" />
            )
          })}
        </g>
      ))}
      {/* Magazine masthead hint */}
      <rect x="30" y="204" width="120" height="30" rx="1"
        stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.2" fill={C.titanium} fillOpacity="0.01" />
      <text x="90" y="218" fill={C.titanium} fillOpacity="0.12" fontSize="6" fontFamily="monospace" textAnchor="middle" letterSpacing="3">VOGUE ITALIA</text>
      <text x="90" y="228" fill={C.copper} fillOpacity="0.1" fontSize="3.5" fontFamily="monospace" textAnchor="middle" letterSpacing="1">SchoolTime × BizarrAudi 2020</text>
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
      {/* Proscenium arch with ornamental keystone */}
      <path d="M55 155 L55 38 Q160 6 265 38 L265 155"
        stroke={C.copper} strokeOpacity="0.22" strokeWidth="0.9" fill="none" />
      <path d="M57 38 Q160 10 263 38" stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.3" fill="none" />
      {/* Keystone ornament */}
      <path d="M154 10 L160 6 L166 10 L163 14 L157 14Z"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.4" fill={C.copper} fillOpacity="0.04" />
      {/* Proscenium column details */}
      <rect x="52" y="38" width="8" height="117" stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" fill={C.copper} fillOpacity="0.015" />
      <rect x="260" y="38" width="8" height="117" stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" fill={C.copper} fillOpacity="0.015" />
      {/* Stage apron line */}
      <line x1="55" y1="155" x2="265" y2="155"
        stroke={C.copper} strokeOpacity="0.18" strokeWidth="0.6" />
      {/* Stage floor with plank lines */}
      <rect x="65" y="112" width="190" height="43" rx="1"
        fill={C.titanium} fillOpacity="0.02" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" />
      {[80, 100, 120, 140, 160, 180, 200, 220, 240].map((x) => (
        <line key={x} x1={x} y1="112" x2={x} y2="155"
          stroke={C.titanium} strokeOpacity="0.025" strokeWidth="0.2" />
      ))}
      {/* Footlights along stage edge */}
      {[75, 95, 115, 135, 155, 175, 195, 215, 235].map((x) => (
        <circle key={x} cx={x} cy="153" r="1.5"
          fill={C.copper} fillOpacity="0.1" stroke={C.copper} strokeOpacity="0.06" strokeWidth="0.2" />
      ))}
      {/* Fly system / batten bars above stage */}
      {[30, 35].map((y) => (
        <line key={y} x1="65" y1={y} x2="255" y2={y}
          stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.3" />
      ))}
      {/* Stage lights on batten — Fresnel fixtures */}
      {[90, 120, 150, 180, 210, 240].map((x, i) => (
        <g key={i}>
          <rect x={x - 5} y="36" width="10" height="8" rx="1.5"
            stroke={C.copper} strokeOpacity="0.18" strokeWidth="0.4" fill={C.copper} fillOpacity="0.04" />
          <line x1={x} y1="36" x2={x} y2="33" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" />
          {/* Light beam cones */}
          <path d={`M${x} 44 L${x - 18} 112 L${x + 18} 112Z`}
            fill={C.copper} fillOpacity="0.015" />
        </g>
      ))}
      {/* Curtain drape lines — multiple folds */}
      <path d="M57 38 C57 48 60 55 57 70 C60 85 57 100 57 115 C60 130 57 140 57 155"
        stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.5" fill="none" />
      <path d="M64 40 C64 52 66 62 64 75 C66 88 64 105 64 120"
        stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" fill="none" />
      <path d="M263 38 C263 48 260 55 263 70 C260 85 263 100 263 115 C260 130 263 140 263 155"
        stroke={C.copper} strokeOpacity="0.1" strokeWidth="0.5" fill="none" />
      <path d="M256 40 C256 52 254 62 256 75 C254 88 256 105 256 120"
        stroke={C.copper} strokeOpacity="0.05" strokeWidth="0.3" fill="none" />
      {/* Performer silhouette on stage */}
      <circle cx="160" cy="118" r="4" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" fill="none" />
      <line x1="160" y1="122" x2="160" y2="140" stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.3" />
      <line x1="160" y1="127" x2="153" y2="133" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.25" />
      <line x1="160" y1="127" x2="167" y2="133" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.25" />
      <line x1="160" y1="140" x2="155" y2="152" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.25" />
      <line x1="160" y1="140" x2="165" y2="152" stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.25" />
      {/* Stage set piece — flat/scenery panel */}
      <rect x="85" y="90" width="25" height="62" rx="1"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.015" />
      <rect x="210" y="95" width="30" height="57" rx="1"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.015" />
      {/* Audience seating rows */}
      {[0, 1, 2].map((row) => (
        <g key={row}>
          {[70, 85, 100, 115, 130, 145].map((x) => (
            <rect key={`${row}-${x}`} x={x} y={162 + row * 5} width="8" height="3" rx="0.5"
              stroke={C.titanium} strokeOpacity={0.04 - row * 0.008} strokeWidth="0.2"
              fill={C.titanium} fillOpacity="0.01" />
          ))}
          {[185, 200, 215, 230, 245, 260].map((x) => (
            <rect key={`${row}-${x}`} x={x} y={162 + row * 5} width="8" height="3" rx="0.5"
              stroke={C.titanium} strokeOpacity={0.04 - row * 0.008} strokeWidth="0.2"
              fill={C.titanium} fillOpacity="0.01" />
          ))}
        </g>
      ))}
      {/* Camera on tripod — center aisle */}
      <rect x="158" y="161" width="14" height="9" rx="1.5"
        stroke={C.titanium} strokeOpacity="0.18" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      <circle cx="163" cy="165" r="3" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      <circle cx="163" cy="165" r="1" fill={C.titanium} fillOpacity="0.08" />
      {/* Tripod legs */}
      <line x1="162" y1="170" x2="155" y2="178" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" />
      <line x1="165" y1="170" x2="165" y2="178" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" />
      <line x1="168" y1="170" x2="175" y2="178" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" />
      {/* Record indicator */}
      <circle cx="170" cy="162" r="1" fill="#E04040" fillOpacity="0.2" />
      {/* Exit sign */}
      <rect x="270" y="42" width="18" height="7" rx="1"
        stroke="#40E070" strokeOpacity="0.08" strokeWidth="0.3" fill="#40E070" fillOpacity="0.02" />
      <text x="279" y="48" fill="#40E070" fillOpacity="0.1" fontSize="3" fontFamily="monospace" textAnchor="middle">EXIT</text>
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">AURORA THEATRE — PRODUCTION</text>
    </svg>
  )
}

function BuffaloCentralTerminal() {
  return (
    <svg viewBox="0 0 320 180" className="w-full h-full" fill="none" xmlns="http://www.w3.org/2000/svg">
      <GridBg color={C.titanium} opacity={0.025} />
      {/* Central tower */}
      <rect x="128" y="16" width="64" height="134" rx="2"
        stroke={C.titanium} strokeOpacity="0.22" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.03" />
      {/* Tower stepped crown — Art Deco setbacks */}
      <rect x="135" y="8" width="50" height="8" rx="1"
        stroke={C.titanium} strokeOpacity="0.18" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      <rect x="142" y="2" width="36" height="6" rx="1"
        stroke={C.titanium} strokeOpacity="0.14" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.03" />
      {/* Tower pinnacle */}
      <line x1="160" y1="2" x2="160" y2="0" stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" />
      {/* Art deco vertical pilaster lines on tower */}
      <line x1="138" y1="16" x2="138" y2="150" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.3" />
      <line x1="182" y1="16" x2="182" y2="150" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.3" />
      {/* Tower windows — arched tops */}
      {Array.from({ length: 6 }).map((_, row) =>
        [141, 155, 169].map((x) => (
          <g key={`${row}-${x}`}>
            <rect x={x} y={22 + row * 18} width="11" height="13" rx="0.5"
              stroke={C.titanium} strokeOpacity="0.07" strokeWidth="0.3"
              fill={C.titanium} fillOpacity="0.02" />
            {/* Arched top */}
            <path d={`M${x} ${25 + row * 18} Q${x + 5.5} ${20 + row * 18} ${x + 11} ${25 + row * 18}`}
              stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.2" fill="none" />
          </g>
        ))
      )}
      {/* Horizontal band between windows */}
      {[38, 56, 74, 92, 110, 128].map((y) => (
        <line key={y} x1="130" y1={y} x2="190" y2={y}
          stroke={C.titanium} strokeOpacity="0.03" strokeWidth="0.2" />
      ))}
      {/* Left wing — concourse */}
      <rect x="40" y="68" width="88" height="82" rx="1"
        stroke={C.titanium} strokeOpacity="0.14" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      {/* Left wing parapet */}
      <line x1="40" y1="68" x2="128" y2="68" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      {/* Left wing windows */}
      {[50, 66, 82, 98, 114].map((x) => (
        <rect key={x} x={x} y="78" width="8" height="20" rx="0.5"
          stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.25" fill={C.titanium} fillOpacity="0.015" />
      ))}
      {/* Left wing entrance arches */}
      {[55, 75, 95, 115].map((x) => (
        <path key={x} d={`M${x} 150 L${x} 110 Q${x + 7} 104 ${x + 14} 110 L${x + 14} 150`}
          stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.25" fill="none" />
      ))}
      {/* Right wing — concourse */}
      <rect x="192" y="68" width="88" height="82" rx="1"
        stroke={C.titanium} strokeOpacity="0.14" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <line x1="192" y1="68" x2="280" y2="68" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      {/* Right wing windows */}
      {[202, 218, 234, 250, 266].map((x) => (
        <rect key={x} x={x} y="78" width="8" height="20" rx="0.5"
          stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.25" fill={C.titanium} fillOpacity="0.015" />
      ))}
      {/* Right wing entrance arches */}
      {[197, 217, 237, 257].map((x) => (
        <path key={x} d={`M${x} 150 L${x} 110 Q${x + 7} 104 ${x + 14} 110 L${x + 14} 150`}
          stroke={C.titanium} strokeOpacity="0.04" strokeWidth="0.25" fill="none" />
      ))}
      {/* Main entrance — grand arch */}
      <path d="M140 150 L140 120 Q160 100 180 120 L180 150"
        stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.5" fill={C.copper} fillOpacity="0.015" />
      {/* Art deco sunburst over entrance */}
      {[-30, -20, -10, 0, 10, 20, 30].map((deg) => {
        const r = (deg - 90) * Math.PI / 180
        return (
          <line key={deg}
            x1={160 + Math.cos(r) * 8} y1={108 + Math.sin(r) * 8}
            x2={160 + Math.cos(r) * 18} y2={108 + Math.sin(r) * 18}
            stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" />
        )
      })}
      {/* Art deco chevrons below tower */}
      <path d="M143 148 L160 136 L177 148" stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" fill="none" />
      <path d="M146 152 L160 142 L174 152" stroke={C.copper} strokeOpacity="0.08" strokeWidth="0.3" fill="none" />
      {/* Ground line */}
      <line x1="30" y1="150" x2="290" y2="150"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.4" />
      {/* Train tracks — two tracks with ties and rails */}
      <line x1="15" y1="160" x2="305" y2="160"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.5" />
      <line x1="15" y1="166" x2="305" y2="166"
        stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.5" />
      <line x1="15" y1="170" x2="305" y2="170"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.4" />
      <line x1="15" y1="176" x2="305" y2="176"
        stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.4" />
      {/* Track ties */}
      {Array.from({ length: 18 }).map((_, i) => {
        const x = 25 + i * 16
        return (
          <g key={i}>
            <line x1={x} y1="158" x2={x} y2="168"
              stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.3" />
            <line x1={x + 2} y1="168" x2={x + 2} y2="178"
              stroke={C.titanium} strokeOpacity="0.035" strokeWidth="0.25" />
          </g>
        )
      })}
      {/* Camera with tripod at track level */}
      <rect x="18" y="155" width="10" height="6" rx="1"
        stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.03" />
      <circle cx="22" cy="157" r="2" stroke={C.titanium} strokeOpacity="0.1" strokeWidth="0.3" fill="none" />
      <text x="8" y="10" fill={C.titanium} fillOpacity="0.12" fontSize="4" fontFamily="monospace">BUFFALO CENTRAL TERMINAL — 1929 ART DECO</text>
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
