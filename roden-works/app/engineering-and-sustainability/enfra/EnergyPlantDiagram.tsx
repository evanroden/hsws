'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'

/* ═══════════════════════════════════════════════════════
   Colors — matches site design tokens
   ═══════════════════════════════════════════════════════ */
const C = {
  copper: '#B87333',
  forest: '#2D5A45',
  titanium: '#8A9BA8',
  white: '#ffffff',
}

/* ═══════════════════════════════════════════════════════
   Component data — position, description, key specs
   ═══════════════════════════════════════════════════════ */
const components = [
  {
    id: 'boiler',
    label: 'Steam Boilers',
    color: C.copper,
    specs: ['2× 600 HP fire-tube', '150 PSI / 350 °F', 'Natural gas + #2 fuel oil'],
    description:
      'Dual-fuel fire-tube boilers generate high-pressure steam for heating, sterilization (autoclaves), kitchen, laundry, and humidification. Steam is the lifeblood of hospital operations — a single VA medical center may consume 40,000+ lb/hr at peak. Redundant units ensure N+1 reliability for life-safety.',
  },
  {
    id: 'chiller',
    label: 'Centrifugal Chillers',
    color: C.forest,
    specs: ['2× 1,200-ton centrifugal', 'CHW supply 42 °F / return 56 °F', 'R-134a refrigerant'],
    description:
      'Water-cooled centrifugal chillers produce chilled water for air conditioning, operating-room cooling, MRI suites, pharmaceutical storage, and server rooms. Each unit circulates refrigerant through an evaporator (cooling the CHW) and a condenser (rejecting heat to the condenser-water loop). Variable-speed drives optimize part-load efficiency.',
  },
  {
    id: 'cooling-tower',
    label: 'Cooling Towers',
    color: C.titanium,
    specs: ['2-cell induced-draft', 'CW supply 85 °F / return 95 °F', 'Evaporative heat rejection'],
    description:
      'Induced-draft cooling towers reject condenser heat to the atmosphere through evaporative cooling. Hot condenser water cascades over fill media while fans draw ambient air upward, evaporating a small fraction and cooling the remainder. Chemical water treatment controls scale, corrosion, and biological growth in the open loop.',
  },
  {
    id: 'generator',
    label: 'Emergency Generators',
    color: C.copper,
    specs: ['2× 2 MW diesel gensets', 'ATS transfer < 10 sec', 'NEC 700 / NFPA 110 Type 10'],
    description:
      'Diesel-electric generators with automatic transfer switches (ATS) ensure uninterrupted power to life-safety loads — ICUs, operating rooms, ventilators, and fire alarm systems. Required to reach full load within 10 seconds of a utility outage. On-site fuel storage provides 96+ hours of runtime at full load per Joint Commission requirements.',
  },
  {
    id: 'pumps',
    label: 'Pumping & Distribution',
    color: C.forest,
    specs: ['Primary / secondary decoupled', 'VFD-driven, N+1 redundancy', 'Underground pipe network'],
    description:
      'Variable-frequency-drive pumps circulate chilled water, condenser water, and condensate through the plant and underground distribution to every building. The primary-secondary decoupled loop allows chillers to run at constant flow while building loads vary. Differential pressure sensors at remote buildings modulate pump speed to match real-time demand.',
  },
  {
    id: 'bas',
    label: 'Building Automation (BAS)',
    color: C.titanium,
    specs: ['ENFRA Connect® platform', 'BACnet / Modbus integration', '24/7 remote monitoring'],
    description:
      'The BAS head-end aggregates data from thousands of field sensors — temperature, pressure, flow, power — and executes optimized control sequences across all mechanical systems. ENFRA Connect® provides real-time dashboards, automated fault detection & diagnostics (AFDD), and predictive maintenance alerts, reducing energy consumption 15-25% over baseline.',
  },
]

/* ═══════════════════════════════════════════════════════
   SVG sub-components for equipment detail
   ═══════════════════════════════════════════════════════ */

function BoilerDrawing({ active }: { active: boolean }) {
  const o = active ? 1 : 0.6
  return (
    <g opacity={o} className="transition-opacity duration-300">
      {/* Boiler #1 shell */}
      <rect x="22" y="38" width="52" height="38" rx="3"
        stroke={C.copper} strokeWidth="0.8" fill={C.copper} fillOpacity="0.06" />
      {/* Fire tubes */}
      {[48, 54, 60, 66].map((y) => (
        <line key={y} x1="28" y1={y} x2="68" y2={y}
          stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" />
      ))}
      {/* Burner */}
      <path d="M45 76 C40 68 45 60 48 60 C51 60 56 68 51 76"
        stroke={C.copper} strokeOpacity="0.5" strokeWidth="0.6" fill={C.copper} fillOpacity="0.15" />
      {/* Stack / flue */}
      <rect x="42" y="22" width="12" height="16" rx="1"
        stroke={C.copper} strokeOpacity="0.4" strokeWidth="0.6" fill={C.copper} fillOpacity="0.03" />
      {/* Steam wisps */}
      <path d="M46 22 C44 16 48 12 46 6" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill="none" />
      <path d="M50 20 C52 14 48 10 51 4" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.4" fill="none" />
      {/* Pressure gauge */}
      <circle cx="64" cy="44" r="5" stroke={C.titanium} strokeOpacity="0.35" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      <line x1="64" y1="44" x2="67" y2="41" stroke={C.copper} strokeOpacity="0.6" strokeWidth="0.5" />
      <circle cx="64" cy="44" r="0.8" fill={C.copper} fillOpacity="0.5" />
      {/* Gauge ticks */}
      {[0, 45, 90, 135, 180].map((deg) => {
        const r = (deg - 90) * Math.PI / 180
        return (
          <line key={deg}
            x1={64 + Math.cos(r) * 4} y1={44 + Math.sin(r) * 4}
            x2={64 + Math.cos(r) * 5} y2={44 + Math.sin(r) * 5}
            stroke={C.titanium} strokeOpacity="0.3" strokeWidth="0.3" />
        )
      })}
      {/* Boiler #2 (behind, smaller) */}
      <rect x="28" y="82" width="42" height="28" rx="2"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      {[92, 97, 102].map((y) => (
        <line key={y} x1="33" y1={y} x2="65" y2={y}
          stroke={C.copper} strokeOpacity="0.12" strokeWidth="0.4" />
      ))}
      {/* Gas supply line */}
      <line x1="4" y1="95" x2="28" y2="95" stroke={C.copper} strokeOpacity="0.3" strokeWidth="1.2" />
      <polygon points="20,93 24,95 20,97" fill={C.copper} fillOpacity="0.35" />
      <text x="4" y="100" fill={C.titanium} fillOpacity="0.35" fontSize="4" fontFamily="monospace">GAS</text>
      {/* Steam output header */}
      <line x1="74" y1="50" x2="90" y2="50" stroke={C.copper} strokeOpacity="0.5" strokeWidth="1.8" />
      <polygon points="84,47.5 89,50 84,52.5" fill={C.copper} fillOpacity="0.4" />
      {/* Labels */}
      <text x="48" y="34" textAnchor="middle" fill={C.white} fillOpacity="0.5" fontSize="5" fontFamily="monospace">BOILER #1</text>
      <text x="49" y="90" textAnchor="middle" fill={C.white} fillOpacity="0.35" fontSize="4" fontFamily="monospace">BOILER #2</text>
      <text x="82" y="46" fill={C.copper} fillOpacity="0.4" fontSize="3.5" fontFamily="monospace">350°F</text>
      <text x="82" y="55" fill={C.copper} fillOpacity="0.35" fontSize="3" fontFamily="monospace">150 PSI</text>
    </g>
  )
}

function ChillerDrawing({ active }: { active: boolean }) {
  const o = active ? 1 : 0.6
  return (
    <g opacity={o} className="transition-opacity duration-300">
      {/* Chiller #1 — evaporator barrel */}
      <rect x="135" y="42" width="60" height="18" rx="9"
        stroke={C.forest} strokeOpacity="0.5" strokeWidth="0.8" fill={C.forest} fillOpacity="0.06" />
      <text x="165" y="54" textAnchor="middle" fill={C.forest} fillOpacity="0.35" fontSize="3.5" fontFamily="monospace">EVAPORATOR</text>
      {/* Tube bundle hint */}
      {[48, 52, 56].map((y) => (
        <line key={y} x1="142" y1={y} x2="188" y2={y}
          stroke={C.forest} strokeOpacity="0.08" strokeWidth="0.3" />
      ))}
      {/* Condenser barrel */}
      <rect x="135" y="68" width="60" height="18" rx="9"
        stroke={C.titanium} strokeOpacity="0.4" strokeWidth="0.7" fill={C.titanium} fillOpacity="0.04" />
      <text x="165" y="80" textAnchor="middle" fill={C.titanium} fillOpacity="0.3" fontSize="3.5" fontFamily="monospace">CONDENSER</text>
      {/* Compressor between barrels */}
      <circle cx="165" cy="63" r="6" stroke={C.forest} strokeOpacity="0.45" strokeWidth="0.7" fill={C.forest} fillOpacity="0.08" />
      <text x="165" y="65" textAnchor="middle" fill={C.white} fillOpacity="0.35" fontSize="3.5" fontFamily="monospace">M</text>
      {/* Refrigerant loop arrows */}
      <path d="M159 57 L159 60" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.4" />
      <path d="M171 66 L171 69" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.4" />
      {/* Chiller #2 (below, secondary) */}
      <rect x="140" y="94" width="50" height="14" rx="7"
        stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.5" fill={C.forest} fillOpacity="0.03" />
      <rect x="140" y="112" width="50" height="14" rx="7"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.02" />
      <circle cx="165" cy="108" r="4" stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.5" fill={C.forest} fillOpacity="0.04" />
      <text x="165" y="110" textAnchor="middle" fill={C.white} fillOpacity="0.2" fontSize="3" fontFamily="monospace">M</text>
      {/* CHW supply output */}
      <line x1="195" y1="51" x2="220" y2="51" stroke={C.forest} strokeOpacity="0.5" strokeWidth="1.8" />
      <polygon points="212,48.5 217,51 212,53.5" fill={C.forest} fillOpacity="0.4" />
      {/* CHW return input */}
      <line x1="195" y1="58" x2="220" y2="58" stroke={C.forest} strokeOpacity="0.3" strokeWidth="1.2" strokeDasharray="3 2" />
      <polygon points="143,55.5 138,58 143,60.5" fill={C.forest} fillOpacity="0.25" />
      {/* CW to cooling towers (down) */}
      <line x1="175" y1="86" x2="175" y2="145" stroke={C.titanium} strokeOpacity="0.35" strokeWidth="1.2" />
      <polygon points="172.5,138 175,143 177.5,138" fill={C.titanium} fillOpacity="0.3" />
      {/* CW return from cooling towers (up) */}
      <line x1="155" y1="145" x2="155" y2="86" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="1.0" strokeDasharray="3 2" />
      <polygon points="152.5,92 155,87 157.5,92" fill={C.titanium} fillOpacity="0.2" />
      {/* Labels */}
      <text x="165" y="36" textAnchor="middle" fill={C.white} fillOpacity="0.5" fontSize="5" fontFamily="monospace">CHILLER #1 — 1,200 TON</text>
      <text x="165" y="92" textAnchor="middle" fill={C.white} fillOpacity="0.3" fontSize="4" fontFamily="monospace">CHILLER #2</text>
      <text x="208" y="47" fill={C.forest} fillOpacity="0.4" fontSize="3.5" fontFamily="monospace">42°F</text>
      <text x="208" y="64" fill={C.forest} fillOpacity="0.3" fontSize="3" fontFamily="monospace">56°F</text>
    </g>
  )
}

function CoolingTowerDrawing({ active }: { active: boolean }) {
  const o = active ? 1 : 0.6
  return (
    <g opacity={o} className="transition-opacity duration-300">
      {/* Tower cell #1 — hyperboloid shape */}
      <path d="M135 218 Q135 185 148 165 Q148 155 144 148 L156 148 Q152 155 152 165 Q165 185 165 218Z"
        stroke={C.titanium} strokeOpacity="0.5" strokeWidth="0.8" fill={C.titanium} fillOpacity="0.04" />
      {/* Fill media lines */}
      {[185, 192, 199, 206].map((y) => (
        <line key={y} x1="138" y1={y} x2="162" y2={y}
          stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.3" />
      ))}
      {/* Fan */}
      <circle cx="150" cy="152" r="4" stroke={C.titanium} strokeOpacity="0.3" strokeWidth="0.5" fill="none" />
      <line x1="147" y1="149" x2="153" y2="155" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.4" />
      <line x1="153" y1="149" x2="147" y2="155" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.4" />
      {/* Mist/evaporation */}
      {[146, 150, 154].map((x, i) => (
        <path key={i} d={`M${x} 148 C${x - 1} 143 ${x + 1} 140 ${x} 136`}
          stroke={C.titanium} strokeOpacity={0.1 + i * 0.03} strokeWidth="0.4" fill="none" />
      ))}
      {/* Basin */}
      <rect x="133" y="218" width="34" height="6" rx="1"
        stroke={C.titanium} strokeOpacity="0.3" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.05" />
      {/* Tower cell #2 */}
      <path d="M175 218 Q175 188 186 168 Q186 158 183 152 L193 152 Q190 158 190 168 Q201 188 201 218Z"
        stroke={C.titanium} strokeOpacity="0.35" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.03" />
      <circle cx="188" cy="155" r="3" stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.4" fill="none" />
      <rect x="173" y="218" width="30" height="6" rx="1"
        stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.03" />
      {/* CW pipe labels */}
      <text x="178" y="143" fill={C.titanium} fillOpacity="0.35" fontSize="3" fontFamily="monospace">95°F</text>
      <text x="145" y="143" fill={C.titanium} fillOpacity="0.3" fontSize="3" fontFamily="monospace">85°F</text>
      {/* Make-up water */}
      <line x1="120" y1="221" x2="133" y2="221" stroke={C.forest} strokeOpacity="0.2" strokeWidth="0.6" />
      <text x="108" y="219" fill={C.forest} fillOpacity="0.25" fontSize="3" fontFamily="monospace">MAKE-UP</text>
      {/* Label */}
      <text x="168" y="234" textAnchor="middle" fill={C.white} fillOpacity="0.4" fontSize="4.5" fontFamily="monospace">COOLING TOWERS</text>
    </g>
  )
}

function GeneratorDrawing({ active }: { active: boolean }) {
  const o = active ? 1 : 0.6
  return (
    <g opacity={o} className="transition-opacity duration-300">
      {/* Generator #1 — engine block */}
      <rect x="20" y="155" width="40" height="24" rx="2"
        stroke={C.copper} strokeOpacity="0.45" strokeWidth="0.7" fill={C.copper} fillOpacity="0.05" />
      {/* Cylinder heads */}
      {[26, 33, 40, 47, 54].map((x) => (
        <rect key={x} x={x} y="158" width="4" height="18" rx="0.5"
          stroke={C.copper} strokeOpacity="0.15" strokeWidth="0.3" fill={C.copper} fillOpacity="0.04" />
      ))}
      <text x="40" y="168" textAnchor="middle" fill={C.white} fillOpacity="0.3" fontSize="3.5" fontFamily="monospace">ENGINE</text>
      {/* Alternator */}
      <circle cx="72" cy="167" r="10" stroke={C.copper} strokeOpacity="0.4" strokeWidth="0.7" fill={C.copper} fillOpacity="0.04" />
      <circle cx="72" cy="167" r="5" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.4" fill="none" />
      <text x="72" y="169" textAnchor="middle" fill={C.white} fillOpacity="0.3" fontSize="3" fontFamily="monospace">ALT</text>
      {/* Shaft connecting engine to alternator */}
      <line x1="60" y1="167" x2="62" y2="167" stroke={C.copper} strokeOpacity="0.3" strokeWidth="1" />
      {/* Generator #2 */}
      <rect x="20" y="188" width="36" height="18" rx="2"
        stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <circle cx="66" cy="197" r="7" stroke={C.copper} strokeOpacity="0.25" strokeWidth="0.5" fill={C.copper} fillOpacity="0.03" />
      <text x="66" y="199" textAnchor="middle" fill={C.white} fillOpacity="0.2" fontSize="2.5" fontFamily="monospace">ALT</text>
      {/* Fuel tank */}
      <rect x="8" y="215" width="32" height="14" rx="2"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.5" fill={C.copper} fillOpacity="0.04" />
      <text x="24" y="224" textAnchor="middle" fill={C.copper} fillOpacity="0.35" fontSize="3.5" fontFamily="monospace">DIESEL</text>
      <text x="24" y="234" textAnchor="middle" fill={C.titanium} fillOpacity="0.25" fontSize="2.5" fontFamily="monospace">96 HR SUPPLY</text>
      {/* Fuel line */}
      <line x1="24" y1="215" x2="24" y2="206" stroke={C.copper} strokeOpacity="0.2" strokeWidth="0.5" />
      {/* ATS (Automatic Transfer Switch) */}
      <rect x="52" y="215" width="30" height="20" rx="2"
        stroke={C.titanium} strokeOpacity="0.4" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.04" />
      <text x="67" y="224" textAnchor="middle" fill={C.white} fillOpacity="0.35" fontSize="3.5" fontFamily="monospace">ATS</text>
      <text x="67" y="231" textAnchor="middle" fill={C.titanium} fillOpacity="0.25" fontSize="2.5" fontFamily="monospace">&lt;10 SEC</text>
      {/* Utility feed into ATS */}
      <line x1="4" y1="225" x2="52" y2="225" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.8" />
      <text x="4" y="220" fill={C.titanium} fillOpacity="0.3" fontSize="3" fontFamily="monospace">UTILITY</text>
      {/* Generator feed into ATS */}
      <line x1="72" y1="177" x2="72" y2="215" stroke={C.copper} strokeOpacity="0.3" strokeWidth="1" />
      {/* Power output from ATS */}
      <line x1="82" y1="225" x2="98" y2="225" stroke={C.copper} strokeOpacity="0.4" strokeWidth="1.5" />
      <polygon points="92,222.5 97,225 92,227.5" fill={C.copper} fillOpacity="0.35" />
      {/* Labels */}
      <text x="40" y="150" fill={C.white} fillOpacity="0.45" fontSize="5" fontFamily="monospace">GEN SET #1 — 2 MW</text>
      <text x="40" y="186" fill={C.white} fillOpacity="0.3" fontSize="4" fontFamily="monospace">GEN SET #2</text>
    </g>
  )
}

function PumpDrawing({ active }: { active: boolean }) {
  const o = active ? 1 : 0.6
  // P&ID pump symbol: circle with triangle inside
  const Pump = ({ x, y, size, color, label }: { x: number; y: number; size: number; color: string; label: string }) => (
    <g>
      <circle cx={x} cy={y} r={size} stroke={color} strokeOpacity="0.45" strokeWidth="0.6" fill={color} fillOpacity="0.06" />
      <polygon
        points={`${x - size * 0.5},${y - size * 0.55} ${x + size * 0.65},${y} ${x - size * 0.5},${y + size * 0.55}`}
        fill={color} fillOpacity="0.2" />
      <text x={x} y={y + size + 5} textAnchor="middle" fill={color} fillOpacity="0.35" fontSize="3" fontFamily="monospace">{label}</text>
    </g>
  )
  return (
    <g opacity={o} className="transition-opacity duration-300">
      {/* Primary CHW pumps */}
      <Pump x={240} y={165} size={6} color={C.forest} label="PRI CHW" />
      <Pump x={260} y={165} size={6} color={C.forest} label="" />
      {/* Secondary CHW pumps */}
      <Pump x={240} y={195} size={6} color={C.forest} label="SEC CHW" />
      <Pump x={260} y={195} size={6} color={C.forest} label="" />
      {/* CW pumps */}
      <Pump x={240} y={220} size={5} color={C.titanium} label="CW" />
      <Pump x={258} y={220} size={5} color={C.titanium} label="" />
      {/* Condensate pump */}
      <Pump x={248} y={245} size={5} color={C.copper} label="COND" />
      {/* VFD indicators */}
      {[165, 195].map((y) => (
        <g key={y}>
          <rect x="268" y={y - 4} width="12" height="8" rx="1"
            stroke={C.forest} strokeOpacity="0.25" strokeWidth="0.4" fill={C.forest} fillOpacity="0.04" />
          <text x="274" y={y + 2} textAnchor="middle" fill={C.forest} fillOpacity="0.3" fontSize="2.8" fontFamily="monospace">VFD</text>
        </g>
      ))}
      {/* Pipe headers — horizontal lines through pumps */}
      <line x1="220" y1="165" x2="295" y2="165" stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.4" />
      <line x1="220" y1="195" x2="295" y2="195" stroke={C.forest} strokeOpacity="0.12" strokeWidth="0.4" />
      <line x1="220" y1="220" x2="280" y2="220" stroke={C.titanium} strokeOpacity="0.12" strokeWidth="0.4" />
      {/* Label */}
      <text x="250" y="150" textAnchor="middle" fill={C.white} fillOpacity="0.4" fontSize="4.5" fontFamily="monospace">PUMP HOUSE</text>
    </g>
  )
}

function BasDrawing({ active }: { active: boolean }) {
  const o = active ? 1 : 0.6
  return (
    <g opacity={o} className="transition-opacity duration-300">
      {/* Head-end workstation — monitor */}
      <rect x="310" y="170" width="45" height="30" rx="2"
        stroke={C.titanium} strokeOpacity="0.45" strokeWidth="0.7" fill={C.titanium} fillOpacity="0.04" />
      {/* Screen content — dashboard */}
      <rect x="314" y="174" width="37" height="20" rx="1"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.03" />
      {/* Dashboard elements — mini bar chart */}
      {[0, 1, 2, 3, 4, 5].map((i) => (
        <rect key={i} x={317 + i * 5} y={188 - (3 + i * 1.2)} width="3" height={3 + i * 1.2} rx="0.3"
          fill={i < 4 ? C.forest : C.copper} fillOpacity={0.15 + i * 0.03} />
      ))}
      {/* Trend line on screen */}
      <polyline points="316,180 322,179 328,181 334,177 340,175 346,176"
        stroke={C.forest} strokeOpacity="0.3" strokeWidth="0.5" fill="none" />
      {/* Status LEDs */}
      <circle cx="318" cy="176" r="1" fill="#2ECC71" fillOpacity="0.35" />
      <circle cx="323" cy="176" r="1" fill="#2ECC71" fillOpacity="0.3" />
      <circle cx="328" cy="176" r="1" fill={C.copper} fillOpacity="0.3" />
      {/* Monitor stand */}
      <line x1="332" y1="200" x2="332" y2="208" stroke={C.titanium} strokeOpacity="0.25" strokeWidth="0.5" />
      <rect x="325" y="208" width="15" height="3" rx="0.5"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.3" fill={C.titanium} fillOpacity="0.03" />
      {/* Network hub */}
      <rect x="315" y="218" width="20" height="10" rx="1"
        stroke={C.titanium} strokeOpacity="0.35" strokeWidth="0.5" fill={C.titanium} fillOpacity="0.04" />
      {[320, 324, 328, 332].map((x) => (
        <circle key={x} cx={x} cy="223" r="0.8" fill="#2ECC71" fillOpacity="0.25" />
      ))}
      <text x="325" y="226" textAnchor="middle" fill={C.titanium} fillOpacity="0.25" fontSize="2.5" fontFamily="monospace">HUB</text>
      {/* BACnet / Modbus labels */}
      <text x="345" y="224" fill={C.titanium} fillOpacity="0.25" fontSize="2.8" fontFamily="monospace">BACnet</text>
      <text x="345" y="230" fill={C.titanium} fillOpacity="0.2" fontSize="2.5" fontFamily="monospace">Modbus</text>
      {/* Monitoring lines to equipment (dashed) */}
      {[
        { x1: 315, y1: 223, x2: 90, y2: 180 },   // to generators
        { x1: 315, y1: 218, x2: 90, y2: 75 },     // to boilers
        { x1: 315, y1: 216, x2: 195, y2: 75 },    // to chillers
        { x1: 315, y1: 220, x2: 200, y2: 200 },   // to cooling towers
        { x1: 315, y1: 225, x2: 280, y2: 195 },   // to pumps
      ].map((line, i) => (
        <line key={i} {...line}
          stroke={C.titanium} strokeOpacity="0.06" strokeWidth="0.4" strokeDasharray="4 3" />
      ))}
      {/* ENFRA Connect label */}
      <rect x="308" y="236" width="50" height="12" rx="1"
        stroke={C.copper} strokeOpacity="0.3" strokeWidth="0.5" fill={C.copper} fillOpacity="0.04" />
      <text x="333" y="244" textAnchor="middle" fill={C.copper} fillOpacity="0.4" fontSize="3.5" fontFamily="monospace">ENFRA Connect®</text>
      {/* Label */}
      <text x="332" y="164" textAnchor="middle" fill={C.white} fillOpacity="0.4" fontSize="4.5" fontFamily="monospace">BAS HEAD-END</text>
    </g>
  )
}

function BuildingLoad() {
  return (
    <g opacity="0.55">
      {/* Hospital building silhouette */}
      <rect x="310" y="34" width="75" height="100" rx="2"
        stroke={C.titanium} strokeOpacity="0.2" strokeWidth="0.6" fill={C.titanium} fillOpacity="0.02" />
      {/* Hospital cross */}
      <line x1="347" y1="40" x2="347" y2="56" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" />
      <line x1="339" y1="48" x2="355" y2="48" stroke={C.titanium} strokeOpacity="0.15" strokeWidth="0.5" />
      {/* Window rows */}
      {[64, 78, 92, 106].map((y) =>
        [318, 330, 342, 354, 366, 378].map((x) => (
          <rect key={`${x}-${y}`} x={x} y={y} width="5" height="7" rx="0.5"
            fill={C.titanium} fillOpacity="0.03" stroke={C.titanium} strokeOpacity="0.05" strokeWidth="0.2" />
        ))
      )}
      {/* AHU symbol inside building */}
      <rect x="330" y="118" width="20" height="10" rx="1"
        stroke={C.forest} strokeOpacity="0.15" strokeWidth="0.4" fill={C.forest} fillOpacity="0.03" />
      <text x="340" y="125" textAnchor="middle" fill={C.forest} fillOpacity="0.2" fontSize="3" fontFamily="monospace">AHU</text>
      {/* Label */}
      <text x="347" y="30" textAnchor="middle" fill={C.white} fillOpacity="0.3" fontSize="4" fontFamily="monospace">HOSPITAL</text>
      <text x="347" y="142" textAnchor="middle" fill={C.titanium} fillOpacity="0.2" fontSize="3" fontFamily="monospace">VA MEDICAL CENTER</text>
    </g>
  )
}

/* ═══════════════════════════════════════════════════════
   Main piping network — connects all equipment
   ═══════════════════════════════════════════════════════ */
function PipingNetwork({ isInView }: { isInView: boolean }) {
  const pipes = [
    // Steam supply: Boilers → Building (copper, solid)
    { d: 'M 90 50 L 300 50 L 300 60 L 310 60', color: C.copper, w: 1.8, dash: '', delay: 0.8, label: 'STEAM SUPPLY', lx: 200, ly: 46 },
    // Condensate return: Building → Boilers (copper, dashed)
    { d: 'M 310 120 L 300 120 L 300 95 L 74 95 L 74 76', color: C.copper, w: 1.0, dash: '3 2', delay: 1.0, label: 'CONDENSATE RETURN', lx: 200, ly: 92 },
    // CHW supply: Chillers → Pumps → Building (forest, solid)
    { d: 'M 220 51 L 295 51 L 295 80 L 310 80', color: C.forest, w: 1.8, dash: '', delay: 1.2, label: 'CHW SUPPLY', lx: 280, ly: 77 },
    // CHW return: Building → Chillers (forest, dashed)
    { d: 'M 310 100 L 295 100 L 295 58 L 220 58', color: C.forest, w: 1.0, dash: '3 2', delay: 1.4, label: 'CHW RETURN', lx: 280, ly: 104 },
    // Electrical: Generators → Building
    { d: 'M 98 225 L 295 225 L 295 130 L 310 130', color: C.copper, w: 1.0, dash: '1 2', delay: 1.8, label: 'EMERGENCY POWER', lx: 200, ly: 222 },
  ]

  return (
    <g>
      {pipes.map((p, i) => (
        <g key={i}>
          <motion.path
            d={p.d}
            stroke={p.color}
            strokeOpacity="0.35"
            strokeWidth={p.w}
            strokeDasharray={p.dash || undefined}
            fill="none"
            initial={{ pathLength: 0 }}
            animate={isInView ? { pathLength: 1 } : {}}
            transition={{ duration: 2, delay: p.delay }}
          />
          <text x={p.lx} y={p.ly} fill={p.color} fillOpacity="0.25" fontSize="3" fontFamily="monospace">{p.label}</text>
        </g>
      ))}
      {/* Underground distribution zone */}
      <rect x="98" y="260" width="200" height="14" rx="1"
        stroke={C.titanium} strokeOpacity="0.08" strokeWidth="0.4" fill={C.titanium} fillOpacity="0.01" />
      <text x="198" y="269" textAnchor="middle" fill={C.titanium} fillOpacity="0.2" fontSize="3.5" fontFamily="monospace">
        UNDERGROUND DISTRIBUTION TUNNEL
      </text>
    </g>
  )
}

/* ═══════════════════════════════════════════════════════
   Clickable overlay zones (invisible hit areas)
   ═══════════════════════════════════════════════════════ */
const zones = [
  { id: 'boiler', x: 4, y: 20, w: 86, h: 100 },
  { id: 'chiller', x: 125, y: 20, w: 100, h: 115 },
  { id: 'cooling-tower', x: 125, y: 135, w: 90, h: 120 },
  { id: 'generator', x: 4, y: 140, w: 100, h: 110 },
  { id: 'pumps', x: 220, y: 145, w: 70, h: 115 },
  { id: 'bas', x: 300, y: 155, w: 70, h: 105 },
]

/* ═══════════════════════════════════════════════════════
   Main component
   ═══════════════════════════════════════════════════════ */
export default function EnergyPlantDiagram() {
  const { ref, isInView } = useInView(0.1)
  const [activeComponent, setActiveComponent] = useState<string | null>(null)
  const active = components.find((c) => c.id === activeComponent)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Interactive Diagram
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Anatomy of a Central Energy Plant.
          </h2>
          <p className="mt-4 text-titanium max-w-2xl">
            The &ldquo;heart and lungs&rdquo; of a hospital — producing steam, chilled water,
            and emergency power for an entire VA medical center campus. Click each system to learn more.
          </p>
        </motion.div>

        {/* Diagram — full width */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.8, delay: 0.3 }}
          className="glass rounded-xl p-4 md:p-6 lg:p-8"
        >
          <div className="relative w-full" style={{ paddingBottom: '68%' }}>
            <svg
              viewBox="0 0 400 275"
              className="absolute inset-0 w-full h-full"
              fill="none"
              preserveAspectRatio="xMidYMid meet"
            >
              {/* Piping first (behind equipment) */}
              <PipingNetwork isInView={isInView} />

              {/* Equipment drawings */}
              <BoilerDrawing active={activeComponent === 'boiler'} />
              <ChillerDrawing active={activeComponent === 'chiller'} />
              <CoolingTowerDrawing active={activeComponent === 'cooling-tower'} />
              <GeneratorDrawing active={activeComponent === 'generator'} />
              <PumpDrawing active={activeComponent === 'pumps'} />
              <BasDrawing active={activeComponent === 'bas'} />
              <BuildingLoad />

              {/* Clickable overlay zones */}
              {zones.map((z) => (
                <rect
                  key={z.id}
                  x={z.x} y={z.y} width={z.w} height={z.h}
                  fill="transparent"
                  className="cursor-pointer"
                  onClick={() => setActiveComponent(activeComponent === z.id ? null : z.id)}
                />
              ))}

              {/* Active highlight border */}
              {activeComponent && (() => {
                const z = zones.find((z) => z.id === activeComponent)
                if (!z) return null
                const comp = components.find((c) => c.id === activeComponent)
                return (
                  <rect
                    x={z.x} y={z.y} width={z.w} height={z.h}
                    rx="3"
                    fill="transparent"
                    stroke={comp?.color || C.titanium}
                    strokeOpacity="0.25"
                    strokeWidth="1"
                    strokeDasharray="4 2"
                    className="pointer-events-none"
                  />
                )
              })()}

              {/* Title */}
              <text x="200" y="12" textAnchor="middle" className="fill-white/30 text-[4px] font-mono tracking-widest">
                CENTRAL ENERGY PLANT — P&amp;ID SCHEMATIC
              </text>
            </svg>
          </div>

          {/* Inline hint when nothing selected */}
          {!active && (
            <p className="text-center text-titanium/40 text-sm mt-4 font-mono tracking-wide">
              Click on any system in the schematic above
            </p>
          )}
        </motion.div>

        {/* Info panel — appears below diagram when a component is selected */}
        {active && (
          <motion.div
            key={active.id}
            initial={{ opacity: 0, y: 12 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.3 }}
            className="glass rounded-xl p-6 md:p-8 mt-6"
          >
            <div className="flex flex-col md:flex-row gap-6 md:gap-10">
              <div className="flex-1">
                <div className="flex items-center gap-3 mb-3">
                  <div
                    className="w-3 h-3 rounded-full"
                    style={{ backgroundColor: active.color }}
                  />
                  <h3 className="font-serif text-xl text-white">{active.label}</h3>
                </div>
                <p className="text-titanium text-sm leading-relaxed">{active.description}</p>
              </div>
              {active.specs && (
                <ul className="space-y-2 md:border-l md:border-white/5 md:pl-10 shrink-0 md:w-64">
                  {active.specs.map((spec, i) => (
                    <li key={i} className="text-titanium/60 text-xs font-mono flex items-start gap-2">
                      <span className="text-copper mt-0.5">&#x25B8;</span>
                      {spec}
                    </li>
                  ))}
                </ul>
              )}
            </div>
            <button
              onClick={() => setActiveComponent(null)}
              className="mt-4 text-titanium/30 hover:text-titanium/60 text-xs font-mono transition-colors"
            >
              Close
            </button>
          </motion.div>
        )}
      </div>
    </section>
  )
}
