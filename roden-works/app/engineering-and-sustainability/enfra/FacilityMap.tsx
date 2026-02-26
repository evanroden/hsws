'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

/* ═══════════════════════════════════════════════════════════════
   Coordinate mapping (approximate lat/lon → SVG)
   Lon: 79.15°W → 75.85°W   →  x: 10 → 690   (206 px/°)
   Lat: 43.55°N → 42.30°N   →  y: 10 → 340    (264 px/°)
   x = 10 + (79.15 - lon) * 206
   y = 10 + (43.55 - lat) * 264
   ═══════════════════════════════════════════════════════════════ */

const facilities = [
  {
    name: 'UMMC — Batavia',
    label: 'Batavia',
    sub: 'UMMC',
    description:
      '131 beds, 785+ employees. Largest private employer in Genesee County. Sole maternity provider for two counties.',
    x: 208,
    y: 155,
  },
  {
    name: "St. Mary's — Rochester",
    label: 'Rochester',
    sub: "ST. MARY'S",
    description:
      'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
    x: 321,
    y: 116,
  },
]

/* ─── Major cities (larger markers) ─── */
const majorCities = [
  { name: 'Buffalo', x: 66, y: 184, r: 3.5, dy: 15 },
  { name: 'Syracuse', x: 628, y: 142, r: 3, dy: -9 },
]

/* ─── Minor cities & towns ─── */
const towns = [
  { name: 'Niagara Falls', x: 31, y: 131, dy: -7 },
  { name: 'Lockport', x: 105, y: 110, dy: -7 },
  { name: 'Medina', x: 167, y: 94, dy: -6 },
  { name: 'Albion', x: 196, y: 100, dy: 10 },
  { name: 'Le Roy', x: 253, y: 160, dy: 10 },
  { name: 'Brockport', x: 259, y: 100, dy: -6 },
  { name: 'Fairport', x: 362, y: 118, dy: 10 },
  { name: 'Palmyra', x: 406, y: 130, dy: 10 },
  { name: 'Newark', x: 434, y: 133, dy: -6 },
  { name: 'Canandaigua', x: 393, y: 195, dy: 10 },
  { name: 'Geneva', x: 457, y: 192, dy: 10 },
  { name: 'Lyons', x: 455, y: 130, dy: 10 },
  { name: 'Clyde', x: 470, y: 128, dy: -6 },
  { name: 'Auburn', x: 541, y: 174, dy: -7 },
  { name: 'Weedsport', x: 544, y: 136, dy: -6 },
  { name: 'Seneca Falls', x: 500, y: 160, dy: 10 },
  { name: 'Oswego', x: 553, y: 34, dy: -6 },
]

/* ─── Finger Lakes (quadratic bezier paths) ─── */
const fingerLakes = [
  /* Minor "Little" Finger Lakes */
  { name: 'Conesus', x1: 307, y1: 208, cx: 306, cy: 225, x2: 305, y2: 242, w: 1.5 },
  { name: 'Hemlock', x1: 329, y1: 214, cx: 327, cy: 228, x2: 325, y2: 242, w: 1.2 },
  { name: 'Canadice', x1: 335, y1: 218, cx: 335, cy: 225, x2: 335, y2: 232, w: 0.8 },
  { name: 'Honeoye', x1: 348, y1: 210, cx: 348, cy: 221, x2: 348, y2: 232, w: 1.2 },
  /* Major Finger Lakes */
  { name: 'Canandaigua', x1: 393, y1: 195, cx: 388, cy: 232, x2: 381, y2: 269, w: 2.8 },
  { name: 'Keuka', x1: 443, y1: 245, cx: 436, cy: 278, x2: 428, y2: 312, w: 2.2 },
  { name: 'Seneca', x1: 467, y1: 186, cx: 471, cy: 248, x2: 476, y2: 316, w: 4.5 },
  { name: 'Cayuga', x1: 509, y1: 180, cx: 529, cy: 244, x2: 551, y2: 314, w: 4 },
  { name: 'Owasco', x1: 554, y1: 178, cx: 555, cy: 208, x2: 556, y2: 234, w: 2 },
  { name: 'Skaneateles', x1: 570, y1: 175, cx: 578, cy: 205, x2: 586, y2: 234, w: 2.2 },
  { name: 'Otisco', x1: 603, y1: 178, cx: 603, cy: 188, x2: 603, y2: 198, w: 1 },
]

/* ─── County boundaries (N–S lines) ─── */
const countyLines = [
  { x: 61, y1: 70, y2: 340, left: 'NIAGARA', right: 'ERIE' },
  { x: 152, y1: 62, y2: 340, left: 'ERIE', right: 'GENESEE' },
  { x: 266, y1: 58, y2: 340, left: 'ORLEANS', right: 'MONROE' },
  { x: 402, y1: 82, y2: 340, left: 'WAYNE', right: 'ONTARIO' },
  { x: 490, y1: 84, y2: 340, left: '', right: 'SENECA' },
  { x: 515, y1: 70, y2: 340, left: '', right: 'CAYUGA' },
  { x: 594, y1: 40, y2: 340, left: '', right: 'ONONDAGA' },
]

/* ─── Shoreline (18 control points, west → east) ─── */
const ontarioShore = `
  M 10 86
  C 18 85, 24 85, 29 86
  C 45 76, 65 70, 78 71
  C 88 67, 94 66, 99 65
  C 130 60, 170 63, 202 65
  C 225 61, 238 59, 247 60
  C 260 58, 270 57, 278 57
  C 290 60, 298 68, 305 73
  C 315 78, 320 82, 325 84
  C 332 90, 338 97, 344 100
  C 350 97, 358 90, 366 84
  C 375 82, 383 83, 391 84
  C 415 83, 440 83, 459 84
  C 466 87, 470 90, 473 92
  C 485 85, 500 77, 514 71
  C 530 55, 542 42, 553 34
  C 575 24, 610 28, 637 31
  C 660 29, 678 29, 690 28
`

export default function FacilityMap() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section
      className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
      ref={ref}
    >
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Facilities
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Western New York footprint.
          </h2>
        </motion.div>

        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.8, delay: 0.3 }}
          className="glass rounded-xl p-4 md:p-6 lg:p-8"
        >
          <div className="relative w-full" style={{ paddingBottom: '50%' }}>
            <svg
              viewBox="0 0 700 350"
              className="absolute inset-0 w-full h-full"
              fill="none"
            >
              {/* ═══ LAYER 1: WATER FILLS ═══ */}

              {/* Lake Ontario fill */}
              <motion.path
                d={`
                  M 0 0 L 700 0 L 690 28
                  C 678 29, 660 29, 637 31
                  C 610 28, 575 24, 553 34
                  C 542 42, 530 55, 514 71
                  C 500 77, 485 85, 473 92
                  C 470 90, 466 87, 459 84
                  C 440 83, 415 83, 391 84
                  C 383 83, 375 82, 366 84
                  C 358 90, 350 97, 344 100
                  C 338 97, 332 90, 325 84
                  C 320 82, 315 78, 305 73
                  C 298 68, 290 60, 278 57
                  C 270 57, 260 58, 247 60
                  C 238 59, 225 61, 202 65
                  C 170 63, 130 60, 99 65
                  C 94 66, 88 67, 78 71
                  C 65 70, 45 76, 29 86
                  C 24 85, 18 85, 10 86
                  L 0 86 Z
                `}
                fill="rgba(138,155,168,0.10)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.2 }}
              />

              {/* Lake Erie fill (NE tip) */}
              <motion.path
                d={`
                  M 66 187
                  C 72 200, 76 215, 76 229
                  C 62 242, 45 252, 31 258
                  C 18 268, 8 275, 0 282
                  L 0 187 Z
                `}
                fill="rgba(138,155,168,0.08)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1 }}
              />

              {/* ═══ LAYER 2: LAND FILL ═══ */}
              <motion.path
                d={`
                  M 10 86 ${ontarioShore.replace('M 10 86', '')}
                  L 700 28 L 700 350 L 0 350 L 0 86 Z
                `}
                fill="rgba(27,58,45,0.035)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.5 }}
              />

              {/* ═══ LAYER 3: COUNTY BOUNDARIES ═══ */}
              {countyLines.map((line, i) => (
                <motion.g
                  key={`county-${i}`}
                  initial={{ opacity: 0 }}
                  animate={isInView ? { opacity: 1 } : {}}
                  transition={{ duration: 0.5, delay: 1 + i * 0.05 }}
                >
                  <line
                    x1={line.x}
                    y1={line.y1}
                    x2={line.x}
                    y2={line.y2}
                    stroke="rgba(45,90,69,0.07)"
                    strokeWidth="0.5"
                  />
                  {/* County name labels */}
                  {line.right && (
                    <text
                      x={line.x + 6}
                      y={line.y2 - 6}
                      className="fill-forest/15 text-[4px] font-mono tracking-wider"
                    >
                      {line.right}
                    </text>
                  )}
                </motion.g>
              ))}

              {/* ═══ LAYER 4: NIAGARA ESCARPMENT ═══ */}
              <motion.path
                d={`
                  M 0 108
                  C 30 107, 60 106, 105 105
                  C 150 104, 200 103, 260 100
                  C 320 98, 400 95, 460 90
                  C 520 86, 560 78, 600 72
                `}
                fill="none"
                stroke="rgba(138,155,168,0.06)"
                strokeWidth="0.6"
                strokeDasharray="2 3"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.5, delay: 1.5 }}
              />
              <text
                x="170"
                y="100"
                className="fill-titanium/8 text-[3.5px] font-mono tracking-wider"
              >
                NIAGARA ESCARPMENT
              </text>

              {/* ═══ LAYER 5: ERIE CANAL ═══ */}
              <motion.path
                d={`
                  M 66 153
                  Q 85 148, 105 108
                  Q 135 100, 167 96
                  Q 200 98, 240 100
                  Q 270 102, 327 106
                  Q 362 108, 406 110
                  Q 434 113, 455 116
                  Q 480 120, 508 130
                  Q 540 135, 580 138
                  Q 605 139, 625 140
                `}
                fill="none"
                stroke="rgba(138,155,168,0.18)"
                strokeWidth="0.8"
                strokeDasharray="3 2"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2.5, delay: 1.2 }}
              />
              <text
                x="220"
                y="95"
                className="fill-titanium/15 text-[5px] font-mono tracking-wider"
              >
                ERIE CANAL
              </text>

              {/* ═══ LAYER 6: FINGER LAKES ═══ */}
              {fingerLakes.map((lake, i) => (
                <motion.path
                  key={lake.name}
                  d={`M ${lake.x1} ${lake.y1} Q ${lake.cx} ${lake.cy} ${lake.x2} ${lake.y2}`}
                  fill="none"
                  stroke="rgba(138,155,168,0.25)"
                  strokeWidth={lake.w}
                  strokeLinecap="round"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1, delay: 1.3 + i * 0.06 }}
                />
              ))}
              {/* Keuka NE branch (Y-shape) */}
              <motion.path
                d="M 436 260 Q 448 248 457 240"
                fill="none"
                stroke="rgba(138,155,168,0.2)"
                strokeWidth="1.5"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 0.6, delay: 1.6 }}
              />
              {/* Keuka NW branch */}
              <motion.path
                d="M 436 260 Q 424 248 413 238"
                fill="none"
                stroke="rgba(138,155,168,0.18)"
                strokeWidth="1.2"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 0.6, delay: 1.65 }}
              />
              {/* Individual lake labels */}
              {[
                { name: 'Conesus', x: 307, y: 250, s: 3.5 },
                { name: 'Hemlock', x: 325, y: 250, s: 3.5 },
                { name: 'Honeoye', x: 348, y: 240, s: 3.5 },
                { name: 'Canandaigua', x: 374, y: 275, s: 4 },
                { name: 'Keuka', x: 420, y: 318, s: 4 },
                { name: 'Seneca', x: 478, y: 322, s: 5 },
                { name: 'Cayuga', x: 555, y: 320, s: 5 },
                { name: 'Owasco', x: 558, y: 240, s: 3.5 },
                { name: 'Skaneateles', x: 592, y: 240, s: 3.5 },
              ].map((l) => (
                <text
                  key={l.name}
                  x={l.x}
                  y={l.y}
                  textAnchor="middle"
                  className="fill-titanium/12 font-mono"
                  style={{ fontSize: `${l.s}px` }}
                >
                  {l.name}
                </text>
              ))}
              {/* Region label */}
              <text
                x="490"
                y="338"
                textAnchor="middle"
                className="fill-titanium/10 text-[7px] font-mono tracking-[0.2em]"
              >
                FINGER LAKES REGION
              </text>

              {/* ═══ LAYER 7: RIVERS ═══ */}

              {/* Genesee River (flows N through Rochester to Lake Ontario) */}
              <motion.path
                d={`
                  M 325 84
                  Q 326 92, 325 100
                  Q 324 108, 321 116
                  Q 323 130, 327 150
                  Q 330 170, 333 190
                  Q 335 210, 335 230
                `}
                fill="none"
                stroke="rgba(138,155,168,0.25)"
                strokeWidth="1.2"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.8 }}
              />
              <text
                x="338"
                y="178"
                className="fill-titanium/12 text-[4.5px] font-mono"
                transform="rotate(-85, 338, 178)"
              >
                GENESEE R.
              </text>
              {/* High Falls annotation */}
              <line
                x1="316"
                y1="108"
                x2="314"
                y2="112"
                stroke="rgba(184,115,51,0.2)"
                strokeWidth="0.5"
              />
              <text
                x="310"
                y="114"
                className="fill-copper/20 text-[3px] font-mono"
                textAnchor="end"
              >
                High Falls
              </text>

              {/* Niagara River (flows N from Lake Erie to Lake Ontario) */}
              <motion.path
                d={`
                  M 29 86
                  Q 30 95, 32 105
                  Q 30 115, 27 125
                  Q 26 130, 27 131
                  Q 30 140, 35 150
                  Q 42 162, 50 170
                  Q 58 178, 66 184
                `}
                fill="none"
                stroke="rgba(138,155,168,0.3)"
                strokeWidth="2"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 0.6 }}
              />
              <text
                x="15"
                y="108"
                className="fill-titanium/14 text-[4.5px] font-mono"
                transform="rotate(-82, 15, 108)"
              >
                NIAGARA R.
              </text>

              {/* Grand Island */}
              <ellipse
                cx="42"
                cy="148"
                rx="9"
                ry="15"
                fill="rgba(27,58,45,0.06)"
                stroke="rgba(138,155,168,0.14)"
                strokeWidth="0.5"
              />
              <text
                x="56"
                y="149"
                className="fill-titanium/12 text-[3.5px] font-mono"
              >
                Grand Is.
              </text>

              {/* Niagara Falls marker (waterfall hash lines) */}
              {[0, 3, 6, 9].map((dx) => (
                <line
                  key={dx}
                  x1={22 + dx}
                  y1="131"
                  x2={22 + dx}
                  y2="135"
                  stroke="rgba(184,115,51,0.25)"
                  strokeWidth="0.6"
                />
              ))}
              <line
                x1="20"
                y1="131"
                x2="34"
                y2="131"
                stroke="rgba(184,115,51,0.35)"
                strokeWidth="0.8"
              />

              {/* Oswego River (Seneca/Cayuga outlet to Lake Ontario at Oswego) */}
              <motion.path
                d={`
                  M 509 180
                  Q 520 170, 528 155
                  Q 540 130, 546 100
                  Q 550 65, 553 34
                `}
                fill="none"
                stroke="rgba(138,155,168,0.18)"
                strokeWidth="0.8"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 1 }}
              />
              <text
                x="538"
                y="90"
                className="fill-titanium/10 text-[4px] font-mono"
                transform="rotate(-80, 538, 90)"
              >
                OSWEGO R.
              </text>

              {/* Tonawanda Creek (east-west, north of Buffalo area) */}
              <motion.path
                d="M 66 155 Q 80 153, 95 152 Q 115 148, 140 148 Q 160 150, 175 155"
                fill="none"
                stroke="rgba(138,155,168,0.12)"
                strokeWidth="0.6"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1, delay: 1.1 }}
              />
              <text
                x="120"
                y="145"
                textAnchor="middle"
                className="fill-titanium/8 text-[3px] font-mono"
              >
                TONAWANDA CK.
              </text>

              {/* Clyde River / Seneca River (connecting Finger Lakes to Oswego R.) */}
              <motion.path
                d="M 467 186 Q 485 175, 509 180 Q 509 180, 509 180"
                fill="none"
                stroke="rgba(138,155,168,0.12)"
                strokeWidth="0.6"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 0.8, delay: 1.2 }}
              />

              {/* Montezuma NWR area (hatched marsh between Cayuga & Seneca north ends) */}
              <rect
                x="495"
                y="172"
                width="22"
                height="14"
                rx="3"
                fill="rgba(45,90,69,0.05)"
                stroke="rgba(45,90,69,0.1)"
                strokeWidth="0.3"
                strokeDasharray="1.5 1"
              />
              <text
                x="506"
                y="170"
                textAnchor="middle"
                className="fill-forest/12 text-[3px] font-mono"
              >
                MONTEZUMA NWR
              </text>

              {/* ═══ LAYER 8: SHORELINES (strokes) ═══ */}

              {/* Lake Ontario shore */}
              <motion.path
                d={ontarioShore}
                fill="none"
                stroke="rgba(138,155,168,0.5)"
                strokeWidth="1.5"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2.5 }}
              />
              {/* Lake Ontario label */}
              <text
                x="200"
                y="25"
                textAnchor="middle"
                className="fill-titanium/18 text-[12px] font-mono tracking-[0.4em]"
              >
                LAKE ONTARIO
              </text>

              {/* Shoreline feature labels */}
              {[
                { name: 'Olcott', x: 99, y: 60 },
                { name: 'Pt. Breeze', x: 202, y: 60 },
                { name: 'Hamlin Beach', x: 278, y: 52 },
                { name: 'Braddock Bay', x: 305, y: 68 },
                { name: 'Charlotte', x: 325, y: 79 },
                { name: 'Irondequoit Bay', x: 352, y: 108 },
                { name: 'Sodus Bay', x: 459, y: 78 },
                { name: 'Fair Haven', x: 514, y: 66 },
              ].map((f) => (
                <text
                  key={f.name}
                  x={f.x}
                  y={f.y}
                  textAnchor="middle"
                  className="fill-titanium/12 text-[3.5px] font-mono"
                >
                  {f.name}
                </text>
              ))}

              {/* Lake Erie shore */}
              <motion.path
                d={`
                  M 66 187
                  C 72 200, 76 215, 76 229
                  C 62 242, 45 252, 31 258
                  C 18 268, 8 275, 0 282
                `}
                fill="none"
                stroke="rgba(138,155,168,0.35)"
                strokeWidth="1"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 0.4 }}
              />
              <text
                x="22"
                y="248"
                className="fill-titanium/14 text-[9px] font-mono tracking-[0.2em]"
                transform="rotate(-60, 22, 248)"
              >
                LAKE ERIE
              </text>

              {/* ═══ LAYER 9: HIGHWAYS ═══ */}

              {/* I-90 / NYS Thruway (bypasses Rochester to the south) */}
              <motion.path
                d={`
                  M 82 163
                  Q 130 155, 169 152
                  Q 190 150, 208 150
                  Q 230 153, 253 160
                  Q 280 155, 323 152
                  Q 345 156, 364 160
                  Q 381 163, 416 155
                  Q 449 158, 480 152
                  Q 510 145, 542 145
                  Q 585 137, 600 133
                  Q 615 131, 628 131
                `}
                fill="none"
                stroke="rgba(184,115,51,0.35)"
                strokeWidth="2"
                strokeDasharray="8 4"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2.5, delay: 0.8 }}
              />

              {/* I-490 spur to Rochester (splits from I-90 at Le Roy, rejoins at Victor) */}
              <motion.path
                d={`
                  M 253 160
                  Q 275 145, 300 130
                  Q 315 122, 321 120
                  Q 340 125, 362 130
                  Q 364 145, 364 160
                `}
                fill="none"
                stroke="rgba(184,115,51,0.2)"
                strokeWidth="1"
                strokeDasharray="5 3"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.5, delay: 1.5 }}
              />
              <text
                x="290"
                y="128"
                className="fill-copper/20 text-[4px] font-mono"
              >
                I-490
              </text>

              {/* I-390 (N-S through Rochester area) */}
              <motion.path
                d="M 305 73 Q 310 90, 315 110 Q 320 140, 318 170 Q 315 200, 310 230"
                fill="none"
                stroke="rgba(184,115,51,0.15)"
                strokeWidth="0.8"
                strokeDasharray="4 3"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 1.6 }}
              />
              <text
                x="304"
                y="202"
                className="fill-copper/15 text-[3.5px] font-mono"
              >
                I-390
              </text>

              {/* Route 20 (east-west, south of I-90) */}
              <motion.path
                d={`
                  M 66 195
                  Q 150 185, 208 175
                  Q 280 180, 340 185
                  Q 395 195, 457 192
                  Q 500 185, 541 174
                  Q 585 162, 628 155
                `}
                fill="none"
                stroke="rgba(138,155,168,0.1)"
                strokeWidth="0.6"
                strokeDasharray="4 2"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2, delay: 1.8 }}
              />
              <text
                x="285"
                y="185"
                className="fill-titanium/10 text-[3.5px] font-mono"
              >
                US-20
              </text>

              {/* I-90 shields */}
              {[
                { x: 140, y: 161 },
                { x: 460, y: 161 },
              ].map((s, i) => (
                <g key={`shield-${i}`}>
                  <rect
                    x={s.x}
                    y={s.y}
                    width="26"
                    height="13"
                    rx="3"
                    fill="rgba(184,115,51,0.08)"
                    stroke="rgba(184,115,51,0.3)"
                    strokeWidth="0.5"
                  />
                  <text
                    x={s.x + 13}
                    y={s.y + 9.5}
                    textAnchor="middle"
                    className="fill-copper/45 text-[7px] font-mono"
                  >
                    I-90
                  </text>
                </g>
              ))}

              {/* ═══ LAYER 10: SERVICE CORRIDOR ═══ */}
              <motion.line
                x1={facilities[0].x}
                y1={facilities[0].y}
                x2={facilities[1].x}
                y2={facilities[1].y}
                stroke="#2D5A45"
                strokeWidth="2.5"
                strokeDasharray="8 5"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 1.5 }}
              />
              {/* Distance badge */}
              <motion.g
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 0.5, delay: 2 }}
              >
                <rect
                  x={(facilities[0].x + facilities[1].x) / 2 - 24}
                  y={(facilities[0].y + facilities[1].y) / 2 - 17}
                  width="48"
                  height="17"
                  rx="8"
                  fill="rgba(45,90,69,0.15)"
                  stroke="rgba(45,90,69,0.35)"
                  strokeWidth="0.6"
                />
                <text
                  x={(facilities[0].x + facilities[1].x) / 2}
                  y={(facilities[0].y + facilities[1].y) / 2 - 6}
                  textAnchor="middle"
                  className="fill-forest-light/60 text-[8px] font-mono"
                >
                  ~30 mi
                </text>
              </motion.g>

              {/* ═══ LAYER 11: CITIES & TOWNS ═══ */}

              {/* Major cities */}
              {majorCities.map((c, i) => (
                <motion.g
                  key={c.name}
                  initial={{ opacity: 0 }}
                  animate={isInView ? { opacity: 1 } : {}}
                  transition={{ duration: 0.4, delay: 1 + i * 0.1 }}
                >
                  <circle
                    cx={c.x}
                    cy={c.y}
                    r={c.r}
                    fill="rgba(138,155,168,0.35)"
                  />
                  <text
                    x={c.x}
                    y={c.y + c.dy}
                    textAnchor="middle"
                    className="fill-titanium/60 text-[10px] font-mono"
                  >
                    {c.name}
                  </text>
                </motion.g>
              ))}

              {/* Minor towns */}
              {towns.map((t, i) => (
                <motion.g
                  key={t.name}
                  initial={{ opacity: 0 }}
                  animate={isInView ? { opacity: 1 } : {}}
                  transition={{ duration: 0.3, delay: 1.2 + i * 0.04 }}
                >
                  <circle
                    cx={t.x}
                    cy={t.y}
                    r="1.3"
                    fill="rgba(138,155,168,0.2)"
                  />
                  <text
                    x={t.x}
                    y={t.y + t.dy}
                    textAnchor="middle"
                    className="fill-titanium/35 text-[5.5px] font-mono"
                  >
                    {t.name}
                  </text>
                </motion.g>
              ))}

              {/* Onondaga Lake (near Syracuse) */}
              <ellipse
                cx="622"
                cy="128"
                rx="6"
                ry="9"
                fill="rgba(138,155,168,0.1)"
                stroke="rgba(138,155,168,0.2)"
                strokeWidth="0.4"
              />
              <text
                x="635"
                y="126"
                className="fill-titanium/10 text-[3px] font-mono"
              >
                Onondaga L.
              </text>

              {/* ═══ LAYER 12: FACILITY MARKERS ═══ */}
              {facilities.map((f, i) => (
                <motion.g
                  key={f.name}
                  initial={{ opacity: 0, scale: 0 }}
                  animate={isInView ? { opacity: 1, scale: 1 } : {}}
                  transition={{ duration: 0.5, delay: 1.3 + i * 0.3 }}
                >
                  <motion.circle
                    cx={f.x}
                    cy={f.y}
                    r="16"
                    fill="none"
                    stroke="#2D5A45"
                    strokeWidth="0.8"
                    animate={{
                      r: [16, 28, 16],
                      opacity: [0.4, 0, 0.4],
                    }}
                    transition={{
                      repeat: Infinity,
                      duration: 3,
                      delay: i * 1.5,
                    }}
                  />
                  <circle
                    cx={f.x}
                    cy={f.y}
                    r="12"
                    fill="rgba(45,90,69,0.12)"
                    stroke="#2D5A45"
                    strokeWidth="1"
                  />
                  <circle cx={f.x} cy={f.y} r="7" fill="#2D5A45" />
                  <circle cx={f.x} cy={f.y} r="3" fill="#FAFAFA" />
                </motion.g>
              ))}

              {/* ═══ LAYER 13: FACILITY LABELS ═══ */}
              {facilities.map((f, i) => (
                <motion.g
                  key={`label-${f.name}`}
                  initial={{ opacity: 0, y: 5 }}
                  animate={isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.5, delay: 1.6 + i * 0.3 }}
                >
                  <text
                    x={f.x}
                    y={f.y - 28}
                    textAnchor="middle"
                    className="fill-white text-[14px] font-serif"
                  >
                    {f.label}
                  </text>
                  <text
                    x={f.x}
                    y={f.y - 16}
                    textAnchor="middle"
                    className="fill-titanium/60 text-[7px] font-mono tracking-[0.15em]"
                  >
                    {f.sub}
                  </text>
                </motion.g>
              ))}

              {/* ═══ LAYER 14: MAP FURNITURE ═══ */}

              {/* Compass rose */}
              <g transform="translate(660, 290)">
                <line
                  x1="0"
                  y1="12"
                  x2="0"
                  y2="-12"
                  stroke="rgba(138,155,168,0.3)"
                  strokeWidth="0.6"
                />
                <line
                  x1="-8"
                  y1="0"
                  x2="8"
                  y2="0"
                  stroke="rgba(138,155,168,0.15)"
                  strokeWidth="0.4"
                />
                {/* N arrow */}
                <polygon
                  points="0,-12 -2.5,-6 0,-8 2.5,-6"
                  fill="rgba(138,155,168,0.35)"
                />
                <text
                  x="0"
                  y="-16"
                  textAnchor="middle"
                  className="fill-titanium/40 text-[5px] font-mono"
                >
                  N
                </text>
                <text
                  x="0"
                  y="19"
                  textAnchor="middle"
                  className="fill-titanium/15 text-[3.5px] font-mono"
                >
                  S
                </text>
                <text
                  x="12"
                  y="2"
                  textAnchor="middle"
                  className="fill-titanium/15 text-[3.5px] font-mono"
                >
                  E
                </text>
                <text
                  x="-12"
                  y="2"
                  textAnchor="middle"
                  className="fill-titanium/15 text-[3.5px] font-mono"
                >
                  W
                </text>
              </g>

              {/* Scale bar */}
              <g transform="translate(595, 320)">
                <line
                  x1="0"
                  y1="0"
                  x2="50"
                  y2="0"
                  stroke="rgba(138,155,168,0.3)"
                  strokeWidth="0.8"
                />
                <line
                  x1="0"
                  y1="-2"
                  x2="0"
                  y2="2"
                  stroke="rgba(138,155,168,0.3)"
                  strokeWidth="0.6"
                />
                <line
                  x1="50"
                  y1="-2"
                  x2="50"
                  y2="2"
                  stroke="rgba(138,155,168,0.3)"
                  strokeWidth="0.6"
                />
                <line
                  x1="25"
                  y1="-1.5"
                  x2="25"
                  y2="1.5"
                  stroke="rgba(138,155,168,0.2)"
                  strokeWidth="0.4"
                />
                <text
                  x="0"
                  y="7"
                  textAnchor="middle"
                  className="fill-titanium/25 text-[3.5px] font-mono"
                >
                  0
                </text>
                <text
                  x="25"
                  y="7"
                  textAnchor="middle"
                  className="fill-titanium/20 text-[3px] font-mono"
                >
                  15
                </text>
                <text
                  x="50"
                  y="7"
                  textAnchor="middle"
                  className="fill-titanium/25 text-[3.5px] font-mono"
                >
                  30 mi
                </text>
              </g>
            </svg>
          </div>

          {/* Facility details */}
          <div className="grid grid-cols-1 md:grid-cols-2 gap-6 mt-8 pt-6 border-t border-white/5">
            {facilities.map((f, i) => (
              <motion.div
                key={f.name}
                initial={{ opacity: 0, y: 20 }}
                animate={isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 1.8 + i * 0.2 }}
              >
                <h3 className="font-serif text-lg text-white mb-2">
                  {f.name}
                </h3>
                <p className="text-titanium text-sm leading-relaxed">
                  {f.description}
                </p>
              </motion.div>
            ))}
          </div>
        </motion.div>
      </div>
    </section>
  )
}
