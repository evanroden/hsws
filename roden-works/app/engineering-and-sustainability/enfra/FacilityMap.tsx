'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

/* ═══════════════════════════════════════════════════════
   Geographic coordinate mapping (approximate)
   Lon range: 79.5°W → 75.5°W  →  x: 20 → 480
   Lat range: 44.0°N → 42.2°N  →  y: 20 → 300
   ═══════════════════════════════════════════════════════ */

const facilities = [
  {
    name: 'UMMC — Batavia',
    description:
      '131 beds, 785+ employees. Largest private employer in Genesee County. Sole maternity provider for two counties.',
    x: 171,
    y: 176,
  },
  {
    name: "St. Mary's — Rochester",
    description:
      'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
    x: 237,
    y: 151,
  },
]

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
          <div className="relative w-full" style={{ paddingBottom: '62%' }}>
            <svg
              viewBox="0 0 500 310"
              className="absolute inset-0 w-full h-full"
              fill="none"
            >
              {/* ─── LAKE ONTARIO ─── */}
              {/* Fill: top of viewport down to south shore */}
              <motion.path
                d={`
                  M 0 0 L 500 0 L 500 84
                  C 480 86 460 88 440 90
                  C 420 94 400 98 385 102
                  C 370 106 355 112 340 118
                  C 325 123 310 130 295 133
                  C 280 135 265 134 255 136
                  Q 248 138 242 140
                  Q 237 136 230 132
                  C 218 126 205 122 190 119
                  C 175 117 160 118 145 120
                  C 130 122 115 124 100 128
                  C 88 132 78 135 72 138
                  L 20 0 Z
                `}
                fill="rgba(138,155,168,0.07)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.2 }}
              />
              {/* Shore stroke */}
              <motion.path
                d={`
                  M 72 138
                  C 78 135 88 132 100 128
                  C 115 124 130 122 145 120
                  C 160 118 175 117 190 119
                  C 205 122 218 126 230 132
                  Q 237 136 242 140
                  Q 248 138 255 136
                  C 265 134 280 135 295 133
                  C 310 130 325 123 340 118
                  C 355 112 370 106 385 102
                  C 400 98 420 94 440 90
                  C 460 88 480 86 500 84
                `}
                fill="none"
                stroke="rgba(138,155,168,0.35)"
                strokeWidth="0.8"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2 }}
              />
              <text
                x="200"
                y="60"
                textAnchor="middle"
                className="fill-titanium/20 text-[10px] font-mono tracking-[0.3em]"
              >
                LAKE ONTARIO
              </text>

              {/* ─── LAKE ERIE ─── */}
              {/* Fill: from Buffalo waterfront curving southwest off the map */}
              <motion.path
                d={`
                  M 91 196
                  C 87 207 82 220 75 232
                  C 65 250 52 265 38 280
                  C 28 292 18 300 0 310
                  L 0 196 Z
                `}
                fill="rgba(138,155,168,0.07)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.2 }}
              />
              {/* Shore stroke */}
              <motion.path
                d={`
                  M 91 196
                  C 87 207 82 220 75 232
                  C 65 250 52 265 38 280
                  C 28 292 18 300 0 310
                `}
                fill="none"
                stroke="rgba(138,155,168,0.35)"
                strokeWidth="0.8"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.3 }}
              />
              <text
                x="30"
                y="250"
                className="fill-titanium/15 text-[9px] font-mono tracking-[0.2em]"
                transform="rotate(-55, 30, 250)"
              >
                LAKE ERIE
              </text>

              {/* ─── NIAGARA RIVER ─── */}
              {/* Wide waterway connecting the two lakes */}
              <motion.path
                d={`
                  M 68 138 C 67 150 66 160 68 168
                  C 70 176 76 184 82 190
                  C 86 194 89 196 91 198
                `}
                fill="none"
                stroke="rgba(138,155,168,0.3)"
                strokeWidth="3"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 0.5 }}
              />
              {/* River label */}
              <text
                x="55"
                y="170"
                className="fill-titanium/15 text-[6px] font-mono"
                transform="rotate(-80, 55, 170)"
              >
                NIAGARA R.
              </text>
              {/* Niagara Falls marker */}
              <line
                x1="64"
                y1="163"
                x2="74"
                y2="163"
                stroke="rgba(138,155,168,0.2)"
                strokeWidth="0.5"
              />
              <text
                x="78"
                y="165"
                className="fill-titanium/20 text-[5px] font-mono"
              >
                Falls
              </text>

              {/* ─── LAND FILL ─── */}
              <motion.path
                d={`
                  M 91 198
                  C 89 196 86 194 82 190
                  C 76 184 70 176 68 168
                  C 66 160 67 150 68 138
                  L 72 138
                  C 78 135 88 132 100 128
                  C 115 124 130 122 145 120
                  C 160 118 175 117 190 119
                  C 205 122 218 126 230 132
                  Q 237 136 242 140
                  Q 248 138 255 136
                  C 265 134 280 135 295 133
                  C 310 130 325 123 340 118
                  C 355 112 370 106 385 102
                  C 400 98 420 94 440 90
                  C 460 88 480 86 500 84
                  L 500 310 L 0 310
                  L 0 196
                  C 0 196 91 198 91 198 Z
                `}
                fill="rgba(27,58,45,0.06)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.5 }}
              />

              {/* ─── FINGER LAKES (subtle) ─── */}
              {[
                { x: 274, y1: 192, y2: 240, label: '' },
                { x: 296, y1: 200, y2: 260, label: '' },
                { x: 318, y1: 185, y2: 262, label: '' },
                { x: 340, y1: 182, y2: 255, label: '' },
              ].map((lake, i) => (
                <motion.line
                  key={i}
                  x1={lake.x}
                  y1={lake.y1}
                  x2={lake.x + (i % 2 ? 2 : -2)}
                  y2={lake.y2}
                  stroke="rgba(138,155,168,0.12)"
                  strokeWidth="2.5"
                  strokeLinecap="round"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1, delay: 1.2 + i * 0.1 }}
                />
              ))}
              <text
                x="306"
                y="275"
                textAnchor="middle"
                className="fill-titanium/10 text-[6px] font-mono tracking-wide"
              >
                FINGER LAKES
              </text>

              {/* ─── COUNTY LINES (subtle) ─── */}
              {[
                { x1: 120, y1: 124, x2: 120, y2: 310 },
                { x1: 155, y1: 120, x2: 155, y2: 310 },
                { x1: 205, y1: 120, x2: 205, y2: 310 },
                { x1: 260, y1: 134, x2: 260, y2: 310 },
                { x1: 330, y1: 120, x2: 330, y2: 310 },
              ].map((line, i) => (
                <motion.line
                  key={i}
                  {...line}
                  stroke="rgba(45,90,69,0.06)"
                  strokeWidth="0.4"
                  initial={{ opacity: 0 }}
                  animate={isInView ? { opacity: 1 } : {}}
                  transition={{ duration: 0.5, delay: 1 + i * 0.05 }}
                />
              ))}

              {/* ─── I-90 / NYS THRUWAY ─── */}
              <motion.path
                d={`
                  M 91 198
                  Q 120 192 150 182
                  Q 170 177 190 178
                  Q 215 180 240 170
                  Q 255 165 270 175
                  Q 290 188 320 192
                  Q 350 190 380 175
                  Q 395 168 405 168
                  L 440 170
                `}
                fill="none"
                stroke="rgba(184,115,51,0.2)"
                strokeWidth="1.5"
                strokeDasharray="6 3"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2, delay: 0.8 }}
              />
              {/* I-90 shield */}
              <rect
                x="178"
                y="188"
                width="22"
                height="11"
                rx="2"
                fill="rgba(184,115,51,0.08)"
                stroke="rgba(184,115,51,0.2)"
                strokeWidth="0.4"
              />
              <text
                x="189"
                y="196"
                textAnchor="middle"
                className="fill-copper/35 text-[6px] font-mono"
              >
                I-90
              </text>

              {/* ─── SERVICE CORRIDOR ─── */}
              <motion.line
                x1={facilities[0].x}
                y1={facilities[0].y}
                x2={facilities[1].x}
                y2={facilities[1].y}
                stroke="#2D5A45"
                strokeWidth="1.5"
                strokeDasharray="5 3"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 1.5 }}
              />
              <text
                x={(facilities[0].x + facilities[1].x) / 2}
                y={(facilities[0].y + facilities[1].y) / 2 - 8}
                textAnchor="middle"
                className="fill-forest-light/35 text-[7px] font-mono"
              >
                ~30 mi
              </text>

              {/* ─── REFERENCE CITIES ─── */}
              {/* Buffalo */}
              <circle cx="91" cy="196" r="2.5" fill="rgba(138,155,168,0.25)" />
              <text
                x="91"
                y="210"
                textAnchor="middle"
                className="fill-titanium/40 text-[8px] font-mono"
              >
                Buffalo
              </text>
              {/* Syracuse */}
              <circle cx="405" cy="168" r="2" fill="rgba(138,155,168,0.2)" />
              <text
                x="405"
                y="158"
                textAnchor="middle"
                className="fill-titanium/30 text-[7px] font-mono"
              >
                Syracuse
              </text>
              {/* Geneva */}
              <circle cx="310" cy="192" r="1.2" fill="rgba(138,155,168,0.15)" />
              <text
                x="310"
                y="204"
                textAnchor="middle"
                className="fill-titanium/20 text-[5.5px] font-mono"
              >
                Geneva
              </text>
              {/* Canandaigua */}
              <circle cx="274" cy="188" r="1.2" fill="rgba(138,155,168,0.15)" />
              <text
                x="274"
                y="200"
                textAnchor="middle"
                className="fill-titanium/20 text-[5.5px] font-mono"
              >
                Canandaigua
              </text>

              {/* ─── FACILITY MARKERS ─── */}
              {facilities.map((f, i) => (
                <motion.g
                  key={f.name}
                  initial={{ opacity: 0, scale: 0 }}
                  animate={isInView ? { opacity: 1, scale: 1 } : {}}
                  transition={{ duration: 0.5, delay: 1.3 + i * 0.3 }}
                >
                  {/* Pulsing ring */}
                  <motion.circle
                    cx={f.x}
                    cy={f.y}
                    r="10"
                    fill="none"
                    stroke="#2D5A45"
                    strokeWidth="0.5"
                    animate={{
                      r: [10, 18, 10],
                      opacity: [0.4, 0, 0.4],
                    }}
                    transition={{
                      repeat: Infinity,
                      duration: 3,
                      delay: i * 1.5,
                    }}
                  />
                  {/* Solid marker */}
                  <circle
                    cx={f.x}
                    cy={f.y}
                    r="5"
                    fill="#2D5A45"
                  />
                  <circle
                    cx={f.x}
                    cy={f.y}
                    r="2.5"
                    fill="#FAFAFA"
                  />
                </motion.g>
              ))}

              {/* ─── FACILITY LABELS ─── */}
              {/* Batavia / UMMC */}
              <text
                x={facilities[0].x}
                y={facilities[0].y - 18}
                textAnchor="middle"
                className="fill-white text-[9px] font-serif"
              >
                Batavia
              </text>
              <text
                x={facilities[0].x}
                y={facilities[0].y - 8}
                textAnchor="middle"
                className="fill-titanium/50 text-[6px] font-mono tracking-wider"
              >
                UMMC
              </text>
              {/* Rochester / St. Mary&apos;s */}
              <text
                x={facilities[1].x}
                y={facilities[1].y - 18}
                textAnchor="middle"
                className="fill-white text-[9px] font-serif"
              >
                Rochester
              </text>
              <text
                x={facilities[1].x}
                y={facilities[1].y - 8}
                textAnchor="middle"
                className="fill-titanium/50 text-[6px] font-mono tracking-wider"
              >
                {"ST. MARY'S"}
              </text>
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
