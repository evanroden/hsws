'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const facilities = [
  {
    name: 'UMMC — Batavia',
    label: 'Batavia',
    sub: 'UMMC',
    description:
      '131 beds, 785+ employees. Largest private employer in Genesee County. Sole maternity provider for two counties.',
    x: 225,
    y: 155,
  },
  {
    name: "St. Mary's — Rochester",
    label: 'Rochester',
    sub: "ST. MARY'S",
    description:
      'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
    x: 385,
    y: 132,
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
          <div className="relative w-full" style={{ paddingBottom: '43%' }}>
            <svg
              viewBox="0 0 700 300"
              className="absolute inset-0 w-full h-full"
              fill="none"
            >
              {/* ─── LAKE ONTARIO ─── */}
              <motion.path
                d={`
                  M 0 0 L 700 0 L 700 52
                  C 660 54 620 57 580 61
                  C 540 66 500 72 460 78
                  C 420 84 390 90 360 95
                  C 330 100 300 103 270 104
                  C 240 105 210 104 180 101
                  C 155 98 130 94 105 90
                  C 80 86 55 80 30 74
                  L 0 68 Z
                `}
                fill="rgba(138,155,168,0.04)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.2 }}
              />
              {/* Shoreline */}
              <motion.path
                d={`
                  M 0 68
                  C 30 74 55 80 105 90
                  C 130 94 155 98 180 101
                  C 210 104 240 105 270 104
                  C 300 103 330 100 360 95
                  C 390 90 420 84 460 78
                  C 500 72 540 66 580 61
                  C 620 57 660 54 700 52
                `}
                fill="none"
                stroke="rgba(138,155,168,0.3)"
                strokeWidth="1"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2 }}
              />
              <text
                x="400"
                y="40"
                textAnchor="middle"
                className="fill-titanium/12 text-[11px] font-mono tracking-[0.4em]"
              >
                LAKE ONTARIO
              </text>

              {/* ─── LAKE ERIE (hint) ─── */}
              <motion.path
                d={`
                  M 58 170
                  C 52 190 42 215 30 240
                  C 20 258 10 275 0 290
                  L 0 170 Z
                `}
                fill="rgba(138,155,168,0.03)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1 }}
              />
              <motion.path
                d={`
                  M 58 170
                  C 52 190 42 215 30 240
                  C 20 258 10 275 0 290
                `}
                fill="none"
                stroke="rgba(138,155,168,0.2)"
                strokeWidth="0.8"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 0.3 }}
              />
              <text
                x="18"
                y="230"
                className="fill-titanium/10 text-[8px] font-mono tracking-[0.15em]"
                transform="rotate(-62, 18, 230)"
              >
                LAKE ERIE
              </text>

              {/* ─── NIAGARA RIVER ─── */}
              <motion.path
                d="M 42 90 C 40 110 42 135 48 150 C 52 160 56 168 58 170"
                fill="none"
                stroke="rgba(138,155,168,0.15)"
                strokeWidth="1.2"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1, delay: 0.5 }}
              />

              {/* ─── LAND FILL ─── */}
              <motion.path
                d={`
                  M 0 68
                  C 30 74 55 80 105 90
                  C 130 94 155 98 180 101
                  C 210 104 240 105 270 104
                  C 300 103 330 100 360 95
                  C 390 90 420 84 460 78
                  C 500 72 540 66 580 61
                  C 620 57 660 54 700 52
                  L 700 300 L 0 300 Z
                `}
                fill="rgba(27,58,45,0.02)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.5 }}
              />

              {/* ─── FINGER LAKES (subtle wisps) ─── */}
              {[
                { x: 450, y1: 148, y2: 220 },
                { x: 468, y1: 155, y2: 235 },
                { x: 486, y1: 142, y2: 232 },
                { x: 504, y1: 138, y2: 225 },
              ].map((lake, i) => (
                <motion.line
                  key={i}
                  x1={lake.x}
                  y1={lake.y1}
                  x2={lake.x + (i % 2 ? 2 : -2)}
                  y2={lake.y2}
                  stroke="rgba(138,155,168,0.06)"
                  strokeWidth="1.2"
                  strokeLinecap="round"
                  initial={{ pathLength: 0 }}
                  animate={isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 0.8, delay: 1.2 + i * 0.1 }}
                />
              ))}
              <text
                x="477"
                y="248"
                textAnchor="middle"
                className="fill-titanium/8 text-[7px] font-mono tracking-wide"
              >
                FINGER LAKES
              </text>

              {/* ─── I-90 / NYS THRUWAY ─── */}
              <motion.path
                d={`
                  M 80 168
                  C 120 160 170 152 225 148
                  C 280 144 330 136 385 132
                  C 440 130 500 135 560 140
                  C 590 142 610 143 630 143
                `}
                fill="none"
                stroke="rgba(184,115,51,0.18)"
                strokeWidth="1.5"
                strokeDasharray="8 4"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2, delay: 0.8 }}
              />
              {/* I-90 shield */}
              <rect
                x="138"
                y="158"
                width="28"
                height="14"
                rx="3"
                fill="rgba(184,115,51,0.06)"
                stroke="rgba(184,115,51,0.18)"
                strokeWidth="0.5"
              />
              <text
                x="152"
                y="168"
                textAnchor="middle"
                className="fill-copper/30 text-[7px] font-mono"
              >
                I-90
              </text>

              {/* ─── SERVICE CORRIDOR (hero connection) ─── */}
              <motion.line
                x1={facilities[0].x}
                y1={facilities[0].y}
                x2={facilities[1].x}
                y2={facilities[1].y}
                stroke="#2D5A45"
                strokeWidth="2"
                strokeDasharray="6 4"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.2, delay: 1.5 }}
              />
              <motion.g
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 0.5, delay: 2 }}
              >
                <rect
                  x={(facilities[0].x + facilities[1].x) / 2 - 22}
                  y={(facilities[0].y + facilities[1].y) / 2 - 18}
                  width="44"
                  height="16"
                  rx="8"
                  fill="rgba(45,90,69,0.12)"
                  stroke="rgba(45,90,69,0.25)"
                  strokeWidth="0.5"
                />
                <text
                  x={(facilities[0].x + facilities[1].x) / 2}
                  y={(facilities[0].y + facilities[1].y) / 2 - 7}
                  textAnchor="middle"
                  className="fill-forest-light/50 text-[8px] font-mono"
                >
                  ~30 mi
                </text>
              </motion.g>

              {/* ─── REFERENCE CITIES ─── */}
              {/* Buffalo */}
              <circle
                cx="80"
                cy="168"
                r="3"
                fill="rgba(138,155,168,0.3)"
              />
              <text
                x="80"
                y="186"
                textAnchor="middle"
                className="fill-titanium/45 text-[10px] font-mono"
              >
                Buffalo
              </text>

              {/* Syracuse */}
              <circle
                cx="630"
                cy="143"
                r="2.5"
                fill="rgba(138,155,168,0.2)"
              />
              <text
                x="630"
                y="135"
                textAnchor="middle"
                className="fill-titanium/30 text-[9px] font-mono"
              >
                Syracuse
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
                    r="14"
                    fill="none"
                    stroke="#2D5A45"
                    strokeWidth="0.6"
                    animate={{
                      r: [14, 26, 14],
                      opacity: [0.35, 0, 0.35],
                    }}
                    transition={{
                      repeat: Infinity,
                      duration: 3,
                      delay: i * 1.5,
                    }}
                  />
                  {/* Outer glow */}
                  <circle
                    cx={f.x}
                    cy={f.y}
                    r="10"
                    fill="rgba(45,90,69,0.1)"
                    stroke="#2D5A45"
                    strokeWidth="0.8"
                  />
                  {/* Solid marker */}
                  <circle cx={f.x} cy={f.y} r="6" fill="#2D5A45" />
                  <circle cx={f.x} cy={f.y} r="2.5" fill="#FAFAFA" />
                </motion.g>
              ))}

              {/* ─── FACILITY LABELS ─── */}
              {facilities.map((f, i) => (
                <motion.g
                  key={`label-${f.name}`}
                  initial={{ opacity: 0, y: 5 }}
                  animate={isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.5, delay: 1.6 + i * 0.3 }}
                >
                  <text
                    x={f.x}
                    y={f.y - 26}
                    textAnchor="middle"
                    className="fill-white text-[13px] font-serif"
                  >
                    {f.label}
                  </text>
                  <text
                    x={f.x}
                    y={f.y - 14}
                    textAnchor="middle"
                    className="fill-titanium/55 text-[7px] font-mono tracking-[0.15em]"
                  >
                    {f.sub}
                  </text>
                </motion.g>
              ))}
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
