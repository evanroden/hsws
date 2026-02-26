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
    x: 230,
    y: 150,
  },
  {
    name: "St. Mary's — Rochester",
    label: 'Rochester',
    sub: "ST. MARY'S",
    description:
      'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
    x: 395,
    y: 128,
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
          <div className="relative w-full" style={{ paddingBottom: '30%' }}>
            <svg
              viewBox="0 10 700 210"
              className="absolute inset-0 w-full h-full"
              fill="none"
            >
              {/* ─── LAKE ONTARIO ─── */}
              {/* Fill */}
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
                fill="rgba(138,155,168,0.12)"
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
                stroke="rgba(138,155,168,0.5)"
                strokeWidth="1.5"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2 }}
              />
              <text
                x="500"
                y="38"
                textAnchor="middle"
                className="fill-titanium/25 text-[12px] font-mono tracking-[0.4em]"
              >
                LAKE ONTARIO
              </text>

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
                fill="rgba(27,58,45,0.04)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.5 }}
              />

              {/* ─── I-90 / NYS THRUWAY ─── */}
              <motion.path
                d={`
                  M 80 162
                  C 130 156 180 150 230 146
                  C 280 142 340 134 395 128
                  C 450 125 510 130 570 136
                  C 600 138 620 140 640 140
                `}
                fill="none"
                stroke="rgba(184,115,51,0.35)"
                strokeWidth="2"
                strokeDasharray="8 4"
                strokeLinecap="round"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 2, delay: 0.8 }}
              />
              {/* I-90 shield */}
              <rect
                x="140"
                y="152"
                width="32"
                height="16"
                rx="3"
                fill="rgba(184,115,51,0.08)"
                stroke="rgba(184,115,51,0.3)"
                strokeWidth="0.6"
              />
              <text
                x="156"
                y="163"
                textAnchor="middle"
                className="fill-copper/45 text-[8px] font-mono font-medium"
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
                  x={(facilities[0].x + facilities[1].x) / 2 - 26}
                  y={(facilities[0].y + facilities[1].y) / 2 - 20}
                  width="52"
                  height="18"
                  rx="9"
                  fill="rgba(45,90,69,0.15)"
                  stroke="rgba(45,90,69,0.35)"
                  strokeWidth="0.6"
                />
                <text
                  x={(facilities[0].x + facilities[1].x) / 2}
                  y={(facilities[0].y + facilities[1].y) / 2 - 8}
                  textAnchor="middle"
                  className="fill-forest-light/60 text-[9px] font-mono"
                >
                  ~30 mi
                </text>
              </motion.g>

              {/* ─── REFERENCE CITIES ─── */}
              {/* Buffalo */}
              <circle
                cx="80"
                cy="162"
                r="4"
                fill="rgba(138,155,168,0.35)"
              />
              <text
                x="80"
                y="180"
                textAnchor="middle"
                className="fill-titanium/60 text-[11px] font-mono"
              >
                Buffalo
              </text>

              {/* Syracuse */}
              <circle
                cx="640"
                cy="140"
                r="3.5"
                fill="rgba(138,155,168,0.25)"
              />
              <text
                x="640"
                y="132"
                textAnchor="middle"
                className="fill-titanium/50 text-[10px] font-mono"
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
                    r="16"
                    fill="none"
                    stroke="#2D5A45"
                    strokeWidth="0.8"
                    animate={{
                      r: [16, 30, 16],
                      opacity: [0.4, 0, 0.4],
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
                    r="12"
                    fill="rgba(45,90,69,0.12)"
                    stroke="#2D5A45"
                    strokeWidth="1"
                  />
                  {/* Solid marker */}
                  <circle cx={f.x} cy={f.y} r="7" fill="#2D5A45" />
                  <circle cx={f.x} cy={f.y} r="3" fill="#FAFAFA" />
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
                    y={f.y - 28}
                    textAnchor="middle"
                    className="fill-white text-[15px] font-serif"
                  >
                    {f.label}
                  </text>
                  <text
                    x={f.x}
                    y={f.y - 16}
                    textAnchor="middle"
                    className="fill-titanium/65 text-[8px] font-mono tracking-[0.15em]"
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
