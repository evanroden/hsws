'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const facilities = [
  {
    name: 'UMMC — Batavia',
    description: '131 beds, 785+ employees. Largest private employer in Genesee County. Sole maternity provider for two counties.',
    x: 48,
    y: 42,
  },
  {
    name: "St. Mary's — Rochester",
    description: 'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
    x: 70,
    y: 38,
  },
]

export default function FacilityMap() {
  const { ref, isInView } = useInView(0.1)

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
          className="glass rounded-xl p-6 md:p-8"
        >
          <div className="relative w-full" style={{ paddingBottom: '65%' }}>
            <svg viewBox="0 0 100 65" className="absolute inset-0 w-full h-full">
              {/* Lake Ontario — fills the top portion */}
              <motion.path
                d="M 20 0 L 100 0 L 100 28 Q 95 32 88 30 Q 80 27 72 29 Q 65 31 58 28 Q 50 25 42 27 Q 35 29 28 26 Q 22 24 20 28 L 20 0 Z"
                fill="rgba(138,155,168,0.08)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1 }}
              />
              <motion.path
                d="M 20 28 Q 22 24 28 26 Q 35 29 42 27 Q 50 25 58 28 Q 65 31 72 29 Q 80 27 88 30 Q 95 32 100 28"
                fill="none"
                stroke="rgba(138,155,168,0.3)"
                strokeWidth="0.4"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.5 }}
              />
              <text x="60" y="16" textAnchor="middle" className="fill-titanium/25 text-[3px] font-mono tracking-widest">
                LAKE ONTARIO
              </text>

              {/* Lake Erie — left edge */}
              <motion.path
                d="M 0 30 Q 2 35 5 40 Q 8 48 10 65"
                fill="none"
                stroke="rgba(138,155,168,0.3)"
                strokeWidth="0.4"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.3 }}
              />
              <motion.path
                d="M 0 30 Q 2 35 5 40 Q 8 48 10 65 L 0 65 L 0 30 Z"
                fill="rgba(138,155,168,0.08)"
                stroke="none"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1 }}
              />
              <text x="3" y="44" className="fill-titanium/20 text-[2px] font-mono" transform="rotate(-75, 3, 44)">
                LAKE ERIE
              </text>

              {/* Niagara River */}
              <motion.path
                d="M 20 28 Q 18 30 17 33 Q 15 36 14 38 Q 12 42 10 45"
                fill="none"
                stroke="rgba(138,155,168,0.2)"
                strokeWidth="0.3"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1, delay: 0.6 }}
              />

              {/* Land area — WNY region */}
              <motion.path
                d="M 10 45 Q 12 42 14 38 Q 15 36 17 33 Q 18 30 20 28 Q 22 24 28 26 Q 35 29 42 27 Q 50 25 58 28 Q 65 31 72 29 Q 80 27 88 30 Q 95 32 100 28 L 100 65 L 10 65 Z"
                fill="rgba(27,58,45,0.08)"
                stroke="rgba(45,90,69,0.15)"
                strokeWidth="0.3"
                initial={{ opacity: 0 }}
                animate={isInView ? { opacity: 1 } : {}}
                transition={{ duration: 1.5, delay: 0.5 }}
              />

              {/* County dividers — subtle grid lines */}
              <motion.line x1="32" y1="27" x2="32" y2="65" stroke="rgba(45,90,69,0.1)" strokeWidth="0.2"
                initial={{ opacity: 0 }} animate={isInView ? { opacity: 1 } : {}} transition={{ duration: 0.5, delay: 1 }} />
              <motion.line x1="45" y1="26" x2="45" y2="65" stroke="rgba(45,90,69,0.1)" strokeWidth="0.2"
                initial={{ opacity: 0 }} animate={isInView ? { opacity: 1 } : {}} transition={{ duration: 0.5, delay: 1.1 }} />
              <motion.line x1="60" y1="28" x2="60" y2="65" stroke="rgba(45,90,69,0.1)" strokeWidth="0.2"
                initial={{ opacity: 0 }} animate={isInView ? { opacity: 1 } : {}} transition={{ duration: 0.5, delay: 1.2 }} />
              <motion.line x1="78" y1="29" x2="78" y2="65" stroke="rgba(45,90,69,0.1)" strokeWidth="0.2"
                initial={{ opacity: 0 }} animate={isInView ? { opacity: 1 } : {}} transition={{ duration: 0.5, delay: 1.3 }} />

              {/* Reference cities — smaller, dimmer */}
              <circle cx="25" cy="38" r="0.6" fill="rgba(138,155,168,0.3)" />
              <text x="25" y="42" textAnchor="middle" className="fill-titanium/30 text-[2px] font-mono">Buffalo</text>

              <circle cx="90" cy="38" r="0.6" fill="rgba(138,155,168,0.3)" />
              <text x="90" y="42" textAnchor="middle" className="fill-titanium/30 text-[2px] font-mono">Syracuse</text>

              {/* I-90 / Thruway — connecting line */}
              <motion.path
                d="M 25 39 Q 38 42 48 41 Q 60 39 70 37 Q 82 36 90 38"
                fill="none"
                stroke="rgba(184,115,51,0.15)"
                strokeWidth="0.3"
                strokeDasharray="1.5 0.8"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1.5, delay: 1 }}
              />
              <text x="50" y="53" textAnchor="middle" className="fill-copper/20 text-[1.8px] font-mono">I-90</text>

              {/* Connection line between facilities */}
              <motion.line
                x1={facilities[0].x}
                y1={facilities[0].y}
                x2={facilities[1].x}
                y2={facilities[1].y}
                stroke="#2D5A45"
                strokeWidth="0.4"
                strokeDasharray="1.5 0.8"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1, delay: 1.5 }}
              />
              <text
                x={(facilities[0].x + facilities[1].x) / 2}
                y={(facilities[0].y + facilities[1].y) / 2 - 2}
                textAnchor="middle"
                className="fill-forest-light/40 text-[1.8px] font-mono"
              >
                ~30 mi
              </text>

              {/* Facility markers */}
              {facilities.map((f, i) => (
                <motion.g
                  key={f.name}
                  initial={{ opacity: 0, scale: 0 }}
                  animate={isInView ? { opacity: 1, scale: 1 } : {}}
                  transition={{ duration: 0.5, delay: 1.2 + i * 0.3 }}
                >
                  <motion.circle
                    cx={f.x}
                    cy={f.y}
                    r="3"
                    fill="none"
                    stroke="#2D5A45"
                    strokeWidth="0.2"
                    animate={{ r: [3, 5, 3], opacity: [0.5, 0, 0.5] }}
                    transition={{ repeat: Infinity, duration: 3, delay: i * 1.5 }}
                  />
                  <circle cx={f.x} cy={f.y} r="1.8" fill="#2D5A45" />
                  <circle cx={f.x} cy={f.y} r="0.9" fill="#FAFAFA" />
                </motion.g>
              ))}

              {/* City labels */}
              <text x={facilities[0].x} y={facilities[0].y - 4} textAnchor="middle" className="fill-white text-[2.8px] font-serif">
                Batavia
              </text>
              <text x={facilities[0].x} y={facilities[0].y - 1.5} textAnchor="middle" className="fill-titanium/50 text-[1.8px] font-mono">
                UMMC
              </text>

              <text x={facilities[1].x} y={facilities[1].y - 4} textAnchor="middle" className="fill-white text-[2.8px] font-serif">
                Rochester
              </text>
              <text x={facilities[1].x} y={facilities[1].y - 1.5} textAnchor="middle" className="fill-titanium/50 text-[1.8px] font-mono">
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
                <h3 className="font-serif text-lg text-white mb-2">{f.name}</h3>
                <p className="text-titanium text-sm leading-relaxed">{f.description}</p>
              </motion.div>
            ))}
          </div>
        </motion.div>
      </div>
    </section>
  )
}
