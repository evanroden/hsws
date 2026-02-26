'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const facilities = [
  {
    name: 'UMMC — Batavia',
    description: '131 beds, 785+ employees. Largest private employer in Genesee County. Sole maternity provider for two counties.',
    x: 35,
    y: 42,
  },
  {
    name: "St. Mary's — Rochester",
    description: 'Opened 1857. 13,000+ annual dialysis treatments. Behavioral health, homeless healthcare, and senior housing.',
    x: 55,
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
          {/* Stylized map */}
          <div className="relative w-full" style={{ paddingBottom: '50%' }}>
            <svg viewBox="0 0 100 50" className="absolute inset-0 w-full h-full">
              {/* Simplified WNY region outline */}
              <motion.path
                d="M 10 10 Q 30 5 50 8 Q 70 5 90 12 L 90 45 Q 70 48 50 45 Q 30 48 10 42 Z"
                fill="rgba(27,58,45,0.1)"
                stroke="rgba(45,90,69,0.3)"
                strokeWidth="0.3"
                initial={{ pathLength: 0, opacity: 0 }}
                animate={isInView ? { pathLength: 1, opacity: 1 } : {}}
                transition={{ duration: 1.5 }}
              />

              {/* Lake Ontario */}
              <motion.path
                d="M 25 5 Q 50 2 75 5"
                fill="none"
                stroke="rgba(138,155,168,0.2)"
                strokeWidth="0.3"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1, delay: 0.5 }}
              />
              <text x="50" y="4" textAnchor="middle" className="fill-titanium/20 text-[2px] font-mono">
                Lake Ontario
              </text>

              {/* Connection line between facilities */}
              <motion.line
                x1={facilities[0].x}
                y1={facilities[0].y}
                x2={facilities[1].x}
                y2={facilities[1].y}
                stroke="#2D5A45"
                strokeWidth="0.3"
                strokeDasharray="1 0.5"
                initial={{ pathLength: 0 }}
                animate={isInView ? { pathLength: 1 } : {}}
                transition={{ duration: 1, delay: 1.2 }}
              />

              {/* Facility markers */}
              {facilities.map((f, i) => (
                <motion.g
                  key={f.name}
                  initial={{ opacity: 0, scale: 0 }}
                  animate={isInView ? { opacity: 1, scale: 1 } : {}}
                  transition={{ duration: 0.5, delay: 0.8 + i * 0.3 }}
                >
                  {/* Pulse ring */}
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
                  <circle cx={f.x} cy={f.y} r="1.5" fill="#2D5A45" />
                  <circle cx={f.x} cy={f.y} r="0.8" fill="#FAFAFA" />
                </motion.g>
              ))}

              {/* City labels */}
              <text x={facilities[0].x} y={facilities[0].y + 5} textAnchor="middle" className="fill-white text-[2.5px] font-serif">
                Batavia
              </text>
              <text x={facilities[1].x} y={facilities[1].y + 5} textAnchor="middle" className="fill-white text-[2.5px] font-serif">
                Rochester
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
                transition={{ duration: 0.5, delay: 1.5 + i * 0.2 }}
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
