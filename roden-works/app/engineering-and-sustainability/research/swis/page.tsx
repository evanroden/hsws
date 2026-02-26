'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

export default function SWISPage() {
  const { ref, isInView } = useInView(0.1)
  const [flowRate, setFlowRate] = useState(300)

  const wedgePosition = Math.max(0, Math.min(100, ((300 - flowRate) / 200) * 100))

  return (
    <>
      <Breadcrumbs items={[{ label: 'Engineering', href: '/engineering-and-sustainability' }, { label: 'Research', href: '/engineering-and-sustainability/research' }, { label: 'SWIS' }]} />

      <section className="relative min-h-[50vh] flex items-end bg-slate-950">
        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div initial={{ opacity: 0, y: 30 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.7 }}>
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">Environmental Research</span>
            <h1 className="font-serif text-display text-white max-w-4xl">Saltwater Intrusion Study</h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">A first-of-kind longitudinal study proposal on saltwater intrusion into the Greater New Orleans water supply and its health impacts.</p>
          </motion.div>
        </div>
      </section>

      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="max-w-3xl mb-16">
            <h2 className="font-serif text-heading text-white mb-6">The 2023 Crisis</h2>
            <div className="space-y-4 text-titanium leading-relaxed">
              <p>In 2023, the Mississippi River&apos;s flow dropped below critical thresholds — reaching 130,000–150,000 cubic feet per second when the safe threshold is approximately 300,000 cfs. As river flow decreased, a saltwater wedge from the Gulf of Mexico advanced upstream, threatening the drinking water supply for 1.2 million residents of Greater New Orleans.</p>
              <p>This proposal outlines a first-of-kind longitudinal study examining the health impacts of saltwater intrusion on municipal water systems — an increasingly urgent research need as climate change intensifies drought conditions along the Mississippi.</p>
            </div>
          </div>

          {/* Saltwater Wedge Simulator */}
          <div ref={ref}>
            <motion.div initial={{ opacity: 0, y: 30 }} animate={isInView ? { opacity: 1, y: 0 } : {}} transition={{ duration: 0.6 }} className="mb-8">
              <span className="font-mono text-xs tracking-widest uppercase text-copper">Interactive Simulation</span>
              <h2 className="font-serif text-heading text-white mt-3">Saltwater wedge dynamics.</h2>
              <p className="text-titanium mt-2">Drag the slider to simulate different river flow rates and observe how the saltwater wedge advances upstream.</p>
            </motion.div>

            <motion.div initial={{ opacity: 0 }} animate={isInView ? { opacity: 1 } : {}} transition={{ delay: 0.3 }} className="glass rounded-xl p-6 md:p-8">
              <div className="relative w-full" style={{ paddingBottom: '35%' }}>
                <svg viewBox="0 0 200 70" className="absolute inset-0 w-full h-full">
                  {/* River bed */}
                  <rect x="0" y="45" width="200" height="25" fill="rgba(138,155,168,0.1)" />
                  <text x="100" y="60" textAnchor="middle" className="fill-titanium/20 text-[4px] font-mono">RIVER BED</text>

                  {/* Fresh water */}
                  <rect x="0" y="10" width="200" height="35" fill="rgba(45,90,69,0.15)" />

                  {/* Saltwater wedge */}
                  <motion.path
                    d={`M ${200} 45 L ${200} 20 Q ${200 - wedgePosition * 1.5} 30 ${200 - wedgePosition * 2} 45 Z`}
                    fill="rgba(184,115,51,0.2)"
                    stroke="#B87333"
                    strokeWidth="0.5"
                    animate={{ d: `M 200 45 L 200 20 Q ${200 - wedgePosition * 1.5} 30 ${200 - wedgePosition * 2} 45 Z` }}
                    transition={{ duration: 0.3 }}
                  />

                  {/* Labels */}
                  <text x="30" y="30" className="fill-forest-light text-[4px] font-mono">FRESHWATER</text>
                  <text x={Math.max(140, 200 - wedgePosition * 1.2)} y="38" className="fill-copper text-[3.5px] font-mono">SALT WEDGE</text>

                  {/* Flow direction */}
                  <motion.line x1="10" y1="25" x2="50" y2="25" stroke="#2D5A45" strokeWidth="0.5" markerEnd="url(#arrow)" />
                  <defs><marker id="arrow" viewBox="0 0 10 10" refX="5" refY="5" markerWidth="4" markerHeight="4" orient="auto-start-reverse"><path d="M 0 0 L 10 5 L 0 10 z" fill="#2D5A45" /></marker></defs>
                  <text x="30" y="22" className="fill-forest-light/60 text-[3px] font-mono">RIVER FLOW →</text>

                  {/* New Orleans marker */}
                  <circle cx="60" cy="8" r="2" fill="#FAFAFA" />
                  <text x="60" y="6" textAnchor="middle" className="fill-white text-[3px] font-serif">New Orleans</text>
                </svg>
              </div>

              {/* Slider */}
              <div className="mt-6 pt-6 border-t border-white/5">
                <div className="flex items-center justify-between mb-3">
                  <span className="text-sm text-titanium">River Flow Rate</span>
                  <span className={`font-mono text-sm ${flowRate < 200 ? 'text-copper' : 'text-forest-light'}`}>
                    {flowRate.toLocaleString()} kcfs
                  </span>
                </div>
                <input
                  type="range"
                  min="100"
                  max="400"
                  value={flowRate}
                  onChange={(e) => setFlowRate(Number(e.target.value))}
                  className="w-full accent-copper"
                />
                <div className="flex justify-between text-xs text-titanium/40 font-mono mt-1">
                  <span>100 kcfs (Crisis)</span>
                  <span>300 kcfs (Safe threshold)</span>
                  <span>400 kcfs</span>
                </div>
                {flowRate < 200 && (
                  <motion.p initial={{ opacity: 0 }} animate={{ opacity: 1 }} className="mt-3 text-copper text-sm">
                    At this flow rate, the saltwater wedge threatens the drinking water intake for 1.2 million residents.
                  </motion.p>
                )}
              </div>
            </motion.div>
          </div>
        </div>
      </section>
    </>
  )
}
