'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'

const annualData = [
  { year: 'Year 1', before: 14.2, after: 7.3, savings: 6.9 },
  { year: 'Year 5', before: 15.8, after: 7.6, savings: 8.2 },
  { year: 'Year 10', before: 18.1, after: 8.2, savings: 9.9 },
  { year: 'Year 15', before: 20.8, after: 8.9, savings: 11.9 },
  { year: 'Year 20', before: 23.9, after: 9.7, savings: 14.2 },
  { year: 'Year 25', before: 27.4, after: 10.6, savings: 16.8 },
  { year: 'Year 30', before: 31.5, after: 11.6, savings: 19.9 },
]

export default function SavingsVisualization() {
  const { ref, isInView } = useInView(0.1)
  const [view, setView] = useState<'annual' | 'cumulative'>('annual')

  const maxValue = view === 'annual' ? 35 : 400
  let cumBefore = 0
  let cumAfter = 0

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="flex flex-col md:flex-row md:items-end justify-between gap-4 mb-12"
        >
          <div>
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Energy Savings
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              {view === 'annual' ? 'Annual energy cost comparison.' : '30-year cumulative projection.'}
            </h2>
          </div>
          <div className="flex gap-2">
            <button
              onClick={() => setView('annual')}
              className={`px-4 py-2 rounded-lg text-sm font-mono transition-all ${
                view === 'annual'
                  ? 'bg-forest-light text-white'
                  : 'bg-white/5 text-titanium hover:bg-white/10'
              }`}
            >
              Annual
            </button>
            <button
              onClick={() => setView('cumulative')}
              className={`px-4 py-2 rounded-lg text-sm font-mono transition-all ${
                view === 'cumulative'
                  ? 'bg-forest-light text-white'
                  : 'bg-white/5 text-titanium hover:bg-white/10'
              }`}
            >
              30-Year Projection
            </button>
          </div>
        </motion.div>

        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.8, delay: 0.3 }}
          className="glass rounded-xl p-6 md:p-8"
        >
          {/* Chart */}
          <div className="space-y-6">
            {annualData.map((item, i) => {
              cumBefore += item.before
              cumAfter += item.after
              const beforeVal = view === 'annual' ? item.before : cumBefore
              const afterVal = view === 'annual' ? item.after : cumAfter

              return (
                <div key={item.year} className="group">
                  <div className="flex items-center gap-4 mb-2">
                    <span className="font-mono text-xs text-titanium w-16">{item.year}</span>
                    <div className="flex-1 space-y-1.5">
                      {/* Before */}
                      <div className="flex items-center gap-3">
                        <div
                          className="h-4 bg-white/5 rounded-full flex-1 overflow-hidden"
                          role="progressbar"
                          aria-valuenow={beforeVal}
                          aria-valuemin={0}
                          aria-valuemax={maxValue}
                          aria-label={`${item.year} without ENFRA: $${beforeVal.toFixed(1)}M`}
                        >
                          <motion.div
                            initial={{ width: 0 }}
                            animate={isInView ? { width: `${(beforeVal / maxValue) * 100}%` } : {}}
                            transition={{ duration: 0.8, delay: 0.5 + i * 0.1 }}
                            className="h-full bg-titanium/30 rounded-full"
                          />
                        </div>
                        <span className="font-mono text-xs text-titanium/60 w-14 text-right">
                          ${beforeVal.toFixed(1)}M
                        </span>
                      </div>
                      {/* After */}
                      <div className="flex items-center gap-3">
                        <div
                          className="h-4 bg-white/5 rounded-full flex-1 overflow-hidden"
                          role="progressbar"
                          aria-valuenow={afterVal}
                          aria-valuemin={0}
                          aria-valuemax={maxValue}
                          aria-label={`${item.year} with ENFRA: $${afterVal.toFixed(1)}M`}
                        >
                          <motion.div
                            initial={{ width: 0 }}
                            animate={isInView ? { width: `${(afterVal / maxValue) * 100}%` } : {}}
                            transition={{ duration: 0.8, delay: 0.6 + i * 0.1 }}
                            className="h-full bg-gradient-to-r from-forest to-forest-light rounded-full"
                          />
                        </div>
                        <span className="font-mono text-xs text-forest-light w-14 text-right">
                          ${afterVal.toFixed(1)}M
                        </span>
                      </div>
                    </div>
                  </div>
                </div>
              )
            })}
          </div>

          {/* Legend */}
          <div className="mt-8 pt-6 border-t border-white/5 flex flex-wrap gap-6">
            <div className="flex items-center gap-2">
              <div className="w-3 h-3 rounded-full bg-titanium/30" />
              <span className="text-xs text-titanium">Without ENFRA</span>
            </div>
            <div className="flex items-center gap-2">
              <div className="w-3 h-3 rounded-full bg-forest-light" />
              <span className="text-xs text-titanium">With ENFRA optimization</span>
            </div>
            <div className="ml-auto">
              <span className="text-xs text-titanium/40 font-mono">Illustrative data based on announced partnership figures</span>
            </div>
          </div>
        </motion.div>
      </div>
    </section>
  )
}
