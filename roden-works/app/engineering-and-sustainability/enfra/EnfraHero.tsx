'use client'

import { motion } from 'framer-motion'
import StatCounter from '@/components/ui/StatCounter'

export default function EnfraHero() {
  return (
    <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
      {/* Animated energy flow background */}
      <div className="absolute inset-0">
        <svg className="absolute inset-0 w-full h-full opacity-[0.05]" viewBox="0 0 1200 600">
          {/* Circuit-like energy paths */}
          {Array.from({ length: 8 }).map((_, i) => (
            <motion.line
              key={i}
              x1={100 + i * 140}
              y1={0}
              x2={100 + i * 140}
              y2={600}
              stroke="#2D5A45"
              strokeWidth="1"
              initial={{ pathLength: 0 }}
              animate={{ pathLength: 1 }}
              transition={{ duration: 2, delay: i * 0.2, ease: 'easeInOut' }}
            />
          ))}
          {Array.from({ length: 5 }).map((_, i) => (
            <motion.line
              key={`h-${i}`}
              x1={0}
              y1={60 + i * 120}
              x2={1200}
              y2={60 + i * 120}
              stroke="#2D5A45"
              strokeWidth="1"
              initial={{ pathLength: 0 }}
              animate={{ pathLength: 1 }}
              transition={{ duration: 2, delay: 0.5 + i * 0.2, ease: 'easeInOut' }}
            />
          ))}
        </svg>
      </div>

      <div className="content-width relative z-10 pb-12 md:pb-16">
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
        >
          <span className="font-mono text-xs tracking-widest uppercase text-forest-light mb-4 block">
            Energy-as-a-Service
          </span>
          <h1 className="font-serif text-display text-white max-w-3xl">
            ENFRA × Rochester Regional Health
          </h1>
          <p className="mt-4 text-titanium text-lg max-w-2xl">
            A $143.8 million, 30-year partnership to optimize hospital energy infrastructure — the systems that produce the steam, chilled water, and electricity hospitals need to function.
          </p>
        </motion.div>

        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.6, delay: 0.4 }}
          className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
        >
          <StatCounter value={143.8} prefix="$" suffix="M" label="Partnership Value" />
          <StatCounter value={354.6} prefix="$" suffix="M" label="30-Year Savings" />
          <StatCounter value={52.5} suffix="%" label="Electricity Reduction" />
          <StatCounter value={6.9} prefix="$" suffix="M" label="Year 1 Savings" />
        </motion.div>
      </div>
    </section>
  )
}
