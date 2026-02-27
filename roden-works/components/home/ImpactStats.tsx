'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import StatCounter from '@/components/ui/StatCounter'
import { IMPACT_STATS } from '@/lib/constants'

export default function ImpactStats() {
  const { ref, isInView } = useInView(0.2)

  return (
    <section className="relative py-16 md:py-24 overflow-hidden" ref={ref}>
      {/* Background gradient */}
      <div className="absolute inset-0 bg-gradient-to-r from-forest/10 via-slate-950 to-forest/10" />

      <div className="content-width relative z-10">
        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.6 }}
          className="text-center mb-10"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            By the Numbers
          </span>
        </motion.div>

        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.8, delay: 0.2 }}
          className="grid grid-cols-2 sm:grid-cols-3 md:grid-cols-5 gap-4 md:gap-0 md:divide-x divide-white/10"
        >
          {IMPACT_STATS.map((stat) => (
            <StatCounter
              key={stat.label}
              value={stat.value}
              prefix={stat.prefix}
              suffix={stat.suffix}
              label={stat.label}
            />
          ))}
        </motion.div>
      </div>
    </section>
  )
}
