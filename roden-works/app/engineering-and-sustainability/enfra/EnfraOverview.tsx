'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

export default function EnfraOverview() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
          {/* About ENFRA */}
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              About ENFRA
            </span>
            <h2 className="font-serif text-heading text-white mb-6">
              The largest privately-owned EaaS company in the United States.
            </h2>
            <div className="space-y-4 text-titanium leading-relaxed">
              <p>
                Founded in 1919 as Bernhard and rebranded in May 2025, ENFRA is a vertically integrated energy services company with 2,600+ employees across 25+ offices in 24 states. Their portfolio exceeds $2 billion in financed projects delivering $87 million in guaranteed annual utility savings.
              </p>
              <p>
                ENFRA&apos;s Energy-as-a-Service model converts hospital capital expenditure into predictable operating expense. They design, build, finance, operate, and maintain energy systems under long-term agreements — typically 30 years — using proprietary software called ENFRA Connect® for real-time monitoring and fault detection.
              </p>
              <p>
                Key partnerships include Ochsner Health (the first-ever not-for-profit EaaS in 2017), Hackensack Meridian ($134M), Beacon Health ($54.2M), and Novant Health ($855M — the largest healthcare EaaS transaction ever). The EaaS business has grown at 35%+ CAGR since 2017.
              </p>
            </div>
          </motion.div>

          {/* Evan's Role */}
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.2 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Role
            </span>
            <h2 className="font-serif text-heading text-white mb-6">
              Sustainability Engineer II / Asset Manager
            </h2>
            <div className="space-y-4 text-titanium leading-relaxed">
              <p>
                Evan manages Central Energy Plants at two Rochester Regional Health facilities: United Memorial Medical Center (UMMC) in Batavia, NY and St. Mary&apos;s Medical Center in Rochester, NY — as part of a $143.8 million, 30-year EaaS partnership announced January 20, 2026.
              </p>
              <p>
                His responsibilities include overseeing subcontractors, managing maintenance budgets, performing energy data analysis for optimization, and ensuring the continuous operation of the infrastructure that hospitals depend on for heating, cooling, sterilization, and power.
              </p>
            </div>

            {/* RRH Context */}
            <div className="mt-8 glass rounded-xl p-6">
              <h3 className="font-serif text-lg text-white mb-3">Rochester Regional Health</h3>
              <ul className="space-y-2 text-sm text-titanium">
                <li className="flex items-start gap-2">
                  <span className="text-forest-light mt-1">•</span>
                  Nine hospitals, 500+ ambulatory facilities, 19,400+ employees
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-forest-light mt-1">•</span>
                  Second-largest employer in Rochester, NY
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-forest-light mt-1">•</span>
                  Goal: 100% renewable electricity — one of healthcare&apos;s most aggressive sustainability targets
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-forest-light mt-1">•</span>
                  Second-largest solar project in New York State (5.5 MW)
                </li>
              </ul>
            </div>
          </motion.div>
        </div>
      </div>
    </section>
  )
}
