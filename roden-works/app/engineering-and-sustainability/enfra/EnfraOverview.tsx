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
              An energy services company built around EaaS.
            </h2>
            <div className="space-y-4 text-titanium leading-relaxed">
              <p>
                Founded in 1919 as Bernhard and rebranded in May 2025, ENFRA is a vertically integrated energy services company with 2,600+ employees across 25+ offices in 24 states. Its portfolio includes more than $2 billion in financed projects with $87 million in guaranteed annual utility savings.
              </p>
              <p>
                Under ENFRA&apos;s Energy-as-a-Service model, a hospital pays a predictable operating expense instead of funding plant upgrades as capital. ENFRA designs, builds, finances, operates, and maintains the energy systems under long-term agreements, typically 30 years, and monitors them with its own software, ENFRA Connect®, for real-time data and fault detection.
              </p>
              <p>
                Clients include Ochsner Health (the first not-for-profit EaaS deal, in 2017), Hackensack Meridian ($134M), Beacon Health ($54.2M), and Novant Health ($855M, the largest healthcare EaaS transaction to date). The EaaS business has grown at more than 35% CAGR since 2017.
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
                I manage the Central Energy Plants at two Rochester Regional Health facilities: United Memorial Medical Center (UMMC) in Batavia, NY and St. Mary&apos;s Medical Center in Rochester, NY. The work falls under a $143.8 million, 30-year EaaS partnership announced January 20, 2026.
              </p>
              <p>
                I oversee subcontractors, manage maintenance budgets, and analyze energy data to find savings. My main job is keeping the plants running, since the hospitals depend on them for heating, cooling, sterilization, and power.
              </p>
            </div>

            {/* RRH Context */}
            <div className="mt-8 glass rounded-xl p-6">
              <h3 className="font-serif text-lg text-white mb-3">Rochester Regional Health</h3>
              <ul className="space-y-2 text-sm text-titanium">
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
                  Nine hospitals, 500+ ambulatory facilities, 19,400+ employees
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
                  Second-largest employer in Rochester, NY
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
                  Goal: 100% renewable electricity
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
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
