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
              {/* Sources:
                  https://enfrasolutions.com/about ("Established 1919", 3,000+ employees, 29 office locations,
                    EaaS portfolio past $2B in financed projects with $87M in guaranteed annual utility savings)
                  https://enfrasolutions.com/bernhard-rebrands-as-enfra-to-reflect-energy-infrastructure-leadership-and-future-growth-2
                    (Bernhard renamed ENFRA May 1, 2025; operates in 24 states; EaaS grew >35% CAGR since 2017).
                  The Bernhard name dates to 2014-15, when five companies united, so "founded in 1919 as Bernhard" was wrong. */}
              <p>
                ENFRA traces its roots to 1919 and operated as Bernhard until it rebranded in May 2025. It is an energy infrastructure company with 3,000+ employees, 29 offices, and operations in 24 states. Its EaaS portfolio includes more than $2 billion in financed projects with $87 million in guaranteed annual utility savings.
              </p>
              <p>
                Under ENFRA&apos;s Energy-as-a-Service model, a hospital pays a predictable operating expense instead of funding plant upgrades as capital. ENFRA designs, builds, finances, operates, and maintains the energy systems under long-term agreements, typically 30 years, and monitors them with its own software, ENFRA Connect®, for real-time data and fault detection.
              </p>
              {/* Ochsner: https://bernhard.com/?p=7574 (2017, first U.S. healthcare Energy Asset Concession)
                  Hackensack Meridian $134M: https://informedinfrastructure.com/post/bernhard-and-hackensack-meridian-health-forge-a-transformative-30-year-energy-partnership
                  Beacon $54.2M: https://enfrasolutions.com/enfra-and-beacon-health-system-partner-on-30-year-energy-as-a-service-agreement-to-advance-sustainability-and-efficiency-2
                  Novant $855M, "largest EaaS transaction in U.S. healthcare history" (2025): https://enfrasolutions.com/projects/novant-health */}
              <p>
                Clients include Ochsner Health (in 2017, the first U.S. healthcare Energy Asset Concession), Hackensack Meridian ($134M), Beacon Health ($54.2M), and Novant Health ($855M, which ENFRA called the largest EaaS transaction in U.S. healthcare history when it was announced in 2025). The EaaS business has grown at more than 35% CAGR since 2017.
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
                {/* RRH names the Rochester site "St. Mary's Medical Campus": https://rochesterregional.org/locations/medical-campuses/st-marys */}
                I manage the Central Energy Plants at two Rochester Regional Health facilities: United Memorial Medical Center (UMMC) in Batavia, NY and St. Mary&apos;s Medical Campus in Rochester, NY. The work falls under a $143.8 million, 30-year EaaS partnership, announced January 20, 2026, that covers all nine RRH hospital locations.
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
                  {/* https://www.rochesterregional.org/about/facts-and-statistics (9 hospital locations,
                      557 practice locations, 19.4K+ employees, second-largest employer in Rochester) */}
                  Nine hospitals, 557 practice locations, 19,400+ employees
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
                  Second-largest employer in Rochester, NY
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
                  {/* https://rochesterregional.org/hub/solar-energy (100% of electricity use by 2025) */}
                  Set a goal of sourcing 100% of its electricity from renewables by 2025
                </li>
                <li className="flex items-start gap-2">
                  <span className="text-verdigris mt-1">•</span>
                  {/* https://greensparksolar.com/2019/04/24/rochester-regional-health/ (5.48 MW, Parma, NY;
                      "second-largest single-site solar farm in New York State" at activation, 2019) */}
                  A 5.48 MW solar farm in Parma, NY, the second-largest single-site solar farm in New York State when it came online in 2019
                </li>
              </ul>
            </div>
          </motion.div>
        </div>
      </div>
    </section>
  )
}
