'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

export default function Bio() {
  const { ref, isInView } = useInView(0.2)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <div className="max-w-3xl">
          <motion.span
            initial={{ opacity: 0, y: 10 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.5 }}
            className="inline-block font-mono text-xs tracking-widest uppercase text-copper mb-6"
          >
            Biography
          </motion.span>

          <div className="space-y-6 text-lg text-titanium leading-relaxed">
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.1 }}
            >
              {/* Fact-check: co-founded with East Aurora High School classmates; proposal presented to Assemblyman DiPietro Sept 2018, who then sponsored an opt-out bill.
                  Source: https://www.tmj4.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors
                  NY lowest donor registration rate (30% vs 55% national, Dec 2018): https://nyulangone.org/news/transplant-institute-study-aims-boost-organ-donation */}
              In 2016, at fifteen, Evan Roden co-founded the Youth Coalition For Organ Donation in East Aurora, New York. The coalition is a registered nonprofit advocacy group, and its opt-out proposal became a bill in the New York State Assembly aimed at New York&apos;s donor registration rate, then the lowest in the nation.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              {/* Fact-check: Tulane awards the B.S.E. in Biomedical Engineering: https://catalog.tulane.edu/science-engineering/biomedical-engineering/biomedical-engineering-major/
                  The black carbon / blood pressure paper (Rabito et al., Indoor Air 2020; data collected 2016) predates Evan's time at Tulane and does not list him:
                  https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/ */}
              At Tulane University, Evan earned a Bachelor of Science in Engineering in Biomedical Engineering and worked in three research labs. On a co-op at the VA, he designed 3D-printed tools that let veterans with double-arm loss place and remove their own dentures. He also studied membrane-active peptides for drug delivery in the Wimley Lab at Tulane School of Medicine and investigated the cardiovascular effects of indoor air pollution in New Orleans. That work built on an earlier Tulane study linking residential black carbon exposure to higher systolic blood pressure.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.3 }}
            >
              At the same time, Evan worked as a cinematographer with Claiborne Avenue Productions under Albert J. Moten, Jr., where he operated BlackMagic 6K and Sony a7s II cameras on productions across New Orleans. He produced digital marketing content for Tulane&apos;s Freeman School of Business, gave a TEDx talk on youth political participation, and walked the runway for Vogue Italy in BizarrAudi&apos;s SchoolTime collection.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.4 }}
            >
              {/* Fact-check: $143.8M, 30-year EaaS partnership: https://enfrasolutions.com/enfra-and-rochester-regional-health-launch-30-year-energy-as-a-service-partnership-to-modernize-system-wide-infrastructure-and-advance-sustainability
                  Convergint title matches the Convergint page (Account Executive, San Francisco). */}
              Since graduating, Evan has worked in three industries. At Odoo, he was an account executive implementing ERP systems for manufacturing, food and beverage, and retail clients, and in one month he hit 160% of his non-recurring revenue goal. At Convergint in San Francisco, he worked on fire and life safety systems as an account executive. He now works at ENFRA, where he manages the central energy plants at two Rochester Regional Health hospitals under a $143.8 million, 30-year Energy-as-a-Service partnership. Those plants produce the steam, chilled water, and electricity the hospitals run on.
            </motion.p>
          </div>
        </div>
      </div>
    </section>
  )
}
