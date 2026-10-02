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
              At seventeen, Evan Roden co-founded the Youth Coalition For Organ Donation in East Aurora, New York. The coalition is a 501(c)(4) nonpartisan lobbying organization, and it went on to shape legislation addressing New York&apos;s donor registration rate, the lowest in the nation.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              At Tulane University, Evan earned a Bachelor of Engineering in Biomedical/Medical Engineering and worked in three research labs. He designed 3D-printed prosthetic devices for veterans at the VA, studied membrane protein structures for drug delivery in the Wimley Lab at Tulane School of Medicine, and investigated the cardiovascular effects of indoor air pollution in New Orleans. The air pollution work contributed to published findings linking black carbon exposure to elevated blood pressure.
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
              Since graduating, Evan has worked in three industries. At Odoo, he was an account executive implementing ERP systems for manufacturing, food and beverage, and retail clients, and in one month he hit 160% of his non-recurring revenue goal. At Convergint in San Francisco, he consulted on fire and life safety systems as a systems integration specialist. He now works at ENFRA, where he manages the central energy plants for Rochester Regional Health under a $143.8 million, 30-year Energy-as-a-Service partnership. Those plants produce the steam, chilled water, and electricity the hospitals run on.
            </motion.p>
          </div>
        </div>
      </div>
    </section>
  )
}
