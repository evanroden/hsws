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
              At seventeen, Evan Roden co-founded the Youth Coalition For Organ Donation in East Aurora, New York — a 501(c)(4) nonpartisan lobbying organization that would go on to shape legislation addressing the state&apos;s lowest-in-nation donor registration rate. That early instinct — to identify a system failure, understand its root causes, and build something to fix it — has defined every chapter of his career since.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              At Tulane University, Evan earned his Bachelor of Engineering in Biomedical/Medical Engineering. But the transcript only tells part of the story. Across three research labs, he designed 3D-printed prosthetic devices for veterans at the VA, studied membrane protein structures for next-generation drug delivery in the Wimley Lab at Tulane School of Medicine, and investigated the cardiovascular effects of indoor air pollution in New Orleans — work that contributed to published findings linking black carbon exposure to elevated blood pressure.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.3 }}
            >
              Simultaneously, Evan built a parallel career in visual storytelling. As a cinematographer with Claiborne Avenue Productions under Albert J. Moten, Jr., he operated BlackMagic 6K and Sony a7s II cameras on productions across New Orleans. He produced digital marketing content for Tulane&apos;s Freeman School of Business, gave a TEDx talk on youth political participation, and walked the runway for Vogue Italy in BizarrAudi&apos;s SchoolTime collection.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.4 }}
            >
              After graduation, Evan pivoted through three distinct industries in rapid succession — each one deepening his understanding of complex systems. At Odoo, he worked as an account executive implementing ERP systems for manufacturing, food and beverage, and retail clients — hitting 160% of his non-recurring revenue goal in a single month. At Convergint, he consulted on fire and life safety systems as a systems integration specialist in San Francisco. And now, at ENFRA, he manages central energy plants for Rochester Regional Health as part of a $143.8 million, 30-year Energy-as-a-Service partnership — the infrastructure that produces the steam, chilled water, and electricity that keeps hospitals operational.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.5 }}
              className="text-white"
            >
              The through-line is systems. Whether biological, mechanical, legislative, or narrative — Evan sees the architecture beneath the surface and works to make it better.
            </motion.p>
          </div>
        </div>
      </div>
    </section>
  )
}
