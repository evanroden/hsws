'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const certifications = [
  'TRIZ Associate — Systematic innovation methodology from Altshuller\'s 40 inventive principles',
  'Odoo Certification — Enterprise resource planning implementation',
  'Biomedical Responsible Conduct of Research Course',
  'Conflict of Interest — Group 1: Biomedical Research Investigators and Key Personnel',
]

export default function Education() {
  const { ref, isInView } = useInView(0.2)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <div className="grid grid-cols-1 md:grid-cols-2 gap-16">
          {/* Education */}
          <motion.div
            initial={{ opacity: 0, x: -20 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
              Education
            </span>
            <div className="glass rounded-xl p-8">
              <div className="flex items-start gap-4">
                <div className="w-12 h-12 rounded-lg bg-forest/20 flex items-center justify-center flex-shrink-0">
                  <span className="font-serif text-lg text-forest-light">T</span>
                </div>
                <div>
                  <h3 className="font-serif text-xl text-white">Tulane University</h3>
                  <p className="text-titanium mt-1">
                    Bachelor of Engineering, Biomedical/Medical Engineering
                  </p>
                  <p className="text-titanium/60 text-sm mt-1">2020 – 2024</p>
                </div>
              </div>
            </div>
          </motion.div>

          {/* Certifications */}
          <motion.div
            initial={{ opacity: 0, x: 20 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.2 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
              Certifications
            </span>
            <ul className="space-y-4">
              {certifications.map((cert, i) => (
                <motion.li
                  key={i}
                  initial={{ opacity: 0, y: 10 }}
                  animate={isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.4, delay: 0.3 + i * 0.1 }}
                  className="glass rounded-lg p-4 text-sm text-titanium"
                >
                  {cert}
                </motion.li>
              ))}
            </ul>
          </motion.div>
        </div>
      </div>
    </section>
  )
}
