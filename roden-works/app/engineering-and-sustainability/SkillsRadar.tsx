'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const skills = [
  { name: 'Python', level: 75, category: 'Programming' },
  { name: 'R', level: 90, category: 'Programming' },
  { name: 'TRIZ', level: 70, category: 'Methodology' },
  { name: 'Fusion 360', level: 80, category: 'CAD' },
  { name: 'FlowIt', level: 75, category: '3D Printing' },
  { name: 'Data Analysis', level: 90, category: 'Analytics' },
  { name: 'ERP (Odoo)', level: 85, category: 'Software' },
  { name: 'NFPA 72', level: 75, category: 'Compliance' },
  { name: 'Energy Optimization', level: 85, category: 'Engineering' },
]

export default function SkillsRadar() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Technical Skills
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Interdisciplinary toolkit.
          </h2>
        </motion.div>

        {/* Interactive Skills Grid */}
        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-3 gap-4">
          {skills.map((skill, i) => (
            <motion.div
              key={skill.name}
              initial={{ opacity: 0, scale: 0.95 }}
              animate={isInView ? { opacity: 1, scale: 1 } : {}}
              transition={{ duration: 0.5, delay: i * 0.05 }}
              className="glass rounded-xl p-5 group hover:bg-white/10 hover:border-white/20 transition-all duration-300 cursor-default"
            >
              <div className="flex items-center justify-between mb-3">
                <div>
                  <h3 className="text-white text-sm font-medium">{skill.name}</h3>
                  <span className="text-titanium/60 text-xs font-mono">{skill.category}</span>
                </div>
                <span className="font-mono text-sm text-copper">{skill.level}%</span>
              </div>
              <div className="h-1.5 bg-white/5 rounded-full overflow-hidden">
                <motion.div
                  initial={{ width: 0 }}
                  animate={isInView ? { width: `${skill.level}%` } : {}}
                  transition={{ duration: 1, delay: 0.3 + i * 0.08, ease: [0.16, 1, 0.3, 1] }}
                  className="h-full rounded-full bg-gradient-to-r from-forest-light to-forest"
                />
              </div>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
