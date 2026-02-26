'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const skillCategories = [
  {
    name: 'Engineering',
    skills: [
      { name: 'Python', level: 75 },
      { name: 'R', level: 90 },
      { name: 'TRIZ', level: 70 },
      { name: 'Autodesk Fusion 360', level: 80 },
      { name: 'FlowIt (3D Printing)', level: 75 },
      { name: 'Energy Modeling', level: 85 },
      { name: 'Data Analysis', level: 90 },
    ],
  },
  {
    name: 'Business',
    skills: [
      { name: 'ERP Implementation', level: 85 },
      { name: 'SaaS Sales', level: 90 },
      { name: 'NRR/MRR Optimization', level: 85 },
      { name: 'Subcontractor Management', level: 80 },
      { name: 'Account Management', level: 90 },
    ],
  },
  {
    name: 'Compliance',
    skills: [
      { name: 'NFPA 72', level: 75 },
      { name: 'Fire & Life Safety', level: 80 },
      { name: 'Biomedical Research Conduct', level: 85 },
      { name: 'EaaS Compliance', level: 80 },
    ],
  },
  {
    name: 'Creative',
    skills: [
      { name: 'Premiere Pro', level: 90 },
      { name: 'After Effects', level: 80 },
      { name: 'DaVinci Resolve', level: 85 },
      { name: 'Cinema Grade', level: 75 },
      { name: 'BlackMagic 6K', level: 85 },
      { name: 'Sony a7s II', level: 80 },
    ],
  },
  {
    name: 'Communication',
    skills: [
      { name: 'Public Speaking (TEDx)', level: 95 },
      { name: 'Lobbying', level: 85 },
      { name: 'Grant Writing', level: 75 },
      { name: 'Social Media (Hootsuite)', level: 80 },
      { name: 'Brand Identity Design', level: 70 },
    ],
  },
]

export default function Skills() {
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
            Capabilities
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Skills matrix.
          </h2>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-8">
          {skillCategories.map((category, ci) => (
            <motion.div
              key={category.name}
              initial={{ opacity: 0, y: 30 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: ci * 0.1 }}
              className="glass rounded-xl p-6"
            >
              <h3 className="font-mono text-sm tracking-widest uppercase text-copper mb-6">
                {category.name}
              </h3>
              <div className="space-y-4">
                {category.skills.map((skill, si) => (
                  <div key={skill.name}>
                    <div className="flex items-center justify-between mb-1.5">
                      <span className="text-sm text-white">{skill.name}</span>
                      <span className="text-xs text-titanium/60 font-mono">{skill.level}%</span>
                    </div>
                    <div className="h-1 bg-white/5 rounded-full overflow-hidden">
                      <motion.div
                        initial={{ width: 0 }}
                        animate={isInView ? { width: `${skill.level}%` } : {}}
                        transition={{
                          duration: 0.8,
                          delay: 0.3 + ci * 0.1 + si * 0.05,
                          ease: [0.16, 1, 0.3, 1],
                        }}
                        className="h-full bg-gradient-to-r from-forest-light to-copper rounded-full"
                      />
                    </div>
                  </div>
                ))}
              </div>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
