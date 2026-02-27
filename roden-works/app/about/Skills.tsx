'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'

const skillCategories = [
  {
    name: 'Engineering',
    skills: [
      { name: 'Python', level: 75, context: 'Data pipelines & automation scripts' },
      { name: 'R', level: 90, context: 'Statistical analysis across 3 research labs' },
      { name: 'TRIZ', level: 70, context: 'Systematic innovation methodology' },
      { name: 'Autodesk Fusion 360', level: 80, context: 'Prosthetic device modeling for VA' },
      { name: 'FlowIt (3D Printing)', level: 75, context: 'Custom prosthetic fabrication' },
      { name: 'Energy Modeling', level: 85, context: 'Central energy plant optimization' },
      { name: 'Data Analysis', level: 90, context: 'Used across all research projects' },
    ],
  },
  {
    name: 'Business',
    skills: [
      { name: 'ERP Implementation', level: 85, context: 'Odoo deployments for manufacturing clients' },
      { name: 'SaaS Sales', level: 90, context: 'Hit 160% of NRR goal' },
      { name: 'NRR/MRR Optimization', level: 85, context: 'Revenue growth strategy' },
      { name: 'Subcontractor Management', level: 80, context: 'ENFRA facility operations' },
      { name: 'Account Management', level: 90, context: 'Enterprise client relationships' },
    ],
  },
  {
    name: 'Compliance',
    skills: [
      { name: 'NFPA 72', level: 75, context: 'Fire alarm system standards' },
      { name: 'Fire & Life Safety', level: 80, context: 'Convergint systems integration' },
      { name: 'Biomedical Research Conduct', level: 85, context: 'IRB-approved research protocols' },
      { name: 'EaaS Compliance', level: 80, context: 'Energy performance contracting' },
    ],
  },
  {
    name: 'Creative',
    skills: [
      { name: 'Premiere Pro', level: 90, context: 'Primary editing suite' },
      { name: 'After Effects', level: 80, context: 'Motion graphics & VFX' },
      { name: 'DaVinci Resolve', level: 85, context: 'Color grading workflow' },
      { name: 'Cinema Grade', level: 75, context: 'On-set color correction' },
      { name: 'BlackMagic 6K', level: 85, context: 'Claiborne Avenue Productions' },
      { name: 'Sony a7s II', level: 80, context: 'Documentary & event work' },
    ],
  },
  {
    name: 'Communication',
    skills: [
      { name: 'Public Speaking (TEDx)', level: 95, context: 'TEDxTulane keynote speaker' },
      { name: 'Lobbying', level: 85, context: '7+ years with the YCOD' },
      { name: 'Grant Writing', level: 75, context: 'Research funding proposals' },
      { name: 'Social Media (Hootsuite)', level: 80, context: 'Tulane Freeman marketing' },
      { name: 'Brand Identity Design', level: 70, context: 'YCOD & personal branding' },
    ],
  },
]

export default function Skills() {
  const { ref, isInView } = useInView(0.1)
  const [hoveredSkill, setHoveredSkill] = useState<string | null>(null)

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
                  <div
                    key={skill.name}
                    className="relative group cursor-default"
                    onMouseEnter={() => setHoveredSkill(`${category.name}-${skill.name}`)}
                    onMouseLeave={() => setHoveredSkill(null)}
                  >
                    <div className="flex items-center justify-between mb-1.5">
                      <span className="text-sm text-white group-hover:text-copper transition-colors duration-200">
                        {skill.name}
                      </span>
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
                        className={`h-full bg-gradient-to-r from-forest-light to-copper rounded-full transition-shadow duration-300 ${
                          hoveredSkill === `${category.name}-${skill.name}` ? 'shadow-[0_0_8px_rgba(184,115,51,0.4)]' : ''
                        }`}
                      />
                    </div>
                    {/* Tooltip */}
                    {hoveredSkill === `${category.name}-${skill.name}` && (
                      <motion.div
                        initial={{ opacity: 0, y: 4 }}
                        animate={{ opacity: 1, y: 0 }}
                        className="absolute left-0 top-full mt-1 z-10 px-3 py-1.5 rounded-lg bg-slate-950/95 border border-white/10 backdrop-blur-sm"
                      >
                        <span className="text-xs text-titanium whitespace-nowrap">{skill.context}</span>
                      </motion.div>
                    )}
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
