'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const skillCategories = [
  {
    name: 'Engineering',
    skills: [
      { name: 'Python', context: 'Data pipelines & automation scripts' },
      { name: 'R', context: 'Statistical analysis for research projects' },
      { name: 'TRIZ', context: 'Systematic innovation methodology' },
      { name: 'Autodesk Fusion 360', context: 'Prosthetic device modeling for the VA' },
      { name: 'FlowIt (3D Printing)', context: 'Custom prosthetic fabrication' },
      { name: 'Energy Modeling', context: 'Central energy plant optimization' },
      { name: 'Data Analysis', context: 'Used across all research projects' },
    ],
  },
  {
    name: 'Business',
    skills: [
      { name: 'ERP Implementation', context: 'Odoo deployments for manufacturing clients' },
      { name: 'SaaS Sales', context: 'Hit 160% of non-recurring revenue goal' },
      { name: 'Revenue Growth', context: 'Upsells & new module implementations' },
      { name: 'Subcontractor Management', context: 'ENFRA facility operations' },
      { name: 'Account Management', context: 'Enterprise client relationships' },
    ],
  },
  {
    name: 'Compliance',
    skills: [
      { name: 'NFPA 72', context: 'Fire alarm system standards' },
      { name: 'Fire & Life Safety', context: 'Convergint systems integration' },
      { name: 'Biomedical Research Conduct', context: 'Human-subjects research protocols' },
      { name: 'EaaS Compliance', context: 'Energy performance contracting' },
    ],
  },
  {
    name: 'Creative',
    skills: [
      { name: 'Premiere Pro', context: 'Primary editing suite' },
      { name: 'After Effects', context: 'Motion graphics & VFX' },
      { name: 'DaVinci Resolve', context: 'Color grading workflow' },
      { name: 'Cinema Grade', context: 'On-set color correction' },
      { name: 'BlackMagic 6K', context: 'Claiborne Avenue Productions' },
      { name: 'Sony a7s II', context: 'Documentary & event work' },
    ],
  },
  {
    name: 'Communication',
    skills: [
      { name: 'Public Speaking', context: 'TEDxTulane speaker' },
      { name: 'Lobbying', context: '7+ years with The YCOD' },
      { name: 'Grant Writing', context: 'Research funding proposals' },
      { name: 'Social Media (Hootsuite)', context: 'YCOD campaign strategy' },
      { name: 'Brand Identity Design', context: 'YCOD brand identity' },
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
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Capabilities</span>
          <h2 className="font-serif text-heading text-white mt-3">Skills matrix.</h2>
        </motion.div>

        <div className="columns-1 md:columns-2 xl:columns-3 gap-4 [column-fill:_balance]">
          {skillCategories.map((category, ci) => (
            <motion.div
              key={category.name}
              initial={{ opacity: 0, y: 24 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: ci * 0.08, ease: [0.16, 1, 0.3, 1] }}
              className="mb-4 break-inside-avoid rounded-2xl border border-white/[0.08] bg-surface p-6"
            >
              <div className="flex items-baseline justify-between mb-2">
                <h3 className="font-mono text-[11px] tracking-[0.18em] uppercase text-copper-light">{category.name}</h3>
                <span className="font-mono text-[11px] text-faint">{String(category.skills.length).padStart(2, '0')}</span>
              </div>
              <dl className="divide-y divide-white/[0.06]">
                {category.skills.map((skill) => (
                  <div key={skill.name} className="flex items-baseline justify-between gap-4 py-3">
                    <dt className="text-[15px] font-medium text-white">{skill.name}</dt>
                    <dd className="text-sm text-muted text-right">{skill.context}</dd>
                  </div>
                ))}
              </dl>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
