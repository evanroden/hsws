'use client'

import Link from 'next/link'
import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

interface Skill {
  name: string
  context: string
  project?: { label: string; href: string }
}

// Context lines are drawn from the case-study pages they link to.
const groups: { name: string; skills: Skill[] }[] = [
  {
    name: 'Energy & building systems',
    skills: [
      {
        name: 'Energy optimization',
        context: 'Central energy plant operations and performance for hospital campuses.',
        project: { label: 'ENFRA', href: '/engineering-and-sustainability/enfra' },
      },
      {
        name: 'NFPA 72',
        context: 'Fire alarm design, inspection, testing & maintenance.',
        project: { label: 'Convergint', href: '/engineering-and-sustainability/convergint' },
      },
    ],
  },
  {
    name: 'Data & computation',
    skills: [
      {
        name: 'Python',
        context: 'Data analysis and automation across energy optimization and epidemiological research.',
      },
      {
        name: 'R',
        context: 'Statistical analysis. My main tool for biomedical and environmental research.',
        project: { label: 'Research', href: '/engineering-and-sustainability/research' },
      },
      {
        name: 'Data analysis',
        context: 'Energy data for plant optimization; every research project.',
      },
    ],
  },
  {
    name: 'Design & fabrication',
    skills: [
      {
        name: 'Autodesk Fusion 360',
        context: 'Parametric CAD for assistive tools for veterans.',
        project: { label: 'VA Assistive Devices', href: '/engineering-and-sustainability/research/va-prosthetics' },
      },
      {
        name: 'FlowIt',
        context: 'Adaptive slicing for FDM and SLA 3D printing.',
        project: { label: 'VA Assistive Devices', href: '/engineering-and-sustainability/research/va-prosthetics' },
      },
      { name: 'TRIZ', context: 'Systematic innovation methodology.' },
    ],
  },
  {
    name: 'Enterprise systems',
    skills: [
      {
        name: 'ERP (Odoo)',
        context: 'MRP, analytic accounting, and customer-portal implementations.',
        project: { label: 'Odoo', href: '/engineering-and-sustainability/odoo' },
      },
    ],
  },
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
          className="mb-12 max-w-2xl"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Technical Skills</span>
          <h2 className="font-serif text-heading text-white mt-3">Tools I use.</h2>
          <p className="mt-4 text-titanium leading-relaxed">
            Where a tool has a project link, that is where I used it.
          </p>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 xl:grid-cols-4 gap-4">
          {groups.map((group, gi) => (
            <motion.div
              key={group.name}
              initial={{ opacity: 0, y: 24 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: gi * 0.08, ease: [0.16, 1, 0.3, 1] }}
              className="rounded-2xl border border-white/[0.08] bg-surface p-6"
            >
              <h3 className="font-mono text-[11px] tracking-[0.18em] uppercase text-copper-light mb-2">{group.name}</h3>
              <ul className="divide-y divide-white/[0.06]">
                {group.skills.map((skill) => (
                  <li key={skill.name} className="py-4">
                    <div className="flex items-start justify-between gap-3">
                      <span className="text-[15px] font-medium text-white">{skill.name}</span>
                      {skill.project && (
                        <Link
                          href={skill.project.href}
                          className="shrink-0 inline-flex items-center gap-1 rounded-full border border-white/10 px-2 py-0.5 text-[11px] font-medium text-titanium hover:text-white hover:border-white/25 transition-colors"
                        >
                          {skill.project.label}
                          <span aria-hidden="true">&rarr;</span>
                        </Link>
                      )}
                    </div>
                    <p className="mt-1 text-sm text-muted leading-relaxed">{skill.context}</p>
                  </li>
                ))}
              </ul>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
