'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const timelineData = [
  {
    year: '2026–',
    title: 'ENFRA',
    role: 'Sustainability Engineer II / Asset Manager',
    description: 'Managing Central Energy Plants for Rochester Regional Health. $143.8M EaaS partnership.',
    color: 'bg-forest-light',
  },
  {
    year: '2025',
    title: 'Convergint',
    role: 'Account Executive',
    description: 'Fire and life safety systems integration in San Francisco. Started in the CDP in Chicago.',
    color: 'bg-titanium',
  },
  {
    year: '2024–2025',
    title: 'Odoo',
    role: 'Account Executive',
    description: 'ERP implementations for manufacturing, F&B, and retail. MRP, analytic accounting.',
    color: 'bg-copper',
  },
  {
    year: '2023–2025',
    title: 'Tulane Weatherhead',
    role: 'Biomedical Engineering Researcher',
    description: 'Research device compliance study. HAPS and SWIS research projects.',
    color: 'bg-forest-light',
  },
  {
    year: '2022–2025',
    title: 'U.S. Dept. of Veterans Affairs',
    role: 'Biomedical Engineer / Project Manager',
    description: '3D-printed prosthetic devices for veterans. Taylor Foundation partnership.',
    color: 'bg-copper',
  },
  {
    year: '2022–2023',
    title: 'Tulane School of Medicine',
    role: 'Research Assistant — Wimley Lab',
    description: 'Membrane protein models, peptide pore design, combinatorial chemistry.',
    color: 'bg-titanium',
  },
  {
    year: '2020–2024',
    title: 'Tulane University',
    role: 'B.E. Biomedical/Medical Engineering',
    description: 'Three research labs. TEDx speaker.',
    color: 'bg-forest-light',
  },
]

export default function Timeline() {
  const { ref, isInView } = useInView(0.05)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-16"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Career Timeline
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            A trajectory of systems.
          </h2>
        </motion.div>

        <div className="relative">
          {/* Vertical line */}
          <div className="absolute left-4 md:left-1/2 top-0 bottom-0 w-px bg-white/10" />

          <div className="space-y-12">
            {timelineData.map((item, i) => (
              <motion.div
                key={item.title + item.year}
                initial={{ opacity: 0, y: 30 }}
                animate={isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: i * 0.1 }}
                className={`relative flex flex-col md:flex-row items-start gap-8 ${
                  i % 2 === 0 ? 'md:flex-row' : 'md:flex-row-reverse'
                }`}
              >
                {/* Dot */}
                <div className="absolute left-4 md:left-1/2 w-3 h-3 -translate-x-1.5 rounded-full border-2 border-white/20 bg-slate-950 z-10">
                  <div className={`w-full h-full rounded-full ${item.color} opacity-60`} />
                </div>

                {/* Content */}
                <div className={`ml-12 md:ml-0 md:w-1/2 ${i % 2 === 0 ? 'md:pr-16 md:text-right' : 'md:pl-16'}`}>
                  <span className="font-mono text-xs text-copper">{item.year}</span>
                  <h3 className="font-serif text-xl text-white mt-1">{item.title}</h3>
                  <p className="text-sm text-titanium mt-1">{item.role}</p>
                  <p className="text-sm text-titanium/70 mt-2 leading-relaxed">{item.description}</p>
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </div>
    </section>
  )
}
