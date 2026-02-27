'use client'

import { useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import AnimatedCard from '@/components/ui/AnimatedCard'
import { ProjectIllustration } from '@/components/ui/ProjectIllustrations'
import { useInView } from '@/lib/hooks'

type FilterKey = 'all' | 'EaaS' | 'Research' | 'Biomedical' | 'Systems Integration' | 'ERP' | 'Molecular Biology'

const filterOptions: { key: FilterKey; label: string }[] = [
  { key: 'all', label: 'All' },
  { key: 'EaaS', label: 'EaaS' },
  { key: 'Research', label: 'Research' },
  { key: 'Biomedical', label: 'Biomedical' },
  { key: 'Systems Integration', label: 'Systems' },
  { key: 'ERP', label: 'ERP' },
]

const caseStudies = [
  {
    href: '/engineering-and-sustainability/enfra',
    slug: 'enfra',
    title: 'ENFRA × Rochester Regional Health',
    description: '$143.8 million, 30-year Energy-as-a-Service partnership. Managing Central Energy Plants at UMMC and St. Mary\'s Medical Center.',
    label: 'EaaS',
  },
  {
    href: '/engineering-and-sustainability/convergint',
    slug: 'convergint',
    title: 'Convergint',
    description: 'Fire and life safety systems integration. NFPA 72 compliance, intelligent detection, and emergency communications.',
    label: 'Systems Integration',
  },
  {
    href: '/engineering-and-sustainability/odoo',
    slug: 'odoo',
    title: 'Odoo',
    description: 'ERP implementations for manufacturing, F&B, and retail clients. Hit 160% of non-recurring revenue goal.',
    label: 'ERP',
  },
  {
    href: '/engineering-and-sustainability/research/va-prosthetics',
    slug: 'va-prosthetics',
    title: 'VA Prosthetics',
    description: 'Custom 3D-printed prosthetic devices for American veterans, modeled in Fusion 360.',
    label: 'Biomedical',
  },
  {
    href: '/engineering-and-sustainability/research/haps',
    slug: 'haps',
    title: 'Household Air Pollution Study',
    description: 'Research on PM2.5, black carbon, and NO2 exposure and cardiovascular outcomes in New Orleans.',
    label: 'Research',
  },
  {
    href: '/engineering-and-sustainability/research/swis',
    slug: 'swis',
    title: 'Saltwater Intrusion Study',
    description: 'First-of-kind longitudinal study proposal on saltwater intrusion into the Greater New Orleans water supply.',
    label: 'Research',
  },
  {
    href: '/engineering-and-sustainability/research/wimley-lab',
    slug: 'wimley-lab',
    title: 'Wimley Lab — Membrane Proteins',
    description: 'Peptide assemblies interacting with membrane proteins. Applications in antibiotic-resistant drug design.',
    label: 'Molecular Biology',
  },
]

export default function CaseStudyGrid() {
  const [activeFilter, setActiveFilter] = useState<FilterKey>('all')
  const { ref, isInView } = useInView(0.05)

  const filtered = activeFilter === 'all'
    ? caseStudies
    : caseStudies.filter((s) => s.label === activeFilter)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <div className="mb-12">
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Case Studies
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Deep dives.
          </h2>
        </div>

        {/* Filter buttons */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="flex flex-wrap gap-3 mb-10"
        >
          {filterOptions.map((opt) => (
            <button
              key={opt.key}
              onClick={() => setActiveFilter(opt.key)}
              className={`px-4 py-2 rounded-lg text-sm font-mono transition-all duration-300 ${
                activeFilter === opt.key
                  ? 'bg-copper text-white'
                  : 'bg-white/5 text-titanium hover:bg-white/10'
              }`}
            >
              {opt.label}
            </button>
          ))}
        </motion.div>

        <motion.div layout className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          <AnimatePresence mode="popLayout">
            {filtered.map((study, i) => (
              <motion.div
                key={study.href}
                layout
                initial={{ opacity: 0, scale: 0.9 }}
                animate={{ opacity: 1, scale: 1 }}
                exit={{ opacity: 0, scale: 0.9 }}
                transition={{ duration: 0.4, delay: i * 0.05 }}
              >
                <AnimatedCard index={0} {...study}>
                  <div className="mb-4 -mx-2 opacity-80">
                    <ProjectIllustration slug={study.slug} variant="card" />
                  </div>
                </AnimatedCard>
              </motion.div>
            ))}
          </AnimatePresence>
        </motion.div>
      </div>
    </section>
  )
}
