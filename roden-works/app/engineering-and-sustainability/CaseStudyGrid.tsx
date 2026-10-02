'use client'

import { useState } from 'react'
import { motion, AnimatePresence } from 'framer-motion'
import AnimatedCard from '@/components/ui/AnimatedCard'
import SegmentedControl from '@/components/ui/SegmentedControl'
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
    description: '$143.8 million, 30-year Energy-as-a-Service partnership. I manage the Central Energy Plants at UMMC and St. Mary\'s Medical Campus.',
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
    description: 'ERP implementations for manufacturing, F&B, and retail clients. Hit 160% of my non-recurring revenue goal in my best month.',
    label: 'ERP',
  },
  {
    href: '/engineering-and-sustainability/research/va-prosthetics',
    slug: 'va-prosthetics',
    title: 'VA Prosthetics',
    description: '3D-printed dental and maxillofacial prosthetic models for veterans, modeled in Fusion 360.',
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
    description: 'Longitudinal study proposal on saltwater intrusion into the Greater New Orleans water supply.',
    label: 'Research',
  },
  {
    href: '/engineering-and-sustainability/research/wimley-lab',
    slug: 'wimley-lab',
    title: 'Wimley Lab: Membrane Proteins',
    description: 'Peptide assemblies interacting with lipid membranes. Applications in antibiotic-resistant drug design.',
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
            Selected work.
          </h2>
        </div>

        {/* Filters */}
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-10 overflow-x-auto no-scrollbar"
        >
          <SegmentedControl<FilterKey>
            label="Filter case studies"
            size="md"
            value={activeFilter}
            onChange={setActiveFilter}
            options={filterOptions.map((o) => ({ value: o.key, label: o.label }))}
          />
        </motion.div>

        {/* 4-column grid: the flagship spans two, so seven studies tile evenly (2+1+1 / 1+1+1+1) */}
        <motion.div layout className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4 md:gap-5">
          <AnimatePresence mode="popLayout">
            {filtered.map((study, i) => (
              <motion.div
                key={study.href}
                layout
                initial={{ opacity: 0, scale: 0.96 }}
                animate={{ opacity: 1, scale: 1 }}
                exit={{ opacity: 0, scale: 0.96 }}
                transition={{ duration: 0.35, delay: i * 0.04 }}
                className={activeFilter === 'all' && i === 0 ? 'md:col-span-2' : ''}
              >
                <AnimatedCard index={0} {...study}>
                  <div className="mb-5 -mx-2">
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
