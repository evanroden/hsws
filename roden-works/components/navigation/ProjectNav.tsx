'use client'

import Link from 'next/link'
import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const allProjects = [
  {
    slug: 'enfra',
    title: 'ENFRA × Rochester Regional Health',
    category: 'EaaS',
    description: '$143.8M, 30-year Energy-as-a-Service partnership.',
    href: '/engineering-and-sustainability/enfra',
  },
  {
    slug: 'convergint',
    title: 'Convergint',
    category: 'Systems Integration',
    description: 'Fire and life safety systems integration. NFPA 72 compliance.',
    href: '/engineering-and-sustainability/convergint',
  },
  {
    slug: 'odoo',
    title: 'Odoo',
    category: 'ERP',
    description: 'ERP implementations. 160% of non-recurring revenue goal.',
    href: '/engineering-and-sustainability/odoo',
  },
  {
    slug: 'ycod',
    title: 'The YCOD',
    category: 'Advocacy',
    description: 'Opt-out organ donation advocacy and legislative reform.',
    href: '/advocacy-and-civic/ycod',
  },
  {
    slug: 'va-prosthetics',
    title: 'VA Prosthetics',
    category: 'Biomedical',
    description: 'Custom 3D-printed prosthetic devices for American veterans.',
    href: '/engineering-and-sustainability/research/va-prosthetics',
  },
  {
    slug: 'cinematography',
    title: 'Cinematography',
    category: 'Creative',
    description: 'Claiborne Avenue Productions and Tulane Freeman School.',
    href: '/studio/cinematography',
  },
  {
    slug: 'haps',
    title: 'Household Air Pollution Study',
    category: 'Research',
    description: 'PM2.5, black carbon, and cardiovascular outcomes research.',
    href: '/engineering-and-sustainability/research/haps',
  },
  {
    slug: 'swis',
    title: 'Saltwater Intrusion Study',
    category: 'Research',
    description: 'Longitudinal study on saltwater intrusion into the Greater New Orleans water supply.',
    href: '/engineering-and-sustainability/research/swis',
  },
  {
    slug: 'wimley-lab',
    title: 'Wimley Lab — Membrane Proteins',
    category: 'Molecular Biology',
    description: 'Peptide assemblies interacting with membrane proteins for drug design.',
    href: '/engineering-and-sustainability/research/wimley-lab',
  },
]

export default function ProjectNav({ currentSlug }: { currentSlug: string }) {
  const { ref, isInView } = useInView(0.1)
  const currentIndex = allProjects.findIndex((p) => p.slug === currentSlug)
  const prevProject = allProjects[(currentIndex - 1 + allProjects.length) % allProjects.length]
  const nextProject = allProjects[(currentIndex + 1) % allProjects.length]

  return (
    <section className="border-t border-white/5 bg-slate-950" ref={ref}>
      <div className="content-width">
        <div className="grid grid-cols-1 md:grid-cols-2">
          {/* Previous */}
          <motion.div
            initial={{ opacity: 0, x: -20 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.5 }}
          >
            <Link
              href={prevProject.href}
              className="group block py-12 md:py-16 md:pr-8 border-b md:border-b-0 md:border-r border-white/5"
            >
              <span className="font-mono text-xs text-titanium/40 tracking-widest uppercase">
                &larr; Previous Project
              </span>
              <h3 className="font-serif text-xl md:text-2xl text-white mt-3 group-hover:text-copper transition-colors duration-300">
                {prevProject.title}
              </h3>
              <span className="inline-block mt-2 font-mono text-xs text-copper/60">
                {prevProject.category}
              </span>
              <p className="text-titanium text-sm mt-2">{prevProject.description}</p>
            </Link>
          </motion.div>

          {/* Next */}
          <motion.div
            initial={{ opacity: 0, x: 20 }}
            animate={isInView ? { opacity: 1, x: 0 } : {}}
            transition={{ duration: 0.5, delay: 0.1 }}
          >
            <Link
              href={nextProject.href}
              className="group block py-12 md:py-16 md:pl-8 text-right"
            >
              <span className="font-mono text-xs text-titanium/40 tracking-widest uppercase">
                Next Project &rarr;
              </span>
              <h3 className="font-serif text-xl md:text-2xl text-white mt-3 group-hover:text-copper transition-colors duration-300">
                {nextProject.title}
              </h3>
              <span className="inline-block mt-2 font-mono text-xs text-copper/60">
                {nextProject.category}
              </span>
              <p className="text-titanium text-sm mt-2">{nextProject.description}</p>
            </Link>
          </motion.div>
        </div>
      </div>
    </section>
  )
}
