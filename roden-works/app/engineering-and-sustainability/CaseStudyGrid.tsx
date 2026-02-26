'use client'

import AnimatedCard from '@/components/ui/AnimatedCard'

const caseStudies = [
  {
    href: '/engineering-and-sustainability/enfra',
    title: 'ENFRA × Rochester Regional Health',
    description: '$143.8 million, 30-year Energy-as-a-Service partnership. Managing Central Energy Plants at UMMC and St. Mary\'s Medical Center.',
    label: 'EaaS',
  },
  {
    href: '/engineering-and-sustainability/convergint',
    title: 'Convergint',
    description: 'Fire and life safety systems integration. NFPA 72 compliance, intelligent detection, and emergency communications.',
    label: 'Systems Integration',
  },
  {
    href: '/engineering-and-sustainability/odoo',
    title: 'Odoo',
    description: 'ERP implementations for manufacturing, F&B, and retail clients. Hit 160% of non-recurring revenue goal.',
    label: 'ERP',
  },
  {
    href: '/engineering-and-sustainability/research/va-prosthetics',
    title: 'VA Prosthetics',
    description: 'Custom 3D-printed prosthetic devices for American veterans, modeled in Fusion 360.',
    label: 'Biomedical',
  },
  {
    href: '/engineering-and-sustainability/research/haps',
    title: 'Household Air Pollution Study',
    description: 'Research on PM2.5, black carbon, and NO2 exposure and cardiovascular outcomes in New Orleans.',
    label: 'Research',
  },
  {
    href: '/engineering-and-sustainability/research/swis',
    title: 'Saltwater Intrusion Study',
    description: 'First-of-kind longitudinal study proposal on saltwater intrusion into the Greater New Orleans water supply.',
    label: 'Research',
  },
  {
    href: '/engineering-and-sustainability/research/wimley-lab',
    title: 'Wimley Lab — Membrane Proteins',
    description: 'Peptide assemblies interacting with membrane proteins. Applications in antibiotic-resistant drug design.',
    label: 'Molecular Biology',
  },
]

export default function CaseStudyGrid() {
  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5">
      <div className="content-width">
        <div className="mb-12">
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Case Studies
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Deep dives.
          </h2>
        </div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-6">
          {caseStudies.map((study, i) => (
            <AnimatedCard key={study.href} index={i} {...study} />
          ))}
        </div>
      </div>
    </section>
  )
}
