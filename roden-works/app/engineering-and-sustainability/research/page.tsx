'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import AnimatedCard from '@/components/ui/AnimatedCard'
import { ProjectIllustration } from '@/components/ui/ProjectIllustrations'

const researchProjects = [
  {
    href: '/engineering-and-sustainability/research/va-prosthetics',
    slug: 'va-prosthetics',
    title: 'VA Assistive Devices',
    description:
      // Scope and VA co-op (WOC) status confirmed by Evan, Oct 2026.
      'Tools that let veterans with double-arm loss put in and take out their own dentures and other oral appliances, modeled in Fusion 360 and 3D-printed during my VA co-op.',
    label: 'Biomedical Engineering',
  },
  {
    href: '/engineering-and-sustainability/research/haps',
    slug: 'haps',
    title: 'Household Air Pollution Study',
    description:
      // The published result is Rabito et al., Indoor Air (2021), https://pmc.ncbi.nlm.nih.gov/articles/PMC7985991/
      // (+7.55 mmHg systolic per 1 µg/m³ indoor black carbon). It used earlier data and does not list Evan
      // as an author, so the project is described as building on it, not as producing it.
      'Research on indoor PM2.5, black carbon, and NO2 exposure and cardiovascular outcomes in New Orleans homes, building on an earlier Tulane study that linked indoor black carbon to higher systolic blood pressure.',
    label: 'Environmental Health',
  },
  {
    href: '/engineering-and-sustainability/research/swis',
    slug: 'swis',
    title: 'Saltwater Intrusion Study',
    description:
      // "close to a million residents in four parishes": https://www.pbs.org/newshour/nation/why-salt-water-is-threatening-drinking-water-in-new-orleans-and-what-officials-are-doing-about-it
      'A longitudinal study proposal on saltwater intrusion into the Greater New Orleans water supply, prompted by the 2023 Mississippi River crisis that threatened drinking water for close to a million residents.',
    label: 'Water Resources',
  },
  {
    href: '/engineering-and-sustainability/research/wimley-lab',
    slug: 'wimley-lab',
    title: 'Wimley Lab: Membrane Proteins',
    description:
      'Peptide assemblies interacting with lipid bilayer membranes at Tulane School of Medicine. Applications in antibiotic-resistant drug design, pH-responsive drug delivery, and biosensor engineering.',
    label: 'Molecular Biology',
  },
]

export default function ResearchPage() {
  const { ref: gridRef, isInView: gridInView } = useInView(0.1)
  const { ref: contextRef, isInView: contextInView } = useInView(0.1)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          { label: 'Research' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
        {/* Molecular lattice background */}
        <div className="absolute inset-0">
          <svg
            className="absolute inset-0 w-full h-full opacity-[0.03]"
            viewBox="0 0 1200 600"
          >
            {Array.from({ length: 12 }).map((_, i) => {
              const cx = 100 + (i % 4) * 300
              const cy = 150 + Math.floor(i / 4) * 150
              return (
                <motion.circle
                  key={i}
                  cx={cx}
                  cy={cy}
                  r="40"
                  fill="none"
                  stroke="#2D5A45"
                  strokeWidth="1"
                  initial={{ pathLength: 0 }}
                  animate={{ pathLength: 1 }}
                  transition={{ duration: 2, delay: i * 0.15 }}
                />
              )
            })}
            {Array.from({ length: 8 }).map((_, i) => (
              <motion.line
                key={`line-${i}`}
                x1={100 + (i % 4) * 300}
                y1={150 + Math.floor(i / 4) * 150}
                x2={400 + (i % 3) * 300}
                y2={150 + Math.floor((i + 1) / 4) * 150}
                stroke="#2D5A45"
                strokeWidth="0.5"
                initial={{ pathLength: 0 }}
                animate={{ pathLength: 1 }}
                transition={{ duration: 2, delay: 0.5 + i * 0.1 }}
              />
            ))}
          </svg>
        </div>

        <div className="content-width w-full relative z-10 pb-16 md:pb-24 pt-32">
          <motion.span
            initial={{ opacity: 0, y: 10 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.5, delay: 0.2 }}
            className="inline-block font-mono text-xs tracking-widest uppercase text-copper mb-4"
          >
            Research Portfolio
          </motion.span>
          <motion.h1
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{
              duration: 0.7,
              delay: 0.3,
              ease: [0.16, 1, 0.3, 1],
            }}
            className="font-serif text-display-xl text-white max-w-4xl"
          >
            Research
          </motion.h1>
          <motion.p
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, delay: 0.5 }}
            className="mt-6 text-lg md:text-xl text-titanium max-w-2xl leading-relaxed"
          >
            From 2022 to 2025 I did biomedical engineering and environmental
            health research at Tulane University. The projects covered
            assistive tools for veterans, indoor air and drinking water, and
            membrane proteins.
          </motion.p>
        </div>
      </section>

      {/* Research Projects Grid */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
        ref={gridRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={gridInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Projects
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Four projects.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              The work spans device design, environmental epidemiology, water
              supply research, and molecular biophysics.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {researchProjects.map((project, i) => (
              <AnimatedCard key={project.href} index={i} {...project}>
                <div className="mb-4 -mx-2 opacity-80">
                  <ProjectIllustration slug={project.slug} variant="card" />
                </div>
              </AnimatedCard>
            ))}
          </div>
        </div>
      </section>

      {/* Research Context */}
      <section className="section-padding bg-slate-950" ref={contextRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={contextInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Context
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Where I did the work.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {[
              {
                lab: 'U.S. Department of Veterans Affairs',
                pi: 'Co-op (WOC appointment)',
                years: '2022 - 2025',
                focus:
                  'Assistive tools for veterans with double-arm loss to place and remove dentures and oral appliances.',
              },
              {
                lab: 'Weatherhead School of Public Health',
                pi: 'Dr. Felicia Rabito',
                years: '2023 - 2025',
                focus:
                  'Environmental health epidemiology: indoor air quality monitoring and longitudinal water quality research.',
              },
              {
                lab: 'Wimley Lab, Dept. of Biochemistry',
                pi: 'Dr. William Wimley',
                years: '2022 - 2023',
                focus:
                  'Membrane protein biophysics, combinatorial peptide chemistry, and synthetic molecular evolution.',
              },
            ].map((lab, i) => (
              <motion.div
                key={lab.lab}
                initial={{ opacity: 0, y: 20 }}
                animate={contextInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="glass rounded-xl p-6"
              >
                <span className="font-mono text-xs text-copper">
                  {lab.years}
                </span>
                <h3 className="font-serif text-lg text-white mt-2 mb-1">
                  {lab.lab}
                </h3>
                <p className="text-muted text-xs font-mono mb-3">
                  {lab.pi}
                </p>
                <p className="text-titanium text-sm leading-relaxed">
                  {lab.focus}
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
