'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import AnimatedCard from '@/components/ui/AnimatedCard'

const researchProjects = [
  {
    href: '/engineering-and-sustainability/research/va-prosthetics',
    title: 'VA Prosthetics',
    description:
      'Custom 3D-printed prosthetic devices for American veterans, designed in Autodesk Fusion 360 and fabricated using FlowIt adaptive manufacturing. A partnership between Tulane University and the U.S. Department of Veterans Affairs restoring autonomy through engineering.',
    label: 'Biomedical Engineering',
  },
  {
    href: '/engineering-and-sustainability/research/haps',
    title: 'Household Air Pollution Study',
    description:
      'Research on indoor PM2.5, black carbon, and NO2 exposure and cardiovascular outcomes in New Orleans homes. Published finding linking highest-quartile black carbon exposure to a clinically significant increase in systolic blood pressure.',
    label: 'Environmental Health',
  },
  {
    href: '/engineering-and-sustainability/research/swis',
    title: 'Saltwater Intrusion Study',
    description:
      'First-of-kind longitudinal study proposal on saltwater intrusion into the Greater New Orleans water supply, prompted by the 2023 Mississippi River crisis that threatened drinking water for 1.2 million residents.',
    label: 'Water Resources',
  },
  {
    href: '/engineering-and-sustainability/research/wimley-lab',
    title: 'Wimley Lab — Membrane Proteins',
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

        <div className="content-width relative z-10 pb-16 md:pb-24 pt-32">
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
            Four years of biomedical engineering and environmental health
            research at Tulane University — from prosthetic devices for veterans
            to the molecular mechanics of membrane proteins.
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
              From bench to bedside.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Each project represents a different dimension of biomedical
              engineering — human-centered design, environmental epidemiology,
              water systems resilience, and molecular biophysics.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {researchProjects.map((project, i) => (
              <AnimatedCard key={project.href} index={i} {...project} />
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
              Three labs, one university.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {[
              {
                lab: 'VA Prosthetics Lab',
                pi: 'Taylor Foundation Partnership',
                years: '2022 - 2025',
                focus:
                  'Assistive device design and 3D printing for veterans with limb differences and mobility challenges.',
              },
              {
                lab: 'Weatherhead School of Public Health',
                pi: 'Dr. Felicia Rabito',
                years: '2023 - 2025',
                focus:
                  'Environmental health epidemiology — indoor air quality monitoring and longitudinal water quality research.',
              },
              {
                lab: 'Wimley Lab — Dept. of Biochemistry',
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
                <p className="text-titanium/60 text-xs font-mono mb-3">
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
