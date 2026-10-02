'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import MembraneModel from './MembraneModel'
import { Cite, SourceList } from '@/components/ui/Sources'
import { WIMLEY_SOURCES as S } from './sources'

const applications = [
  {
    title: 'Antibiotic-Resistant Drug Design',
    description:
      'These peptides kill bacteria by punching pores in their membranes, so they sidestep the resistance mechanisms that target metabolic drugs. They stay active in whole blood, a test most antimicrobial peptides fail.',
    cites: ['starr2020'],
    color: '#B87333',
  },
  {
    title: 'pH-Responsive Drug Delivery',
    description:
      'The pHD (pH-dependent) peptides form nanopores only at pH < 6. Tumor microenvironments are acidic, so a carrier built on them could stay inactive in healthy tissue and release its drug at the tumor.',
    cites: ['phd2017', 'tumor-acid'],
    color: '#2D5A45',
  },
  {
    title: 'Biosensor Engineering',
    description:
      'Self-assembling nanopores can be engineered to detect specific molecules. Because the pore geometry is controlled, they could support single-molecule detection of infectious disease and cancer biomarkers.',
    cites: ['nanopore-detect'],
    color: '#8A9BA8',
  },
]

const keyPeptides = [
  {
    name: 'Macrolittins',
    origin: 'Evolved from melittin (bee venom)',
    // Macrolittins release macromolecular cargo from PC vesicles at ratios as
    // low as ~1 peptide per 1,000 lipids and show no measurable cytolytic
    // activity against human cells. Source:
    // https://medicine.tulane.edu/wimley-lab/pore-forming-peptides
    mechanism:
      'Form large pores in lipid membranes at very low peptide-to-lipid ratios to release macromolecule-sized cargo, with no measurable toxicity to human cells.',
    cites: ['macrolittins2018', 'acsnano2024'],
  },
  {
    name: 'pHD Peptides',
    origin: 'Synthetic molecular evolution',
    mechanism:
      'pH-dependent nanopores that stay inactive at physiological pH (7.4) and open at acidic pH (< 6), which makes them candidates for tumor-targeted delivery.',
    cites: ['phd2017'],
  },
  {
    // MelP5 is the Wimley lab's first-generation gain-of-function melittin
    // variant and the parent used to evolve the macrolittins. Source:
    // https://medicine.tulane.edu/wimley-lab/pore-forming-peptides
    // (ATRAM was removed here: it is from Francisco Barrera's lab at the
    // University of Tennessee, not the Wimley lab.)
    name: 'MelP5',
    origin: 'Gain-of-function melittin variant',
    mechanism:
      'A potent equilibrium pore-former evolved from melittin. It releases macromolecule-sized cargo from lipid vesicles and served as the parent for the macrolittins.',
    cites: ['melp5-2014', 'acsnano2024'],
  },
]

export default function WimleyLabPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)
  const { ref: vizRef, isInView: vizInView } = useInView(0.1)
  const { ref: appRef, isInView: appInView } = useInView(0.1)
  const { ref: peptideRef, isInView: peptideInView } = useInView(0.1)

  return (
    <>
      <BreadcrumbJsonLd
        items={[
          { name: 'Engineering', href: '/engineering-and-sustainability' },
          { name: 'Research', href: '/engineering-and-sustainability/research' },
          { name: 'Wimley Lab' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          {
            label: 'Research',
            href: '/engineering-and-sustainability/research',
          },
          { label: 'Wimley Lab' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1000} />
      </div>

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-clinical/5 to-slate-950 overflow-hidden">
        {/* Abstract molecular lattice */}
        <div className="absolute inset-0">
          <svg
            className="absolute inset-0 w-full h-full opacity-[0.04]"
            viewBox="0 0 1200 600"
          >
            {/* Lipid bilayer lines */}
            <motion.line
              x1="0"
              y1="280"
              x2="1200"
              y2="280"
              stroke="#8A9BA8"
              strokeWidth="1"
              initial={{ pathLength: 0 }}
              animate={{ pathLength: 1 }}
              transition={{ duration: 2 }}
            />
            <motion.line
              x1="0"
              y1="320"
              x2="1200"
              y2="320"
              stroke="#8A9BA8"
              strokeWidth="1"
              initial={{ pathLength: 0 }}
              animate={{ pathLength: 1 }}
              transition={{ duration: 2, delay: 0.3 }}
            />
            {/* Pore structures */}
            {Array.from({ length: 4 }).map((_, i) => (
              <motion.g key={i}>
                {Array.from({ length: 6 }).map((_, j) => (
                  <motion.circle
                    key={j}
                    cx={
                      250 +
                      i * 250 +
                      18 * Math.cos((j * Math.PI * 2) / 6)
                    }
                    cy={
                      300 + 18 * Math.sin((j * Math.PI * 2) / 6)
                    }
                    r="5"
                    fill="none"
                    stroke="#2D5A45"
                    strokeWidth="0.5"
                    initial={{ scale: 0, opacity: 0 }}
                    animate={{ scale: 1, opacity: 0.6 }}
                    transition={{
                      duration: 0.5,
                      delay: 1 + i * 0.3 + j * 0.05,
                    }}
                  />
                ))}
              </motion.g>
            ))}
          </svg>
        </div>

        <div className="content-width w-full relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Tulane School of Medicine
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Wimley Lab
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              I used membrane protein models and combinatorial chemistry to
              design peptide assemblies that interact with lipid bilayers, for
              use in drug design, drug delivery, and diagnostics.
            </p>
          </motion.div>
        </div>
      </section>

      {/* About + Molecular Visualization */}
      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <div ref={contentRef}>
              <motion.div
                initial={{ opacity: 0, y: 30 }}
                animate={contentInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6 }}
              >
                <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                  May 2022 &ndash; Oct 2023
                </span>
                <h2 className="font-serif text-heading text-white mb-6">
                  Research Assistant
                </h2>
                <div className="space-y-4 text-titanium leading-relaxed">
                  <p>
                    In Dr. William Wimley&apos;s lab (George A. Adrouny
                    Professor of Biochemistry at Tulane School of Medicine),<Cite sources={S} id="wimley-faculty" /> I
                    used PDB membrane protein models and combinatorial chemistry
                    to design peptide assemblies that interact with membrane
                    proteins and lipid bilayers.
                  </p>
                  <p>
                    I used high-throughput screening to identify peptides that
                    form pores across the membrane. These included the pHD
                    peptide family (nanopores activated at pH &lt; 6)<Cite sources={S} id="phd2017" /> and
                    macrolittins, which were evolved from melittin,<Cite sources={S} id="macrolittins2018" /> the main
                    cytolytic component of bee venom.<Cite sources={S} id="bee-venom" />
                  </p>
                  <p>
                    The lab&apos;s method, synthetic molecular evolution, runs
                    repeated rounds of library design, synthesis, and screening
                    to select peptides with specific membrane activity.<Cite sources={S} id="tulane-sme" />{' '}
                    {/* Softened: "self-assembling biosensor components" removed; no published
                        Wimley-lab source found for a biosensor product. */}
                    It has produced antibacterial peptides that work in whole
                    blood<Cite sources={S} id="starr2020" /> and pH-triggered
                    pore-formers aimed at drug delivery.<Cite sources={S} id="phd2017" />
                  </p>
                </div>

                <div className="mt-8 rounded-xl border border-white/[0.08] bg-surface p-6">
                  <h3 className="font-serif text-lg text-white mb-3">
                    Lab Context
                  </h3>
                  <ul className="space-y-2 text-sm text-titanium">
                    {/* Removed an unsourced "$1.6M NIH grant" bullet and an
                        unsourced "10,000+ variants per screen" figure. The
                        JHU/Hristova collaboration is documented at
                        https://medicine.tulane.edu/wimley-lab/pore-forming-peptides */}
                    <li className="flex items-start gap-2">
                      <span className="text-copper mt-1">&#8226;</span>
                      Combinatorial peptide libraries screened by iterative
                      synthetic molecular evolution<Cite sources={S} id="tulane-sme" />
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-copper mt-1">&#8226;</span>
                      Collaboration with the Hristova Lab at Johns Hopkins
                      University<Cite sources={S} id="tulane-pore" />
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-copper mt-1">&#8226;</span>
                      Applications spanning infectious disease, oncology, and
                      diagnostics
                    </li>
                  </ul>
                </div>
              </motion.div>
            </div>

            {/* Molecular Visualization — interactive membrane model */}
            <div ref={vizRef}>
              <motion.div
                initial={{ opacity: 0, y: 24 }}
                animate={vizInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6 }}
              >
                <MembraneModel />
              </motion.div>
            </div>
          </div>
        </div>
      </section>

      {/* Key Peptide Families */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
        ref={peptideRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={peptideInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Peptide Families
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Peptide families from the lab.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              The Wimley Lab has used synthetic molecular evolution to develop
              several peptide families, each with different membrane activity
              and a different intended use.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {keyPeptides.map((peptide, i) => (
              <motion.div
                key={peptide.name}
                initial={{ opacity: 0, y: 20 }}
                animate={peptideInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="rounded-xl border border-white/[0.08] bg-surface p-6"
              >
                <h3 className="font-serif text-xl text-white mb-1">
                  {peptide.name}
                </h3>
                <span className="text-copper text-xs font-mono">
                  {peptide.origin}
                </span>
                <p className="text-titanium text-sm leading-relaxed mt-3">
                  {peptide.mechanism}
                  <Cite sources={S} id={peptide.cites} />
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Applications */}
      <section className="section-padding bg-slate-950" ref={appRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={appInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Applications
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Potential applications.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {applications.map((app, i) => (
              <motion.div
                key={app.title}
                initial={{ opacity: 0, y: 20 }}
                animate={appInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="rounded-xl border border-white/[0.08] bg-surface p-6 border-t-2"
                style={{ borderTopColor: app.color + '60' }}
              >
                <div
                  className="w-3 h-3 rounded-full mb-4"
                  style={{ backgroundColor: app.color }}
                />
                <h3 className="font-serif text-lg text-white mb-3">
                  {app.title}
                </h3>
                <p className="text-titanium text-sm leading-relaxed">
                  {app.description}
                  <Cite sources={S} id={app.cites} />
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
      <SourceList sources={S} />
      <ProjectNav currentSlug="wimley-lab" />
    </>
  )
}
