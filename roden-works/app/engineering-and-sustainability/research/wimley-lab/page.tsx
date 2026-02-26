'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

const applications = [
  {
    title: 'Antibiotic-Resistant Drug Design',
    description:
      'Peptide assemblies that bypass conventional resistance mechanisms by disrupting bacterial membranes through physical pore formation rather than metabolic inhibition. These peptides remain effective even in whole blood environments — a critical benchmark that most antimicrobial peptides fail.',
    color: '#B87333',
  },
  {
    title: 'pH-Responsive Drug Delivery',
    description:
      'The pHD peptide family (pH-dependent) forms nanopores that activate specifically at pH < 6, enabling targeted drug release in acidic tumor microenvironments. This selectivity means the delivery vehicle is inert in healthy tissue and activates only at the disease site.',
    color: '#2D5A45',
  },
  {
    title: 'Biosensor Engineering',
    description:
      'Self-assembling nanopore structures that can be engineered to detect specific molecular signatures. The controlled geometry of peptide pores enables single-molecule detection capabilities for diagnostic applications in infectious disease and cancer biomarkers.',
    color: '#8A9BA8',
  },
]

const keyPeptides = [
  {
    name: 'Macrolittins',
    origin: 'Evolved from melittin (bee venom)',
    mechanism:
      'Form large, stable pores in lipid membranes at nanomolar concentrations. Potent antibacterial activity maintained in physiological conditions.',
  },
  {
    name: 'pHD Peptides',
    origin: 'Synthetic molecular evolution',
    mechanism:
      'pH-dependent nanopores that remain inactive at physiological pH (7.4) and activate at acidic pH (< 6). Ideal for tumor-targeted delivery.',
  },
  {
    name: 'ATRAM',
    origin: 'Acidity-Triggered Rational Membrane insertion',
    mechanism:
      'Peptide that inserts into membranes only under acidic conditions, serving as a molecular switch for controlled membrane disruption.',
  },
]

export default function WimleyLabPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)
  const { ref: vizRef, isInView: vizInView } = useInView(0.1)
  const { ref: appRef, isInView: appInView } = useInView(0.1)
  const { ref: peptideRef, isInView: peptideInView } = useInView(0.1)

  return (
    <>
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

        <div className="content-width relative z-10 pb-12 md:pb-16">
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
              Membrane protein models and combinatorial chemistry — designing
              peptide assemblies that interact with lipid bilayer membranes for
              applications in drug design, delivery, and diagnostics.
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
                    Professor of Biochemistry at Tulane School of Medicine), Evan
                    used PDB membrane protein models with combinatorial chemistry
                    to design peptide assemblies that interact with membrane
                    proteins and lipid bilayers.
                  </p>
                  <p>
                    The work involved identifying membrane-spanning peptide pore
                    structures using high-throughput screening — including the
                    pHD peptide family (nanopores activated at pH &lt; 6) and
                    macrolittins (evolved from melittin, the primary cytolytic
                    component of bee venom).
                  </p>
                  <p>
                    The lab&apos;s approach — synthetic molecular evolution —
                    uses iterative rounds of peptide library design, synthesis,
                    and functional screening to evolve peptides with specific
                    membrane-interacting properties. This has produced peptides
                    effective as antibacterial agents in whole blood,
                    pH-responsive drug delivery vehicles, and self-assembling
                    biosensor components.
                  </p>
                </div>

                <div className="mt-8 glass rounded-xl p-6">
                  <h3 className="font-serif text-lg text-white mb-3">
                    Lab Context
                  </h3>
                  <ul className="space-y-2 text-sm text-titanium">
                    <li className="flex items-start gap-2">
                      <span className="text-copper mt-1">&#8226;</span>
                      $1.6M NIH grant for nanopore medicine research
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-copper mt-1">&#8226;</span>
                      Combinatorial peptide libraries with 10,000+ variants per
                      screen
                    </li>
                    <li className="flex items-start gap-2">
                      <span className="text-copper mt-1">&#8226;</span>
                      Collaboration with Tulane Biochemistry and Biomedical
                      Engineering departments
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

            {/* Molecular Visualization */}
            <div ref={vizRef}>
              <motion.div
                initial={{ opacity: 0, scale: 0.95 }}
                animate={vizInView ? { opacity: 1, scale: 1 } : {}}
                transition={{ duration: 0.6 }}
                className="glass rounded-xl aspect-square flex flex-col items-center justify-center p-8 text-center relative overflow-hidden"
              >
                {/* Lipid bilayer + pore SVG */}
                <svg
                  viewBox="0 0 200 200"
                  className="absolute inset-0 w-full h-full opacity-10"
                >
                  {/* Bilayer upper leaflet */}
                  <line
                    x1="0"
                    y1="80"
                    x2="200"
                    y2="80"
                    stroke="#8A9BA8"
                    strokeWidth="0.5"
                  />
                  {/* Bilayer lower leaflet */}
                  <line
                    x1="0"
                    y1="120"
                    x2="200"
                    y2="120"
                    stroke="#8A9BA8"
                    strokeWidth="0.5"
                  />
                  {/* Lipid head groups */}
                  {Array.from({ length: 20 }).map((_, i) => (
                    <g key={`lipid-${i}`}>
                      <circle
                        cx={10 + i * 10}
                        cy="80"
                        r="2.5"
                        fill="rgba(138,155,168,0.2)"
                        stroke="rgba(138,155,168,0.4)"
                        strokeWidth="0.3"
                      />
                      <circle
                        cx={10 + i * 10}
                        cy="120"
                        r="2.5"
                        fill="rgba(138,155,168,0.2)"
                        stroke="rgba(138,155,168,0.4)"
                        strokeWidth="0.3"
                      />
                    </g>
                  ))}
                  {/* Peptide pore assembly */}
                  {Array.from({ length: 6 }).map((_, i) => (
                    <motion.circle
                      key={`pore-${i}`}
                      cx={
                        100 +
                        18 * Math.cos((i * Math.PI * 2) / 6)
                      }
                      cy={
                        100 +
                        18 * Math.sin((i * Math.PI * 2) / 6)
                      }
                      r="5"
                      fill="rgba(45,90,69,0.2)"
                      stroke="rgba(45,90,69,0.6)"
                      strokeWidth="0.5"
                      initial={{ scale: 0 }}
                      animate={vizInView ? { scale: 1 } : {}}
                      transition={{
                        duration: 0.4,
                        delay: 0.5 + i * 0.1,
                      }}
                    />
                  ))}
                  {/* Central pore lumen */}
                  <motion.circle
                    cx="100"
                    cy="100"
                    r="8"
                    fill="none"
                    stroke="rgba(184,115,51,0.4)"
                    strokeWidth="0.5"
                    strokeDasharray="2 1"
                    initial={{ scale: 0 }}
                    animate={vizInView ? { scale: 1 } : {}}
                    transition={{ duration: 0.6, delay: 1.2 }}
                  />
                </svg>

                <div className="relative z-10">
                  <div className="w-20 h-20 rounded-full bg-forest/10 flex items-center justify-center mb-6">
                    <svg
                      className="w-10 h-10 text-forest-light"
                      fill="none"
                      stroke="currentColor"
                      viewBox="0 0 24 24"
                      strokeWidth="1"
                    >
                      <circle cx="12" cy="12" r="3" />
                      <circle cx="12" cy="4" r="1.5" />
                      <circle cx="18.5" cy="8" r="1.5" />
                      <circle cx="18.5" cy="16" r="1.5" />
                      <circle cx="12" cy="20" r="1.5" />
                      <circle cx="5.5" cy="16" r="1.5" />
                      <circle cx="5.5" cy="8" r="1.5" />
                    </svg>
                  </div>
                  <h3 className="font-serif text-xl text-white mb-2">
                    Molecular Visualization
                  </h3>
                  <p className="text-titanium text-sm">
                    Lipid bilayer membrane with self-assembling peptide pores.
                    Six peptide subunits form a transmembrane channel allowing
                    controlled molecular transport.
                  </p>
                </div>
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
              Evolved molecules.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Through synthetic molecular evolution, the Wimley Lab has
              developed peptide families with distinct membrane-interacting
              properties — each designed for a specific biomedical application.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {keyPeptides.map((peptide, i) => (
              <motion.div
                key={peptide.name}
                initial={{ opacity: 0, y: 20 }}
                animate={peptideInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="glass rounded-xl p-6"
              >
                <h3 className="font-serif text-xl text-white mb-1">
                  {peptide.name}
                </h3>
                <span className="text-copper text-xs font-mono">
                  {peptide.origin}
                </span>
                <p className="text-titanium text-sm leading-relaxed mt-3">
                  {peptide.mechanism}
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
              From membrane to medicine.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {applications.map((app, i) => (
              <motion.div
                key={app.title}
                initial={{ opacity: 0, y: 20 }}
                animate={appInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="glass rounded-xl p-6 border-t-2"
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
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
    </>
  )
}
