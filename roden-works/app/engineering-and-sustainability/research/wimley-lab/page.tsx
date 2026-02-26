'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

const applications = [
  { title: 'Antibiotic-Resistant Drug Design', desc: 'Peptide assemblies that bypass conventional resistance mechanisms, effective even in whole blood environments.' },
  { title: 'Drug Delivery', desc: 'pH-responsive peptide pores (pHD family) that activate at pH < 6, enabling targeted drug release in acidic tumor microenvironments.' },
  { title: 'Biosensor Engineering', desc: 'Self-assembling nanopore structures that can detect specific molecular signatures for diagnostic applications.' },
]

export default function WimleyLabPage() {
  const { ref, isInView } = useInView(0.1)

  return (
    <>
      <Breadcrumbs items={[{ label: 'Engineering', href: '/engineering-and-sustainability' }, { label: 'Research', href: '/engineering-and-sustainability/research' }, { label: 'Wimley Lab' }]} />

      <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-clinical/5 to-slate-950">
        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div initial={{ opacity: 0, y: 30 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.7 }}>
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">Tulane School of Medicine</span>
            <h1 className="font-serif text-display text-white max-w-4xl">Wimley Lab</h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">Membrane protein models and combinatorial chemistry — designing peptide assemblies that interact with lipid bilayer membranes.</p>
          </motion.div>
        </div>
      </section>

      <section className="section-padding bg-slate-950">
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <div>
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">May 2022 – Oct 2023</span>
              <h2 className="font-serif text-heading text-white mb-6">Research Assistant</h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>In Dr. William Wimley&apos;s lab (George A. Adrouny Professor of Biochemistry), used PDB membrane protein models with combinatorial chemistry to design peptide assemblies that interact with membrane proteins.</p>
                <p>Identified membrane-spanning peptide pore structures using high-throughput screening — including the pHD peptide family (nanopores activated at pH &lt; 6) and macrolittins (evolved from melittin, the primary component of bee venom).</p>
                <p>The lab&apos;s synthetic molecular evolution approach has produced peptides effective as antibacterial agents in whole blood, pH-responsive drug delivery vehicles, and self-assembling biosensor components — supported by a $1.6M grant for nanopore medicine.</p>
              </div>
            </div>

            {/* Molecular visualization placeholder */}
            <div ref={ref}>
              <motion.div initial={{ opacity: 0, scale: 0.95 }} animate={isInView ? { opacity: 1, scale: 1 } : {}} transition={{ duration: 0.6 }} className="glass rounded-xl aspect-square flex flex-col items-center justify-center p-8 text-center relative overflow-hidden">
                {/* Abstract molecular representation */}
                <svg viewBox="0 0 200 200" className="absolute inset-0 w-full h-full opacity-10">
                  {/* Lipid bilayer */}
                  <line x1="0" y1="80" x2="200" y2="80" stroke="#8A9BA8" strokeWidth="0.5" />
                  <line x1="0" y1="120" x2="200" y2="120" stroke="#8A9BA8" strokeWidth="0.5" />
                  {/* Pore */}
                  {Array.from({ length: 6 }).map((_, i) => (
                    <circle key={i} cx={100 + 20 * Math.cos((i * Math.PI * 2) / 6)} cy={100 + 20 * Math.sin((i * Math.PI * 2) / 6)} r="4" fill="none" stroke="#2D5A45" strokeWidth="0.5" />
                  ))}
                </svg>

                <div className="relative z-10">
                  <div className="w-20 h-20 rounded-full bg-forest/10 flex items-center justify-center mb-6">
                    <svg className="w-10 h-10 text-forest-light" fill="none" stroke="currentColor" viewBox="0 0 24 24" strokeWidth="1">
                      <circle cx="12" cy="12" r="3" />
                      <circle cx="12" cy="4" r="1.5" />
                      <circle cx="18.5" cy="8" r="1.5" />
                      <circle cx="18.5" cy="16" r="1.5" />
                      <circle cx="12" cy="20" r="1.5" />
                      <circle cx="5.5" cy="16" r="1.5" />
                      <circle cx="5.5" cy="8" r="1.5" />
                    </svg>
                  </div>
                  <h3 className="font-serif text-xl text-white mb-2">Molecular Visualization</h3>
                  <p className="text-titanium text-sm">Lipid bilayer membrane with self-assembling peptide pores — interactive 3D scene planned.</p>
                </div>
              </motion.div>
            </div>
          </div>

          {/* Applications */}
          <div className="mt-16">
            <h2 className="font-serif text-heading text-white mb-8">Applications</h2>
            <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
              {applications.map((app, i) => (
                <motion.div key={app.title} initial={{ opacity: 0, y: 20 }} animate={isInView ? { opacity: 1, y: 0 } : {}} transition={{ duration: 0.5, delay: i * 0.1 }} className="glass rounded-xl p-6">
                  <h3 className="font-serif text-lg text-white mb-2">{app.title}</h3>
                  <p className="text-titanium text-sm leading-relaxed">{app.desc}</p>
                </motion.div>
              ))}
            </div>
          </div>
        </div>
      </section>
    </>
  )
}
