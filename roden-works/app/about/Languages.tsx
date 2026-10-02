'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

// Proficiency uses the standard five-level scale (as on LinkedIn / ILR), shown as
// discrete steps rather than an invented percentage.
const LEVELS = ['Elementary', 'Limited Working', 'Professional Working', 'Full Professional', 'Native'] as const
type Level = (typeof LEVELS)[number]

const languages: { name: string; level: Level; type: 'natural' | 'technical'; description: string }[] = [
  { name: 'English', level: 'Native', type: 'natural', description: 'Native language.' },
  {
    name: 'Classical Latin',
    level: 'Full Professional',
    type: 'natural',
    description: 'The Latin of Cicero, Caesar, and Virgil, used for literature and philosophy in the Roman Republic and Empire.',
  },
  {
    name: 'Ecclesiastical Latin',
    level: 'Full Professional',
    type: 'natural',
    description: 'The Latin of the Catholic Church, still used in Vatican documents and the liturgy.',
  },
  {
    name: 'Interlingua',
    level: 'Professional Working',
    type: 'natural',
    // https://en.wikipedia.org/wiki/Interlingua ("most widely used" superlative removed: unsourced)
    description: 'A naturalistic international auxiliary language developed between 1937 and 1951 by IALA. People who know a Romance language can usually read it without prior study.',
  },
  { name: 'Chinese (Mandarin)', level: 'Limited Working', type: 'natural', description: 'Currently studying.' },
  {
    name: 'Python',
    level: 'Professional Working',
    type: 'technical',
    description: 'Data analysis, automation, and scientific computing for energy optimization and epidemiological research.',
  },
  {
    name: 'R',
    level: 'Full Professional',
    type: 'technical',
    description: 'Statistical analysis and data visualization. The main tool for the biomedical and environmental research projects.',
  },
]

function LevelMeter({ level, accent }: { level: Level; accent: string }) {
  const step = LEVELS.indexOf(level) + 1
  return (
    <div className="flex items-center gap-3" aria-label={`Proficiency: ${level} (${step} of ${LEVELS.length})`} role="img">
      <div className="flex gap-1" aria-hidden="true">
        {LEVELS.map((_, i) => (
          <span key={i} className="h-1.5 w-6 rounded-full" style={{ background: i < step ? accent : 'rgba(255,255,255,0.08)' }} />
        ))}
      </div>
      <span className="w-[8.5rem] text-xs text-titanium">{level}</span>
    </div>
  )
}

export default function Languages() {
  const { ref, isInView } = useInView(0.1)
  const natural = languages.filter((l) => l.type === 'natural')
  const technical = languages.filter((l) => l.type === 'technical')

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">Languages</span>
          <h2 className="font-serif text-heading text-white mt-3">Spoken and programming languages.</h2>
          <p className="mt-4 text-titanium max-w-2xl">
            Proficiency is shown on the standard five-level scale, from elementary to native.
          </p>
        </motion.div>

        <div className="grid grid-cols-1 lg:grid-cols-5 gap-6 items-start">
          {[
            { title: 'Natural languages', items: natural, accent: '#B87333', span: 'lg:col-span-3' },
            { title: 'Technical languages', items: technical, accent: '#3DA887', span: 'lg:col-span-2' },
          ].map((col, ci) => (
            <motion.div
              key={col.title}
              initial={{ opacity: 0, y: 24 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: ci * 0.1 }}
              className={`rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-8 ${col.span}`}
            >
              <h3 className="font-mono text-[11px] tracking-[0.18em] uppercase text-muted mb-2">{col.title}</h3>
              <ul className="divide-y divide-white/[0.06]">
                {col.items.map((lang) => (
                  <li key={lang.name} className="py-5">
                    <div className="flex flex-col gap-2 sm:flex-row sm:items-center sm:justify-between mb-2">
                      <h4 className={`${col.accent === '#3DA887' ? 'font-mono text-base' : 'font-serif text-xl'} text-white`}>
                        {lang.name}
                      </h4>
                      <LevelMeter level={lang.level} accent={col.accent} />
                    </div>
                    <p className="text-sm text-muted leading-relaxed max-w-xl">{lang.description}</p>
                  </li>
                ))}
              </ul>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
