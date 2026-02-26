'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const languages = [
  {
    name: 'English',
    level: 'Native',
    type: 'natural',
    description: 'Native language',
    proficiency: 100,
  },
  {
    name: 'Classical Latin',
    level: 'Full Professional',
    type: 'natural',
    description: 'The Latin of Cicero, Caesar, and Virgil — the literary and philosophical language of the Roman Republic and Empire.',
    proficiency: 90,
  },
  {
    name: 'Ecclesiastical Latin',
    level: 'Full Professional',
    type: 'natural',
    description: 'The Latin of the Catholic Church, Vatican documents, and liturgical tradition — maintained as a living language of scholarship and worship.',
    proficiency: 90,
  },
  {
    name: 'Interlingua',
    level: 'Professional Working',
    type: 'natural',
    description: 'The most widely used naturalistic international auxiliary language, developed by IALA. Immediately readable by speakers of Romance languages without prior study.',
    proficiency: 75,
  },
  {
    name: 'Chinese (Mandarin)',
    level: 'Limited Working',
    type: 'natural',
    description: 'Developing proficiency in Mandarin Chinese.',
    proficiency: 35,
  },
  {
    name: 'Python',
    level: 'Professional Working',
    type: 'technical',
    description: 'Data analysis, automation, scientific computing. Applied across energy optimization and epidemiological research.',
    proficiency: 75,
  },
  {
    name: 'R',
    level: 'Full Professional',
    type: 'technical',
    description: 'Statistical analysis, data visualization, research methodology. Primary tool for biomedical and environmental research.',
    proficiency: 90,
  },
]

export default function Languages() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Languages
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            A polyglot perspective.
          </h2>
          <p className="mt-4 text-titanium max-w-2xl">
            From the precision of Classical Latin to the logic of Python — each language opens a different way of thinking about problems.
          </p>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {/* Natural languages */}
          <div>
            <h3 className="font-mono text-xs tracking-widest uppercase text-titanium/60 mb-4">
              Natural Languages
            </h3>
            <div className="space-y-4">
              {languages
                .filter((l) => l.type === 'natural')
                .map((lang, i) => (
                  <motion.div
                    key={lang.name}
                    initial={{ opacity: 0, x: -20 }}
                    animate={isInView ? { opacity: 1, x: 0 } : {}}
                    transition={{ duration: 0.5, delay: i * 0.1 }}
                    className="glass rounded-xl p-5"
                  >
                    <div className="flex items-center justify-between mb-2">
                      <h4 className="font-serif text-lg text-white">{lang.name}</h4>
                      <span className="font-mono text-xs text-copper">{lang.level}</span>
                    </div>
                    <p className="text-titanium text-sm leading-relaxed mb-3">{lang.description}</p>
                    <div className="h-1 bg-white/5 rounded-full overflow-hidden">
                      <motion.div
                        initial={{ width: 0 }}
                        animate={isInView ? { width: `${lang.proficiency}%` } : {}}
                        transition={{ duration: 1, delay: 0.5 + i * 0.1, ease: [0.16, 1, 0.3, 1] }}
                        className="h-full bg-gradient-to-r from-forest-light to-copper rounded-full"
                      />
                    </div>
                  </motion.div>
                ))}
            </div>
          </div>

          {/* Technical languages */}
          <div>
            <h3 className="font-mono text-xs tracking-widest uppercase text-titanium/60 mb-4">
              Technical Languages
            </h3>
            <div className="space-y-4">
              {languages
                .filter((l) => l.type === 'technical')
                .map((lang, i) => (
                  <motion.div
                    key={lang.name}
                    initial={{ opacity: 0, x: 20 }}
                    animate={isInView ? { opacity: 1, x: 0 } : {}}
                    transition={{ duration: 0.5, delay: i * 0.1 }}
                    className="glass rounded-xl p-5 border-forest/20"
                  >
                    <div className="flex items-center justify-between mb-2">
                      <h4 className="font-serif text-lg text-white font-mono">{lang.name}</h4>
                      <span className="font-mono text-xs text-forest-light">{lang.level}</span>
                    </div>
                    <p className="text-titanium text-sm leading-relaxed mb-3">{lang.description}</p>
                    <div className="h-1 bg-white/5 rounded-full overflow-hidden">
                      <motion.div
                        initial={{ width: 0 }}
                        animate={isInView ? { width: `${lang.proficiency}%` } : {}}
                        transition={{ duration: 1, delay: 0.5 + i * 0.1, ease: [0.16, 1, 0.3, 1] }}
                        className="h-full bg-gradient-to-r from-forest to-forest-light rounded-full"
                      />
                    </div>
                  </motion.div>
                ))}
            </div>
          </div>
        </div>
      </div>
    </section>
  )
}
