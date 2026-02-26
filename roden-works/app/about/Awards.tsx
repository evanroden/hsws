'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const awards = [
  {
    title: 'American Real Heroes Award',
    org: 'American Red Cross',
    description: 'Nominated for education leadership and founding The YCOD organ donation advocacy organization.',
  },
  {
    title: 'C40 Reinventing Cities Award',
    org: 'Mayor of New Orleans',
    description: 'Comprehensive urban revitalization plan for New Orleans East covering disaster planning, solar energy, transit, and green housing.',
  },
  {
    title: 'Summa Cum Laude',
    org: 'Tulane University',
    description: 'Highest academic honors in Biomedical/Medical Engineering.',
  },
  {
    title: 'Boy of the Year',
    org: 'Boys & Girls Club of America',
    description: 'National recognition for outstanding youth achievement and community impact.',
  },
  {
    title: 'Best Debater & Speaker',
    org: 'Academic Competition',
    description: 'Recognition for excellence in argumentation and public discourse.',
  },
  {
    title: 'Best Student of 2022',
    org: 'Academic Achievement',
    description: 'Top student recognition during undergraduate studies.',
  },
  {
    title: 'National Latin Honor Society Honoree',
    org: 'National Latin Honor Society',
    description: 'Official honoree for excellence in Classical and Ecclesiastical Latin.',
  },
]

export default function Awards() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper">
            Awards & Honors
          </span>
          <h2 className="font-serif text-heading text-white mt-3">Recognition.</h2>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
          {awards.map((award, i) => (
            <motion.div
              key={award.title}
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: i * 0.08 }}
              className="glass rounded-xl p-6 group hover:bg-white/10 hover:border-copper/20 transition-all duration-300"
            >
              <div className="w-8 h-8 rounded-full bg-copper/10 flex items-center justify-center mb-4">
                <svg viewBox="0 0 24 24" className="w-4 h-4 text-copper" fill="currentColor">
                  <path d="M12 2l3.09 6.26L22 9.27l-5 4.87 1.18 6.88L12 17.77l-6.18 3.25L7 14.14 2 9.27l6.91-1.01L12 2z" />
                </svg>
              </div>
              <h3 className="font-serif text-lg text-white mb-1">{award.title}</h3>
              <p className="text-copper text-xs font-mono mb-2">{award.org}</p>
              <p className="text-titanium text-sm leading-relaxed">{award.description}</p>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
