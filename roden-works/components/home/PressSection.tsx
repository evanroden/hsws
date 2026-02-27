'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const pressItems = [
  {
    source: 'TEDxTulane',
    quote: 'A compelling argument for lowering the barriers to youth political participation — delivered by someone who has already broken through them.',
    link: '/about/ted',
    year: '2023',
  },
  {
    source: 'Tulane School of Science & Engineering',
    quote: 'Biomedical engineering research bridging prosthetic design, membrane protein studies, and environmental health — contributing to published findings on air pollution and cardiovascular outcomes.',
    year: '2020–2024',
  },
  {
    source: 'ENFRA',
    quote: 'Managing central energy plants as part of the $143.8M Rochester Regional Health EaaS partnership — delivering $354.6M in guaranteed savings across a 30-year term.',
    year: '2024–Present',
  },
  {
    source: 'Vogue Italy — BizarrAudi',
    quote: 'Featured in the SchoolTime collection runway presentation, bridging engineering discipline with creative expression.',
    link: '/studio/modeling',
    year: '2020',
  },
]

export default function PressSection() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-slate-950 border-t border-white/5" ref={ref}>
      <div className="content-width">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="mb-12"
        >
          <div className="flex items-center gap-3 mb-3">
            <span className="w-8 h-px bg-copper/50" />
            <span className="font-mono text-xs tracking-[0.2em] uppercase text-copper">
              Press & Recognition
            </span>
          </div>
          <h2 className="font-serif text-heading text-white">
            In the record.
          </h2>
        </motion.div>

        <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
          {pressItems.map((item, i) => (
            <motion.div
              key={item.source}
              initial={{ opacity: 0, y: 30 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: i * 0.1, ease: [0.16, 1, 0.3, 1] }}
              className="glass rounded-xl p-6 md:p-8 hover:bg-white/10 hover:border-white/20 transition-all duration-500"
            >
              <div className="flex items-center justify-between mb-4">
                <span className="font-mono text-xs tracking-widest uppercase text-copper">
                  {item.source}
                </span>
                <span className="font-mono text-xs text-titanium/40">{item.year}</span>
              </div>
              <p className="text-titanium text-sm leading-relaxed italic">
                &ldquo;{item.quote}&rdquo;
              </p>
              {item.link && (
                <a
                  href={item.link}
                  className="inline-flex items-center gap-1.5 mt-4 text-xs text-titanium/50 hover:text-copper transition-colors"
                >
                  Read more <span>&rarr;</span>
                </a>
              )}
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
