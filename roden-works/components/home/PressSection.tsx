'use client'

import { motion } from 'framer-motion'
import Link from 'next/link'
import { useInView } from '@/lib/hooks'

// Outlets named in The YCOD's 2020 national media campaign (see /advocacy-and-civic/ycod)
const outlets = ['CBC', 'Yahoo News', 'Business Insider', 'WKBW', 'Spectrum News']

const recognition = [
  {
    title: 'C40 Reinventing Cities Award',
    issuer: 'Mayor of New Orleans',
    detail: 'For a revitalization plan for New Orleans East covering disaster planning, solar, transit, and green housing.',
    href: '/advocacy-and-civic/nola-east',
  },
  {
    title: 'TEDxTulane speaker',
    issuer: 'Youth political participation',
    detail: 'A talk on how young people are structurally shut out of politics.',
    href: '/about/ted',
  },
  {
    title: 'Real Heroes nominee',
    issuer: 'American Red Cross · 2021',
    detail: 'Nominated for the Real Heroes Education Award for co-founding and leading The YCOD.',
    href: '/advocacy-and-civic/ycod',
  },
  {
    title: 'Vogue Italy',
    issuer: 'BizarrAudi · SchoolTime · 2020',
    detail: "Runway modeling for Vogue Italy's feature of BizarrAudi's SchoolTime collection.",
    href: '/studio/modeling',
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
            <span className="font-mono text-xs tracking-[0.2em] uppercase text-copper">Recognition</span>
          </div>
          <h2 className="font-serif text-heading text-white">On the record.</h2>
        </motion.div>

        {/* Media coverage strip */}
        <motion.div
          initial={{ opacity: 0 }}
          animate={isInView ? { opacity: 1 } : {}}
          transition={{ duration: 0.6, delay: 0.1 }}
          className="mb-10 rounded-2xl border border-white/[0.08] bg-surface px-6 py-6 md:px-8 flex flex-col gap-4 md:flex-row md:items-center md:justify-between"
        >
          <p className="text-sm text-muted md:max-w-[15rem] shrink-0">
            Advocacy for The YCOD covered by
          </p>
          <ul className="flex flex-wrap items-center gap-x-8 gap-y-3">
            {outlets.map((name) => (
              <li key={name} className="font-sans text-lg md:text-xl font-semibold tracking-tight text-titanium">
                {name}
              </li>
            ))}
          </ul>
        </motion.div>

        <div className="grid grid-cols-1 sm:grid-cols-2 lg:grid-cols-4 gap-4">
          {recognition.map((item, i) => (
            <motion.div
              key={item.title}
              initial={{ opacity: 0, y: 24 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.15 + i * 0.08, ease: [0.16, 1, 0.3, 1] }}
            >
              <Link
                href={item.href}
                className="group flex h-full flex-col rounded-2xl border border-white/[0.08] bg-surface p-6 transition-colors duration-300 hover:border-white/20 hover:bg-surface-raised"
              >
                <span aria-hidden="true" className="mb-5 block h-px w-8 bg-copper/60 transition-all duration-300 group-hover:w-12" />
                <h3 className="font-serif text-xl text-white leading-snug">{item.title}</h3>
                <p className="mt-1 font-mono text-[11px] tracking-wider uppercase text-copper-light">{item.issuer}</p>
                <p className="mt-4 text-sm text-titanium leading-relaxed flex-1">{item.detail}</p>
                <span className="mt-5 inline-flex items-center gap-1.5 text-xs font-medium text-muted group-hover:text-white transition-colors">
                  View project <span aria-hidden="true" className="transition-transform group-hover:translate-x-0.5">&rarr;</span>
                </span>
              </Link>
            </motion.div>
          ))}
        </div>
      </div>
    </section>
  )
}
