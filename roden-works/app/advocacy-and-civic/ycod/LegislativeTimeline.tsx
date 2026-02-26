'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

const events = [
  { year: '2017', title: 'Founded The YCOD', description: 'Co-founded with Henry McLaughlin, Grace Tapani, and Sage Sellers in East Aurora, NY.' },
  { year: '2018', title: 'Coalition Building', description: 'Established partnerships with WaitList Zero, ONE8FIFTY, and the Chris Klug Foundation.' },
  { year: '2019', title: 'Legislative Introduction', description: 'Opt-out organ donation bill introduced in the NY Assembly.' },
  { year: '2020', title: 'National Media Campaign', description: 'Coverage by CBC, Yahoo News, Business Insider, WKBW, and Spectrum News.' },
  { year: '2021', title: 'Bill Revision', description: 'Evan personally drafted the revised NY Assembly Bill A07954 — presumed consent at the DMV.' },
  { year: '2021', title: 'Real Heroes Nomination', description: 'Nominated for the American Red Cross Real Heroes Education Award.' },
  { year: '2022–24', title: 'Continued Advocacy', description: 'Sustained lobbying, social media campaigns, and coalition management while attending Tulane.' },
  { year: '2023', title: 'Living Donor Support Act Passed', description: 'Advocated for the NYS Living Donor Support Act — removing financial barriers for living organ donors through reimbursement for lost wages, travel, and child care. The bill passed into law.' },
  { year: '2024', title: 'Transition', description: 'After 7+ years of leadership, Evan transitioned focus while the framework and coalition persist.' },
]

export default function LegislativeTimeline() {
  const { ref, isInView } = useInView(0.05)

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
            Legislative Journey
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            Seven years in the making.
          </h2>
        </motion.div>

        <div className="relative">
          <div className="absolute left-4 md:left-8 top-0 bottom-0 w-px bg-copper/20" />
          <div className="space-y-8">
            {events.map((event, i) => (
              <motion.div
                key={i}
                initial={{ opacity: 0, x: -20 }}
                animate={isInView ? { opacity: 1, x: 0 } : {}}
                transition={{ duration: 0.5, delay: i * 0.1 }}
                className="relative pl-12 md:pl-20"
              >
                <div className="absolute left-2.5 md:left-6.5 w-3 h-3 rounded-full bg-slate-950 border-2 border-copper/40 z-10" />
                <span className="font-mono text-xs text-copper">{event.year}</span>
                <h3 className="font-serif text-lg text-white mt-1">{event.title}</h3>
                <p className="text-titanium text-sm mt-1 leading-relaxed">{event.description}</p>
              </motion.div>
            ))}
          </div>
        </div>
      </div>
    </section>
  )
}
