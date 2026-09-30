'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

// Every fact here is restated from the story text beside it
const glance = [
  { label: 'Founded', value: 'August 2017 · East Aurora, NY' },
  { label: 'Co-founders', value: 'Henry McLaughlin, Grace Tapani, Sage Sellers' },
  { label: 'Primary bill', value: 'NY Assembly Bill A07954 — opt-out donation at the DMV (2021 revision drafted by Evan)' },
  { label: 'Passed', value: 'NYS Living Donor Support Act' },
  { label: 'Partners', value: 'WaitList Zero · ONE8FIFTY · Chris Klug Foundation' },
  { label: 'Recognition', value: '2021 American Red Cross Real Heroes Education Award nominee' },
]

export default function YcodStory() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width grid grid-cols-1 lg:grid-cols-12 gap-12 lg:gap-8">
        <div className="lg:col-span-7">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">The Story</span>
            <h2 className="font-serif text-heading text-white mb-8">
              A seventeen-year-old&apos;s answer to a systemic failure.
            </h2>
          </motion.div>

          <div className="space-y-6 text-titanium leading-relaxed text-[17px]">
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.1 }}
            >
              In August 2017, Evan Roden co-founded The Youth Coalition For Organ Donation in East Aurora, New York alongside Henry McLaughlin, Grace Tapani, and Sage Sellers. The premise was simple, the problem was not: more than 100,000 Americans wait on the transplant list at any given time, 17 die every day, and New York has the lowest organ donor designation rate in the nation — hovering between 37% and 42%.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              The YCOD&apos;s primary legislative vehicle is <span className="text-white">NY Assembly Bill A07954</span> — an opt-out organ donation bill that would create a presumed consent system at the Department of Motor Vehicles. Under this model, adults would be registered as organ donors by default unless they actively decline. The 2021 revised draft of the bill was written by Evan himself.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.3 }}
            >
              The YCOD&apos;s work sits at the intersection of public health and racial justice. Black Americans make up 27% of the organ transplant waiting list but represent only 13% of organ donors. The average kidney wait time for a Black patient is 1,335 days — nearly twice the 734-day wait for white patients. Sixty percent of all waitlisted patients are people of color.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.4 }}
            >
              Over seven years, Evan led the organization through national media campaigns (CBC, Yahoo News, Business Insider), built partnerships with WaitList Zero, ONE8FIFTY, and the Chris Klug Foundation, managed social media strategy via Hootsuite and Trello, designed the brand identity, and was nominated for the 2021 American Red Cross Real Heroes Education Award for this work.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.5 }}
            >
              Beyond the opt-out bill, Evan advocated for the <span className="text-white">Living Donor Support Act</span> in New York State — legislation designed to remove financial barriers for living organ donors by providing reimbursement for lost wages, travel, and child care expenses. The bill passed, making New York one of the first states to formally support living donors and addressing a key inequity in the donation system.
            </motion.p>

            <motion.blockquote
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.6 }}
              className="mt-10 border-l-2 border-copper/60 pl-6 text-white text-xl md:text-2xl font-serif leading-snug"
            >
              The work is not finished. But the framework is built, legislation has been passed, and the coalition endures.
            </motion.blockquote>
          </div>
        </div>

        {/* At a glance — sticky on desktop */}
        <motion.aside
          initial={{ opacity: 0, y: 24 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.7, delay: 0.25 }}
          className="lg:col-span-4 lg:col-start-9"
        >
          <div className="lg:sticky lg:top-28 rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-7">
            <h3 className="font-mono text-[11px] tracking-[0.18em] uppercase text-copper-light">At a glance</h3>
            <dl className="mt-4 divide-y divide-white/[0.06]">
              {glance.map((item) => (
                <div key={item.label} className="py-4">
                  <dt className="text-xs text-muted">{item.label}</dt>
                  <dd className="mt-1 text-sm text-white leading-relaxed">{item.value}</dd>
                </div>
              ))}
            </dl>
          </div>
        </motion.aside>
      </div>
    </section>
  )
}
