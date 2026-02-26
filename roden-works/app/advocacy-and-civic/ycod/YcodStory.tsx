'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

export default function YcodStory() {
  const { ref, isInView } = useInView(0.1)

  return (
    <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={ref}>
      <div className="content-width">
        <div className="max-w-3xl">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
              The Story
            </span>
            <h2 className="font-serif text-heading text-white mb-8">
              A seventeen-year-old&apos;s answer to a systemic failure.
            </h2>
          </motion.div>

          <div className="space-y-6 text-titanium leading-relaxed">
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

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.6 }}
              className="text-white text-lg font-serif"
            >
              The work is not finished. But the framework is built, legislation has been passed, and the coalition endures.
            </motion.p>
          </div>
        </div>
      </div>
    </section>
  )
}
