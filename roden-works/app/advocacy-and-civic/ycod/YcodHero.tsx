'use client'

import { motion } from 'framer-motion'

export default function YcodHero() {
  return (
    <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-copper/5 to-slate-950 overflow-hidden">
      <div className="content-width relative z-10 pb-12 md:pb-16">
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={{ opacity: 1, y: 0 }}
          transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
        >
          <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
            501(c)(4) Nonpartisan Advocacy
          </span>
          <h1 className="font-serif text-display text-white max-w-4xl">
            The Youth Coalition For Organ Donation
          </h1>
          <p className="mt-4 text-titanium text-lg max-w-2xl">
            Founded in 2017, The YCOD is a youth-led organization advocating for presumed consent organ donation legislation — addressing the crisis that costs 17 lives every single day.
          </p>
        </motion.div>
      </div>
    </section>
  )
}
