'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

// Every fact here is restated from the story text beside it. Sources (fact-check 2026-10):
// - A07954 (2019-20 session): sponsor Asm. David DiPietro, introduced May 29, 2019, died in the
//   Transportation Committee. https://www.nysenate.gov/legislation/bills/2019/A7954
// - S4334 (2021-22 session): Sen. Patrick Gallivan's Senate version, introduced Feb 3, 2021.
//   https://www.nysenate.gov/legislation/bills/2021/S4334
// - Living Donor Support Act: S1594/A146, signed Dec 29, 2022, Chapter 814 of 2022.
//   https://www.nysenate.gov/legislation/bills/2021/S1594
// - Red Cross nomination and co-founders: Spectrum News,
//   https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition
// - Coverage: WKBW, https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors ;
//   Spectrum News, https://spectrumlocalnews.com/nys/buffalo/news/2021/01/13/college-students-push-for-more-organ-donations-in-ny- ;
//   WENY, https://weny.com/story/43131791/college-activists-pushing-for-change-to-organ-donor-registration-process-in-nys
const glance = [
  { label: 'Co-founded', value: 'August 2017 · East Aurora, NY' },
  { label: 'Co-founders', value: 'Henry McLaughlin, Grace Tapani, Sage Sellers' },
  { label: 'Primary bill', value: 'NY Assembly Bill A07954 (2019–20), sponsored by Assemblyman David DiPietro: opt-out donation at the DMV. Senate version S4334 introduced in 2021.' },
  { label: 'Passed', value: 'NYS Living Donor Support Act (signed December 2022)' },
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
              I co-founded The YCOD in high school.
            </h2>
          </motion.div>

          <div className="space-y-6 text-titanium leading-relaxed text-[17px]">
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.1 }}
            >
              In August 2017, I co-founded The Youth Coalition For Organ Donation in East Aurora, New York with Henry McLaughlin, Grace Tapani, and Sage Sellers. We started in the Donate Life club at East Aurora High School. More than 100,000 Americans are on the transplant waiting list, 17 die every day, and when we started only about 37% of New Yorkers were registered donors, then the lowest rate in the nation.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              Our main bill was <span className="text-white">NY Assembly Bill A07954</span>, sponsored by Assemblyman David DiPietro in 2019, which would create a presumed consent system at the Department of Motor Vehicles. Applicants would be registered as organ donors by default unless they decline. In 2021 Senator Patrick Gallivan introduced the Senate version, S4334. I wrote the 2021 revised draft.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.3 }}
            >
              Organ donation is also a racial justice issue. Black Americans make up about 27% of the organ transplant waiting list but only about 12% of organ donors. People of color are about 40% of the U.S. population but 60% of the waiting list.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.4 }}
            >
              Over seven years I ran our media outreach (coverage included WKBW, Spectrum News, and WENY), built partnerships with WaitList Zero, ONE8FIFTY, and the Chris Klug Foundation, managed our social media in Hootsuite and Trello, and designed the brand identity. The work earned a nomination for the 2021 American Red Cross Real Heroes Education Award.
            </motion.p>

            <motion.p
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.5 }}
            >
              I also advocated for the <span className="text-white">Living Donor Support Act</span> in New York State, which reimburses living organ donors for lost wages, travel, lodging, and child care. Governor Hochul signed it in December 2022.
            </motion.p>

            <motion.blockquote
              initial={{ opacity: 0, y: 20 }}
              animate={isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.6 }}
              className="mt-10 border-l-2 border-copper/60 pl-6 text-white text-xl md:text-2xl font-serif leading-snug"
            >
              The opt-out bill is still unfinished work. The Living Donor Support Act passed, and the coalition is still active.
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
