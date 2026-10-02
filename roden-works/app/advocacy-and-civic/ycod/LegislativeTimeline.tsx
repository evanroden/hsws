'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'

type Kind = 'milestone' | 'legislative' | 'default'

// Sources: A07954 https://www.nysenate.gov/legislation/bills/2019/A7954 ; S4334
// https://www.nysenate.gov/legislation/bills/2021/S4334 ; Living Donor Support Act (S1594/A146, signed
// Dec 29, 2022, Ch. 814) https://www.nysenate.gov/legislation/bills/2021/S1594 ; coverage: WKBW
// https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors ,
// Spectrum News https://spectrumlocalnews.com/nys/buffalo/news/2021/01/13/college-students-push-for-more-organ-donations-in-ny-
// CBC, Yahoo News and Business Insider coverage could not be found and was removed (owner to confirm).
const events: { year: string; title: string; description: string; kind: Kind }[] = [
  { year: '2017', title: 'Co-founded The YCOD', description: 'With Henry McLaughlin, Grace Tapani, and Sage Sellers in East Aurora, NY.', kind: 'default' },
  { year: '2018', title: 'Coalition Building', description: 'Established partnerships with WaitList Zero, ONE8FIFTY, and the Chris Klug Foundation.', kind: 'default' },
  { year: '2019', title: 'Legislative Introduction', description: 'Assemblyman David DiPietro introduced A07954, our opt-out organ donation bill, in the NY Assembly in May 2019.', kind: 'legislative' },
  { year: '2020–21', title: 'Media Campaign', description: 'Coverage by WKBW (syndicated to Scripps stations nationally), Spectrum News, and WENY.', kind: 'default' },
  { year: '2021', title: 'Bill Revision', description: 'I drafted the 2021 revision of our presumed consent bill. Senator Patrick Gallivan introduced the Senate version, S4334, in February 2021.', kind: 'legislative' },
  { year: '2021', title: 'Real Heroes Nomination', description: 'Nominated for the American Red Cross Real Heroes Education Award.', kind: 'default' },
  { year: '2022', title: 'Living Donor Support Act Passed', description: 'Advocated for the NYS Living Donor Support Act, which reimburses living organ donors for lost wages, travel, lodging, and child care. Governor Hochul signed it into law in December 2022.', kind: 'milestone' },
  { year: '2022–24', title: 'Continued Advocacy', description: 'Kept up lobbying, social media, and coalition work while at Tulane.', kind: 'default' },
  { year: '2024', title: 'Transition', description: 'After about seven years with The YCOD, I stepped back. The coalition continues.', kind: 'default' },
]

export default function LegislativeTimeline() {
  const { ref, isInView } = useInView(0.05)

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width grid grid-cols-1 lg:grid-cols-12 gap-10 lg:gap-8">
        <motion.div
          initial={{ opacity: 0, y: 20 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6 }}
          className="lg:col-span-4"
        >
          <div className="lg:sticky lg:top-28">
            <span className="font-mono text-xs tracking-widest uppercase text-copper">Legislative Timeline</span>
            <h2 className="font-serif text-heading text-white mt-3">Seven years of work.</h2>
            <ul className="mt-8 space-y-3 text-sm text-titanium">
              <li className="flex items-center gap-3">
                <span aria-hidden="true" className="h-3 w-3 rounded-full bg-verdigris ring-4 ring-verdigris/15" />
                Passed into law
              </li>
              <li className="flex items-center gap-3">
                <span aria-hidden="true" className="h-3 w-3 rounded-full bg-copper" />
                Legislative action
              </li>
              <li className="flex items-center gap-3">
                <span aria-hidden="true" className="h-3 w-3 rounded-full border-2 border-white/25 bg-slate-950" />
                Organizing &amp; outreach
              </li>
            </ul>
          </div>
        </motion.div>

        <ol className="lg:col-span-8">
          {events.map((event, i) => {
            const last = i === events.length - 1
            return (
              <motion.li
                key={`${event.year}-${event.title}`}
                initial={{ opacity: 0, y: 16 }}
                animate={isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.1 + i * 0.07 }}
                className="grid grid-cols-[56px_20px_1fr] md:grid-cols-[80px_24px_1fr] gap-x-3 md:gap-x-5"
              >
                <span className="pt-1 font-mono text-xs text-copper-light text-right tabular-nums">{event.year}</span>

                {/* Rail + node: the line runs through the padded item so nodes connect */}
                <div className="relative flex justify-center">
                  {!last && <span aria-hidden="true" className="absolute top-3 bottom-0 w-px bg-white/[0.12]" />}
                  <span
                    aria-hidden="true"
                    className={`relative mt-1.5 h-3 w-3 rounded-full ${
                      event.kind === 'milestone'
                        ? 'bg-verdigris ring-4 ring-verdigris/15'
                        : event.kind === 'legislative'
                          ? 'bg-copper'
                          : 'border-2 border-white/25 bg-slate-950'
                    }`}
                  />
                </div>

                <div className={last ? 'pb-0' : 'pb-9'}>
                  <div className="flex flex-wrap items-center gap-2">
                    <h3 className="font-serif text-xl text-white">{event.title}</h3>
                    {event.kind === 'milestone' && (
                      <span className="rounded-full border border-verdigris/30 bg-verdigris/10 px-2 py-0.5 text-[11px] font-medium text-verdigris">
                        Passed into law
                      </span>
                    )}
                  </div>
                  <p className="mt-1.5 text-sm text-titanium leading-relaxed max-w-2xl">{event.description}</p>
                </div>
              </motion.li>
            )
          })}
        </ol>
      </div>
    </section>
  )
}
