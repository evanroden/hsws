'use client'

import { motion, useReducedMotion } from 'framer-motion'
import type { ReactNode } from 'react'
import { useInView, useCountUp } from '@/lib/hooks'
// Sources for every figure on this section:
// - Waiting list (100,000+), 17 deaths/day, a new person added every 8 minutes: HRSA,
//   https://www.organdonor.gov/learn/organ-donation-statistics
// - NY ~37% registered, then the lowest opt-in rate of any state: WKBW (Olivia Proia),
//   https://www.wxyz.com/news/national/college-freshmen-in-new-york-develop-plan-to-encourage-more-organ-donors
//   and Spectrum News, https://mynews13.com/fl/orlando/news/2021/09/24/wny-teens-nominated-for-american-red-cross-award-for-organ-donation-coalition
// - NY passed 50% registered in 2024; national average 64%: City & State,
//   https://cityandstateny.com/opinion/2025/03/opinion-new-york-reached-major-health-milestone-we-cannot-take-our-foot-gas/403592
// - Black Americans ~27% of the waiting list, ~12% of organ donors: HHS Office of Minority Health,
//   https://minorityhealth.hhs.gov/organ-transplants-and-blackafrican-americans
// - People of color 40% of the U.S. population but 60% of the waiting list (OPTN, Sept 2023): HRSA,
//   https://www.organdonor.gov/sites/default/files/organ-donor/professional/materials/lets-talk-donor-diversity-infographic-english.pdf
// The earlier wait-time-by-race and state-rate charts were removed: their figures could not be traced
// to a source (the 1,335 vs 734 day kidney wait is a 2009 University of Maryland figure comparing Black
// patients with all other patients, not with white patients).
const NY_RATE = 37

export default function CrisisDashboard() {
  const { ref, isInView } = useInView(0.05)
  const reduceMotion = useReducedMotion()
  // Initial render must match the server, so reduced motion only shortens the count-up
  const waitlistCount = useCountUp(100000, reduceMotion ? 1 : 2500)
  const waitlist = Math.round(waitlistCount.count)

  const reveal = (delay: number) => ({
    initial: { opacity: 0, y: 24 },
    animate: isInView ? { opacity: 1, y: 0 } : {},
    transition: { duration: reduceMotion ? 0 : 0.6, delay: reduceMotion ? 0 : delay },
  })

  return (
    <section className="section-padding bg-slate-950" ref={ref}>
      <div className="content-width">
        <motion.div {...reveal(0)} className="mb-10 md:mb-12">
          <span className="font-mono text-xs tracking-widest uppercase text-copper">The Crisis</span>
          <h2 className="font-serif text-heading text-white mt-3">The organ donation crisis in numbers.</h2>
        </motion.div>

        {/* KPI row */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-4 md:gap-6 mb-4 md:mb-6">
          <motion.div {...reveal(0.1)} className="h-full">
            <StatTile label="People on the waiting list" note="Americans waiting for a transplant at any given time">
              <span className="sr-only">100,000+</span>
              <span ref={waitlistCount.ref} aria-hidden="true">
                {waitlist.toLocaleString('en-US')}+
              </span>
            </StatTile>
          </motion.div>

          <motion.div {...reveal(0.2)} className="h-full">
            <StatTile label="People die waiting every day" note="Another person is added to the national waiting list every 8 minutes">
              17
            </StatTile>
          </motion.div>

          <motion.div {...reveal(0.3)} className="h-full">
            <StatTile
              label="NY donor registration rate when we started"
              note="Then the lowest in the country. New York passed 50% in 2024, still below the 64% national average."
              meter={
                <div className="mt-5 h-1.5 w-full overflow-hidden rounded-full bg-copper/15" aria-hidden="true">
                  <motion.div
                    className="h-full rounded-full bg-copper"
                    initial={{ width: '0%' }}
                    animate={isInView ? { width: `${NY_RATE}%` } : {}}
                    transition={{ duration: reduceMotion ? 0 : 1, delay: reduceMotion ? 0 : 0.6, ease: [0.16, 1, 0.3, 1] }}
                  />
                </div>
              }
            >
              ~{NY_RATE}%
            </StatTile>
          </motion.div>
        </div>

        {/* Context (prose in place of the earlier unsourced charts) */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 md:gap-6">
          <motion.div {...reveal(0.4)} className="h-full">
            <div className="h-full rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-7">
              <h3 className="font-serif text-xl text-white">Who waits</h3>
              <p className="mt-3 text-sm leading-relaxed text-titanium">
                People of color are about 40% of the U.S. population but 60% of the transplant waiting list. Black Americans make up about 27% of the waiting list but only about 12% of organ donors.
              </p>
              <p className="mt-4 text-xs leading-relaxed text-muted">Sources: HRSA (OPTN data, Sept. 2023); HHS Office of Minority Health.</p>
            </div>
          </motion.div>
          <motion.div {...reveal(0.5)} className="h-full">
            <div className="h-full rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-7">
              <h3 className="font-serif text-xl text-white">New York&apos;s registry</h3>
              <p className="mt-3 text-sm leading-relaxed text-titanium">
                When we started, only about 37% of New Yorkers were registered donors, the lowest opt-in rate of any state. The rate has risen since: New York passed 50% in 2024, but it still trails the national average of 64%.
              </p>
              <p className="mt-4 text-xs leading-relaxed text-muted">Sources: WKBW and Spectrum News coverage of The YCOD; City &amp; State (2025).</p>
            </div>
          </motion.div>
        </div>
      </div>
    </section>
  )
}

function StatTile({
  label,
  note,
  meter,
  children,
}: {
  label: string
  note?: string
  meter?: ReactNode
  children: ReactNode
}) {
  return (
    <div className="h-full rounded-2xl border border-white/[0.08] bg-surface p-6 md:p-7">
      <p className="text-sm text-titanium">{label}</p>
      <p className="mt-3 font-sans text-4xl md:text-5xl font-semibold tracking-tight leading-none text-white">
        {children}
      </p>
      {meter}
      {note && <p className="mt-4 text-xs leading-relaxed text-muted">{note}</p>}
    </div>
  )
}
