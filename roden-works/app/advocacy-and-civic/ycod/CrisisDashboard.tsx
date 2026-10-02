'use client'

import { motion, useReducedMotion } from 'framer-motion'
import type { ReactNode } from 'react'
import { useInView, useCountUp } from '@/lib/hooks'
import WaitTimeChart from './WaitTimeChart'
import DonorRateChart from './DonorRateChart'

const stateData = [
  { state: 'NY', rate: 37, label: 'New York' },
  { state: 'TX', rate: 52, label: 'Texas' },
  { state: 'CA', rate: 49, label: 'California' },
  { state: 'FL', rate: 56, label: 'Florida' },
  { state: 'PA', rate: 61, label: 'Pennsylvania' },
  { state: 'OH', rate: 64, label: 'Ohio' },
  { state: 'MT', rate: 89, label: 'Montana' },
  { state: 'AK', rate: 87, label: 'Alaska' },
]

const racialDisparity = [
  { group: 'Black', waitDays: 1335, pctWaitlist: 27, pctDonors: 13 },
  { group: 'White', waitDays: 734, pctWaitlist: 35, pctDonors: 60 },
  { group: 'Hispanic', waitDays: 1050, pctWaitlist: 21, pctDonors: 17 },
  { group: 'Asian', waitDays: 900, pctWaitlist: 8, pctDonors: 5 },
]

const NY_RATE = stateData.find((s) => s.state === 'NY')?.rate ?? 37

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
            <StatTile label="People die waiting every day" note="Approximately 7,500 organs are wasted annually">
              17
            </StatTile>
          </motion.div>

          <motion.div {...reveal(0.3)} className="h-full">
            <StatTile
              label="NY donor designation rate"
              note="The lowest in the country"
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

        {/* Charts */}
        <div className="grid grid-cols-1 lg:grid-cols-2 gap-4 md:gap-6">
          <motion.div {...reveal(0.4)} className="h-full">
            <WaitTimeChart data={racialDisparity} highlight="Black" reference="White" animate={isInView} />
          </motion.div>
          <motion.div {...reveal(0.5)} className="h-full">
            <DonorRateChart data={stateData} highlight="NY" animate={isInView} />
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
