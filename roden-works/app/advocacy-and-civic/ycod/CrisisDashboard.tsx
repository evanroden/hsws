'use client'

import { motion } from 'framer-motion'
import { useInView, useCountUp } from '@/lib/hooks'

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

export default function CrisisDashboard() {
  const { ref, isInView } = useInView(0.05)
  const waitlistCount = useCountUp(100000, 2500)

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
            The Crisis
          </span>
          <h2 className="font-serif text-heading text-white mt-3">
            The organ donation crisis in numbers.
          </h2>
        </motion.div>

        {/* Main stat cards */}
        <div className="grid grid-cols-1 md:grid-cols-3 gap-6 mb-8">
          {/* Waiting list counter */}
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.1 }}
            className="glass rounded-xl p-6 md:p-8 text-center relative overflow-hidden"
          >
            <motion.div
              className="absolute inset-0 bg-copper/5"
              animate={{ opacity: [0.3, 0.6, 0.3] }}
              transition={{ repeat: Infinity, duration: 2, ease: 'easeInOut' }}
            />
            <div className="relative z-10">
              <span ref={waitlistCount.ref} className="block font-serif text-4xl md:text-5xl text-white">
                {Math.round(waitlistCount.count).toLocaleString()}+
              </span>
              <span className="block mt-2 text-copper text-sm font-mono">
                People on the waiting list
              </span>
              <div className="mt-4 flex items-center justify-center gap-2">
                <motion.div
                  className="w-2 h-2 rounded-full bg-copper"
                  animate={{ scale: [1, 1.3, 1] }}
                  transition={{ repeat: Infinity, duration: 1.5 }}
                />
                <span className="text-titanium/60 text-xs">Live pulse</span>
              </div>
            </div>
          </motion.div>

          {/* Daily deaths */}
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.2 }}
            className="glass rounded-xl p-6 md:p-8 text-center"
          >
            <span className="block font-serif text-4xl md:text-5xl text-white">17</span>
            <span className="block mt-2 text-copper text-sm font-mono">
              People die waiting every day
            </span>
            <p className="mt-4 text-titanium/50 text-xs">
              Approximately 7,500 organs are wasted annually
            </p>
          </motion.div>

          {/* NY rate */}
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.3 }}
            className="glass rounded-xl p-6 md:p-8 text-center border-copper/20"
          >
            <span className="block font-serif text-4xl md:text-5xl text-copper">~37%</span>
            <span className="block mt-2 text-copper text-sm font-mono">
              NY donor designation rate
            </span>
            <p className="mt-4 text-titanium/50 text-xs">
              The lowest in the entire country
            </p>
          </motion.div>
        </div>

        {/* Racial disparity chart */}
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6, delay: 0.4 }}
          className="glass rounded-xl p-6 md:p-8 mb-8"
        >
          <h3 className="font-serif text-xl text-white mb-2">
            Racial disparities in transplant waiting times
          </h3>
          <p className="text-titanium/60 text-sm mb-6">
            60% of all waitlisted patients are people of color. Black Americans make up 27% of the waiting list but only 13% of donors.
          </p>

          <div className="space-y-6">
            {racialDisparity.map((item, i) => (
              <div key={item.group}>
                <div className="flex items-center justify-between mb-2">
                  <span className="text-white text-sm">{item.group}</span>
                  <span className="font-mono text-xs text-titanium">
                    Avg. kidney wait: {item.waitDays.toLocaleString()} days
                  </span>
                </div>
                <div
                  className="h-3 bg-white/5 rounded-full overflow-hidden"
                  role="progressbar"
                  aria-valuenow={item.waitDays}
                  aria-valuemin={0}
                  aria-valuemax={1400}
                  aria-label={`${item.group}: average kidney wait ${item.waitDays.toLocaleString()} days`}
                >
                  <motion.div
                    initial={{ width: 0 }}
                    animate={isInView ? { width: `${(item.waitDays / 1400) * 100}%` } : {}}
                    transition={{ duration: 1, delay: 0.6 + i * 0.15 }}
                    className={`h-full rounded-full ${
                      item.group === 'Black'
                        ? 'bg-gradient-to-r from-copper to-copper/60'
                        : 'bg-gradient-to-r from-titanium/40 to-titanium/20'
                    }`}
                  />
                </div>
              </div>
            ))}
          </div>
        </motion.div>

        {/* State comparison */}
        <motion.div
          initial={{ opacity: 0, y: 30 }}
          animate={isInView ? { opacity: 1, y: 0 } : {}}
          transition={{ duration: 0.6, delay: 0.5 }}
          className="glass rounded-xl p-6 md:p-8"
        >
          <h3 className="font-serif text-xl text-white mb-2">
            Donor registration rates by state
          </h3>
          <p className="text-titanium/60 text-sm mb-6">
            New York consistently ranks last in organ donor designation rate.
          </p>

          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {stateData
              .sort((a, b) => a.rate - b.rate)
              .map((state, i) => (
                <motion.div
                  key={state.state}
                  initial={{ opacity: 0, scale: 0.9 }}
                  animate={isInView ? { opacity: 1, scale: 1 } : {}}
                  transition={{ duration: 0.4, delay: 0.7 + i * 0.08 }}
                  className={`rounded-lg p-4 text-center ${
                    state.state === 'NY'
                      ? 'bg-copper/10 border border-copper/30'
                      : 'bg-white/[0.03]'
                  }`}
                >
                  <span className="font-mono text-xs text-titanium/60">{state.label}</span>
                  <span className={`block font-serif text-2xl mt-1 ${
                    state.state === 'NY' ? 'text-copper' : 'text-white'
                  }`}>
                    {state.rate}%
                  </span>
                </motion.div>
              ))}
          </div>
        </motion.div>
      </div>
    </section>
  )
}
