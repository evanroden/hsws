'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import CoverageChart from './CoverageChart'
import SpeedChart from './SpeedChart'

const coverageData = [
  { area: 'Town of Aurora (Overall)', connected: 62, underserved: 25, unserved: 13 },
  { area: 'East Aurora Village', connected: 89, underserved: 8, unserved: 3 },
  { area: 'Rural Aurora (South)', connected: 34, underserved: 38, unserved: 28 },
  { area: 'Rural Aurora (North)', connected: 41, underserved: 32, unserved: 27 },
  { area: 'Cayuga County Avg.', connected: 55, underserved: 28, unserved: 17 },
]

const speedTiers = [
  { label: 'FCC "Broadband" Minimum', down: 25, up: 3, adequate: false },
  { label: 'Rural Aurora Average', down: 12, up: 1.5, adequate: false },
  { label: 'Urban National Average', down: 195, up: 24, adequate: true },
  { label: 'TABI Target', down: 100, up: 100, adequate: true },
]

const pillars = [
  {
    title: 'Infrastructure Assessment',
    description:
      'Mapped existing broadband infrastructure across the Town of Aurora. Found gaps in fiber, cable, and fixed wireless coverage by checking FCC Form 477 data against resident surveys.',
  },
  {
    title: 'Municipal Broadband Model',
    description:
      'Proposed a publicly owned fiber-to-the-premises (FTTP) network modeled on municipal broadband in Chattanooga, TN and Wilson, NC. Projected cost per household and revenue over 20 years.',
  },
  {
    title: 'Digital Equity Framework',
    description:
      'Proposed subsidized service tiers for low-income households, public Wi-Fi at community centers and libraries, and device lending for households without a computer.',
  },
  {
    title: 'Economic Impact Analysis',
    description:
      'Estimated that closing the broadband gap could raise property values by 3-6%, make remote work possible for 200+ households, and help small businesses in agriculture, tourism, and home-based work.',
  },
]

export default function TabiPage() {
  const heroView = useInView(0.1)
  const gapView = useInView(0.05)
  const speedView = useInView(0.05)
  const pillarsView = useInView(0.05)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'Aurora Broadband' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-copper/5 to-slate-950 overflow-hidden">
        {/* Network background animation */}
        <div className="absolute inset-0">
          <svg className="absolute inset-0 w-full h-full opacity-[0.04]" viewBox="0 0 1200 600">
            {/* Network nodes and connections */}
            {Array.from({ length: 20 }).map((_, i) => {
              const x = 60 + (i % 5) * 270
              const y = 60 + Math.floor(i / 5) * 140
              return (
                <motion.g key={i}>
                  <motion.circle
                    cx={x}
                    cy={y}
                    r="4"
                    fill="#B87333"
                    initial={{ opacity: 0 }}
                    animate={{ opacity: [0, 0.6, 0] }}
                    transition={{ repeat: Infinity, duration: 3, delay: i * 0.3 }}
                  />
                  {i < 15 && (
                    <motion.line
                      x1={x}
                      y1={y}
                      x2={60 + ((i + 5) % 5) * 270}
                      y2={60 + Math.floor((i + 5) / 5) * 140}
                      stroke="#B87333"
                      strokeWidth="0.5"
                      initial={{ pathLength: 0 }}
                      animate={{ pathLength: [0, 1, 0] }}
                      transition={{ repeat: Infinity, duration: 4, delay: i * 0.2 }}
                    />
                  )}
                </motion.g>
              )
            })}
          </svg>
        </div>

        <div className="content-width w-full relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Digital Equity Initiative
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Town of Aurora Broadband Initiative
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              A proposal to close the rural broadband gap in the Town of Aurora, New York, where nearly one in three households lack adequate internet access.
            </p>
          </motion.div>
        </div>
      </section>

      {/* The Problem */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5" ref={heroView.ref}>
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
                The Digital Divide
              </span>
              <h2 className="font-serif text-heading text-white mb-8">
                Rural Aurora is underserved.
              </h2>
            </motion.div>

            <div className="space-y-6 text-titanium leading-relaxed">
              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 }}
              >
                The Town of Aurora is in Western New York. The village of East Aurora has reasonable broadband service, but the rural areas around it do not. Residents in southern and northern Aurora routinely get download speeds below the FCC&apos;s 25/3 Mbps broadband threshold, and many have no wired broadband option at all.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                Students can&apos;t finish homework. Farmers can&apos;t use precision agriculture tools or reach commodity markets online. Elderly residents can&apos;t use telehealth, and home-based businesses have trouble competing.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                I wrote the Town of Aurora Broadband Initiative (TABI) as a proposal to close this gap with municipal fiber, digital equity programs, and community partnerships. It adapts municipal broadband models from other parts of the country to rural Western New York.
              </motion.p>
            </div>
          </div>
        </div>
      </section>

      {/* Animated Coverage Gap Visualization */}
      <section className="section-padding bg-slate-950" ref={gapView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={gapView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Coverage Gap
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Coverage by area.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Coverage is much better in the village than in the rural parts of town. The farther from East Aurora village, the worse it gets.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 24 }}
            animate={gapView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.2 }}
          >
            <CoverageChart data={coverageData} animate={gapView.isInView} />
          </motion.div>
        </div>
      </section>

      {/* Speed Comparison */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={speedView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={speedView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Speed Comparison
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Rural Aurora vs. the rest of the country.
            </h2>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 24 }}
            animate={speedView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.2 }}
          >
            <SpeedChart data={speedTiers} highlight="Rural Aurora Average" animate={speedView.isInView} />
          </motion.div>
        </div>
      </section>

      {/* Proposal Pillars */}
      <section className="section-padding bg-slate-950" ref={pillarsView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={pillarsView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              The Proposal
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Four parts of the proposal.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {pillars.map((pillar, i) => (
              <motion.div
                key={pillar.title}
                initial={{ opacity: 0, y: 30 }}
                animate={pillarsView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 + i * 0.12 }}
                className="glass rounded-xl p-6 md:p-8"
              >
                <div className="flex items-start gap-4">
                  <div className="flex-shrink-0 w-10 h-10 rounded-lg bg-copper/10 border border-copper/20 flex items-center justify-center">
                    <span className="font-mono text-sm text-copper font-bold">{String(i + 1).padStart(2, '0')}</span>
                  </div>
                  <div>
                    <h3 className="font-serif text-lg text-white mb-3">{pillar.title}</h3>
                    <p className="text-titanium text-sm leading-relaxed">{pillar.description}</p>
                  </div>
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Closing */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5">
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              whileInView={{ opacity: 1, y: 0 }}
              viewport={{ once: true }}
              transition={{ duration: 0.6 }}
              className="text-white text-lg font-serif leading-relaxed"
            >
              Without reliable broadband, rural households in Aurora have a harder time with school, healthcare, and work. TABI proposes a way for the town to fix that with public fiber and targeted support for low-income households.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
