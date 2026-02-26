'use client'

import { motion } from 'framer-motion'
import { useState, useEffect } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

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
      'Comprehensive mapping of existing broadband infrastructure across the Town of Aurora. Identified gaps in fiber, cable, and fixed wireless coverage using FCC Form 477 data cross-referenced with resident surveys.',
  },
  {
    title: 'Municipal Broadband Model',
    description:
      'Developed a proposal for a publicly-owned fiber-to-the-premises (FTTP) network modeled on successful municipal broadband deployments in Chattanooga, TN and Wilson, NC. Projected cost-per-household and revenue sustainability over a 20-year horizon.',
  },
  {
    title: 'Digital Equity Framework',
    description:
      'Proposed subsidized connectivity tiers for low-income households, a public Wi-Fi program for community centers and libraries, and device lending programs to address the hardware gap alongside the connectivity gap.',
  },
  {
    title: 'Economic Impact Analysis',
    description:
      'Estimated that closing the broadband gap could increase property values by 3-6%, enable remote work opportunities for 200+ households, and support small business growth in agriculture, tourism, and home-based enterprises.',
  },
]

export default function TabiPage() {
  const heroView = useInView(0.1)
  const gapView = useInView(0.05)
  const speedView = useInView(0.05)
  const pillarsView = useInView(0.05)
  const [animationStep, setAnimationStep] = useState(0)

  useEffect(() => {
    if (!gapView.isInView) return
    const interval = setInterval(() => {
      setAnimationStep((prev) => {
        if (prev >= 4) {
          clearInterval(interval)
          return prev
        }
        return prev + 1
      })
    }, 600)
    return () => clearInterval(interval)
  }, [gapView.isInView])

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

        <div className="content-width relative z-10 pb-12 md:pb-16">
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
              Closing the rural broadband gap in Western New York. A digital equity proposal for Cayuga County&apos;s underserved communities where nearly one in three households lack adequate internet access.
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
                Rural broadband is infrastructure, not a luxury.
              </h2>
            </motion.div>

            <div className="space-y-6 text-titanium leading-relaxed">
              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 }}
              >
                The Town of Aurora sits in Western New York, straddling Erie and Cayuga Counties. While the village center of East Aurora has reasonable broadband service, the surrounding rural areas face a stark connectivity gap. Residents in southern and northern Aurora routinely experience download speeds below the FCC&apos;s 25/3 Mbps broadband threshold — and many have no wired broadband option at all.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                This gap is not just an inconvenience. It affects students who cannot complete homework assignments. It affects farmers who cannot access precision agriculture tools or commodity markets. It affects elderly residents who cannot access telehealth services. It affects home-based businesses that cannot compete in an increasingly digital economy.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                The Town of Aurora Broadband Initiative (TABI) is a proposal to close this gap through a combination of municipal fiber infrastructure, digital equity programs, and community partnerships. The initiative draws on successful models from across the country and adapts them to the specific needs of rural Western New York.
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
              Where connectivity falls short.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Broadband coverage across the Town of Aurora varies dramatically between the village center and surrounding rural areas. The further from East Aurora village, the worse the connectivity.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0 }}
            animate={gapView.isInView ? { opacity: 1 } : {}}
            transition={{ duration: 0.8, delay: 0.3 }}
            className="glass rounded-xl p-6 md:p-8"
          >
            {/* Animated bar chart */}
            <div className="space-y-8">
              {coverageData.map((item, i) => (
                <motion.div
                  key={item.area}
                  initial={{ opacity: 0, x: -20 }}
                  animate={animationStep >= i ? { opacity: 1, x: 0 } : {}}
                  transition={{ duration: 0.5 }}
                >
                  <div className="flex items-center justify-between mb-2">
                    <span className="text-white text-sm font-medium">{item.area}</span>
                    <span className="font-mono text-xs text-titanium/60">
                      {item.unserved}% unserved
                    </span>
                  </div>
                  <div className="h-6 bg-white/5 rounded-full overflow-hidden flex">
                    {/* Connected */}
                    <motion.div
                      initial={{ width: 0 }}
                      animate={animationStep >= i ? { width: `${item.connected}%` } : {}}
                      transition={{ duration: 0.8, delay: 0.2 }}
                      className="h-full bg-gradient-to-r from-forest to-forest-light rounded-l-full flex items-center justify-end pr-2"
                    >
                      {item.connected >= 30 && (
                        <span className="text-white/80 text-[10px] font-mono">{item.connected}%</span>
                      )}
                    </motion.div>
                    {/* Underserved */}
                    <motion.div
                      initial={{ width: 0 }}
                      animate={animationStep >= i ? { width: `${item.underserved}%` } : {}}
                      transition={{ duration: 0.8, delay: 0.4 }}
                      className="h-full bg-copper/40 flex items-center justify-center"
                    >
                      {item.underserved >= 15 && (
                        <span className="text-white/60 text-[10px] font-mono">{item.underserved}%</span>
                      )}
                    </motion.div>
                    {/* Unserved */}
                    <motion.div
                      initial={{ width: 0 }}
                      animate={animationStep >= i ? { width: `${item.unserved}%` } : {}}
                      transition={{ duration: 0.8, delay: 0.6 }}
                      className="h-full bg-copper/80 rounded-r-full flex items-center justify-center"
                    >
                      {item.unserved >= 10 && (
                        <span className="text-white/80 text-[10px] font-mono">{item.unserved}%</span>
                      )}
                    </motion.div>
                  </div>
                </motion.div>
              ))}
            </div>

            {/* Legend */}
            <div className="mt-8 pt-6 border-t border-white/5 flex flex-wrap gap-6">
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-forest-light" />
                <span className="text-xs text-titanium">Adequately Served (25+ Mbps)</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-copper/40" />
                <span className="text-xs text-titanium">Underserved (10-25 Mbps)</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-copper/80" />
                <span className="text-xs text-titanium">Unserved (&lt;10 Mbps or None)</span>
              </div>
            </div>
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

          <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-4">
            {speedTiers.map((tier, i) => (
              <motion.div
                key={tier.label}
                initial={{ opacity: 0, y: 30 }}
                animate={speedView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.1 + i * 0.1 }}
                className={`glass rounded-xl p-6 text-center ${
                  tier.label === 'TABI Target'
                    ? 'border-forest/30 bg-forest/10'
                    : !tier.adequate
                    ? 'border-copper/20'
                    : ''
                }`}
              >
                <span className="font-mono text-xs text-copper uppercase tracking-widest block mb-4">
                  {tier.label}
                </span>
                <div className="space-y-3">
                  <div>
                    <span className="font-serif text-3xl text-white">{tier.down}</span>
                    <span className="text-titanium text-sm ml-1">Mbps</span>
                    <span className="block font-mono text-xs text-titanium/60 mt-1">Download</span>
                  </div>
                  <div className="w-12 h-px bg-white/10 mx-auto" />
                  <div>
                    <span className="font-serif text-3xl text-white">{tier.up}</span>
                    <span className="text-titanium text-sm ml-1">Mbps</span>
                    <span className="block font-mono text-xs text-titanium/60 mt-1">Upload</span>
                  </div>
                </div>
                <div className="mt-4 pt-4 border-t border-white/5">
                  <span
                    className={`inline-block px-3 py-1 rounded-full text-xs font-mono ${
                      tier.adequate
                        ? 'bg-forest/20 text-forest-light'
                        : 'bg-copper/10 text-copper'
                    }`}
                  >
                    {tier.adequate ? 'Adequate' : 'Inadequate'}
                  </span>
                </div>
              </motion.div>
            ))}
          </div>
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
              Four pillars of digital equity.
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
              Broadband is the infrastructure of the 21st century. Without it, rural communities like Aurora are locked out of education, healthcare, economic opportunity, and civic participation. The Town of Aurora Broadband Initiative is a proposal to ensure that geography does not determine who gets to participate in the digital economy.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
