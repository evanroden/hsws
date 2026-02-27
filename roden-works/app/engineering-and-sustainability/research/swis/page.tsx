'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'

export default function SWISPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)
  const { ref: diagramRef, isInView: diagramInView } = useInView(0.1)
  const { ref: impactRef, isInView: impactInView } = useInView(0.1)
  const [flowRate, setFlowRate] = useState(300)

  // Calculate wedge position: lower flow = further upstream intrusion
  const wedgeExtent = Math.max(
    0,
    Math.min(100, ((300 - flowRate) / 200) * 100)
  )

  return (
    <>
      <BreadcrumbJsonLd
        items={[
          { name: 'Engineering', href: '/engineering-and-sustainability' },
          { name: 'Research', href: '/engineering-and-sustainability/research' },
          { name: 'SWIS' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          {
            label: 'Research',
            href: '/engineering-and-sustainability/research',
          },
          { label: 'SWIS' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1300} />
      </div>

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
        {/* Water wave animation */}
        <div className="absolute inset-0">
          <svg
            className="absolute inset-0 w-full h-full opacity-[0.04]"
            viewBox="0 0 1200 600"
          >
            {Array.from({ length: 3 }).map((_, i) => (
              <motion.path
                key={i}
                d={`M 0 ${300 + i * 40} Q 300 ${260 + i * 40} 600 ${300 + i * 40} T 1200 ${300 + i * 40}`}
                fill="none"
                stroke="#2D5A45"
                strokeWidth="1.5"
                initial={{ pathLength: 0 }}
                animate={{ pathLength: 1 }}
                transition={{
                  duration: 3,
                  delay: i * 0.5,
                  ease: 'easeInOut',
                }}
              />
            ))}
          </svg>
        </div>

        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Environmental Research
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Saltwater Intrusion Study
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              A first-of-kind longitudinal study proposal on saltwater intrusion
              into the Greater New Orleans water supply and its health impacts on
              1.2 million residents.
            </p>
          </motion.div>
        </div>
      </section>

      {/* The 2023 Crisis */}
      <section className="section-padding bg-slate-950" ref={contentRef}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={contentInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                The Crisis
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                When the river could not push back.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  In the fall of 2023, the Mississippi River&apos;s flow dropped
                  to dangerously low levels — reaching 130,000 to 150,000 cubic
                  feet per second when the safe threshold is approximately
                  300,000 cfs. As the river&apos;s freshwater flow weakened, a
                  saltwater wedge from the Gulf of Mexico advanced upstream along
                  the riverbed, threatening to reach the drinking water intake
                  for the Greater New Orleans metropolitan area.
                </p>
                <p>
                  The U.S. Army Corps of Engineers constructed an emergency
                  underwater sill — a physical barrier on the riverbed — to slow
                  the saltwater&apos;s advance. Water utilities issued
                  advisories. The crisis raised existential questions about the
                  long-term viability of New Orleans&apos;s water supply as
                  climate change intensifies drought conditions along the
                  Mississippi.
                </p>
                <p>
                  This was not a hypothetical scenario. For weeks, salinity
                  levels crept toward the intake point. If the wedge had
                  reached the treatment plant at Carrollton, the city would
                  have faced a drinking water emergency for 1.2 million
                  people.
                </p>
              </div>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={contentInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                The Proposal
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                First-of-kind longitudinal study.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  The Saltwater Intrusion Study (SWIS) proposes a longitudinal
                  research framework to examine the health impacts of saltwater
                  intrusion events on municipal water systems — an increasingly
                  urgent research need as climate change alters hydrology
                  patterns across the Mississippi River basin.
                </p>
                <p>
                  The study would track salinity levels, chloride
                  concentrations, and disinfection byproduct formation in
                  treated water during and after intrusion events, correlating
                  these measurements with health outcomes in the exposed
                  population — including hypertension, kidney function, and
                  cardiovascular endpoints.
                </p>
              </div>

              <div className="mt-8 glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">
                  Research Questions
                </h3>
                <ul className="space-y-2 text-sm text-titanium">
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    How do acute saltwater intrusion events affect treated water
                    chemistry?
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    What are the cardiovascular and renal health impacts of
                    elevated sodium in municipal water?
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Do current water treatment processes adequately mitigate
                    salinity during intrusion events?
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    What monitoring infrastructure is needed for early warning
                    and adaptive response?
                  </li>
                </ul>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Animated Saltwater Wedge Cross-Section */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
        ref={diagramRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={diagramInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-8"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Interactive Cross-Section
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Saltwater wedge dynamics.
            </h2>
            <p className="text-titanium mt-2 max-w-2xl">
              As river flow decreases, the denser saltwater from the Gulf of
              Mexico pushes upstream along the riverbed. Drag the slider to
              simulate different flow rates and observe the wedge advancing
              toward New Orleans.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0 }}
            animate={diagramInView ? { opacity: 1 } : {}}
            transition={{ duration: 0.8, delay: 0.3 }}
            className="glass rounded-xl p-6 md:p-8"
          >
            <div className="relative w-full" style={{ paddingBottom: '35%' }}>
              <svg
                viewBox="0 0 200 70"
                className="absolute inset-0 w-full h-full"
              >
                {/* Sky */}
                <rect
                  x="0"
                  y="0"
                  width="200"
                  height="10"
                  fill="rgba(11,18,21,0.5)"
                />

                {/* River banks / surface labels */}
                <text
                  x="5"
                  y="8"
                  className="fill-titanium/30 text-[3px] font-mono"
                >
                  UPSTREAM
                </text>
                <text
                  x="175"
                  y="8"
                  className="fill-titanium/30 text-[3px] font-mono"
                >
                  GULF OF MEXICO
                </text>

                {/* Fresh water layer */}
                <rect
                  x="0"
                  y="10"
                  width="200"
                  height="35"
                  fill="rgba(45,90,69,0.12)"
                />

                {/* River bed / sediment */}
                <rect
                  x="0"
                  y="45"
                  width="200"
                  height="25"
                  fill="rgba(138,155,168,0.08)"
                />
                <text
                  x="100"
                  y="58"
                  textAnchor="middle"
                  className="fill-titanium/15 text-[4px] font-mono"
                >
                  RIVER BED
                </text>

                {/* Animated saltwater wedge */}
                <motion.path
                  d={`M 200 45 L 200 18 Q ${200 - wedgeExtent * 1.5} 28 ${200 - wedgeExtent * 2} 45 Z`}
                  fill="rgba(184,115,51,0.15)"
                  stroke="#B87333"
                  strokeWidth="0.5"
                  animate={{
                    d: `M 200 45 L 200 18 Q ${200 - wedgeExtent * 1.5} 28 ${200 - wedgeExtent * 2} 45 Z`,
                  }}
                  transition={{ duration: 0.3 }}
                />

                {/* Saltwater label */}
                {wedgeExtent > 10 && (
                  <text
                    x={Math.max(155, 200 - wedgeExtent * 1.2)}
                    y="38"
                    className="fill-copper text-[3.5px] font-mono"
                  >
                    SALT WEDGE
                  </text>
                )}

                {/* Freshwater label */}
                <text
                  x="30"
                  y="28"
                  className="fill-forest-light text-[4px] font-mono"
                >
                  FRESHWATER
                </text>

                {/* Flow direction arrow */}
                <defs>
                  <marker
                    id="flow-arrow"
                    viewBox="0 0 10 10"
                    refX="5"
                    refY="5"
                    markerWidth="4"
                    markerHeight="4"
                    orient="auto-start-reverse"
                  >
                    <path d="M 0 0 L 10 5 L 0 10 z" fill="#2D5A45" />
                  </marker>
                </defs>
                <motion.line
                  x1="10"
                  y1="22"
                  x2="50"
                  y2="22"
                  stroke="#2D5A45"
                  strokeWidth="0.5"
                  markerEnd="url(#flow-arrow)"
                  initial={{ pathLength: 0 }}
                  animate={diagramInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1, delay: 0.5 }}
                />
                <text
                  x="30"
                  y="19"
                  textAnchor="middle"
                  className="fill-forest-light/50 text-[2.5px] font-mono"
                >
                  RIVER FLOW
                </text>

                {/* New Orleans marker */}
                <motion.g
                  initial={{ opacity: 0 }}
                  animate={diagramInView ? { opacity: 1 } : {}}
                  transition={{ delay: 1 }}
                >
                  <line
                    x1="60"
                    y1="10"
                    x2="60"
                    y2="14"
                    stroke="#FAFAFA"
                    strokeWidth="0.5"
                  />
                  <circle cx="60" cy="9" r="1.5" fill="#FAFAFA" />
                  <text
                    x="60"
                    y="6"
                    textAnchor="middle"
                    className="fill-white text-[3px] font-serif"
                  >
                    New Orleans
                  </text>
                </motion.g>

                {/* Carrollton intake marker */}
                <motion.g
                  initial={{ opacity: 0 }}
                  animate={diagramInView ? { opacity: 1 } : {}}
                  transition={{ delay: 1.2 }}
                >
                  <line
                    x1="65"
                    y1="14"
                    x2="65"
                    y2="30"
                    stroke="#8A9BA8"
                    strokeWidth="0.3"
                    strokeDasharray="1 0.5"
                  />
                  <text
                    x="65"
                    y="34"
                    textAnchor="middle"
                    className="fill-titanium/40 text-[2px] font-mono"
                  >
                    WATER INTAKE
                  </text>
                </motion.g>

                {/* Emergency sill */}
                {flowRate < 250 && (
                  <motion.g initial={{ opacity: 0 }} animate={{ opacity: 1 }}>
                    <rect
                      x="110"
                      y="38"
                      width="6"
                      height="7"
                      fill="rgba(138,155,168,0.3)"
                      stroke="#8A9BA8"
                      strokeWidth="0.3"
                    />
                    <text
                      x="113"
                      y="37"
                      textAnchor="middle"
                      className="fill-titanium/40 text-[2px] font-mono"
                    >
                      SILL
                    </text>
                  </motion.g>
                )}
              </svg>
            </div>

            {/* Flow rate slider */}
            <div className="mt-6 pt-6 border-t border-white/5">
              <div className="flex items-center justify-between mb-3">
                <span className="text-sm text-titanium">
                  River Flow Rate
                </span>
                <span
                  className={`font-mono text-sm ${
                    flowRate < 200 ? 'text-copper' : 'text-forest-light'
                  }`}
                >
                  {flowRate.toLocaleString()} kcfs
                </span>
              </div>
              <input
                type="range"
                min="100"
                max="400"
                value={flowRate}
                onChange={(e) => setFlowRate(Number(e.target.value))}
                className="w-full accent-copper"
              />
              <div className="flex justify-between text-xs text-titanium/40 font-mono mt-1">
                <span>100 kcfs (Crisis)</span>
                <span>300 kcfs (Safe threshold)</span>
                <span>400 kcfs (Normal)</span>
              </div>
              {flowRate < 200 && (
                <motion.p
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  className="mt-3 text-copper text-sm"
                >
                  At this flow rate, the saltwater wedge threatens the
                  Carrollton drinking water intake for 1.2 million residents of
                  Greater New Orleans.
                </motion.p>
              )}
              {flowRate >= 200 && flowRate < 300 && (
                <motion.p
                  initial={{ opacity: 0 }}
                  animate={{ opacity: 1 }}
                  className="mt-3 text-titanium text-sm"
                >
                  Below the safe threshold. The Army Corps of Engineers may
                  deploy an emergency sill to slow the saltwater advance.
                </motion.p>
              )}
            </div>
          </motion.div>
        </div>
      </section>

      {/* Climate Impact */}
      <section className="section-padding bg-slate-950" ref={impactRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={impactInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Why This Matters
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              A preview of the future.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {[
              {
                stat: '1.2M',
                label: 'Residents at risk',
                description:
                  'The Greater New Orleans metropolitan area depends on Mississippi River water treated at the Carrollton plant.',
              },
              {
                stat: '2x',
                label: 'in 12 years',
                description:
                  'Similar low-flow conditions occurred in 2012 and 2023, suggesting increasing frequency as drought patterns intensify.',
              },
              {
                stat: '0',
                label: 'longitudinal studies',
                description:
                  'No long-term health study has ever tracked the impacts of saltwater intrusion on a municipal water supply population.',
              },
            ].map((item, i) => (
              <motion.div
                key={item.label}
                initial={{ opacity: 0, y: 20 }}
                animate={impactInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="glass rounded-xl p-6 text-center"
              >
                <span className="font-serif text-3xl text-copper">
                  {item.stat}
                </span>
                <span className="block text-white text-sm font-mono mt-1">
                  {item.label}
                </span>
                <p className="text-titanium text-sm mt-3 leading-relaxed">
                  {item.description}
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
      <ProjectNav currentSlug="swis" />
    </>
  )
}
