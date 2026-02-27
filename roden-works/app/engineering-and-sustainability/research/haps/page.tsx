'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'

const pollutants = [
  {
    name: 'PM2.5',
    level: 85,
    color: '#B87333',
    description:
      'Fine particulate matter smaller than 2.5 micrometers in diameter. These particles penetrate deep into lung alveoli and cross into the bloodstream, triggering systemic inflammatory responses. Sources include cooking, candles, incense, and tobacco smoke.',
  },
  {
    name: 'Black Carbon',
    level: 72,
    color: '#2D5A45',
    description:
      'A component of soot produced by incomplete combustion of fossil fuels, biomass, and cooking fuels. The study found that participants in the highest quartile of black carbon exposure had a clinically significant +2 mmHg increase in systolic blood pressure.',
  },
  {
    name: 'NO2',
    level: 60,
    color: '#8A9BA8',
    description:
      'Nitrogen dioxide generated primarily by gas stoves and space heaters in indoor environments. NO2 irritates the airways, exacerbates asthma, and contributes to chronic respiratory disease — particularly dangerous in poorly ventilated homes.',
  },
]

export default function HAPSPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)
  const { ref: pollutantRef, isInView: pollutantInView } = useInView(0.1)
  const { ref: findingRef, isInView: findingInView } = useInView(0.1)
  const { ref: methodRef, isInView: methodInView } = useInView(0.1)

  return (
    <>
      <BreadcrumbJsonLd
        items={[
          { name: 'Engineering', href: '/engineering-and-sustainability' },
          { name: 'Research', href: '/engineering-and-sustainability/research' },
          { name: 'HAPS' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          {
            label: 'Research',
            href: '/engineering-and-sustainability/research',
          },
          { label: 'HAPS' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1100} />
      </div>

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-clinical/5 to-slate-950 overflow-hidden">
        {/* Particle drift animation */}
        <div className="absolute inset-0">
          <svg
            className="absolute inset-0 w-full h-full opacity-[0.06]"
            viewBox="0 0 1200 600"
          >
            {Array.from({ length: 30 }).map((_, i) => (
              <motion.circle
                key={i}
                cx={Math.random() * 1200}
                cy={Math.random() * 600}
                r={1 + Math.random() * 3}
                fill="#B87333"
                initial={{ opacity: 0.2, y: 0 }}
                animate={{
                  opacity: [0.2, 0.6, 0.2],
                  y: [0, -20 - Math.random() * 40, 0],
                  x: [0, Math.random() * 20 - 10, 0],
                }}
                transition={{
                  repeat: Infinity,
                  duration: 4 + Math.random() * 4,
                  delay: i * 0.2,
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
              Environmental Health Research
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Household Air Pollution Study
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              Investigating indoor exposure to PM2.5, black carbon, and NO
              <sub>2</sub> in New Orleans homes — and their cardiovascular
              consequences.
            </p>
          </motion.div>
        </div>
      </section>

      {/* About Section */}
      <section className="section-padding bg-slate-950" ref={contentRef}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={contentInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Led by Dr. Felicia Rabito
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                The air inside your home.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  The Household Air Pollution Study (HAPS) at Tulane
                  University&apos;s Weatherhead School of Public Health measured
                  indoor concentrations of PM2.5, black carbon, and nitrogen
                  dioxide across homes in New Orleans — a city where
                  environmental justice and health disparities intersect with
                  particular urgency.
                </p>
                <p>
                  Led by Dr. Felicia Rabito, the study deployed environmental
                  monitors in participant homes to capture continuous exposure
                  data, correlating pollutant concentrations with biologic
                  assessments including ambulatory blood pressure monitoring,
                  respiratory function tests, and inflammatory biomarker analysis.
                </p>
                <p>
                  New Orleans presents a uniquely important study environment:
                  aging housing stock, high rates of gas stove usage, subtropical
                  humidity affecting ventilation patterns, and communities
                  already facing disproportionate cardiovascular disease burden.
                </p>
              </div>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={contentInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Role
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                Research Assistant
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Evan supported the study through environmental data collection,
                  monitor deployment and retrieval, participant coordination, and
                  data processing. Work included deploying PM2.5 and NO
                  <sub>2</sub> monitors in homes, maintaining quality assurance
                  protocols, and assisting with the data pipeline from raw sensor
                  output to analyzable datasets.
                </p>
                <p>
                  This research provided direct exposure to the intersection of
                  engineering measurement and public health — understanding that
                  the sensors and data systems are only meaningful when they
                  connect to human outcomes.
                </p>
              </div>

              <div className="mt-8 glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">
                  Study Design
                </h3>
                <ul className="space-y-2 text-sm text-titanium">
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Environmental monitoring: continuous PM2.5, black carbon, and
                    NO<sub>2</sub> sensors deployed in homes
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Biologic assessment: ambulatory blood pressure, respiratory
                    function, inflammatory markers
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Population: New Orleans residents in neighborhoods with
                    elevated environmental health risks
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Analysis: quartile-based exposure-response modeling for
                    cardiovascular endpoints
                  </li>
                </ul>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Pollutant Profiles */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-copper/5"
        ref={pollutantRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={pollutantInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Pollutant Profiles
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              What you breathe at home matters.
            </h2>
          </motion.div>

          <div className="space-y-6">
            {pollutants.map((p, i) => (
              <motion.div
                key={p.name}
                initial={{ opacity: 0, y: 20 }}
                animate={pollutantInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: i * 0.15 }}
                className="glass rounded-xl p-6"
              >
                <div className="flex justify-between items-start mb-3">
                  <div>
                    <h3 className="font-serif text-xl text-white">
                      {p.name === 'NO2' ? (
                        <>
                          NO<sub>2</sub>
                        </>
                      ) : (
                        p.name
                      )}
                    </h3>
                  </div>
                  <span className="font-mono text-xs text-titanium/60">
                    Relative indoor exposure
                  </span>
                </div>
                <p className="text-titanium text-sm leading-relaxed mb-4">
                  {p.description}
                </p>
                <div className="h-2 bg-white/5 rounded-full overflow-hidden">
                  <motion.div
                    initial={{ width: 0 }}
                    animate={
                      pollutantInView ? { width: `${p.level}%` } : {}
                    }
                    transition={{ duration: 1, delay: 0.5 + i * 0.2 }}
                    className="h-full rounded-full"
                    style={{
                      background: `linear-gradient(90deg, ${p.color}60, ${p.color})`,
                    }}
                  />
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Key Finding */}
      <section className="section-padding bg-slate-950" ref={findingRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={findingInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Published Finding
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              The evidence.
            </h2>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, scale: 0.98 }}
            animate={findingInView ? { opacity: 1, scale: 1 } : {}}
            transition={{ duration: 0.6, delay: 0.2 }}
            className="glass rounded-xl p-8 md:p-12 border-copper/20 text-center"
          >
            <motion.p
              initial={{ opacity: 0, y: 10 }}
              animate={findingInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.5, delay: 0.4 }}
              className="text-copper text-5xl md:text-6xl font-serif"
            >
              +2 mmHg
            </motion.p>
            <p className="text-white text-lg font-serif mt-4">
              Systolic blood pressure increase
            </p>
            <p className="text-titanium text-sm mt-2 max-w-lg mx-auto">
              Participants in the highest quartile of black carbon exposure
              showed a 2 mmHg increase in systolic blood pressure — a clinically
              significant elevation when sustained across a population,
              particularly in communities already facing disproportionate
              cardiovascular disease burden.
            </p>
            <div className="mt-8 pt-6 border-t border-white/5">
              <p className="text-titanium/40 text-xs font-mono">
                A 2 mmHg population-level increase in systolic BP is associated
                with a 7% increase in ischemic heart disease mortality and a 10%
                increase in stroke mortality (Lewington et al., Lancet 2002)
              </p>
            </div>
          </motion.div>
        </div>
      </section>

      {/* Methodology */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
        ref={methodRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={methodInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Methodology
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Measurement instruments.
            </h2>
          </motion.div>

          <div className="grid grid-cols-2 md:grid-cols-4 gap-4">
            {[
              {
                instrument: 'pDR-1500',
                measures: 'Real-time PM2.5',
              },
              {
                instrument: 'MicroAeth AE51',
                measures: 'Black carbon',
              },
              {
                instrument: 'Ogawa Samplers',
                measures: 'Passive NO2',
              },
              {
                instrument: 'Ambulatory BP Monitor',
                measures: '24-hr blood pressure',
              },
            ].map((item, i) => (
              <motion.div
                key={item.instrument}
                initial={{ opacity: 0, y: 20 }}
                animate={methodInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: i * 0.1 }}
                className="glass rounded-lg p-4 text-center"
              >
                <span className="text-white text-sm block">
                  {item.instrument}
                </span>
                <span className="text-titanium/50 text-xs font-mono mt-1 block">
                  {item.measures}
                </span>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
      <ProjectNav currentSlug="haps" />
    </>
  )
}
