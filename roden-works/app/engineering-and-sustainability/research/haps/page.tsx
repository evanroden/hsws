'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import { Evidence, Instruments, PollutantProfiles } from './HapsSections'

// Deterministic pseudo-random drift so server and client render the same hero
const rand = (i: number, k: number) => {
  const x = Math.sin(i * 12.9898 + k * 78.233) * 43758.5453
  // rounded so server and browser float math agree exactly
  return Math.round((x - Math.floor(x)) * 1000) / 1000
}
const PARTICLES = Array.from({ length: 30 }, (_, i) => ({
  cx: rand(i, 1) * 1200,
  cy: rand(i, 2) * 600,
  r: 1 + rand(i, 3) * 3,
  dy: -20 - rand(i, 4) * 40,
  dx: rand(i, 5) * 20 - 10,
  dur: 4 + rand(i, 6) * 4,
}))

export default function HAPSPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)

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
            {PARTICLES.map((pt, i) => (
              <motion.circle
                key={i}
                cx={pt.cx}
                cy={pt.cy}
                r={pt.r}
                fill="#B87333"
                initial={{ opacity: 0.2, y: 0 }}
                animate={{
                  opacity: [0.2, 0.6, 0.2],
                  y: [0, pt.dy, 0],
                  x: [0, pt.dx, 0],
                }}
                transition={{
                  repeat: Infinity,
                  duration: pt.dur,
                  delay: i * 0.2,
                  ease: 'easeInOut',
                }}
              />
            ))}
          </svg>
        </div>

        <div className="content-width w-full relative z-10 pb-12 md:pb-16">
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

      <PollutantProfiles />
      <Evidence />
      <Instruments />
      <ProjectNav currentSlug="haps" />
    </>
  )
}
