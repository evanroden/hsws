'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { BreadcrumbJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import WedgeModel from './WedgeModel'
import { Cite, SourceList } from '@/components/ui/Sources'
import { SWIS_SOURCES as S } from './sources'

export default function SWISPage() {
  const { ref: contentRef, isInView: contentInView } = useInView(0.1)
  const { ref: diagramRef, isInView: diagramInView } = useInView(0.1)
  const { ref: impactRef, isInView: impactInView } = useInView(0.1)

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

        <div className="content-width w-full relative z-10 pb-12 md:pb-16">
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
              A proposed longitudinal study of saltwater intrusion into the
              Greater New Orleans water supply and its effects on residents&apos;
              health.
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
                The 2023 low-water event.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  {/* Softened from "130,000 to 150,000 cfs": 130,000 was a Corps forecast
                      (WWNO, Sept 19, 2023), not a reported flow. Reported flows were
                      ~148,000-150,000 cfs (NBC News; PBS; WWNO live blog). */}
                  In the fall of 2023, the Mississippi River&apos;s flow fell to
                  about 150,000 cubic feet per second, against a safe
                  threshold of roughly 300,000 cfs.<Cite sources={S} id={['pbs', 'wj0929']} /> With less freshwater pushing
                  downstream, a saltwater wedge from the Gulf of Mexico moved
                  upstream along the riverbed toward the drinking water intakes
                  for the Greater New Orleans metropolitan area.
                </p>
                <p>
                  The U.S. Army Corps of Engineers built an emergency underwater
                  sill (a barrier on the riverbed) to slow the saltwater, and
                  water utilities issued advisories.<Cite sources={S} id={['wj0929', 'pbs']} /> The event raised hard
                  questions about the long-term reliability of New
                  Orleans&apos;s water supply if drought on the Mississippi
                  becomes more common.
                </p>
                <p>
                  {/* "close to a million residents in four parishes": https://www.pbs.org/newshour/nation/why-salt-water-is-threatening-drinking-water-in-new-orleans-and-what-officials-are-doing-about-it */}
                  For weeks, salinity crept toward the intakes. Officials
                  warned that the wedge threatened drinking water for close to
                  a million residents across four parishes, including the
                  Carrollton plant that serves New Orleans&apos;s east bank.<Cite sources={S} id="pbs" />
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
                A longitudinal health study.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  The Saltwater Intrusion Study (SWIS) proposes following a
                  population over time to measure how saltwater intrusion into a
                  municipal water system affects health. Climate change is
                  shifting river flows across the Mississippi basin, so these
                  events are worth studying now.
                </p>
                <p>
                  The study would track salinity, chloride, and disinfection
                  byproducts in treated water during and after intrusion events,
                  and compare those measurements with health outcomes in the
                  exposed population, including hypertension, kidney function,
                  and other cardiovascular endpoints.
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
        className="section-padding bg-slate-950"
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
              As river flow drops, denser saltwater from the Gulf of Mexico
              pushes upstream along the riverbed. Drag the slider to change the
              flow rate and watch the wedge move toward New Orleans.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 24 }}
            animate={diagramInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.15 }}
          >
            <WedgeModel />
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
              The case for a long-term study.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
            {[
              {
                // PBS NewsHour, Sept 2023: "close to a million residents in four parishes"
                stat: '~1M',
                cite: 'pbs',
                label: 'Residents at risk',
                description:
                  'In 2023, officials said the saltwater wedge threatened drinking water for close to a million people in four parishes that draw from the Mississippi.',
              },
              {
                // The Army Corps has built emergency saltwater sills near
                // Myrtle Grove (RM 64) in 1988, 1999, 2012, 2022 and 2023.
                // Source: USACE release via GOHSEP, Aug. 29, 2024
                // https://gohsep.la.gov/about/news/usace-to-construct-underwater-sill-to-arrest-saltwater-progression-into-mississippi-river/
                stat: '5',
                cite: 'gohsep',
                label: 'sills since 1988',
                description:
                  'The Army Corps has built emergency underwater sills near Myrtle Grove in 1988, 1999, 2012, 2022 and 2023, the last two years running.',
              },
              {
                // Softened from an unsourced absolute ("No long-term health
                // study has ever..."). Framed as the author's own search and
                // the gap this proposal is designed to fill.
                stat: 'New',
                cite: undefined,
                label: 'longitudinal study',
                description:
                  'I found no long-term health study tracking the impacts of saltwater intrusion on a municipal water supply population. This proposal is designed to fill that gap.',
              },
            ].map((item, i) => (
              <motion.div
                key={item.label}
                initial={{ opacity: 0, y: 20 }}
                animate={impactInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.5, delay: 0.2 + i * 0.15 }}
                className="glass rounded-xl p-6 text-center"
              >
                <span className="block font-sans font-semibold tracking-tight text-4xl text-white">
                  {item.stat}
                </span>
                <span className="block text-titanium text-xs font-mono uppercase tracking-wider mt-2">
                  {item.label}
                </span>
                <p className="text-titanium text-sm mt-3 leading-relaxed">
                  {item.description}
                  {item.cite && <Cite sources={S} id={item.cite} />}
                </p>
              </motion.div>
            ))}
          </div>
        </div>
      </section>
      <SourceList sources={S} />
      <ProjectNav currentSlug="swis" />
    </>
  )
}
