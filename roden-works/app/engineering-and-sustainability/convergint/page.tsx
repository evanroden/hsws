'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import StatCounter from '@/components/ui/StatCounter'
import { BreadcrumbJsonLd, ArticleJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'
import FireSystemDiagram from './FireSystemDiagram'
import CdpJourney, { type CdpStep } from './CdpJourney'
import { Cite, SourceList } from '@/components/ui/Sources'
import { CONVERGINT_SOURCES as S } from './sources'

const cdpJourney: CdpStep[] = [
  {
    phase: 'Week 1-4',
    title: '4-Week Bootcamp',
    // Convergint HQ was in Schaumburg, IL until Oct 2025 (https://dailyherald.com/?p=1301823)
    location: 'Schaumburg, IL',
    description:
      'Technical and sales training at Convergint headquarters in Schaumburg. We covered fire alarm, access control, video surveillance, intrusion, and nurse call systems, with a focus on fire alarm design and inspection.',
    cite: <Cite sources={S} id={['daily-herald-hq', 'ssn-schaumburg']} />,
  },
  {
    phase: 'Week 5-8',
    title: 'Field Training',
    location: 'San Francisco, CA',
    description:
      'I shadowed senior account executives and field technicians on live projects, and sat in on NFPA 72 inspections, system commissioning, and client discovery meetings before taking on accounts.',
  },
  {
    phase: 'Month 3-11',
    title: 'Account Executive',
    location: 'San Francisco, CA',
    description:
      'I managed commercial and enterprise accounts in the San Francisco Bay Area, mostly fire alarm upgrades, inspection/testing/maintenance contracts, and integrated security systems. I built proposals with RSMeans estimating and Convergint pricing tools.',
  },
]

export default function ConvergintPage() {
  const { ref: aboutRef, isInView: aboutInView } = useInView(0.1)
  const { ref: diagramRef, isInView: diagramInView } = useInView(0.1)
  const { ref: journeyRef, isInView: journeyInView } = useInView(0.1)

  return (
    <>
      <ArticleJsonLd
        title="Convergint: Fire & Life Safety Systems Integration"
        description="My year as a fire and life safety account executive at Convergint, from the Career Development Program bootcamp to Bay Area accounts."
        path="/engineering-and-sustainability/convergint"
      />
      <BreadcrumbJsonLd
        items={[
          { name: 'Engineering', href: '/engineering-and-sustainability' },
          { name: 'Convergint' },
        ]}
      />
      <Breadcrumbs
        items={[
          { label: 'Engineering', href: '/engineering-and-sustainability' },
          { label: 'Convergint' },
        ]}
      />
      <div className="content-width -mt-2 mb-4">
        <ReadingTime wordCount={1200} />
      </div>

      {/* Hero Section */}
      <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
        {/* Animated alarm pulse background */}
        <div className="absolute inset-0">
          <svg className="absolute inset-0 w-full h-full opacity-[0.04]" viewBox="0 0 1200 600">
            {Array.from({ length: 6 }).map((_, i) => (
              <motion.circle
                key={i}
                cx={200 + i * 160}
                cy={300}
                r="80"
                fill="none"
                stroke="#C0392B"
                strokeWidth="0.5"
                initial={{ r: 20, opacity: 0.6 }}
                animate={{ r: [20, 80, 20], opacity: [0.6, 0, 0.6] }}
                transition={{
                  repeat: Infinity,
                  duration: 4,
                  delay: i * 0.7,
                  ease: 'easeOut',
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
              Fire &amp; Life Safety Systems Integration
            </span>
            <h1 className="font-serif text-display text-white max-w-3xl">
              Convergint
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              A service-based systems integrator for fire alarm, life safety,
              and security systems.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            {/* All four per Convergint's July 2025 release (during my tenure): "$2.6 billion", "over 10,000
                colleagues", "more than 220 locations", #1 in SDM's Top Systems Integrators for the eighth year
                in a row. https://www.convergint.com/press-releases/convergint-named-1-systems-integrator-by-sdm-magazine-for-eighth-year-in-a-row/ */}
            <StatCounter value={2.6} prefix="$" suffix="B" label="Annual Revenue (2025)" cite={<Cite sources={S} id="sdm-2025" />} />
            <StatCounter value={10000} suffix="+" label="Employees" cite={<Cite sources={S} id="sdm-2025" />} />
            <StatCounter value={220} suffix="+" label="Locations" cite={<Cite sources={S} id="sdm-2025" />} />
            <StatCounter value={8} suffix=" yrs" label="#1 in SDM Rankings (thru 2025)" cite={<Cite sources={S} id="sdm-2025" />} />
          </motion.div>
        </div>
      </section>

      {/* About Section */}
      <section className="section-padding bg-slate-950" ref={aboutRef}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={aboutInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                About Convergint
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                A global systems integrator.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Convergint is a global, service-based systems integrator that
                  SDM Magazine ranked the #1 systems integrator for the eighth
                  year in a row in 2025. As of 2025 it reported $2.6 billion in
                  revenue, 10,000+ colleagues, and 220+ locations worldwide.<Cite sources={S} id="sdm-2025" /> It
                  installs and services fire alarm, life safety, electronic
                  security, and building automation systems for commercial,
                  enterprise, healthcare, and government clients.
                </p>
                <p>
                  Every office works from the same set of company values, which
                  keeps service consistent from one city to the next. The Career Development Program (CDP) trains
                  new account executives with a technical bootcamp, then field
                  mentorship, then a growing set of accounts.
                </p>
                {/* Edwards: "one of the largest Edwards partners in the world", https://www.convergint.com/edwards/
                    (seen in search index; the page returned 404 when re-checked Oct 2026)
                    Replacement source: "one of the largest Edwards dealers in the world",
                    https://www.securityworldmarket.com/na/News/Business-News/convergint-expands-partnership-with-edwards-across-south-carolina
                    Honeywell and Silent Knight: https://old.convergint.com/?p=197304 (domain no longer resolves, Oct 2026;
                    no replacement found, so the Honeywell/Silent Knight clause was removed). Notifier, Simplex and Siemens
                    certified-partner claims could not be verified and were removed. */}
                <p>
                  Convergint is one of the largest dealers in the world for
                  Edwards fire alarm systems.<Cite sources={S} id="edwards" />
                </p>
              </div>
            </motion.div>

            <motion.div
              initial={{ opacity: 0, y: 30 }}
              animate={aboutInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.2 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
                Role
              </span>
              <h2 className="font-serif text-heading text-white mb-6">
                Account Executive, San Francisco
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  I was an Account Executive in Convergint&apos;s San Francisco
                  office from February through December 2025. I joined through the
                  Career Development Program (CDP).
                </p>
                <p>
                  My work covered fire and life safety systems for commercial and
                  enterprise clients across the Bay Area: NFPA 72 inspections and
                  deficiency remediation, new fire alarm designs, mass notification
                  systems, and integrated security.
                </p>
              </div>

              <div className="mt-8 glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">
                  Core Competencies
                </h3>
                <ul className="space-y-2 text-sm text-titanium">
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Fire alarm system design, inspection, testing &amp; maintenance
                    (ITM) per NFPA 72<Cite sources={S} id="nfpa-72" />
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Clean agent suppression systems (FM-200, Novec 1230) per NFPA
                    2001<Cite sources={S} id="nfpa-2001" />
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Mass notification and emergency communication systems
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Access control, video surveillance, and intrusion detection
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Proposal development using RSMeans estimating and Convergint
                    tools
                  </li>
                </ul>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Interactive Fire Safety System Diagram */}
      <section
        className="section-padding bg-gradient-to-b from-slate-950 to-forest/5"
        ref={diagramRef}
      >
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={diagramInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Interactive Diagram
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Anatomy of a fire &amp; life safety system.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Detection, notification, and suppression devices all report to one
              fire alarm control panel. Select a device to see what it does, or run the alarm sequence to follow a single event through the system.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 24 }}
            animate={diagramInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.15 }}
          >
            <FireSystemDiagram />
          </motion.div>
        </div>
      </section>

      {/* CDP Journey */}
      <section className="section-padding bg-slate-950" ref={journeyRef}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={journeyInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-16"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Career Development Program
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              How the program worked.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              The CDP moves new hires from technical training to owning their own
              accounts, so they learn the systems before they sell them.
            </p>
          </motion.div>

          <CdpJourney steps={cdpJourney} animate={journeyInView} />
        </div>
      </section>
      <SourceList sources={S} />
      <ProjectNav currentSlug="convergint" />
    </>
  )
}
