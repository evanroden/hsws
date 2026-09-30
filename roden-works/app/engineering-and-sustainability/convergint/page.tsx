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

const cdpJourney: CdpStep[] = [
  {
    phase: 'Week 1-4',
    title: '4-Week Bootcamp',
    location: 'Chicago, IL',
    description:
      'Intensive technical and sales training at Convergint headquarters in Schaumburg. Covered fire alarm, access control, video surveillance, intrusion, and nurse call systems. Earned NICET-adjacent competencies in fire alarm system design and inspection.',
  },
  {
    phase: 'Week 5-8',
    title: 'Field Training',
    location: 'San Francisco, CA',
    description:
      'Shadowed senior account executives and field technicians on live projects. Participated in NFPA 72 inspections, system commissioning, and client discovery meetings. Built technical fluency by spending time in the field before the desk.',
  },
  {
    phase: 'Month 3-11',
    title: 'Account Executive',
    location: 'San Francisco, CA',
    description:
      'Managed a portfolio of commercial and enterprise accounts in the San Francisco Bay Area. Focused on fire alarm system upgrades, inspection/testing/maintenance contracts, and integrated security solutions. Developed proposals using RSMeans estimating and Convergint pricing tools.',
  },
]

export default function ConvergintPage() {
  const { ref: aboutRef, isInView: aboutInView } = useInView(0.1)
  const { ref: diagramRef, isInView: diagramInView } = useInView(0.1)
  const { ref: journeyRef, isInView: journeyInView } = useInView(0.1)

  return (
    <>
      <ArticleJsonLd
        title="Convergint — Fire & Life Safety Systems Integration"
        description="Fire alarm system design, commissioning, and compliance — from bootcamp to field leadership."
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
              The world&apos;s leading service-based systems integrator, protecting
              people and property through fire alarm, life safety, and security
              technologies.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <StatCounter value={2.6} prefix="$" suffix="B" label="Annual Revenue" />
            <StatCounter value={10000} suffix="+" label="Employees" />
            <StatCounter value={220} suffix="+" label="Locations" />
            <StatCounter value={8} suffix=" yrs" label="#1 SDM Ranking" />
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
                #1 systems integrator, eight years running.
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Convergint is a global, service-based systems integrator ranked
                  #1 by SDM Magazine for eight consecutive years. With $2.6 billion
                  in annual revenue, 10,000+ colleagues, and 220+ locations
                  worldwide, Convergint delivers fire alarm, life safety, electronic
                  security, and building automation solutions to commercial,
                  enterprise, healthcare, and government clients.
                </p>
                <p>
                  Their service-first culture is built on colleague-ownership of a
                  set of values and beliefs, not a franchise model. This creates a
                  consistent client experience whether you are in San Francisco,
                  Singapore, or London. Their Career Development Program (CDP)
                  trains new account executives through a rigorous process:
                  technical bootcamp, field mentorship, and progressive account
                  responsibility.
                </p>
                <p>
                  Convergint holds the rare position of being both a
                  technology-agnostic integrator and a certified service partner
                  for every major fire alarm manufacturer — Notifier by Honeywell,
                  Edwards (Kidde), Simplex (Johnson Controls), Siemens, and others.
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
                Account Executive — San Francisco
              </h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>
                  Evan served as an Account Executive in Convergint&apos;s San
                  Francisco office from February through December 2025, entering the
                  company through their selective Career Development Program (CDP).
                </p>
                <p>
                  His work focused on fire and life safety systems for commercial
                  and enterprise clients across the Bay Area — from NFPA 72 system
                  inspections and deficiency remediation to new fire alarm designs,
                  mass notification systems, and integrated security solutions.
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
                    (ITM) per NFPA 72
                  </li>
                  <li className="flex items-start gap-2">
                    <span className="text-copper mt-1">&#8226;</span>
                    Clean agent suppression systems (FM-200, Novec 1230) per NFPA
                    2001
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
              Every component in a fire protection system is part of an
              interconnected network — from detection to notification to
              suppression. Select any device to understand its role, or run the alarm sequence to watch a single event move through the system.
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
              From bootcamp to Bay Area.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              Convergint&apos;s CDP is a structured path from technical training to
              full account ownership — designed to build integrators who understand
              both the technology and the client relationship.
            </p>
          </motion.div>

          <CdpJourney steps={cdpJourney} animate={journeyInView} />
        </div>
      </section>
      <ProjectNav currentSlug="convergint" />
    </>
  )
}
