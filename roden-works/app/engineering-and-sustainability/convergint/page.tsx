'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import StatCounter from '@/components/ui/StatCounter'
import { BreadcrumbJsonLd, ArticleJsonLd } from '@/components/seo/JsonLd'
import ProjectNav from '@/components/navigation/ProjectNav'
import ReadingTime from '@/components/ui/ReadingTime'

const fireSystemComponents = [
  {
    id: 'smoke-detector',
    label: 'Smoke Detectors',
    x: 10,
    y: 15,
    width: 20,
    height: 20,
    color: '#B87333',
    description:
      'Photoelectric and ionization detectors placed throughout a building to sense smoke particles. Addressable devices report their exact location to the fire alarm control panel for rapid response. NFPA 72 dictates spacing, placement heights, and maintenance intervals.',
  },
  {
    id: 'pull-station',
    label: 'Pull Stations',
    x: 40,
    y: 15,
    width: 20,
    height: 20,
    color: '#C0392B',
    description:
      'Manual fire alarm boxes located at building exits per NFPA 72 requirements. When activated, they send a supervisory signal to the FACP triggering building-wide notification. Double-action stations reduce false alarms in high-traffic environments.',
  },
  {
    id: 'annunciator',
    label: 'Annunciator Panel',
    x: 70,
    y: 15,
    width: 20,
    height: 20,
    color: '#8A9BA8',
    description:
      'The command center for fire response. Graphic annunciator panels display a floor-by-floor map showing the exact zone in alarm. Firefighters use these to pinpoint the origin and direct evacuation. Required at main entrances per AHJ specifications.',
  },
  {
    id: 'sprinkler-riser',
    label: 'Sprinkler Risers',
    x: 10,
    y: 52,
    width: 20,
    height: 22,
    color: '#2D5A45',
    description:
      'Vertical pipes that connect the water supply to the sprinkler system. Each riser serves a zone and includes a tamper switch and flow switch that reports to the FACP. Wet, dry, pre-action, and deluge systems are selected based on the hazard classification per NFPA 13.',
  },
  {
    id: 'clean-agent',
    label: 'Clean Agent Suppression',
    x: 40,
    y: 52,
    width: 20,
    height: 22,
    color: '#2980B9',
    description:
      'Gaseous suppression systems (FM-200, Novec 1230, or Inergen) designed for spaces where water would cause more damage than fire — data centers, museum archives, telecom rooms. The agent suppresses fire by removing heat or displacing oxygen without leaving residue. Governed by NFPA 2001.',
  },
  {
    id: 'facp',
    label: 'Fire Alarm Control Panel',
    x: 70,
    y: 52,
    width: 20,
    height: 22,
    color: '#B87333',
    description:
      'The brain of the fire protection system. The FACP receives signals from every initiating device, processes alarm/trouble/supervisory conditions, activates notification appliances, and communicates with the central monitoring station. Programming defines system behavior — sequences, priorities, and interlocks.',
  },
]

const cdpJourney = [
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
  const [activeComponent, setActiveComponent] = useState<string | null>(null)

  const active = fireSystemComponents.find((c) => c.id === activeComponent)

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

        <div className="content-width relative z-10 pb-12 md:pb-16">
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
              suppression. Click each component to understand its role.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 lg:grid-cols-3 gap-8">
            {/* SVG Diagram */}
            <motion.div
              initial={{ opacity: 0 }}
              animate={diagramInView ? { opacity: 1 } : {}}
              transition={{ duration: 0.8, delay: 0.3 }}
              className="lg:col-span-2 glass rounded-xl p-6 md:p-8"
            >
              <div className="relative w-full" style={{ paddingBottom: '60%' }}>
                <svg
                  viewBox="0 0 100 80"
                  className="absolute inset-0 w-full h-full"
                  fill="none"
                >
                  {/* Title */}
                  <text
                    x="50"
                    y="7"
                    textAnchor="middle"
                    className="fill-white/40 text-[3px] font-serif"
                  >
                    Fire &amp; Life Safety System — Schematic
                  </text>

                  {/* Connection lines — signal flow */}
                  <motion.path
                    d="M 30 25 L 70 63"
                    stroke="#B87333"
                    strokeWidth="0.3"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 0.8 }}
                  />
                  <motion.path
                    d="M 50 35 L 75 52"
                    stroke="#C0392B"
                    strokeWidth="0.3"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 1.0 }}
                  />
                  <motion.path
                    d="M 80 35 L 80 52"
                    stroke="#8A9BA8"
                    strokeWidth="0.3"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 1.2 }}
                  />
                  <motion.path
                    d="M 70 63 L 30 63"
                    stroke="#2D5A45"
                    strokeWidth="0.3"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 1.4 }}
                  />
                  <motion.path
                    d="M 70 63 L 60 63"
                    stroke="#2980B9"
                    strokeWidth="0.3"
                    strokeDasharray="1.5 0.8"
                    initial={{ pathLength: 0 }}
                    animate={diagramInView ? { pathLength: 1 } : {}}
                    transition={{ duration: 1.5, delay: 1.6 }}
                  />

                  {/* Flow label */}
                  <text
                    x="50"
                    y="45"
                    textAnchor="middle"
                    className="fill-titanium/30 text-[2.2px] font-mono"
                  >
                    SIGNAL &amp; NOTIFICATION CIRCUITS
                  </text>

                  {/* Components */}
                  {fireSystemComponents.map((comp, i) => (
                    <motion.g
                      key={comp.id}
                      initial={{ opacity: 0, scale: 0.9 }}
                      animate={diagramInView ? { opacity: 1, scale: 1 } : {}}
                      transition={{ duration: 0.5, delay: 0.5 + i * 0.1 }}
                      onClick={() =>
                        setActiveComponent(
                          activeComponent === comp.id ? null : comp.id
                        )
                      }
                      className="cursor-pointer"
                    >
                      <rect
                        x={comp.x}
                        y={comp.y}
                        width={comp.width}
                        height={comp.height}
                        rx="1.5"
                        fill={
                          activeComponent === comp.id
                            ? comp.color + '30'
                            : 'rgba(255,255,255,0.03)'
                        }
                        stroke={
                          activeComponent === comp.id
                            ? comp.color
                            : 'rgba(255,255,255,0.1)'
                        }
                        strokeWidth={activeComponent === comp.id ? '0.8' : '0.4'}
                        className="transition-all duration-300"
                      />
                      <circle
                        cx={comp.x + comp.width / 2}
                        cy={comp.y + comp.height / 2 - 2}
                        r="3"
                        fill={comp.color + '20'}
                        stroke={comp.color + '60'}
                        strokeWidth="0.3"
                      />
                      <text
                        x={comp.x + comp.width / 2}
                        y={comp.y + comp.height - 2.5}
                        textAnchor="middle"
                        className="fill-white text-[2.2px] font-sans"
                      >
                        {comp.label}
                      </text>
                    </motion.g>
                  ))}
                </svg>
              </div>
            </motion.div>

            {/* Info Panel */}
            <motion.div
              initial={{ opacity: 0, x: 20 }}
              animate={diagramInView ? { opacity: 1, x: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.4 }}
              className="glass rounded-xl p-6"
            >
              {active ? (
                <motion.div
                  key={active.id}
                  initial={{ opacity: 0, y: 10 }}
                  animate={{ opacity: 1, y: 0 }}
                  transition={{ duration: 0.3 }}
                >
                  <div
                    className="w-3 h-3 rounded-full mb-4"
                    style={{ backgroundColor: active.color }}
                  />
                  <h3 className="font-serif text-xl text-white mb-3">
                    {active.label}
                  </h3>
                  <p className="text-titanium text-sm leading-relaxed">
                    {active.description}
                  </p>
                </motion.div>
              ) : (
                <div className="h-full flex flex-col items-center justify-center text-center py-8">
                  <div className="w-12 h-12 rounded-full bg-white/5 flex items-center justify-center mb-4">
                    <svg
                      className="w-6 h-6 text-titanium/40"
                      fill="none"
                      stroke="currentColor"
                      viewBox="0 0 24 24"
                      strokeWidth="1.5"
                    >
                      <path
                        strokeLinecap="round"
                        strokeLinejoin="round"
                        d="M15.042 21.672L13.684 16.6m0 0l-2.51 2.225.569-9.47 5.227 7.917-3.286-.672zM12 2.25V4.5m5.834.166l-1.591 1.591M20.25 10.5H18M7.757 14.743l-1.59 1.591M6 10.5H3.75m4.007-4.243l-1.59-1.591"
                      />
                    </svg>
                  </div>
                  <p className="text-titanium/60 text-sm">
                    Click on a component in the diagram to learn about its role in
                    fire protection.
                  </p>
                </div>
              )}
            </motion.div>
          </div>
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

          <div className="relative">
            {/* Horizontal progress line */}
            <div className="hidden md:block absolute top-12 left-0 right-0 h-px bg-white/10" />

            <div className="grid grid-cols-1 md:grid-cols-3 gap-8">
              {cdpJourney.map((step, i) => (
                <motion.div
                  key={step.title}
                  initial={{ opacity: 0, y: 30 }}
                  animate={journeyInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: i * 0.15 }}
                  className="relative"
                >
                  {/* Step indicator */}
                  <div className="flex items-center gap-4 mb-6">
                    <div className="relative">
                      <motion.div
                        initial={{ scale: 0 }}
                        animate={journeyInView ? { scale: 1 } : {}}
                        transition={{
                          duration: 0.4,
                          delay: 0.3 + i * 0.15,
                        }}
                        className="w-8 h-8 rounded-full bg-copper/20 border border-copper/40 flex items-center justify-center"
                      >
                        <span className="font-mono text-xs text-copper">
                          {i + 1}
                        </span>
                      </motion.div>
                    </div>
                    <span className="font-mono text-xs text-titanium/60">
                      {step.phase}
                    </span>
                  </div>

                  <div className="glass rounded-xl p-6">
                    <h3 className="font-serif text-xl text-white mb-1">
                      {step.title}
                    </h3>
                    <span className="font-mono text-xs text-copper">
                      {step.location}
                    </span>
                    <p className="mt-4 text-titanium text-sm leading-relaxed">
                      {step.description}
                    </p>
                  </div>
                </motion.div>
              ))}
            </div>
          </div>
        </div>
      </section>
      <ProjectNav currentSlug="convergint" />
    </>
  )
}
