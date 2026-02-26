'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

const victories = [
  {
    state: 'New York',
    abbr: 'NY',
    title: 'Climate Leadership & Community Protection Act (CLCPA)',
    description:
      'The most ambitious climate legislation in the country at the time of passage. Mandates 70% renewable electricity by 2030 and net-zero emissions by 2050. Our Climate fellows organized constituent calls, participated in lobby days in Albany, and built grassroots support across Western New York.',
  },
  {
    state: 'Massachusetts',
    abbr: 'MA',
    title: '~$500M for Green Energy Retrofits',
    description:
      'Secured approximately $500 million in funding for green energy building retrofits across the state. Fellows coordinated with local representatives and testified in support of equitable retrofit programs targeting low-income communities and environmental justice neighborhoods.',
  },
  {
    state: 'Oregon',
    abbr: 'OR',
    title: "Governor's Executive Order on Climate",
    description:
      'Supported passage of an executive order establishing emissions reduction targets after the state legislature failed to pass cap-and-trade legislation. Portland-based fellows organized community pressure campaigns and coordinated with state advocacy groups.',
  },
]

const timelineEvents = [
  {
    date: 'Nov 2019',
    title: 'Fellowship Begins',
    description: 'Selected as an Our Climate Fellow. Began training in Portland, Oregon on climate policy communication, legislative strategy, and community organizing.',
  },
  {
    date: 'Dec 2019',
    title: 'State-Level Advocacy Campaigns',
    description: 'Launched coordinated advocacy campaigns across active states. Conducted representative meetings, phone banks, and community outreach events.',
  },
  {
    date: 'Feb 2020',
    title: 'Federal Lobby Day',
    description: 'Traveled to Washington, D.C. for federal-level advocacy. Met with congressional offices to advocate for climate legislation and environmental justice funding.',
  },
  {
    date: 'May 2020',
    title: 'Digital Organizing Pivot',
    description: 'Transitioned all organizing to digital platforms during COVID-19. Led virtual town halls, Zoom lobby meetings, and social media campaigns to sustain momentum.',
  },
  {
    date: 'Jul 2020',
    title: 'Legislative Wins',
    description: 'Celebrated passage of key climate policies across multiple states. The fellowship cohort contributed to victories in New York, Massachusetts, and Oregon.',
  },
  {
    date: 'Oct 2020',
    title: 'Fellowship Concludes',
    description: 'Completed the 12-month fellowship. Continued climate advocacy work through other channels and applied fellowship skills to subsequent civic engagement projects.',
  },
]

// Simplified US state paths for the map visualization
// Active states are highlighted; others are shown as outlines
const usStates: { abbr: string; name: string; cx: number; cy: number }[] = [
  { abbr: 'WA', name: 'Washington', cx: 12, cy: 8 },
  { abbr: 'OR', name: 'Oregon', cx: 11, cy: 15 },
  { abbr: 'CA', name: 'California', cx: 8, cy: 28 },
  { abbr: 'NV', name: 'Nevada', cx: 14, cy: 23 },
  { abbr: 'ID', name: 'Idaho', cx: 18, cy: 14 },
  { abbr: 'MT', name: 'Montana', cx: 24, cy: 8 },
  { abbr: 'WY', name: 'Wyoming', cx: 26, cy: 16 },
  { abbr: 'UT', name: 'Utah', cx: 19, cy: 23 },
  { abbr: 'CO', name: 'Colorado', cx: 27, cy: 24 },
  { abbr: 'AZ', name: 'Arizona', cx: 17, cy: 33 },
  { abbr: 'NM', name: 'New Mexico', cx: 24, cy: 33 },
  { abbr: 'ND', name: 'North Dakota', cx: 34, cy: 8 },
  { abbr: 'SD', name: 'South Dakota', cx: 34, cy: 14 },
  { abbr: 'NE', name: 'Nebraska', cx: 34, cy: 20 },
  { abbr: 'KS', name: 'Kansas', cx: 35, cy: 26 },
  { abbr: 'OK', name: 'Oklahoma', cx: 37, cy: 31 },
  { abbr: 'TX', name: 'Texas', cx: 34, cy: 38 },
  { abbr: 'MN', name: 'Minnesota', cx: 40, cy: 10 },
  { abbr: 'IA', name: 'Iowa', cx: 42, cy: 18 },
  { abbr: 'MO', name: 'Missouri', cx: 43, cy: 26 },
  { abbr: 'AR', name: 'Arkansas', cx: 43, cy: 32 },
  { abbr: 'LA', name: 'Louisiana', cx: 43, cy: 38 },
  { abbr: 'WI', name: 'Wisconsin', cx: 47, cy: 11 },
  { abbr: 'IL', name: 'Illinois', cx: 48, cy: 21 },
  { abbr: 'MS', name: 'Mississippi', cx: 48, cy: 34 },
  { abbr: 'MI', name: 'Michigan', cx: 53, cy: 12 },
  { abbr: 'IN', name: 'Indiana', cx: 53, cy: 21 },
  { abbr: 'AL', name: 'Alabama', cx: 52, cy: 34 },
  { abbr: 'OH', name: 'Ohio', cx: 58, cy: 19 },
  { abbr: 'TN', name: 'Tennessee', cx: 55, cy: 28 },
  { abbr: 'KY', name: 'Kentucky', cx: 57, cy: 25 },
  { abbr: 'GA', name: 'Georgia', cx: 57, cy: 34 },
  { abbr: 'FL', name: 'Florida', cx: 60, cy: 42 },
  { abbr: 'SC', name: 'South Carolina', cx: 62, cy: 31 },
  { abbr: 'NC', name: 'North Carolina', cx: 64, cy: 27 },
  { abbr: 'VA', name: 'Virginia', cx: 65, cy: 23 },
  { abbr: 'WV', name: 'West Virginia', cx: 62, cy: 22 },
  { abbr: 'PA', name: 'Pennsylvania', cx: 67, cy: 17 },
  { abbr: 'NY', name: 'New York', cx: 72, cy: 13 },
  { abbr: 'NJ', name: 'New Jersey', cx: 72, cy: 18 },
  { abbr: 'DE', name: 'Delaware', cx: 71, cy: 21 },
  { abbr: 'MD', name: 'Maryland', cx: 69, cy: 20 },
  { abbr: 'CT', name: 'Connecticut', cx: 76, cy: 15 },
  { abbr: 'RI', name: 'Rhode Island', cx: 78, cy: 15 },
  { abbr: 'MA', name: 'Massachusetts', cx: 78, cy: 13 },
  { abbr: 'VT', name: 'Vermont', cx: 74, cy: 9 },
  { abbr: 'NH', name: 'New Hampshire', cx: 76, cy: 9 },
  { abbr: 'ME', name: 'Maine', cx: 79, cy: 6 },
  { abbr: 'AK', name: 'Alaska', cx: 8, cy: 44 },
  { abbr: 'HI', name: 'Hawaii', cx: 20, cy: 44 },
]

const activeStates = ['NY', 'MA', 'OR']

export default function OurClimatePage() {
  const heroView = useInView(0.1)
  const mapView = useInView(0.05)
  const victoriesView = useInView(0.05)
  const timelineView = useInView(0.05)
  const [hoveredState, setHoveredState] = useState<string | null>(null)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'Our Climate' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-gradient-to-br from-slate-950 via-forest/20 to-slate-950 overflow-hidden">
        <div className="absolute inset-0 overflow-hidden">
          <div
            className="absolute inset-0 opacity-[0.03]"
            style={{
              backgroundImage: 'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
              backgroundSize: '40px 40px',
            }}
          />
        </div>
        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-forest-light mb-4 block">
              Fellowship -- Nov 2019 to Oct 2020
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Our Climate
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              A 12-month fellowship with a youth-led 501(c)(3) organization advocating for equitable climate policy at the state and federal level. Based in Portland, Oregon.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">12</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Month Fellowship</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">3</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">State Victories</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">~$500M</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Green Retrofits (MA)</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">2050</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Net-Zero Target (NY)</span>
            </div>
          </motion.div>
        </div>
      </section>

      {/* Overview */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={heroView.ref}>
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
                About the Fellowship
              </span>
              <h2 className="font-serif text-heading text-white mb-8">
                Youth-led advocacy for climate justice.
              </h2>
            </motion.div>

            <div className="space-y-6 text-titanium leading-relaxed">
              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 }}
              >
                Our Climate is a 501(c)(3) nonprofit that empowers young people to advocate for equitable climate policy. The fellowship program trains cohorts of young organizers in legislative strategy, constituent communication, and grassroots campaign management — then deploys them across states with active climate legislation.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                As a fellow from November 2019 through October 2020, Evan was based in Portland, Oregon and worked on coordinated advocacy campaigns across multiple states. The work included direct lobbying — meeting with state and federal representatives — as well as community outreach, phone banking, digital organizing, and coalition building with environmental justice organizations.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                The fellowship cohort contributed to significant legislative victories in three states: New York, Massachusetts, and Oregon. These wins collectively represented some of the most ambitious climate policy commitments in the United States at the time.
              </motion.p>
            </div>
          </div>
        </div>
      </section>

      {/* US Map Visualization */}
      <section className="section-padding bg-slate-950" ref={mapView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={mapView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Geographic Impact
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Active states with legislative victories.
            </h2>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              Our Climate fellows worked across the country. Highlighted states represent where the cohort achieved measurable policy wins during the 2019-2020 fellowship cycle.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0 }}
            animate={mapView.isInView ? { opacity: 1 } : {}}
            transition={{ duration: 0.8, delay: 0.3 }}
            className="glass rounded-xl p-6 md:p-8"
          >
            <div className="relative w-full" style={{ paddingBottom: '55%' }}>
              <svg viewBox="0 0 88 50" className="absolute inset-0 w-full h-full">
                {/* Background continental outline */}
                <motion.rect
                  x="5"
                  y="3"
                  width="78"
                  height="42"
                  rx="2"
                  fill="none"
                  stroke="rgba(138,155,168,0.08)"
                  strokeWidth="0.3"
                  initial={{ pathLength: 0 }}
                  animate={mapView.isInView ? { pathLength: 1 } : {}}
                  transition={{ duration: 1.5 }}
                />

                {/* State dots */}
                {usStates.map((state, i) => {
                  const isActive = activeStates.includes(state.abbr)
                  const isHovered = hoveredState === state.abbr

                  return (
                    <motion.g
                      key={state.abbr}
                      initial={{ opacity: 0, scale: 0 }}
                      animate={mapView.isInView ? { opacity: 1, scale: 1 } : {}}
                      transition={{ duration: 0.4, delay: 0.5 + i * 0.02 }}
                      onMouseEnter={() => setHoveredState(state.abbr)}
                      onMouseLeave={() => setHoveredState(null)}
                      className="cursor-pointer"
                    >
                      {/* Pulse ring for active states */}
                      {isActive && (
                        <motion.circle
                          cx={state.cx}
                          cy={state.cy}
                          r="3"
                          fill="none"
                          stroke="#2D5A45"
                          strokeWidth="0.3"
                          animate={{ r: [3, 5, 3], opacity: [0.6, 0, 0.6] }}
                          transition={{ repeat: Infinity, duration: 3, delay: activeStates.indexOf(state.abbr) * 1 }}
                        />
                      )}

                      {/* State dot */}
                      <circle
                        cx={state.cx}
                        cy={state.cy}
                        r={isActive ? 2.2 : isHovered ? 1.8 : 1.2}
                        fill={isActive ? '#2D5A45' : isHovered ? 'rgba(138,155,168,0.4)' : 'rgba(138,155,168,0.15)'}
                        className="transition-all duration-300"
                      />
                      {isActive && (
                        <circle cx={state.cx} cy={state.cy} r="0.8" fill="#FAFAFA" />
                      )}

                      {/* State label */}
                      {(isActive || isHovered) && (
                        <text
                          x={state.cx}
                          y={state.cy - (isActive ? 3.5 : 3)}
                          textAnchor="middle"
                          className={`font-mono ${isActive ? 'fill-forest-light' : 'fill-titanium'}`}
                          style={{ fontSize: '2px' }}
                        >
                          {state.abbr}
                        </text>
                      )}
                    </motion.g>
                  )
                })}

                {/* Legend */}
                <circle cx="8" cy="48" r="1.2" fill="#2D5A45" />
                <text x="11" y="48.8" className="fill-titanium font-mono" style={{ fontSize: '2px' }}>
                  Legislative Victory
                </text>
                <circle cx="32" cy="48" r="1.2" fill="rgba(138,155,168,0.15)" />
                <text x="35" y="48.8" className="fill-titanium/50 font-mono" style={{ fontSize: '2px' }}>
                  Other States
                </text>
              </svg>
            </div>

            {/* Active state summary cards */}
            <div className="grid grid-cols-1 md:grid-cols-3 gap-4 mt-6 pt-6 border-t border-white/5">
              {victories.map((v, i) => (
                <motion.div
                  key={v.abbr}
                  initial={{ opacity: 0, y: 20 }}
                  animate={mapView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.5, delay: 1.2 + i * 0.15 }}
                  className="bg-forest/10 border border-forest/20 rounded-lg p-4"
                  onMouseEnter={() => setHoveredState(v.abbr)}
                  onMouseLeave={() => setHoveredState(null)}
                >
                  <span className="font-mono text-xs text-forest-light">{v.abbr}</span>
                  <h3 className="font-serif text-sm text-white mt-1">{v.state}</h3>
                </motion.div>
              ))}
            </div>
          </motion.div>
        </div>
      </section>

      {/* Legislative Victories Detail */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={victoriesView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={victoriesView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Policy Impact
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Three states, three victories.
            </h2>
          </motion.div>

          <div className="space-y-6">
            {victories.map((victory, i) => (
              <motion.div
                key={victory.abbr}
                initial={{ opacity: 0, y: 30 }}
                animate={victoriesView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 + i * 0.15 }}
                className="glass rounded-xl p-6 md:p-8"
              >
                <div className="flex flex-col md:flex-row md:items-start gap-6">
                  <div className="flex-shrink-0">
                    <div className="w-16 h-16 rounded-xl bg-forest/20 border border-forest/30 flex items-center justify-center">
                      <span className="font-mono text-lg text-forest-light font-bold">{victory.abbr}</span>
                    </div>
                  </div>
                  <div className="flex-1">
                    <span className="font-mono text-xs text-copper uppercase tracking-widest">
                      {victory.state}
                    </span>
                    <h3 className="font-serif text-xl text-white mt-2 mb-3">{victory.title}</h3>
                    <p className="text-titanium text-sm leading-relaxed">{victory.description}</p>
                  </div>
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Fellowship Timeline */}
      <section className="section-padding bg-slate-950" ref={timelineView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={timelineView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Fellowship Timeline
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Twelve months of organizing.
            </h2>
          </motion.div>

          <div className="relative">
            <div className="absolute left-4 md:left-8 top-0 bottom-0 w-px bg-forest/30" />
            <div className="space-y-8">
              {timelineEvents.map((event, i) => (
                <motion.div
                  key={i}
                  initial={{ opacity: 0, x: -20 }}
                  animate={timelineView.isInView ? { opacity: 1, x: 0 } : {}}
                  transition={{ duration: 0.5, delay: i * 0.1 }}
                  className="relative pl-12 md:pl-20"
                >
                  <div className="absolute left-2.5 md:left-6.5 w-3 h-3 rounded-full bg-slate-950 border-2 border-forest/50 z-10" />
                  <span className="font-mono text-xs text-forest-light">{event.date}</span>
                  <h3 className="font-serif text-lg text-white mt-1">{event.title}</h3>
                  <p className="text-titanium text-sm mt-1 leading-relaxed">{event.description}</p>
                </motion.div>
              ))}
            </div>
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
              The fellowship ended in October 2020, but the policies it helped pass continue to shape how states approach climate action. The CLCPA remains one of the most comprehensive climate laws in the country. The retrofits funded in Massachusetts are still being deployed. And the organizing skills Evan built during these twelve months informed every civic project that followed.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
