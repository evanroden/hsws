'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import StateTileMap from './StateTileMap'

const victories = [
  {
    state: 'New York',
    abbr: 'NY',
    title: 'Climate Leadership & Community Protection Act (CLCPA)',
    description:
      'Requires 70% renewable electricity by 2030 and net-zero emissions by 2050. Our Climate fellows organized constituent calls, joined lobby days in Albany, and built local support across Western New York.',
  },
  {
    state: 'Massachusetts',
    abbr: 'MA',
    title: '~$500M for Green Energy Retrofits',
    description:
      'About $500 million in state funding for green energy building retrofits. Fellows worked with local representatives and testified for retrofit programs aimed at low-income communities and environmental justice neighborhoods.',
  },
  {
    state: 'Oregon',
    abbr: 'OR',
    title: "Governor's Executive Order on Climate",
    description:
      'After the state legislature failed to pass cap-and-trade, the governor issued an executive order setting emissions reduction targets. Portland-based fellows organized community pressure and worked with state advocacy groups.',
  },
]

const timelineEvents = [
  {
    date: 'Nov 2019',
    title: 'Fellowship Begins',
    description: 'Selected as an Our Climate Fellow. Started training in Portland, Oregon on climate policy communication, legislative strategy, and community organizing.',
  },
  {
    date: 'Dec 2019',
    title: 'State-Level Advocacy Campaigns',
    description: 'Started advocacy campaigns in states with active climate bills: meetings with representatives, phone banks, and community outreach events.',
  },
  {
    date: 'Feb 2020',
    title: 'Federal Lobby Day',
    description: 'Went to Washington, D.C. and met with congressional offices about climate legislation and environmental justice funding.',
  },
  {
    date: 'May 2020',
    title: 'Digital Organizing Pivot',
    description: 'Moved all organizing online during COVID-19. Led virtual town halls, Zoom lobby meetings, and social media campaigns.',
  },
  {
    date: 'Jul 2020',
    title: 'Legislative Wins',
    description: 'The fellowship cohort contributed to climate policy wins in New York, Massachusetts, and Oregon.',
  },
  {
    date: 'Oct 2020',
    title: 'Fellowship Concludes',
    description: 'Finished the 12-month fellowship. I kept doing climate advocacy and used what I learned in later civic projects.',
  },
]

export default function OurClimatePage() {
  const heroView = useInView(0.1)
  const mapView = useInView(0.05)
  const victoriesView = useInView(0.05)
  const timelineView = useInView(0.05)

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
        <div className="content-width w-full relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-verdigris mb-4 block">
              Fellowship · Nov 2019 to Oct 2020
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
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-white">12</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Month Fellowship</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-white">3</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">State Victories</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-white">~$500M</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Green Retrofits (MA)</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-white">2050</span>
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
                Youth-led climate policy advocacy.
              </h2>
            </motion.div>

            <div className="space-y-6 text-titanium leading-relaxed">
              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 }}
              >
                Our Climate is a 501(c)(3) nonprofit that trains young people to advocate for equitable climate policy. The fellowship teaches cohorts of young organizers legislative strategy, constituent communication, and campaign management, then places them in states with active climate legislation.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                I was a fellow from November 2019 through October 2020, based in Portland, Oregon, working on advocacy campaigns in several states. I met with state and federal representatives, did community outreach and phone banking, organized online, and worked with environmental justice organizations.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                Our cohort contributed to legislative wins in three states: New York, Massachusetts, and Oregon.
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
              Where We Worked
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              States with legislative wins.
            </h2>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              Our Climate fellows worked across the country. Highlighted states are where the cohort won policy changes during the 2019–2020 fellowship cycle.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 24 }}
            animate={mapView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.2 }}
          >
            <StateTileMap victories={victories} animate={mapView.isInView} />
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
              Policy Wins
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              What passed in each state.
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
                      <span className="font-mono text-lg text-verdigris font-bold">{victory.abbr}</span>
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
              November 2019 to October 2020.
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
                  <span className="font-mono text-xs text-verdigris">{event.date}</span>
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
              The fellowship ended in October 2020. I have used the organizing skills from that year in my civic work since, including The YCOD and local campaigns in New Orleans.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
