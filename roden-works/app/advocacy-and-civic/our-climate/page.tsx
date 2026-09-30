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
              The fellowship ended in October 2020, but the policies it helped pass continue to shape how states approach climate action. The CLCPA remains one of the most comprehensive climate laws in the country. The retrofits funded in Massachusetts are still being deployed. And the organizing skills Evan built during these twelve months informed every civic project that followed.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
