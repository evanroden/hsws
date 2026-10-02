'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import EngagementDumbbell from './EngagementDumbbell'

const engagementData = [
  { category: 'Overall Engagement', before: 37, after: 74 },
  { category: 'Effective Leadership', before: 28, after: 63 },
  { category: 'Employee Development', before: 31, after: 58 },
  { category: 'Communication & Trust', before: 33, after: 67 },
  { category: 'Work-Life Balance', before: 45, after: 71 },
]

const workstreams = [
  {
    title: 'Focus Group Research',
    description:
      'Produced transcripts and thematic analyses from SAMHSA employee focus groups, covering workplace culture, leadership, communication problems, and obstacles to getting the agency\'s work done. The improvement recommendations were based on these transcripts.',
  },
  {
    title: 'Employee Engagement Analysis',
    description:
      'Compared SAMHSA\'s Federal Employee Viewpoint Survey (FEVS) results against government-wide benchmarks. Broke out what raised and lowered engagement by division, leadership level, and demographic group so the team could decide where to start.',
  },
  {
    title: 'Agency Leadership Program',
    description:
      'Worked within the Partnership\'s Agency Leadership Program, which places consultants inside federal agencies to find organizational problems and fix them with senior leaders.',
  },
  {
    title: 'Best Places to Work Rankings',
    description:
      'The work fed into the Best Places to Work in the Federal Government rankings, which rank over 400 federal organizations by employee engagement data.',
  },
]

const timeline = [
  {
    date: 'Sept 2021',
    title: 'Program Start',
    description: 'Joined the Partnership\'s Federal Workforce team in Washington, D.C. through the Future Leaders program and was assigned to the SAMHSA engagement project.',
  },
  {
    date: 'Oct 2021',
    title: 'Focus Group Facilitation',
    description: 'Started producing and transcribing employee focus groups across SAMHSA divisions. Built a thematic coding framework to sort employee feedback.',
  },
  {
    date: 'Nov 2021',
    title: 'Data Analysis & Reporting',
    description: 'Analyzed FEVS data and focus group findings. Wrote summary reports for SAMHSA leadership and the Partnership\'s consulting team.',
  },
  {
    date: 'Dec 2021',
    title: 'Recommendation Development',
    description: 'Helped write recommendations on leadership communication, professional development, and work-life balance policies.',
  },
  {
    date: 'Jan 2022',
    title: 'Program Conclusion',
    description: 'Finished the program. Over the full Partnership collaboration, SAMHSA\'s engagement score rose from about 37 to 74.',
  },
]

export default function PartnershipPage() {
  const heroView = useInView(0.1)
  const engagementView = useInView(0.05)
  const workView = useInView(0.05)
  const timelineView = useInView(0.05)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'Partnership for Public Service' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[50vh] flex items-end bg-slate-950 overflow-hidden">
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
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Federal Workforce · Sept 2021 to Jan 2022
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Partnership for Public Service
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              I was on the Federal Workforce team in Washington, D.C., working on SAMHSA&apos;s employee engagement project. I ran focus group research and analyzed engagement data.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-white">~37</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Score Before</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-verdigris">74</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Score After</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-copper">2x</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Score Increase</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-sans font-semibold tracking-tight text-3xl md:text-4xl text-white">5 mo</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Duration</span>
            </div>
          </motion.div>
        </div>
      </section>

      {/* About the Partnership */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5" ref={heroView.ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <div>
              <motion.div
                initial={{ opacity: 0, y: 20 }}
                animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6 }}
              >
                <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
                  The Organization
                </span>
                <h2 className="font-serif text-heading text-white mb-8">
                  A nonprofit focused on the federal workforce.
                </h2>
              </motion.div>

              <div className="space-y-6 text-titanium leading-relaxed">
                <motion.p
                  initial={{ opacity: 0, y: 20 }}
                  animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: 0.1 }}
                >
                  The Partnership for Public Service, founded in 2001 by Samuel J. Heyman with a $25 million endowment, is a nonpartisan nonprofit that works to make the federal government more effective. It produces the Best Places to Work in the Federal Government rankings, administers the Samuel J. Heyman Service to America Medals (the &ldquo;Sammies&rdquo;), and runs the Center for Presidential Transition.
                </motion.p>

                <motion.p
                  initial={{ opacity: 0, y: 20 }}
                  animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: 0.2 }}
                >
                  I joined the Federal Workforce team through the Future Leaders program and produced focus group transcripts and engagement analyses for a project with the Substance Abuse and Mental Health Services Administration (SAMHSA). The project was part of the Agency Leadership Program, which helps federal agencies find and fix organizational problems.
                </motion.p>

                <motion.p
                  initial={{ opacity: 0, y: 20 }}
                  animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: 0.3 }}
                >
                  The Future Leaders program offers 10-12 week paid internships (approximately $6,500 plus a $5,500 housing stipend) in federal agencies and partner organizations, giving students direct experience in federal policy and operations.
                </motion.p>
              </div>
            </div>

            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6, delay: 0.3 }}
              className="space-y-6"
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper block">
                About SAMHSA
              </span>
              <div className="glass rounded-xl p-6 md:p-8">
                <h3 className="font-serif text-lg text-white mb-4">
                  Substance Abuse and Mental Health Services Administration
                </h3>
                <p className="text-titanium text-sm leading-relaxed mb-4">
                  SAMHSA is a branch of the U.S. Department of Health and Human Services charged with reducing the impact of substance abuse and mental illness on American communities. The agency administers the National Suicide Prevention Lifeline, the Disaster Distress Helpline, and the SAMHSA Treatment Locator, among other national programs.
                </p>
                <div className="grid grid-cols-2 gap-4 pt-4 border-t border-white/5">
                  <div>
                    <span className="font-sans font-semibold tracking-tight text-2xl text-white block">$6.5B</span>
                    <span className="font-mono text-xs text-muted mt-1 block">Annual Budget</span>
                  </div>
                  <div>
                    <span className="font-sans font-semibold tracking-tight text-2xl text-white block">500+</span>
                    <span className="font-mono text-xs text-muted mt-1 block">Employees</span>
                  </div>
                </div>
              </div>

              <div className="glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">
                  Why Engagement Matters
                </h3>
                <p className="text-titanium text-sm leading-relaxed">
                  Agencies with higher engagement scores tend to deliver services better and keep their employees longer. SAMHSA runs crisis lines and treatment programs, so how well the agency functions affects whether people get help.
                </p>
              </div>
            </motion.div>
          </div>
        </div>
      </section>

      {/* Engagement Score Visualization */}
      <section className="section-padding bg-slate-950" ref={engagementView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={engagementView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Impact
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Engagement scores doubled.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              SAMHSA&apos;s employee engagement scores improved in every measured category during the Partnership&apos;s collaboration. The overall score rose from approximately 37 to 74.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 24 }}
            animate={engagementView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.7, delay: 0.2 }}
          >
            <EngagementDumbbell data={engagementData} animate={engagementView.isInView} />
          </motion.div>
        </div>
      </section>

      {/* Workstreams */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5" ref={workView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={workView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Contributions
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              What I worked on.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {workstreams.map((stream, i) => (
              <motion.div
                key={stream.title}
                initial={{ opacity: 0, y: 30 }}
                animate={workView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 + i * 0.12 }}
                className="glass rounded-xl p-6 md:p-8"
              >
                <div className="flex items-start gap-4">
                  <div className="flex-shrink-0 w-10 h-10 rounded-lg bg-copper/10 border border-copper/20 flex items-center justify-center">
                    <span className="font-mono text-sm text-copper font-bold">
                      {String(i + 1).padStart(2, '0')}
                    </span>
                  </div>
                  <div>
                    <h3 className="font-serif text-lg text-white mb-3">{stream.title}</h3>
                    <p className="text-titanium text-sm leading-relaxed">{stream.description}</p>
                  </div>
                </div>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Timeline */}
      <section className="section-padding bg-slate-950" ref={timelineView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={timelineView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Timeline
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Five months in Washington.
            </h2>
          </motion.div>

          <div className="relative">
            <div className="absolute left-4 md:left-8 top-0 bottom-0 w-px bg-copper/20" />
            <div className="space-y-8">
              {timeline.map((event, i) => (
                <motion.div
                  key={i}
                  initial={{ opacity: 0, x: -20 }}
                  animate={timelineView.isInView ? { opacity: 1, x: 0 } : {}}
                  transition={{ duration: 0.5, delay: i * 0.1 }}
                  className="relative pl-12 md:pl-20"
                >
                  <div className="absolute left-2.5 md:left-6.5 w-3 h-3 rounded-full bg-slate-950 border-2 border-copper/40 z-10" />
                  <span className="font-mono text-xs text-copper">{event.date}</span>
                  <h3 className="font-serif text-lg text-white mt-1">{event.title}</h3>
                  <p className="text-titanium text-sm mt-1 leading-relaxed">{event.description}</p>
                </motion.div>
              ))}
            </div>
          </div>
        </div>
      </section>

      {/* Closing */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-forest/5">
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.p
              initial={{ opacity: 0, y: 20 }}
              whileInView={{ opacity: 1, y: 0 }}
              viewport={{ once: true }}
              transition={{ duration: 0.6 }}
              className="text-white text-lg font-serif leading-relaxed"
            >
              Working on the SAMHSA project showed me how much an agency&apos;s internal health affects the services it delivers to the public.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
