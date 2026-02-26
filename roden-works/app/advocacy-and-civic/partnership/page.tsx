'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

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
      'Produced detailed transcripts and thematic analyses from SAMHSA employee focus groups. Captured candid feedback on workplace culture, leadership effectiveness, communication breakdowns, and barriers to mission delivery. These transcripts formed the evidentiary basis for the improvement recommendations.',
  },
  {
    title: 'Employee Engagement Analysis',
    description:
      'Analyzed Federal Employee Viewpoint Survey (FEVS) data to identify SAMHSA-specific trends against government-wide benchmarks. Mapped engagement drivers and detractors across divisions, leadership levels, and demographic groups to prioritize interventions.',
  },
  {
    title: 'Agency Leadership Program',
    description:
      'Contributed to the Partnership\'s Agency Leadership Program — a structured initiative that embeds consultants within federal agencies to diagnose organizational health issues and implement evidence-based improvement strategies in collaboration with senior leaders.',
  },
  {
    title: 'Best Places to Work Rankings',
    description:
      'Work supported the broader Best Places to Work in the Federal Government initiative, which ranks over 400 federal organizations based on employee engagement data. SAMHSA\'s improvement was among the most significant score increases tracked during this period.',
  },
]

const timeline = [
  {
    date: 'Sept 2021',
    title: 'Program Start',
    description: 'Joined the Partnership\'s Federal Workforce team in Washington, D.C. as part of the Future Leaders program. Assigned to the SAMHSA engagement improvement initiative.',
  },
  {
    date: 'Oct 2021',
    title: 'Focus Group Facilitation',
    description: 'Began producing and transcribing employee focus groups across SAMHSA divisions. Developed thematic coding framework to systematically categorize employee feedback.',
  },
  {
    date: 'Nov 2021',
    title: 'Data Analysis & Reporting',
    description: 'Analyzed FEVS data and focus group findings. Prepared summary reports for SAMHSA leadership and the Partnership\'s consulting team to inform intervention design.',
  },
  {
    date: 'Dec 2021',
    title: 'Recommendation Development',
    description: 'Contributed to the development of improvement recommendations targeting leadership communication, professional development, and work-life balance policies.',
  },
  {
    date: 'Jan 2022',
    title: 'Program Conclusion',
    description: 'Completed the program. SAMHSA engagement scores would rise from approximately 37 to 74 during the broader Partnership collaboration — one of the most significant improvements in federal government rankings.',
  },
]

export default function PartnershipPage() {
  const heroView = useInView(0.1)
  const engagementView = useInView(0.05)
  const workView = useInView(0.05)
  const timelineView = useInView(0.05)
  const [selectedMetric, setSelectedMetric] = useState<number | null>(null)

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

        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">
              Federal Workforce -- Sept 2021 to Jan 2022
            </span>
            <h1 className="font-serif text-display text-white max-w-4xl">
              Partnership for Public Service
            </h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">
              Member of the Federal Workforce team in Washington, D.C. Supporting SAMHSA&apos;s organizational improvement through focus group research, engagement analysis, and evidence-based intervention design.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-10 grid grid-cols-2 md:grid-cols-4 gap-4 md:gap-0 md:divide-x divide-white/10"
          >
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">~37</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Score Before</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-forest-light">74</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Score After</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-copper">2x</span>
              <span className="block mt-2 font-mono text-xs tracking-wide text-titanium uppercase">Score Increase</span>
            </div>
            <div className="text-center px-6 py-4">
              <span className="block font-serif text-3xl md:text-4xl text-white">5 mo</span>
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
                  Making government work better.
                </h2>
              </motion.div>

              <div className="space-y-6 text-titanium leading-relaxed">
                <motion.p
                  initial={{ opacity: 0, y: 20 }}
                  animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: 0.1 }}
                >
                  The Partnership for Public Service, founded in 2001 by Samuel J. Heyman with a $25 million endowment, is the leading nonpartisan organization dedicated to making the federal government more effective. It produces the Best Places to Work in the Federal Government rankings, administers the Samuel J. Heyman Service to America Medals (the &ldquo;Sammies&rdquo;), and runs the Center for Presidential Transition.
                </motion.p>

                <motion.p
                  initial={{ opacity: 0, y: 20 }}
                  animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: 0.2 }}
                >
                  As a member of the Federal Workforce team through the Future Leaders program, Evan produced focus group transcripts and engagement analyses for the Substance Abuse and Mental Health Services Administration (SAMHSA) improvement initiative — part of the Agency Leadership Program that helps federal agencies diagnose and address organizational health challenges.
                </motion.p>

                <motion.p
                  initial={{ opacity: 0, y: 20 }}
                  animate={heroView.isInView ? { opacity: 1, y: 0 } : {}}
                  transition={{ duration: 0.6, delay: 0.3 }}
                >
                  The Future Leaders program provides 10-12 week paid internships (approximately $6,500 plus a $5,500 housing stipend) placing emerging leaders in federal agencies and supporting organizations. It is designed to build the next generation of public servants by providing hands-on experience in federal policy and operations.
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
                    <span className="font-serif text-2xl text-white block">$6.5B</span>
                    <span className="font-mono text-xs text-titanium/60 mt-1 block">Annual Budget</span>
                  </div>
                  <div>
                    <span className="font-serif text-2xl text-white block">500+</span>
                    <span className="font-mono text-xs text-titanium/60 mt-1 block">Employees</span>
                  </div>
                </div>
              </div>

              <div className="glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">
                  Why Engagement Matters
                </h3>
                <p className="text-titanium text-sm leading-relaxed">
                  Federal employee engagement directly impacts mission delivery. Agencies with higher engagement scores show better outcomes in service delivery, innovation, cost-effectiveness, and employee retention. For an agency like SAMHSA, whose work touches millions of Americans in crisis, organizational health is not an abstraction — it determines whether help reaches people who need it.
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
              SAMHSA&apos;s employee engagement scores improved across every measured category during the Partnership&apos;s collaboration. The overall score rose from approximately 37 to 74 — a transformation that placed SAMHSA among the most improved agencies in the federal government.
            </p>
          </motion.div>

          <motion.div
            initial={{ opacity: 0 }}
            animate={engagementView.isInView ? { opacity: 1 } : {}}
            transition={{ duration: 0.8, delay: 0.3 }}
            className="glass rounded-xl p-6 md:p-8"
          >
            <div className="space-y-8">
              {engagementData.map((metric, i) => {
                const isSelected = selectedMetric === i
                const improvement = metric.after - metric.before

                return (
                  <motion.div
                    key={metric.category}
                    initial={{ opacity: 0, y: 20 }}
                    animate={engagementView.isInView ? { opacity: 1, y: 0 } : {}}
                    transition={{ duration: 0.5, delay: 0.4 + i * 0.1 }}
                    className={`cursor-pointer rounded-lg p-4 transition-all duration-300 ${
                      isSelected ? 'bg-white/5' : 'hover:bg-white/[0.02]'
                    }`}
                    onClick={() => setSelectedMetric(isSelected ? null : i)}
                  >
                    <div className="flex items-center justify-between mb-3">
                      <span className="text-white text-sm font-medium">{metric.category}</span>
                      <span className="font-mono text-xs text-forest-light">
                        +{improvement} pts
                      </span>
                    </div>

                    <div className="relative">
                      {/* Before bar */}
                      <div className="h-4 bg-white/5 rounded-full overflow-hidden mb-1.5">
                        <motion.div
                          initial={{ width: 0 }}
                          animate={engagementView.isInView ? { width: `${metric.before}%` } : {}}
                          transition={{ duration: 0.8, delay: 0.6 + i * 0.1 }}
                          className="h-full bg-titanium/30 rounded-full"
                        />
                      </div>

                      {/* After bar */}
                      <div className="h-4 bg-white/5 rounded-full overflow-hidden">
                        <motion.div
                          initial={{ width: 0 }}
                          animate={engagementView.isInView ? { width: `${metric.after}%` } : {}}
                          transition={{ duration: 1, delay: 0.8 + i * 0.1 }}
                          className="h-full bg-gradient-to-r from-forest to-forest-light rounded-full"
                        />
                      </div>

                      {/* Score labels */}
                      <div className="flex items-center justify-between mt-1">
                        <div className="flex items-center gap-4">
                          <span className="font-mono text-xs text-titanium/50">
                            Before: {metric.before}
                          </span>
                          <span className="font-mono text-xs text-forest-light">
                            After: {metric.after}
                          </span>
                        </div>
                      </div>
                    </div>
                  </motion.div>
                )
              })}
            </div>

            {/* Legend */}
            <div className="mt-8 pt-6 border-t border-white/5 flex flex-wrap gap-6">
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-titanium/30" />
                <span className="text-xs text-titanium">Before Partnership Engagement</span>
              </div>
              <div className="flex items-center gap-2">
                <div className="w-3 h-3 rounded-full bg-forest-light" />
                <span className="text-xs text-titanium">After Partnership Engagement</span>
              </div>
              <div className="ml-auto">
                <span className="text-xs text-titanium/40 font-mono">Source: Best Places to Work in the Federal Government</span>
              </div>
            </div>
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
              The work behind the numbers.
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
              The federal workforce serves 330 million Americans. When agencies work better, people get better outcomes — whether that means faster disability claims, more effective disaster response, or more accessible mental health services. The Partnership for Public Service exists to make that improvement possible, and the work at SAMHSA demonstrated what happens when employee engagement is treated as a strategic priority rather than an afterthought.
            </motion.p>
          </div>
        </div>
      </section>
    </>
  )
}
