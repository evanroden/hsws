'use client'

import { motion } from 'framer-motion'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import StatCounter from '@/components/ui/StatCounter'

export default function PartnershipPage() {
  const { ref, isInView } = useInView(0.1)

  return (
    <>
      <Breadcrumbs items={[{ label: 'Advocacy', href: '/advocacy-and-civic' }, { label: 'Partnership for Public Service' }]} />

      <section className="relative min-h-[50vh] flex items-end bg-slate-950">
        <div className="content-width relative z-10 pb-12 md:pb-16">
          <motion.div initial={{ opacity: 0, y: 30 }} animate={{ opacity: 1, y: 0 }} transition={{ duration: 0.7 }}>
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-4 block">Federal Service</span>
            <h1 className="font-serif text-display text-white max-w-4xl">Partnership for Public Service</h1>
            <p className="mt-4 text-titanium text-lg max-w-2xl">Federal Workforce team supporting SAMHSA improvement. Washington, D.C. — September 2021 to January 2022.</p>
          </motion.div>
        </div>
      </section>

      <section className="section-padding bg-slate-950" ref={ref}>
        <div className="content-width">
          <div className="grid grid-cols-1 lg:grid-cols-2 gap-16">
            <div>
              <h2 className="font-serif text-heading text-white mb-6">Improving Federal Agencies</h2>
              <div className="space-y-4 text-titanium leading-relaxed">
                <p>The Partnership for Public Service, founded in 2001 by Samuel J. Heyman with a $25 million gift, is the leading nonpartisan organization dedicated to making the federal government more effective. The Partnership produces the Best Places to Work in the Federal Government rankings, administers the Samuel J. Heyman Service to America Medals (the Sammies), and runs the Center for Presidential Transition.</p>
                <p>As a member of the Federal Workforce team, Evan produced focus group transcripts for the Substance Abuse and Mental Health Services Administration (SAMHSA) improvement initiative — part of the Agency Leadership Program that helps federal agencies improve organizational health and employee engagement.</p>
              </div>
            </div>

            <div>
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">Impact</span>

              <motion.div initial={{ opacity: 0, y: 20 }} animate={isInView ? { opacity: 1, y: 0 } : {}} transition={{ duration: 0.6 }} className="glass rounded-xl p-8 mb-6">
                <h3 className="font-serif text-lg text-white mb-4">SAMHSA Engagement Transformation</h3>
                <div className="flex items-center gap-8">
                  <div className="text-center">
                    <span className="font-serif text-3xl text-titanium/60">~37</span>
                    <span className="block text-xs text-titanium/40 mt-1">Before</span>
                  </div>
                  <div className="flex-1 h-px bg-gradient-to-r from-titanium/20 via-copper to-forest-light relative">
                    <span className="absolute -top-3 left-1/2 -translate-x-1/2 text-xs text-copper">→</span>
                  </div>
                  <div className="text-center">
                    <span className="font-serif text-3xl text-forest-light">74</span>
                    <span className="block text-xs text-titanium/40 mt-1">After</span>
                  </div>
                </div>
                <p className="text-titanium text-sm mt-4">Employee engagement scores doubled during the Partnership&apos;s collaboration, with a 35-point increase in effective leadership metrics.</p>
              </motion.div>

              <motion.div initial={{ opacity: 0, y: 20 }} animate={isInView ? { opacity: 1, y: 0 } : {}} transition={{ duration: 0.6, delay: 0.2 }} className="glass rounded-xl p-6">
                <h3 className="font-serif text-lg text-white mb-3">Future Leaders Program</h3>
                <p className="text-titanium text-sm leading-relaxed">The program provides 10–12 week paid internships (~$6,500 + $5,500 housing stipend) placing emerging leaders in federal agencies to build the next generation of public servants.</p>
              </motion.div>
            </div>
          </div>
        </div>
      </section>
    </>
  )
}
