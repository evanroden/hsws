'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'

const activeProjects = [
  {
    title: 'Fat City Redevelopment',
    funding: '$13M CDBG',
    description:
      'Community Development Block Grant-funded redevelopment of the Fat City entertainment district. The project aims to transform the aging nightlife strip into a mixed-use, walkable neighborhood with improved streetscaping, public lighting, and commercial facade improvements.',
    status: 'Active',
  },
  {
    title: 'Clearview City Center',
    funding: '$100M',
    description:
      'Major conversion of the former Clearview Mall site into a mixed-use town center with residential, retail, office, and civic components. The project represents the largest single investment in Metairie\'s urban core in decades.',
    status: 'Active',
  },
]

const proposalPreviews = [
  {
    category: 'Transit & Mobility',
    items: [
      'Dedicated bus lanes on Veterans Memorial Blvd',
      'Protected bike infrastructure connecting Fat City to Clearview',
      'Pedestrian-priority zones in commercial cores',
      'Shared parking structures to reduce surface lot dependency',
    ],
  },
  {
    category: 'Mixed-Use Development',
    items: [
      'Form-based code recommendations for Veterans corridor',
      'Missing middle housing typologies for residential streets',
      'Ground-floor commercial activation requirements',
      'Affordable housing set-aside framework',
    ],
  },
  {
    category: 'Public Space & Environment',
    items: [
      'Linear park along Metairie drainage canal',
      'Community green spaces on underutilized parcels',
      'Urban tree canopy expansion program',
      'Stormwater management through green infrastructure',
    ],
  },
  {
    category: 'Governance & Identity',
    items: [
      'Community identity and placemaking strategy',
      'Special taxing district feasibility analysis',
      'Coordinated development review process',
      'Long-term capital improvement framework',
    ],
  },
]

export default function MidtownMetairiePage() {
  const contextView = useInView(0.1)
  const projectsView = useInView(0.1)
  const previewView = useInView(0.05)
  const [expandedProject, setExpandedProject] = useState<number | null>(null)

  return (
    <>
      <Breadcrumbs
        items={[
          { label: 'Advocacy', href: '/advocacy-and-civic' },
          { label: 'Midtown Metairie' },
        ]}
      />

      {/* Hero */}
      <section className="relative min-h-[60vh] flex items-center bg-gradient-to-br from-slate-950 via-copper/5 to-slate-950 overflow-hidden">
        <div className="absolute inset-0 overflow-hidden">
          <div
            className="absolute inset-0 opacity-[0.03]"
            style={{
              backgroundImage: 'radial-gradient(circle at 1px 1px, white 1px, transparent 1px)',
              backgroundSize: '40px 40px',
            }}
          />
        </div>

        <div className="content-width relative z-10 text-center max-w-4xl mx-auto">
          <motion.div
            initial={{ opacity: 0, y: 30 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.7, ease: [0.16, 1, 0.3, 1] }}
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
              Urban Planning Proposal
            </span>
            <h1 className="font-serif text-display text-white">
              Midtown Metairie
            </h1>
            <p className="mt-6 text-titanium text-lg leading-relaxed max-w-2xl mx-auto">
              An urban planning proposal for Louisiana&apos;s most populous unincorporated community — reimagining the commercial and civic core of Jefferson Parish&apos;s largest neighborhood.
            </p>
          </motion.div>

          {/* Coming Soon Card */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={{ opacity: 1, y: 0 }}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-12 glass rounded-xl p-8 md:p-12 relative overflow-hidden"
          >
            {/* Animated shimmer */}
            <motion.div
              className="absolute inset-0 bg-gradient-to-r from-transparent via-copper/5 to-transparent"
              animate={{ x: ['-200%', '200%'] }}
              transition={{ repeat: Infinity, duration: 4, ease: 'linear' }}
            />

            <div className="relative z-10">
              <div className="w-16 h-16 rounded-full bg-copper/10 border border-copper/20 flex items-center justify-center mx-auto mb-6">
                <svg
                  className="w-8 h-8 text-copper"
                  fill="none"
                  stroke="currentColor"
                  viewBox="0 0 24 24"
                  strokeWidth="1.5"
                >
                  <path
                    strokeLinecap="round"
                    strokeLinejoin="round"
                    d="M12 6v6h4.5m4.5 0a9 9 0 11-18 0 9 9 0 0118 0z"
                  />
                </svg>
              </div>
              <h2 className="font-serif text-2xl text-white mb-4">
                Coming Soon — Full Proposal
              </h2>
              <p className="text-titanium text-sm leading-relaxed max-w-xl mx-auto">
                The full Midtown Metairie proposal is currently in development. It builds on two major active planning initiatives — the Fat City Redevelopment ($13M CDBG) and the Clearview City Center conversion ($100M) — to envision a walkable, mixed-use urban center for an unincorporated community of over 140,000 residents.
              </p>
            </div>
          </motion.div>
        </div>
      </section>

      {/* Context */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5" ref={contextView.ref}>
        <div className="content-width">
          <div className="max-w-3xl">
            <motion.div
              initial={{ opacity: 0, y: 20 }}
              animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
              transition={{ duration: 0.6 }}
            >
              <span className="font-mono text-xs tracking-widest uppercase text-copper mb-6 block">
                The Context
              </span>
              <h2 className="font-serif text-heading text-white mb-8">
                Why Metairie needs a plan.
              </h2>
            </motion.div>

            <div className="space-y-6 text-titanium leading-relaxed">
              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 }}
              >
                Metairie is the most populous unincorporated community in Louisiana and one of the largest in the United States. With over 140,000 residents, it would be the third-largest city in the state if incorporated — yet it has no mayor, no city council, and no independent planning authority. It is governed as part of Jefferson Parish, which makes coordinated urban planning uniquely challenging.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                The built environment reflects this governance gap. Metairie is defined by auto-oriented commercial strips, surface parking lots, and single-use zoning — the hallmarks of mid-century suburban development. Veterans Memorial Boulevard, the community&apos;s central artery, is a six-lane arterial lined with strip malls, fast food restaurants, and office parks. There is almost no protected bike infrastructure, limited transit, and minimal public space.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                But that is beginning to change. Two major projects — the Fat City Redevelopment and the Clearview City Center conversion — represent a once-in-a-generation opportunity to rethink what Midtown Metairie can become. This proposal aims to connect those dots: to show how these two catalytic investments can anchor a broader transformation of Metairie&apos;s commercial core into something more walkable, more connected, and more resilient.
              </motion.p>
            </div>
          </div>

          {/* Quick stats */}
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6, delay: 0.4 }}
            className="mt-12 grid grid-cols-2 md:grid-cols-4 gap-4"
          >
            {[
              { value: '140K+', label: 'Residents' },
              { value: '#1', label: 'Largest Unincorporated in LA' },
              { value: '$113M+', label: 'Active Investment' },
              { value: '0', label: 'Incorporated Government' },
            ].map((stat, i) => (
              <motion.div
                key={stat.label}
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.4, delay: 0.5 + i * 0.1 }}
                className="glass rounded-xl p-5 text-center"
              >
                <span className="block font-serif text-2xl text-white">{stat.value}</span>
                <span className="block mt-2 font-mono text-xs text-titanium uppercase">{stat.label}</span>
              </motion.div>
            ))}
          </motion.div>
        </div>
      </section>

      {/* Active Projects */}
      <section className="section-padding bg-slate-950" ref={projectsView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={projectsView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Catalytic Investments
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Two projects reshaping the core.
            </h2>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {activeProjects.map((project, i) => (
              <motion.div
                key={project.title}
                initial={{ opacity: 0, y: 30 }}
                animate={projectsView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 + i * 0.15 }}
                className="glass rounded-xl p-6 md:p-8 cursor-pointer hover:bg-white/10 transition-all duration-300"
                onClick={() => setExpandedProject(expandedProject === i ? null : i)}
              >
                <div className="flex items-start justify-between mb-4">
                  <div>
                    <span className="inline-block px-3 py-1 rounded-full bg-copper/10 text-copper font-mono text-xs mb-3">
                      {project.status}
                    </span>
                    <h3 className="font-serif text-xl text-white">{project.title}</h3>
                  </div>
                  <span className="font-serif text-2xl text-copper">{project.funding}</span>
                </div>
                <p className="text-titanium text-sm leading-relaxed">{project.description}</p>
                <div className="mt-4 flex items-center gap-2 text-sm text-titanium/40">
                  <span className="font-mono text-xs">{expandedProject === i ? 'collapse' : 'expand'}</span>
                </div>

                {expandedProject === i && (
                  <motion.div
                    initial={{ opacity: 0, height: 0 }}
                    animate={{ opacity: 1, height: 'auto' }}
                    transition={{ duration: 0.3 }}
                    className="mt-4 pt-4 border-t border-white/5"
                  >
                    <p className="text-titanium/60 text-xs leading-relaxed">
                      {i === 0
                        ? 'The Fat City district, bounded roughly by Division Street, 18th Street, Severn Avenue, and the Metairie Country Club, was once a thriving entertainment destination. The CDBG-funded redevelopment focuses on streetscaping, drainage improvements, public art, and commercial facade grants to attract new tenants and foot traffic.'
                        : 'The Clearview Mall site, anchored at the intersection of Veterans Memorial Blvd and Clearview Pkwy, is being reimagined as a mixed-use town center. The $100M project includes residential towers, a grocery anchor, retail, office space, and structured parking — a radical departure from the enclosed mall format.'}
                    </p>
                  </motion.div>
                )}
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Proposal Preview */}
      <section className="section-padding bg-gradient-to-b from-slate-950 to-copper/5" ref={previewView.ref}>
        <div className="content-width">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            animate={previewView.isInView ? { opacity: 1, y: 0 } : {}}
            transition={{ duration: 0.6 }}
            className="mb-12"
          >
            <span className="font-mono text-xs tracking-widest uppercase text-copper">
              Proposal Preview
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              What the full proposal will cover.
            </h2>
            <p className="mt-4 text-titanium max-w-2xl">
              The Midtown Metairie proposal is organized around four interconnected categories. Below is a preview of the topics and recommendations under development.
            </p>
          </motion.div>

          <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
            {proposalPreviews.map((section, i) => (
              <motion.div
                key={section.category}
                initial={{ opacity: 0, y: 30 }}
                animate={previewView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.1 + i * 0.1 }}
                className="glass rounded-xl p-6 md:p-8"
              >
                <div className="flex items-start gap-4 mb-4">
                  <div className="flex-shrink-0 w-10 h-10 rounded-lg bg-copper/10 border border-copper/20 flex items-center justify-center">
                    <span className="font-mono text-sm text-copper font-bold">
                      {String(i + 1).padStart(2, '0')}
                    </span>
                  </div>
                  <h3 className="font-serif text-lg text-white">{section.category}</h3>
                </div>
                <ul className="space-y-3 ml-14">
                  {section.items.map((item, j) => (
                    <motion.li
                      key={j}
                      initial={{ opacity: 0, x: -10 }}
                      animate={previewView.isInView ? { opacity: 1, x: 0 } : {}}
                      transition={{ duration: 0.4, delay: 0.3 + i * 0.1 + j * 0.05 }}
                      className="flex items-start gap-2"
                    >
                      <span className="w-1 h-1 rounded-full bg-copper/40 mt-2 flex-shrink-0" />
                      <span className="text-titanium text-sm leading-relaxed">{item}</span>
                    </motion.li>
                  ))}
                </ul>
              </motion.div>
            ))}
          </div>
        </div>
      </section>

      {/* Closing CTA */}
      <section className="section-padding bg-slate-950">
        <div className="content-width text-center max-w-2xl mx-auto">
          <motion.div
            initial={{ opacity: 0, y: 20 }}
            whileInView={{ opacity: 1, y: 0 }}
            viewport={{ once: true }}
            transition={{ duration: 0.6 }}
          >
            <div className="glass rounded-xl p-8 md:p-12 relative overflow-hidden">
              <motion.div
                className="absolute inset-0 bg-gradient-to-r from-transparent via-copper/3 to-transparent"
                animate={{ x: ['-200%', '200%'] }}
                transition={{ repeat: Infinity, duration: 6, ease: 'linear' }}
              />
              <div className="relative z-10">
                <span className="font-mono text-xs tracking-widest uppercase text-copper block mb-4">
                  In Development
                </span>
                <p className="text-white font-serif text-lg leading-relaxed">
                  The full Midtown Metairie proposal is being developed with detailed mapping, policy recommendations, and design guidelines. Check back for the complete plan.
                </p>
              </div>
            </div>
          </motion.div>
        </div>
      </section>
    </>
  )
}
