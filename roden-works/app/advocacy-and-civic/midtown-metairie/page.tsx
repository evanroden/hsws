'use client'

import { motion } from 'framer-motion'
import { useState } from 'react'
import { useInView } from '@/lib/hooks'
import Breadcrumbs from '@/components/ui/Breadcrumbs'
import { Cite, SourceList } from '@/components/ui/Sources'
import { MIDTOWN_METAIRIE_SOURCES as S } from './sources'

// Fact-check 2026-10 sources:
// - Fat City Leisure Park: ~$17M total, ~$11.7M federal CDBG + ~$5.4M state; A&E under a cooperative
//   endeavor agreement with Jefferson Parish; target opening end of 2027. The earlier "$13M CDBG" figure
//   could not be sourced. https://hoodline.com/2026/07/fat-city-scores-17-million-park-play-to-jump-start-metairie-nightlife/
// - Clearview City Center: $100M conversion of the 700,000 sq ft Clearview Shopping Center, 260+ apartments,
//   a hotel, ~100,000 sq ft of office, a 14,000 sq ft event green (announced Dec 2019).
//   https://enr.com/articles/48361-100-million-project-will-repurpose-suburban-mall-as-open-air-city-center
//   "Residential towers", a grocery anchor and structured parking were not in any source and were removed.
// - Metairie: 143,507 residents (2020 Census), the largest unincorporated community in Louisiana and the
//   fifth-largest CDP in the U.S. https://en.wikipedia.org/wiki/Metairie,_Louisiana
// - Fat City sits off Veterans Memorial Blvd next to Lakeside Shopping Center: https://www.visitjeffersonparish.com/communities/metairie/
//   (the earlier boundary list ending at "Metairie Country Club" was wrong and was removed)
// - Clearview phase-one construction: https://bizneworleans.com/construction-of-first-phase-of-clearview-redevelopment-begins/
// - Veterans Memorial Blvd is six lanes: https://en.wikipedia.org/wiki/Veterans_Memorial_Boulevard
const activeProjects: { title: string; funding: string; description: string; status: string; sources: string[] }[] = [
  {
    title: 'Fat City Leisure Park',
    funding: '$17M',
    description:
      'A park planned for the Fat City district, funded mostly with federal Community Development Block Grant money (about $11.7 million) plus about $5.4 million from the state. The district\'s backers see it as a catalyst for turning the aging nightlife strip into a walkable, mixed-use neighborhood.',
    status: 'In design',
    sources: ['hoodline'],
  },
  {
    title: 'Clearview City Center',
    funding: '$100M',
    description:
      'Conversion of the Clearview Shopping Center into a mixed-use town center with apartments, a hotel, retail, office space, and an event green.',
    status: 'Active',
    sources: ['enr'],
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
              My urban planning proposal for the commercial core of Metairie, Louisiana&apos;s most populous unincorporated community.<Cite sources={S} id="metairie-wiki" />
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
                Full Proposal in Progress
              </h2>
              <p className="text-titanium text-sm leading-relaxed max-w-xl mx-auto">
                I&apos;m still writing the full proposal. It builds on two projects in progress, the Fat City Leisure Park ($17M, mostly CDBG)<Cite sources={S} id="hoodline" /> and the Clearview City Center conversion ($100M),<Cite sources={S} id="enr" /> and lays out a walkable, mixed-use center for a community of over 140,000 residents.<Cite sources={S} id="metairie-wiki" />
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
                Metairie is the most populous unincorporated community in Louisiana and one of the largest in the United States. It has over 140,000 residents but no mayor, no city council, and no planning authority of its own. Jefferson Parish governs it,<Cite sources={S} id="metairie-wiki" /> which makes coordinated planning hard.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.2 }}
              >
                Most of Metairie was built around the car: commercial strips, surface parking, and single-use zoning typical of mid-century suburbs. Veterans Memorial Boulevard, the main corridor, is six lanes<Cite sources={S} id="veterans-wiki" /> lined with strip malls, fast food restaurants, and office parks. There is almost no protected bike infrastructure, limited transit, and little public space.
              </motion.p>

              <motion.p
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.6, delay: 0.3 }}
              >
                Two projects in progress, the Fat City Leisure Park and the Clearview City Center conversion, give the parish a chance to change that. My proposal shows how those two investments could anchor a more walkable, better connected commercial core between them.
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
              { value: '140K+', label: 'Residents', sources: ['metairie-wiki'] },
              { value: '#1', label: 'Largest Unincorporated in LA', sources: ['metairie-wiki'] },
              { value: '$117M', label: 'Planned Investment', sources: ['hoodline', 'enr'] },
              { value: '0', label: 'Incorporated Government', sources: ['metairie-wiki'] },
            ].map((stat, i) => (
              <motion.div
                key={stat.label}
                initial={{ opacity: 0, y: 20 }}
                animate={contextView.isInView ? { opacity: 1, y: 0 } : {}}
                transition={{ duration: 0.4, delay: 0.5 + i * 0.1 }}
                className="glass rounded-xl p-5 text-center"
              >
                <span className="block font-sans font-semibold tracking-tight text-2xl md:text-3xl text-white">
                  {stat.value}
                  <Cite sources={S} id={stat.sources} />
                </span>
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
              Active Investments
            </span>
            <h2 className="font-serif text-heading text-white mt-3">
              Two projects already in progress.
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
                <p className="text-titanium text-sm leading-relaxed">
                  {project.description}
                  <Cite sources={S} id={project.sources} />
                </p>
                <div className="mt-4 flex items-center gap-2 text-sm text-muted">
                  <span className="font-mono text-xs">{expandedProject === i ? 'collapse' : 'expand'}</span>
                </div>

                {expandedProject === i && (
                  <motion.div
                    initial={{ opacity: 0, height: 0 }}
                    animate={{ opacity: 1, height: 'auto' }}
                    transition={{ duration: 0.3 }}
                    className="mt-4 pt-4 border-t border-white/5"
                  >
                    <p className="text-muted text-xs leading-relaxed">
                      {i === 0 ? (
                        <>
                          Fat City, just off Veterans Memorial Boulevard near Lakeside Shopping Center,<Cite sources={S} id="visitjp" /> was once a busy entertainment district.<Cite sources={S} id="metairie-wiki" /> The planned Leisure Park includes a stroll garden, an oak grove, a children&apos;s play area, bioswales, an event lawn, and a pocket park on the former Crazy Johnnie&apos;s site. Officials aim to open it by the end of 2027.<Cite sources={S} id="hoodline" />
                        </>
                      ) : (
                        <>
                          The Clearview Shopping Center, at Veterans Memorial Blvd and Clearview Pkwy,<Cite sources={S} id="biz-clearview" /> is being converted into a mixed-use town center. The $100M plan includes more than 260 apartments, a hotel, about 100,000 square feet of office space, restaurants, and a 14,000-square-foot green for events.<Cite sources={S} id="enr" />
                        </>
                      )}
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
              The proposal has four sections. These are the topics and recommendations I&apos;m working on.
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
                  I&apos;m still working on the full proposal, including maps, policy recommendations, and design guidelines. I&apos;ll post it here when it&apos;s done.
                </p>
              </div>
            </div>
          </motion.div>
        </div>
      </section>

      <SourceList sources={S} />
    </>
  )
}
